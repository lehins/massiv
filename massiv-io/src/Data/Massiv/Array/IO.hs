{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |
-- Module      : Data.Massiv.Array.IO
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.IO
  ( -- $supported
    -- * Reading
    readArray
  , readImage
  , readImageAuto
  -- * Writing
  , writeArray
  , writeImage
  , writeImageAuto
  -- * Displaying
  , ExternalViewer(..)
  , displayImage
  , displayImageUsing
  , displayImageFile
  -- ** Common viewers
  , defaultViewer
  , eogViewer
  , gpicviewViewer
  , fehViewer
  , gimpViewer
  -- * Supported Image Formats
  , module Data.Massiv.Array.IO.Base
  , module Data.Massiv.Array.IO.Image
  ) where

import Prelude
import Control.Concurrent (forkIO)
import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.IO.Unlift
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Massiv.Array as A
import Data.Massiv.Array.IO.Base hiding (convertEither, fromEitherDecode,
                                  fromMaybeEncode, toProxy)
import Data.Massiv.Array.IO.Image
import Graphics.ColorSpace
import Prelude as P hiding (readFile, writeFile)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.FilePath
import System.IO (hClose, openBinaryTempFile, IOMode(..))
import System.Process (readProcess)
import UnliftIO.IO.File



-- | External viewing application to use for displaying images.
data ExternalViewer =
  ExternalViewer FilePath [String] Int
    -- ^ Any custom viewer, which can be specified:
    --
    -- * @FilePath@ - to the actual viewer executable.
    -- * @[String]@ - command line arguments that will be passed to the executable.
    -- * @Int@ - position index in the above list where `FilePath` to an image should be
    -- injected
  deriving Show



-- | Read an array from one of the supported file formats.
readArray :: (Readable f arr, MonadIO m) =>
             f -- ^ File format that should be used while decoding the file
          -> ReadOptions f -- ^ Any file format related decoding options. Use `def` for default.
          -> FilePath -- ^ Path to the file
          -> m arr
readArray format opts path = liftIO $ do
  bs <- B.readFile path
  fst <$> decodeM format opts bs
{-# INLINE readArray #-}

writeLazyAtomically :: FilePath -> BL.ByteString -> IO ()
writeLazyAtomically filepath bss =
  withBinaryFileDurableAtomic filepath WriteMode $ \h -> Prelude.mapM_ (B.hPut h) (BL.toChunks bss)

-- | Write an array to disk atomically (on non-Windows OSs)
writeArray :: (Writable f arr, MonadIO m) =>
              f -- ^ Format to use while encoding the array
           -> WriteOptions f -- ^ Any file format related encoding options. Use `def` for default.
           -> FilePath
           -> arr
           -> m ()
writeArray format opts filepath arr =
  liftIO $ do
    writeLazyAtomically filepath =<< encodeM format opts arr
{-# INLINE writeArray #-}


-- | Try to guess an image format from file's extension, then attempt to decode it as such. In order
-- to supply the format manually and thus avoid this guessing technique, use `readArray`
-- instead. Color space and precision of the result array must match exactly that of the actual
-- image, in order to apply auto conversion use `readImageAuto` instead.
--
-- Might throw `ConvertError`, `DecodeError` and other standard errors related to file IO.
--
-- Result image will be read as specified by the type signature:
--
-- >>> frog <- readImage "files/frog.jpg" :: IO (Image S YCbCr Word8)
-- >>> displayImage frog
--
-- In case when the result image type does not match the color space or precision of the actual
-- image file, `ConvertError` will be thrown.
--
-- >>> frog <- readImage "files/frog.jpg" :: IO (Image S CMYK Word8)
-- >>> displayImage frog
-- *** Exception: ConvertError "Cannot decode JPG image <Image S YCbCr Word8> as <Image S CMYK Word8>"
--
-- Whenever image is not in the color space or precision that we need, either use `readImageAuto` or
-- manually convert to the desired one by using the appropriate conversion functions:
--
-- >>> frogCMYK <- readImageAuto "files/frog.jpg" :: IO (Image S CMYK Double)
-- >>> displayImage frogCMYK
--
readImage :: (Source S Ix2 (Pixel cs e), ColorSpace cs e, MonadIO m) =>
              FilePath -- ^ File path for an image
           -> m (Image S cs e)
readImage path =
  liftIO $ do
    bs <- B.readFile path
    fst <$> decodeImageM imageReadFormats path bs
{-# INLINE readImage #-}


-- | Same as `readImage`, but will perform any possible color space and
-- precision conversions in order to match the result image type. Very useful
-- whenever image format isn't known at compile time.
readImageAuto :: (Mutable r Ix2 (Pixel cs e), ColorSpace cs e, MonadIO m) =>
                  FilePath -- ^ File path for an image
               -> m (Image r cs e)
readImageAuto path = liftIO $ do
  bs <- B.readFile path
  fst <$> decodeImageM imageReadAutoFormats path bs
{-# INLINE readImageAuto #-}



-- | This function will guess an output file format from the file extension and will write to file
-- any image with the colorspace that is supported by that format. Precision of the image might be
-- adjusted using `Elevator` if precision of the source array is not supported by the image file
-- format. For instance an @(`Image` r `RGBA` `Double`)@ being saved as `PNG` file would be written as
-- @(`Image` r `RGBA` `Word16`)@, thus using highest supported precision `Word16` for that
-- format. If automatic colors space is also desired, `writeImageAuto` can be used instead.
--
-- Can throw `ConvertError`, `EncodeError` and other usual IO errors.
--
writeImage :: (Source r Ix2 (Pixel cs e), ColorSpace cs e, MonadIO m) =>
               FilePath -> Image r cs e -> m ()
writeImage path img = liftIO $ do
  writeLazyAtomically path =<< encodeImageM imageWriteFormats path img


-- | Write an image to file while performing all necessary precisiona and color space conversions.
writeImageAuto
  :: ( Source r Ix2 (Pixel cs e)
     , ColorSpace cs e
     , ToYA cs e
     , ToRGBA cs e
     , ToYCbCr cs e
     , ToCMYK cs e
     , MonadIO m
     )
  => FilePath -> Image r cs e -> m ()
writeImageAuto path img = liftIO $ do
  writeLazyAtomically path =<< encodeImageM imageWriteAutoFormats path img



-- | An image is written as a @.tiff@ file into an operating system's temporary
-- directory and passed as an argument to the external viewer program.
displayImageUsing ::
     (Writable (Auto TIF) (Image r cs e), MonadIO m)
  => ExternalViewer -- ^ Image viewer program
  -> Bool -- ^ Should this function block the current thread until viewer is closed.
  -> Image r cs e -- ^ Image to display
  -> m ()
displayImageUsing viewer block img =
  liftIO $ do
    bs <- encodeM (Auto TIF) () img
    if block
      then display bs
      else void (forkIO (display bs))
  where
    display bs = do
      tmpDir <- fmap (</> "massiv-io") getTemporaryDirectory
      createDirectoryIfMissing True tmpDir
      bracket
        (openBinaryTempFile tmpDir "tmp-img.tiff")
        (hClose . snd)
        (\(imgPath, imgHandle) -> do
           BL.hPut imgHandle bs
           hClose imgHandle
           displayImageFile viewer imgPath)



-- | Displays an image file by calling an external image viewer.
displayImageFile :: MonadIO m => ExternalViewer -> FilePath -> m ()
displayImageFile (ExternalViewer exe args ix) imgPath =
  void $ liftIO $ readProcess exe (argsBefore ++ [imgPath] ++ argsAfter) ""
  where (argsBefore, argsAfter) = P.splitAt ix args


-- | Makes a call to an external viewer that is set as a default image viewer by
-- the OS. This is a non-blocking function call, so it might take some time
-- before an image will appear.
displayImage :: (Writable (Auto TIF) (Image r cs e), MonadIO m) => Image r cs e -> m ()
displayImage = displayImageUsing defaultViewer False

-- | Default viewer is inferred from the operating system.
defaultViewer :: ExternalViewer
defaultViewer =
#if defined(OS_Win32)
  ExternalViewer "explorer.exe" [] 0
#elif defined(OS_Linux)
  ExternalViewer "xdg-open" [] 0
#elif defined(OS_Mac)
  ExternalViewer "open" [] 0
#else
  error "Graphics.Image.IO.defaultViewer: Could not determine default viewer."
#endif


-- | @eog \/tmp\/hip\/img.tiff@
--
-- <https://help.gnome.org/users/eog/stable/ Eye of GNOME>
eogViewer :: ExternalViewer
eogViewer = ExternalViewer "eog" [] 0


-- | @feh --fullscreen --auto-zoom \/tmp\/hip\/img.tiff@
--
-- <https://feh.finalrewind.org/ FEH>
fehViewer :: ExternalViewer
fehViewer = ExternalViewer "feh" ["--fullscreen", "--auto-zoom"] 2


-- | @gpicview \/tmp\/hip\/img.tiff@
--
-- <http://lxde.sourceforge.net/gpicview/ GPicView>
gpicviewViewer :: ExternalViewer
gpicviewViewer = ExternalViewer "gpicview" [] 0


-- | @gimp \/tmp\/hip\/img.tiff@
--
-- <https://www.gimp.org/ GIMP>
gimpViewer :: ExternalViewer
gimpViewer = ExternalViewer "gimp" [] 0


{- $supported

Encoding and decoding of images is done using
<http://hackage.haskell.org/package/JuicyPixels JuicyPixels> and
<http://hackage.haskell.org/package/netpbm netpbm> packages.

List of image formats that are currently supported, and their exact
'ColorSpace's and precision for reading and writing without an implicit
conversion:

* 'BMP':

    * __read__: ('Y' 'Word8'), ('RGB' 'Word8'), ('RGBA' 'Word8')
    * __write__: ('Y' 'Word8'), ('RGB' 'Word8'), ('RGBA' 'Word8')

* 'GIF':

    * __read__: ('RGB' 'Word8'), ('RGBA' 'Word8')
    * __write__: ('RGB' 'Word8')
    * Also supports reading and writing animated images

* 'HDR':

    * __read__: ('RGB' 'Float')
    * __write__: ('RGB' 'Float')

* 'JPG':

    * __read__: ('Y' 'Word8'), ('YA' 'Word8'), ('RGB' 'Word8'), ('CMYK' 'Word8'),
    ('YCbCr', 'Word8')
    * __write__: ('Y' 'Word8'), ('YA', 'Word8'), ('RGB' 'Word8'), ('CMYK' 'Word8'),
    ('YCbCr', 'Word8')

* 'PNG':

    * __read__: ('Y' 'Word8'), ('Y' 'Word16'), ('YA' 'Word8'), ('YA' 'Word16'),
    ('RGB' 'Word8'), ('RGB' 'Word16'), ('RGBA' 'Word8'), ('RGBA' 'Word16')
    * __write__: ('Y' 'Word8'), ('Y' 'Word16'), ('YA' 'Word8'), ('YA' 'Word16'),
    ('RGB' 'Word8'), ('RGB' 'Word16'), ('RGBA' 'Word8'), ('RGBA' 'Word16')

* 'TGA':

    * __read__: ('Y' 'Word8'), ('RGB' 'Word8'), ('RGBA' 'Word8')
    * __write__: ('Y' 'Word8'), ('RGB' 'Word8'), ('RGBA' 'Word8')

* 'TIF':

    * __read__:
    ('Y' 'Word8'), ('Y' 'Word16'), ('Y' 'Word32'), ('Y' 'Float'),
    ('YA' 'Word8'), ('YA' 'Word16'),
    ('RGB' 'Word8'), ('RGB' 'Word16'), ('RGBA' 'Word8'), ('RGBA' 'Word16'),
    ('CMYK' 'Word8'), ('CMYK' 'Word16')
    * __write__:
    ('Y' 'Word8'), ('Y' 'Word16'), ('Y' 'Word32'), ('Y' 'Float'),
    ('YA' 'Word8'), ('YA' 'Word16'),
    ('RGB' 'Word8'), ('RGB' 'Word16'), ('RGBA' 'Word8'), ('RGBA' 'Word16')
    ('CMYK' 'Word8'), ('CMYK' 'Word16'), ('YCbCr' 'Word8')

* 'PBM':

    * __read__: ('Binary' 'Bit')
    * Also supports sequence of images in one file, when read as @['PBM']@

* 'PGM':

    * __read__: ('Y' 'Word8'), ('Y' 'Word16')
    * Also supports sequence of images in one file, when read as @['PGM']@

* 'PPM':

    * __read__: ('RGB' 'Word8'), ('RGB' 'Word16')
    * Also supports sequence of images in one file, when read as @['PPM']@

-}
