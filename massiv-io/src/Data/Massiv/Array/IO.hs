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
-- Copyright   : (c) Alexey Kuleshevich 2018-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.IO
  ( -- * Supported Image Formats
    module Graphics.Pixel
  , Image
    -- $supported
    -- * Reading
  , readArray
  , readArrayWithMetadata
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
  , module Data.Massiv.Array.IO.Image
  -- * All other common reading/writing components
  , module Base
  ) where

import Control.Monad (void)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Massiv.Array as A
import Data.Massiv.Array.IO.Base (Image)
import Data.Massiv.Array.IO.Base as Base (Auto(..), ConvertError(..),
                                          DecodeError(..), EncodeError(..),
                                          FileFormat(..), MonadThrow(..),
                                          Readable(..), Sequence(..),
                                          Writable(..), convertEither,
                                          convertImage, decode', decodeError,
                                          defaultWriteOptions, encode',
                                          encodeError, fromMaybeDecode,
                                          fromMaybeEncode, toProxy)
import Data.Massiv.Array.IO.Image
import qualified Graphics.ColorSpace as CS
import Graphics.Pixel
import Prelude
import Prelude as P hiding (readFile, writeFile)
import System.IO (IOMode(..), hClose, openBinaryTempFile)
import UnliftIO.Concurrent (forkIO)
import UnliftIO.Exception (bracket)
import UnliftIO.IO.File
import UnliftIO.Process (readProcess)
import UnliftIO.Temporary



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



-- | Read an array from one of the supported `Readable` file formats.
--
-- @since 0.1.0
readArray :: (Readable f arr, MonadIO m) =>
             f -- ^ File format that should be used while decoding the file
          -> FilePath -- ^ Path to the file
          -> m arr
readArray format path = liftIO (B.readFile path >>= decodeM format)
{-# INLINE readArray #-}

-- | Read an array from one of the supported file formats. Some formats are capable of
-- preducing format specific metadata.
--
-- @since 0.2.0
readArrayWithMetadata ::
     (Readable f arr, MonadIO m)
  => f -- ^ File format that should be used while decoding the file
  -> FilePath -- ^ Path to the file
  -> m (arr, Metadata f)
readArrayWithMetadata format path = liftIO (B.readFile path >>= decodeWithMetadataM format)
{-# INLINE readArrayWithMetadata #-}

writeLazyAtomically :: FilePath -> BL.ByteString -> IO ()
writeLazyAtomically filepath bss =
  withBinaryFileDurableAtomic filepath WriteMode $ \h -> Prelude.mapM_ (B.hPut h) (BL.toChunks bss)
{-# INLINE writeLazyAtomically #-}

-- | Write an array to disk atomically. On UNIX operating systems writing will happen with
-- guaramtees of atomicity and durability, see `withBinaryFileDurableAtomic`.
--
-- @since 0.2.0
writeArray :: (Writable f arr, MonadIO m) =>
              f -- ^ Format to use while encoding the array
           -> WriteOptions f -- ^ Any file format related encoding options. Use `def` for default.
           -> FilePath
           -> arr
           -> m ()
writeArray format opts filepath arr =
  liftIO (encodeM format opts arr >>= writeLazyAtomically filepath)
{-# INLINE writeArray #-}


-- | Tries to guess an image format from file's extension, then attempts to decode it as
-- such. In order to supply the format manually and thus avoid this guessing technique,
-- use `readArray` instead. Color model and precision of the result image must match
-- exactly that of the actual image.
--
-- May throw `ConvertError`, `DecodeError` and other standard errors related to file IO.
--
-- Resulting image will be read as specified by the type signature:
--
-- >>> frog <- readImage "files/frog.jpg" :: IO (Image S YCbCr Word8)
-- >>> displayImage frog
--
-- In case when the result image type does not match the color space or precision of the
-- actual image file, `ConvertError` will be thrown.
--
-- >>> frog <- readImage "files/frog.jpg" :: IO (Image S CMYK Word8)
-- *** Exception: ConvertError "Cannot decode JPG image <Image S YCbCr Word8> as <Image S CMYK Word8>"
--
-- Whenever image is not in the color model or precision that we need, either use `readImageAuto` or
-- manually convert to the desired one by using the appropriate conversion functions:
--
-- >>> frog <- readImage "files/frog.jpg" :: IO (Image S YCbCr Word8)
-- >>> import Graphics.ColorSpace as CS
-- >>> let frogYCbCr = A.map CS.fromPixelBaseModel frog :: Image D (CS.YCbCr CS.SRGB) Word8
-- >>> let frogSRGB = A.map CS.convertPixel frogYCbCr :: Image D CS.SRGB Word8
-- >>> displayImage frogSRGB
--
-- A simpler approach to achieve the same effect would be to use `readImageAuto`:
--
-- >>> frogSRGB' <- readImageAuto "files/frog.jpg" :: IO (Image S CS.SRGB Word8)
-- >>> compute frogSRGB == frogSRGB'
-- True
--
-- @since 0.1.0
readImage ::
     (ColorModel cs e, MonadIO m)
  => FilePath -- ^ File path for an image
  -> m (Image S cs e)
readImage path = liftIO (B.readFile path >>= decodeImageM imageReadFormats path)
{-# INLINE readImage #-}


-- | Similar to `readImage`, but works on color spaces intead of color models and will
-- perform any possible color space conversion and precision adjustment in order to match
-- the result image type. Very useful whenever image format isn't known at compile time.
--
-- >>> import Graphics.ColorSpace as CS
-- >>> frogCMYK <- readImageAuto "files/frog.jpg" :: IO (Image S (CS.CMYK SRGB) Double)
-- >>> displayImage frogCMYK
--
-- @since 0.1.0
readImageAuto ::
     (Mutable r Ix2 (Pixel cs e), CS.ColorSpace cs i e, MonadIO m)
  => FilePath -- ^ File path for an image
  -> m (Image r cs e)
readImageAuto path = liftIO (B.readFile path >>= decodeImageM imageReadAutoFormats path)
{-# INLINE readImageAuto #-}



-- | This function will guess an output file format from the file extension and will write
-- to file any image with the color model that is supported by that format. In case that
-- automatic precision adjustment or colors space conversion is also desired,
-- `writeImageAuto` can be used instead.
--
-- Can throw `ConvertError`, `EncodeError` and other usual IO errors.
--
--
-- @since 0.1.0
writeImage ::
     (Source r Ix2 (Pixel cs e), ColorModel cs e, MonadIO m) => FilePath -> Image r cs e -> m ()
writeImage path img = liftIO (encodeImageM imageWriteFormats path img >>= writeLazyAtomically path)


-- | Write an image to file while performing all necessary precisiona and color space
-- conversions.
--
-- /Note/ - Base color space is assumed to be `SRGB`
--
-- @since 0.1.0
writeImageAuto ::
     (Source r Ix2 (Pixel cs e), CS.ColorSpace cs i e, CS.ColorSpace (CS.BaseSpace cs) i e, MonadIO m)
  => FilePath
  -> Image r cs e
  -> m ()
writeImageAuto path img =
  liftIO (encodeImageM imageWriteAutoFormats path img >>= writeLazyAtomically path)



-- | An image is written as a @.tiff@ file into an operating system's temporary
-- directory and passed as an argument to the external viewer program.
--
-- @since 0.1.0
displayImageUsing ::
     (Writable (Auto TIF) (Image r cs e), MonadIO m)
  => ExternalViewer -- ^ Image viewer program
  -> Bool -- ^ Should this function block the current thread until viewer is
          -- closed. Supplying `False` is only safe in the ghci session.
  -> Image r cs e -- ^ Image to display
  -> m ()
displayImageUsing viewer block img =
  liftIO $ do
    bs <- encodeM (Auto TIF) () img
    (if block then id else void . forkIO) $ display bs
  where
    display bs =
      withSystemTempDirectory "massiv-io" $ \tmpDir ->
        bracket
          (openBinaryTempFile tmpDir "tmp-img.tiff")
          (hClose . snd)
          (\(imgPath, imgHandle) -> do
             BL.hPut imgHandle bs
             hClose imgHandle
             displayImageFile viewer imgPath)



-- | Displays an image file by calling an external image viewer. It will block until the
-- external viewer is closed.
--
-- @since 0.1.0
displayImageFile :: MonadIO m => ExternalViewer -> FilePath -> m ()
displayImageFile (ExternalViewer exe args ix) imgPath =
  void $ liftIO $ readProcess exe (argsBefore ++ [imgPath] ++ argsAfter) ""
  where (argsBefore, argsAfter) = P.splitAt ix args


-- | Writes an image to a temporary file and makes a call to an external viewer that is
-- set as a default image viewer by the OS. This is a non-blocking function call, so it
-- might take some time before an image will appear.
--
-- /Note/ - This function should only be used in ghci, otherwise use @`displayImage`
-- `defaultViewer` `True`@
--
-- @since 0.1.0
displayImage :: (Writable (Auto TIF) (Image r cs e), MonadIO m) => Image r cs e -> m ()
displayImage = displayImageUsing defaultViewer False

-- | Default viewer is inferred from the operating system.
--
-- @since 0.1.0
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


-- | @eog \/tmp\/massiv\/img.tiff@
--
-- <https://help.gnome.org/users/eog/stable/ Eye of GNOME>
eogViewer :: ExternalViewer
eogViewer = ExternalViewer "eog" [] 0


-- | @feh --fullscreen --auto-zoom \/tmp\/massiv\/img.tiff@
--
-- <https://feh.finalrewind.org/ FEH>
fehViewer :: ExternalViewer
fehViewer = ExternalViewer "feh" ["--fullscreen", "--auto-zoom"] 2


-- | @gpicview \/tmp\/massiv\/img.tiff@
--
-- <http://lxde.sourceforge.net/gpicview/ GPicView>
gpicviewViewer :: ExternalViewer
gpicviewViewer = ExternalViewer "gpicview" [] 0


-- | @gimp \/tmp\/massiv\/img.tiff@
--
-- <https://www.gimp.org/ GIMP>
gimpViewer :: ExternalViewer
gimpViewer = ExternalViewer "gimp" [] 0


{- $supported

Encoding and decoding of images is done using
<http://hackage.haskell.org/package/JuicyPixels JuicyPixels> and
<http://hackage.haskell.org/package/netpbm netpbm> packages.

List of image formats that are currently supported, and their exact 'ColorModel's with
precision for reading and writing without any conversion:

* 'BMP':

    * __read__: ('PixelY' 'Word8'), ('PixelRGB' 'Word8'), ('PixelRGBA' 'Word8')
    * __write__: ('PixelY' 'Word8'), ('PixelRGB' 'Word8'), ('PixelRGBA' 'Word8')

* 'GIF':

    * __read__: ('PixelRGB' 'Word8'), ('PixelRGBA' 'Word8')
    * __write__: ('PixelRGB' 'Word8')
    * Also supports reading and writing animated images

* 'HDR':

    * __read__: ('PixelRGB' 'Float')
    * __write__: ('PixelRGB' 'Float')

* 'JPG':

    * __read__: ('PixelY' 'Word8'), ('PixelYA' 'Word8'), ('PixelRGB' 'Word8'), ('PixelCMYK' 'Word8'),
    ('PixelYCbCr', 'Word8')
    * __write__: ('PixelY' 'Word8'), ('PixelYA', 'Word8'), ('PixelRGB' 'Word8'), ('PixelCMYK' 'Word8'),
    ('PixelYCbCr', 'Word8')

* 'PNG':

    * __read__: ('PixelY' 'Word8'), ('PixelY' 'Word16'), ('PixelYA' 'Word8'), ('PixelYA' 'Word16'),
    ('PixelRGB' 'Word8'), ('PixelRGB' 'Word16'), ('PixelRGBA' 'Word8'), ('PixelRGBA' 'Word16')
    * __write__: ('PixelY' 'Word8'), ('PixelY' 'Word16'), ('PixelYA' 'Word8'), ('PixelYA' 'Word16'),
    ('PixelRGB' 'Word8'), ('PixelRGB' 'Word16'), ('PixelRGBA' 'Word8'), ('PixelRGBA' 'Word16')

* 'TGA':

    * __read__: ('PixelY' 'Word8'), ('PixelRGB' 'Word8'), ('PixelRGBA' 'Word8')
    * __write__: ('PixelY' 'Word8'), ('PixelRGB' 'Word8'), ('PixelRGBA' 'Word8')

* 'TIF':

    * __read__:
    ('PixelY' 'Word8'), ('PixelY' 'Word16'), ('PixelY' 'Word32'), ('PixelY' 'Float'),
    ('PixelYA' 'Word8'), ('PixelYA' 'Word16'),
    ('PixelRGB' 'Word8'), ('PixelRGB' 'Word16'), ('PixelRGBA' 'Word8'), ('PixelRGBA' 'Word16'),
    ('PixelCMYK' 'Word8'), ('PixelCMYK' 'Word16')
    * __write__:
    ('PixelY' 'Word8'), ('PixelY' 'Word16'), ('PixelY' 'Word32'), ('PixelY' 'Float'),
    ('PixelYA' 'Word8'), ('PixelYA' 'Word16'),
    ('PixelRGB' 'Word8'), ('PixelRGB' 'Word16'), ('PixelRGBA' 'Word8'), ('PixelRGBA' 'Word16')
    ('PixelCMYK' 'Word8'), ('PixelCMYK' 'Word16'), ('PixelYCbCr' 'Word8')

* 'PBM':

    * __read__: ('PixelY' 'Bit')
    * Also supports sequence of images in one file, when read as @['PBM']@

* 'PGM':

    * __read__: ('PixelY' 'Word8'), ('PixelY' 'Word16')
    * Also supports sequence of images in one file, when read as @['PGM']@

* 'PPM':

    * __read__: ('PixelRGB' 'Word8'), ('PixelRGB' 'Word16')
    * Also supports sequence of images in one file, when read as @['PPM']@

-}
