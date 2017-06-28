{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- |
-- Module      : Graphics.Image.IO
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.IO
  -- -- * Reading
  -- readImage, readImage',
  -- readImageExact, readImageExact',
  -- -- * Writing
  -- writeImage, writeImage', writeImageExact,
  -- -- * Displaying
  -- ExternalViewer(..),
  -- displayImage,
  -- displayImageUsing,
  -- -- ** Common viewers
  -- displayImageFile,
  -- defaultViewer,
  -- eogViewer,
  -- gpicviewViewer,
  -- fehViewer,
  -- gimpViewer,
  -- -- * Supported Image Formats
  -- module Graphics.Image.IO.Formats

  -- $supported

  -- * Hands on examples
  -- ** Animated GIF

  -- $animation
  where

import           Control.Concurrent         (forkIO)
import           Control.Exception          (bracket)
import           Control.Monad              (void)
--import qualified Control.Monad              as M (foldM)
import           Data.Array.Massiv
import           Data.Array.Massiv.IO.Base
import           Data.Array.Massiv.IO.Image
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
-- import           Data.Maybe                 (fromMaybe)
-- import           Data.Proxy
import           Graphics.ColorSpace
import           Prelude                    as P hiding (readFile, writeFile)
import           System.Directory           (createDirectoryIfMissing,
                                             getTemporaryDirectory)
import           System.FilePath
import           System.IO                  (hClose, openBinaryTempFile)
import           System.Process             (readProcess)

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
readArray :: Readable f arr =>
             f -- ^ File format that should be used while decoding the file
          -> ReadOptions f -- ^ Any file format related decoding options. Use `def` for default.
          -> FilePath -- ^ Path to the file
          -> IO arr
readArray format opts path = decode format opts <$> B.readFile path
{-# INLINE readArray #-}


writeArray :: Writable f arr =>
              f -- ^ Format to use while encoding the array
           -> WriteOptions f -- ^ Any file format related encoding options. Use `def` for default.
           -> FilePath
           -> arr -> IO ()
writeArray format opts path arr = BL.writeFile path (encode format opts arr)
{-# INLINE writeArray #-}


-- | Try to guess an image format from file's extension, then attempt to decode
-- it as such. In order to supply the format manually and thus avoid this
-- guessing technique, use `readArray` instead. Color space and precision of the
-- result array must match exactly that of the actual image, in order to apply
-- auto conversion use `readImageAuto` instead.
--
-- Might throw `ConvertError`, `DecodeError` and other standard errors related
-- to file IO.
--
-- Result image will be read as specified by the type signature:
--
-- >>> frog :: Image S YCbCr Word8 <- readImage "files/frog.jpg"
-- >>> displayImage frog
--
-- In case when the result image type does not match the color space or
-- precision of the actual image file, `ConvertError` will be thrown.
--
-- >>> frog :: Image S YCbCr Double <- readImage "files/frog.jpg"
-- >>> frog `seq` ()
-- *** Exception: ConvertError "Cannot decode JPG image <Image S YCbCr Word8> as <Image S YCbCr Double>"
--
readImage :: (Source S DIM2 (Pixel cs e), ColorSpace cs e) =>
              FilePath -- ^ File path for an image
           -> IO (Image S cs e)
readImage path = decodeImage imageReadFormats path <$> B.readFile path
{-# INLINE readImage #-}


-- | Same as `readImage`, but will perform any possible color space and
-- precision conversions in order to match the result image type. Very useful
-- whenever image format isn't known at compile time.
readImageAuto :: (Target r DIM2 (Pixel cs e), ColorSpace cs e) =>
                  FilePath -- ^ File path for an image
               -> IO (Image r cs e)
readImageAuto path = decodeImage imageReadAutoFormats path <$> B.readFile path
{-# INLINE readImageAuto #-}



-- | Inverse of the 'readImage', but similarly to it, will guess an output file
-- format from the file extension and will write to file any image with the
-- color space that is supported by that format. Precision of the image might be
-- adjusted using `Elevator` if precision of the source array is not supported
-- by the image file format. For instance, <'Image' @r@ 'RGBA' 'Double'> being
-- saved as 'PNG' file would be written as <'Image' @r@ 'RGBA' 'Word16'>, thus
-- using highest supported precision 'Word16' for that format. If automatic
-- colors space is also desired, `writeImageAuto` can be used instead.
--
-- Might throw `ConvertError`, `EncodeError` and other standard errors related
-- to file IO.
--
writeImage :: (Source r DIM2 (Pixel cs e), ColorSpace cs e) =>
               FilePath -> Image r cs e -> IO ()
writeImage path = BL.writeFile path . encodeImage imageWriteFormats path



writeImageAuto
  :: ( Source r DIM2 (Pixel cs e)
     , ColorSpace cs e
     , ToYA cs e
     , ToRGBA cs e
     , ToYCbCr cs e
     , ToCMYK cs e
     )
  => FilePath -> Image r cs e -> IO ()
writeImageAuto path = BL.writeFile path . encodeImage imageWriteAutoFormats path




-- -- | Just like `readImage`, but will throw an exception if incorrect format is
-- -- detected.
-- readImage' :: Readable (Image S cs e) InputFormat =>
--               FilePath -> IO (Image S cs e)
-- readImage' path = either error id <$> readImage path
-- {-# INLINE readImage' #-}


-- -- | This function allows for reading all supported image in their exact
-- -- colorspace and precision. Only `S` image representation can be read
-- -- natively, but `Graphics.Image.exchange` can be use later to switch to a
-- -- different representation. For instance, "frog.jpg" image can be read into
-- -- it's 'Graphics.Image.ColorSpace.YCbCr' colorspace with
-- -- 'Graphics.Image.ColorSpace.Word8' precision:
-- --
-- -- >>> readImageExact JPG "images/frog.jpg" :: IO (Either String (Image S YCbCr Word8))
-- -- Right <Image S YCbCr (Word8): 200x320>
-- --
-- -- The drawback here is that colorspace and precision has to match exactly,
-- -- otherwise it will return an error:
-- --
-- -- >>> readImageExact JPG "images/frog.jpg" :: IO (Either String (Image S RGB Word8))
-- -- Left "JuicyPixel decoding error: Input image is in YCbCr8 (Pixel YCbCr Word8), cannot convert it to RGB8 (Pixel RGB Word8) colorspace."
-- --
-- -- Any attempt to read an image in a color space, which is not supported by
-- -- supplied format, will result in a compile error. Refer to 'Readable' class
-- -- for all images that can be decoded.
-- readImageExact :: Readable img format =>
--                   format
--                   -- ^ A file format that an image should be read as. See
--                    -- <#g:4 Supported Image Formats>
--                -> FilePath -- ^ Location of an image.
--                -> IO (Either String img)
-- readImageExact format path = fmap (decode format) (B.readFile path)
-- {-# INLINE readImageExact #-}


-- -- | Just like `readImageExact`, but will throw an exception if incorrect format
-- -- is detected.
-- readImageExact' :: Readable b format => format -> FilePath -> IO b
-- readImageExact' format path = either error id <$> readImageExact format path
-- {-# INLINE readImageExact' #-}





-- -- | Write an image in a specific format, while supplying any format specific
-- -- options. Precision and color space, that an image will be written as, is decided
-- -- from image's type. Attempt to write image file in a format that does not
-- -- support color space and precision combination will result in a compile error.
-- writeImageExact :: Writable img format =>
--                    format
--                    -- ^ A file format that an image should be saved in. See
--                    -- <#g:4 Supported Image Formats>
--                 -> [SaveOption format] -- ^ A list of format specific options.
--                 -> FilePath -- ^ Location where an image should be written.
--                 -> img -- ^ An image to write. Can be a list of images in case
--                        -- of formats supporting animation.
--                 -> IO ()
-- writeImageExact format opts path = BL.writeFile path . encode format opts
-- {-# INLINE writeImageExact #-}


-- | An image is written as a @.tiff@ file into an operating system's temporary
-- directory and passed as an argument to the external viewer program.
-- displayImageUsing :: Writable (Auto TIF) (Image r cs e) =>
--                      ExternalViewer -- ^ Image viewer program
--                   -> Bool -- ^ Should a call block the cuurrent thread untul viewer is closed.
--                   -> Image r cs e -> IO ()
displayImageUsing :: Writable (Auto TIF) (Image r cs e) =>
                     ExternalViewer -- ^ Image viewer program
                  -> Bool -- ^ Should a call block the cuurrent thread untul viewer is closed.
                  -> Image r cs e -> IO ()
displayImageUsing viewer block img =
  if block
    then display
    else void $ forkIO display
  where
    display = do
      tmpDir <- fmap (</> "hip") getTemporaryDirectory
      createDirectoryIfMissing True tmpDir
      bracket
        (openBinaryTempFile tmpDir "tmp-img.tiff")
        (hClose . snd)
        (\(imgPath, imgHandle) -> do
           BL.hPut imgHandle (encode (Auto TIF) () img)
           hClose imgHandle
           displayImageFile viewer imgPath)



-- | Displays an image file by calling an external image viewer.
displayImageFile :: ExternalViewer -> FilePath -> IO ()
displayImageFile (ExternalViewer exe args ix) imgPath =
  void $ readProcess exe (argsBefore ++ [imgPath] ++ argsAfter) ""
  where (argsBefore, argsAfter) = P.splitAt ix args


-- | Makes a call to an external viewer that is set as a default image viewer by
-- the OS. This is a non-blocking function call, so it might take some time
-- before an image will appear.
displayImage :: Writable (Auto TIF) (Image r cs e) => Image r cs e -> IO ()
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
    * Also supports reading and writing animated images, when used as @'GIFA'@

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

    * __read__: ('Y' 'Word8'), ('Y' 'Word16'), ('YA' 'Word8'), ('YA' 'Word16'),
    ('RGB' 'Word8'), ('RGB' 'Word16'), ('RGBA' 'Word8'), ('RGBA' 'Word16'),
    ('CMYK' 'Word8'), ('CMYK' 'Word16')
    * __write__: ('Y' 'Word8'), ('Y' 'Word16'), ('YA' 'Word8'), ('YA' 'Word16'),
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



{- $animation

JuicyPixels is capable of encoding/decoding all sorts of poular formats, one of
which is animated GIFs. Here I would like to present a short demonstration on
how it is possible to work with image seqences.

<<images/downloaded/strawberry.gif>>

So, we download and image, but it's a little bit too big, and it's in RGBA
colorspace.

* Read an animated GIF as a list of images:

>>> imgs <- readImageExact' GIFA "images/downloaded/strawberry.gif" :: IO [(GifDelay, Image S RGBA Word8)]

* convert to `RGB` colorspace by dropping alpha channel and increasing precision,
since we cannot write GIFs in RGBA colorspace:

>>> let imgsRGB = fmap (fmap toImageRGB) imgs

* if `toImageRGB` hadn't increased the precision to `Double` in the previous
  step, `Bilinear` interpolation would have simply destroyed the image quality
  in this step. Scale all images in the sequence by a half:

>>> let imgsRGBsmall = fmap (fmap (scale Bilinear Edge (0.5, 0.5))) imgsRGB

* Here we save the sequence as a new animated image. We don't need to drop
  precision back to `Word8`, it will be taken care for us:

>>> writeImageExact GIFA [GIFALooping LoopingForever] "images/strawberry.gif" imgsRGBsmall

* Now lets extend the animation a bit:

>>> writeImageExact GIFA [GIFALooping LoopingForever] "images/strawberry_backwards.gif" (imgsRGBsmall ++ reverse imgsRGBsmall)

<<images/strawberry.gif>> <<images/strawberry_backwards.gif>>

-}
