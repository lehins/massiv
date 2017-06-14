{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- |
-- Module      : Graphics.Image.IO
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.IO (
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
  ) where

import           Control.Concurrent         (forkIO)
import           Control.Exception          (bracket)
import           Control.Monad              (guard, msum, void)
import qualified Control.Monad              as M (foldM)
import qualified Data.ByteString            as B (readFile)
import qualified Data.ByteString.Lazy       as BL (hPut, writeFile)
import           Data.Char                  (toLower)
import           Data.Maybe                 (fromMaybe)

import           Data.Array.Massiv
import           Data.Array.Massiv.IO.Base
import           Data.Array.Massiv.IO.Image
import           Data.Proxy
import           Graphics.ColorSpace
import           Prelude                    as P hiding (readFile, writeFile)
import           System.Directory           (createDirectoryIfMissing,
                                             getTemporaryDirectory)
import           System.FilePath            (takeExtension, (</>))
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


guessFormat :: (FileFormat f, Enum f) => FilePath -> Either String f
guessFormat path =
  maybe (Left $ "Could not deduce format from file name: " ++ path) Right $
  headMaybe . dropWhile (not . isFormat e) . enumFrom . toEnum $ 0
  where
    e = P.map toLower . takeExtension $ path
    headMaybe ls =
      if P.null ls
        then Nothing
        else Just $ head ls


-- checkFormat :: FileFormat f => f -> FilePath -> Bool
-- checkFormat f path = isFormat e f
--   where e = P.map toLower . takeExtension $ path
--         headMaybe ls = if P.null ls then Nothing else Just $ head ls

-- getReadbleFormat :: Readable f arr => Proxy arr -> FilePath -> Maybe f
-- getReadbleFormat _ path =
--   msum
--     [ do guard (checkFormat BMP path)
--          return BMP
--     , do guard (checkFormat PNG path)
--          return PNG
--     ]


instance FileFormat ImageFormat where

  ext ImageBMP = ext BMP
  ext ImageGIF = ext GIF
  -- ext ImageHDR = ext HDR
  -- ext ImageJPG = ext JPG
  ext ImagePNG = ext PNG
  -- ext ImageTGA = ext TGA
  -- ext ImageTIF = ext TIF
  -- ext ImagePNM = ext PPM

  exts ImageBMP = exts BMP
  exts ImageGIF = exts GIF
  -- exts ImageHDR = exts HDR
  -- exts ImageJPG = exts JPG
  exts ImagePNG = exts PNG
  -- exts ImageTGA = exts TGA
  -- exts ImageTIF = exts TIF
  -- exts ImagePNM = [ext PBM, ext PGM, ext PPM]

data ImageFormat
  = ImageBMP
  | ImagePNG
  | ImageGIF
  deriving (Enum, Show)

instance (Storable (Pixel cs e), ColorSpace cs e) => Readable ImageFormat (Image S cs e) where

  decode ImageBMP _ = decode (Keen BMP) ()
  decode ImagePNG _ = decode (Keen PNG) ()


instance (Storable (Pixel cs e), ToRGBA cs e, ToYA cs e, ColorSpace cs e) =>
  Writable ImageFormat (Image S cs e) where

  encode ImageBMP _ = encode BMP ()
  encode ImagePNG _ = encode PNG ()

-- | This function will try to guess an image format from file's extension,
-- then it will attempt to decode it as such. It will fall back onto the rest of
-- the supported formats and will try to read them regarless of file's
-- extension. Whenever image cannot be decoded, 'Left' containing all errors for
-- each attempted format will be returned, and 'Right' containing an image
-- otherwise. Image will be read with a type signature specified:
--
--  >>> frog <- readImage "images/frog.jpg" :: IO (Either String (Image S RGB Word8))
--  >>> displayImage frog
--
readImage :: Readable ImageFormat (Image S cs e) =>
             FilePath -- ^ File path for an image
          -> IO (Either String (Image S cs e))
readImage path = do
  bs <- B.readFile path
  return $ do
    f :: ImageFormat <- guessFormat path
    decode f () bs
{-# INLINE readImage #-}




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


-- | Just like 'readImage', this function will guess an output file format from the
-- extension and write to file any image that is in one of 'Y', 'YA', 'RGB' or
-- 'RGBA' color spaces with 'Double' precision. While doing necessary
-- conversions the choice will be given to the most suited color space supported
-- by the format. For instance, in case of a 'PNG' format, an ('Image' @arr@
-- 'RGBA' 'Double') would be written as @RGBA16@, hence preserving transparency
-- and using highest supported precision 'Word16'. At the same time, writing
-- that image in 'GIF' format would save it in @RGB8@, since 'Word8' is the
-- highest precision 'GIF' supports and it currently cannot be saved with
-- transparency.
writeImage :: (Writable ImageFormat (Image r cs e)) =>
              FilePath     -- ^ Location where an image should be written.
           -> Image r cs e -- ^ An image to write.
           -> IO ()
writeImage path img = do
  let eEncoded = do
        f :: ImageFormat <- guessFormat path
        encode f () img
  case eEncoded of
    Left err -> error err
    Right bs -> BL.writeFile path bs
{-# INLINE writeImage #-}


-- writeImage'
--   :: Array2D r cs e
--   => FilePath -- ^ Location where an image should be written.
--   -> Image r cs e -- ^ An image to write.
--   -> IO ()
-- writeImage' path img =
--   case mEncoded of
--     Just encoded -> BL.writeFile path encoded
--     Nothing ->
--       error $
--       "writeImage: Could not guess output format. Use 'writeImageExact' " ++
--       "or supply a filename with supported format."
--   where
--     mEncoded = do
--       msum
--         [ do guard (checkFormat BMP path)
--              return $ encodeBMP img
--         , do guard (checkFormat PNG path)
--              return $ encodePNG img
--         ]
-- {-# INLINE writeImage' #-}

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


-- -- | An image is written as a @.tiff@ file into an operating system's temporary
-- -- directory and passed as an argument to the external viewer program.
-- displayImageUsing :: (Array2D r cs e, Storable (Pixel cs e),
--                       Writable (Image S cs e) TIF) =>
--                      ExternalViewer -- ^ External viewer to use
--                   -> Bool -- ^ Should the call be blocking
--                   -> Image r cs e -- ^ Image to display
--                   -> IO ()
-- displayImageUsing viewer block img = do
--   let display = do
--         tmpDir <- fmap (</> "hip") getTemporaryDirectory
--         createDirectoryIfMissing True tmpDir
--         bracket (openBinaryTempFile tmpDir "tmp-img.tiff")
--           (hClose . snd)
--           (\ (imgPath, imgHandle) -> do
--               BL.hPut imgHandle $ encode TIF [] $ exchange S img
--               hClose imgHandle
--               displayImageFile viewer imgPath)
--   if block
--     then display
--     else void $ forkIO display



-- -- | Displays an image file by calling an external image viewer.
-- displayImageFile :: ExternalViewer -> FilePath -> IO ()
-- displayImageFile (ExternalViewer exe args ix) imgPath =
--   void $ readProcess exe (argsBefore ++ [imgPath] ++ argsAfter) ""
--   where (argsBefore, argsAfter) = splitAt ix args


-- -- | Makes a call to an external viewer that is set as a default image viewer by
-- -- the OS. This is a non-blocking function call, so it might take some time
-- -- before an image will appear.
-- displayImage :: (Array2D r cs e, Storable (Pixel cs e),
--                  Writable (Image S cs e) TIF) =>
--                 Image r cs e -- ^ Image to be displayed
--              -> IO ()
-- displayImage = displayImageUsing defaultViewer False

-- -- | Default viewer is inferred from the operating system.
-- defaultViewer :: ExternalViewer
-- defaultViewer =
-- #if defined(OS_Win32)
--   (ExternalViewer "explorer.exe" [] 0)
-- #elif defined(OS_Linux)
--   (ExternalViewer "xdg-open" [] 0)
-- #elif defined(OS_Mac)
--   (ExternalViewer "open" [] 0)
-- #else
--   error "Graphics.Image.IO.defaultViewer: Could not determine default viewer."
-- #endif


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
