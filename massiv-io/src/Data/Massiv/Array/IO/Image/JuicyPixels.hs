{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module      : Data.Massiv.Array.IO.Image.JuicyPixels
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.IO.Image.JuicyPixels
  ( -- * JuicyPixels formats
    -- ** BMP
    BMP(..)
    -- ** GIF
  , GIF(..)
  , WriteOptionsGIF
  , woGetPaletteOptionsGIF
  , woSetPaletteOptionsGIF
  , JP.PaletteOptions(..)
  , JP.PaletteCreationMethod(..)
    -- *** Animated
  , WriteOptionsSequenceGIF
  , woGetGifLoopingGIFs
  , woGetPaletteOptionsGIFs
  , woSetGifLoopingGIFs
  , woSetPaletteOptionsGIFs
  , JP.GifDelay
  , JP.GifLooping(..)
  -- ** HDR
  , HDR(..)
  -- ** JPG
  , JPG(..)
  , WriteOptionsJPG
  , woGetQualityJPG
  , woSetQualityJPG
  -- ** PNG
  , PNG(..)
  -- ** TGA
  , TGA(..)
  -- ** TIF
  , TIF(..)
  -- * JuciyPixels conversion
  -- ** To JuicyPixels
  -- O(1) Conversion to JuicyPixels images
  , toAnyCS
  , toJPImageY8
  , toJPImageYA8
  , toJPImageY16
  , toJPImageYA16
  , toJPImageYF
  , toJPImageRGB8
  , toJPImageRGBA8
  , toJPImageRGB16
  , toJPImageRGBA16
  , toJPImageRGBF
  , toJPImageYCbCr8
  , toJPImageCMYK8
  , toJPImageCMYK16
  -- ** From JuicyPixels
  , fromDynamicImage
  , fromAnyDynamicImage
  ) where

import Prelude as P

import qualified Codec.Picture as JP
import qualified Codec.Picture.ColorQuant as JP
import qualified Codec.Picture.Gif as JP
import qualified Codec.Picture.Jpg as JP
import qualified Codec.Picture.Types as TypesJP
import Control.Monad (guard, msum, unless)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL (ByteString)
import Data.Default.Class (Default(..))
import Data.Massiv.Array as M
import Data.Massiv.Array.IO.Base
import Data.Massiv.Array.Manifest.Vector
import Data.Typeable
import qualified Data.Vector.Storable as V
import Foreign.Storable (Storable(sizeOf))
import Graphics.ColorSpace

#if !MIN_VERSION_massiv(0, 2, 7)
pattern Sz :: ix -> ix
pattern Sz ix = ix
type Sz ix = ix
#endif
#if !MIN_VERSION_massiv(0, 3, 0)
fromVectorM ::
     (Construct S ix a, Mutable S ix a, Storable a)
  => Comp
  -> Sz ix
  -> V.Vector a
  -> Maybe (Array S ix a)
fromVectorM comp sz = pure . fromVector comp sz
#endif

--------------------------------------------------------------------------------
-- BMP Format ------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Bitmap image with @.bmp@ extension.
data BMP = BMP deriving Show

instance FileFormat BMP where

  ext _ = ".bmp"


instance (ColorSpace cs e, Source r Ix2 (Pixel cs e)) =>
         Writable BMP (Image r cs e) where
  encodeM f _ img = fromMaybeEncode f (toProxy img) $ encodeBMP img

instance (ColorSpace cs e, ToRGBA cs e, Source r Ix2 (Pixel cs e)) =>
         Writable (Auto BMP) (Image r cs e) where
  encodeM f _ = encodeAuto f encodeBMP id toPixelRGBA toPixelRGB toPixelRGBA


instance ColorSpace cs e => Readable BMP (Image S cs e) where
  decodeM f _ = fromEitherDecode_ f showJP fromDynamicImage . JP.decodeBitmap


instance (Mutable r Ix2 (Pixel cs e), ColorSpace cs e) =>
         Readable (Auto BMP) (Image r cs e) where
  decodeM f _ = fromEitherDecode_ f showJP fromAnyDynamicImage . JP.decodeBitmap


encodeBMP :: forall r cs e . (ColorSpace cs e, Source r Ix2 (Pixel cs e))
          => Image r cs e -> Maybe BL.ByteString
encodeBMP img =
  msum
    [ do Refl <- eqT :: Maybe (cs :~: Y)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ JP.encodeBitmap $ toJPImageY8 img
           , pure $ JP.encodeBitmap $ toJPImageY8 $ M.map toWord8 img
           ]
    , do Refl <- eqT :: Maybe (cs :~: RGB)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ JP.encodeBitmap $ toJPImageRGB8 img
           , pure $ JP.encodeBitmap $ toJPImageRGB8 $ M.map toWord8 img
           ]
    , do Refl <- eqT :: Maybe (cs :~: RGBA)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ JP.encodeBitmap $ toJPImageRGBA8 img
           , pure $ JP.encodeBitmap $ toJPImageRGBA8 $ M.map toWord8 img
           ]
    ]


--------------------------------------------------------------------------------
-- PNG Format ------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Portable Network Graphics image with @.png@ extension.
data PNG = PNG deriving Show

instance FileFormat PNG where

  ext _ = ".png"

instance (ColorSpace cs e, Source r Ix2 (Pixel cs e)) =>
         Writable PNG (Image r cs e) where
  encodeM f _ img = fromMaybeEncode f (toProxy img) (encodePNG img)


instance (ColorSpace cs e, ToYA cs e, ToRGBA cs e, Source r Ix2 (Pixel cs e)) =>
         Writable (Auto PNG) (Image r cs e) where
  encodeM f _ = encodeAuto f encodePNG id toPixelYA toPixelRGB toPixelRGBA


instance ColorSpace cs e => Readable PNG (Image S cs e) where
  decodeM f _ = fromEitherDecode_ f showJP fromDynamicImage . JP.decodePng

instance (Mutable r Ix2 (Pixel cs e), ColorSpace cs e) =>
         Readable (Auto PNG) (Image r cs e) where
  decodeM f _ = fromEitherDecode_ f showJP fromAnyDynamicImage . JP.decodePng



encodePNG :: forall r cs e. (ColorSpace cs e, Source r Ix2 (Pixel cs e))
          => Image r cs e -> Maybe BL.ByteString
encodePNG img =
  msum
    [ do Refl <- eqT :: Maybe (cs :~: Y)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ JP.encodePng $ toJPImageY8 img
           , do Refl <- eqT :: Maybe (e :~: Word16)
                pure $ JP.encodePng $ toJPImageY16 img
           , pure $ JP.encodePng $ toJPImageY16 $ M.map toWord16 img
           ]
    , do Refl <- eqT :: Maybe (cs :~: YA)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ JP.encodePng $ toJPImageYA8 img
           , do Refl <- eqT :: Maybe (e :~: Word16)
                pure $ JP.encodePng $ toJPImageYA16 img
           , pure $ JP.encodePng $ toJPImageYA16 $ M.map toWord16 img
           ]
    , do Refl <- eqT :: Maybe (cs :~: RGB)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ JP.encodePng $ toJPImageRGB8 img
           , do Refl <- eqT :: Maybe (e :~: Word16)
                pure $ JP.encodePng $ toJPImageRGB16 img
           , pure $ JP.encodePng $ toJPImageRGB16 $ M.map toWord16 img
           ]
    , do Refl <- eqT :: Maybe (cs :~: RGBA)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ JP.encodePng $ toJPImageRGBA8 img
           , do Refl <- eqT :: Maybe (e :~: Word16)
                pure $ JP.encodePng $ toJPImageRGBA16 img
           , pure $ JP.encodePng $ toJPImageRGBA16 $ M.map toWord16 img
           ]
    ]



--------------------------------------------------------------------------------
-- GIF Format ------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Graphics Interchange Format image with @.gif@ extension.
data GIF = GIF deriving Show

newtype WriteOptionsGIF = WriteOptionsGIF
  { woGetPaletteOptionsGIF :: JP.PaletteOptions
  }

woSetPaletteOptionsGIF :: JP.PaletteOptions -> WriteOptionsGIF -> WriteOptionsGIF
woSetPaletteOptionsGIF palOpts opts = opts { woGetPaletteOptionsGIF = palOpts }

instance Default WriteOptionsGIF where
  def = WriteOptionsGIF JP.defaultPaletteOptions

instance FileFormat GIF where
  type WriteOptions GIF = WriteOptionsGIF

  ext _ = ".gif"


data WriteOptionsSequenceGIF = WriteOptionsSequenceGIF
  { woGetPaletteOptionsGIFs :: JP.PaletteOptions
  , woGetGifLoopingGIFs     :: JP.GifLooping
  }

woSetPaletteOptionsGIFs :: JP.PaletteOptions -> WriteOptionsSequenceGIF -> WriteOptionsSequenceGIF
woSetPaletteOptionsGIFs palOpts opts = opts { woGetPaletteOptionsGIFs = palOpts }


woSetGifLoopingGIFs :: JP.GifLooping -> WriteOptionsSequenceGIF -> WriteOptionsSequenceGIF
woSetGifLoopingGIFs looping opts = opts { woGetGifLoopingGIFs = looping }


instance Default WriteOptionsSequenceGIF where
  def = WriteOptionsSequenceGIF JP.defaultPaletteOptions JP.LoopingNever

instance FileFormat (Sequence GIF) where

  type WriteOptions (Sequence GIF) = WriteOptionsSequenceGIF
  ext _ = ext GIF


instance FileFormat (Sequence (Auto GIF)) where

  type WriteOptions (Sequence (Auto GIF)) = WriteOptions (Sequence GIF)
  ext _ = ext GIF


instance (ColorSpace cs e, Source r Ix2 (Pixel cs e)) =>
         Writable GIF (Image r cs e) where
  encodeM f opt img =
    encodeError =<< fromMaybeEncode f (toProxy img) (encodeGIF opt img)

instance (ColorSpace cs e, ToY cs e, ToRGB cs e, Source r Ix2 (Pixel cs e)) =>
         Writable (Auto GIF) (Image r cs e) where
  encodeM f opt img =
    encodeError =<< encodeAuto f (encodeGIF opt) id toPixelY toPixelRGB toPixelRGB img


instance ColorSpace cs e => Readable GIF (Image S cs e) where
  decodeM f _ = fromEitherDecode_ f showJP fromDynamicImage . JP.decodeGif

instance (Mutable r Ix2 (Pixel cs e), ColorSpace cs e) =>
         Readable (Auto GIF) (Image r cs e) where
  decodeM f _ = fromEitherDecode_ f showJP fromAnyDynamicImage . JP.decodeGif


instance ColorSpace cs e =>
         Readable (Sequence GIF) (Array B Ix1 (Image S cs e)) where
  decodeM f _ = decodeGIFs f fromDynamicImage

instance (Mutable r Ix2 (Pixel cs e), ColorSpace cs e) =>
         Readable (Sequence (Auto GIF)) (Array B Ix1 (Image r cs e)) where
  decodeM f _ = decodeGIFs f fromAnyDynamicImage


instance (ColorSpace cs e, Source r Ix2 (Pixel cs e)) =>
         Writable (Sequence GIF) (Array B Ix1 (JP.GifDelay, Image r cs e)) where
  encodeM f opts img = do
    encodeError =<< fromMaybeEncode f (Proxy :: Proxy (Image r cs e)) (encodeGIFs opts img)


instance ColorSpace cs e =>
         Readable (Sequence GIF) (Array B Ix1 (JP.GifDelay, Image S cs e)) where
  decodeM f _ = decodeGIFsWithDelays f fromDynamicImage


fromEitherDecode_ ::
     (MonadThrow m, ColorSpace cs e, FileFormat f, Typeable r)
  => f
  -> (a -> String)
  -> (a -> Maybe (Image r cs e))
  -> Either String a
  -> m (Image r cs e, Maybe B.ByteString)
fromEitherDecode_ f showCS conv eImg = do
  jImg <- either (throwM . DecodeError) pure eImg
  img <- fromEitherDecode f showCS conv jImg
  pure (img, Nothing)

-- Animated GIF Format frames reading into an Array of Images

decodeGIFs
  :: (FileFormat f, Mutable r Ix2 (Pixel cs e), ColorSpace cs e, MonadThrow m)
  => f
  -> (JP.DynamicImage -> Maybe (Image r cs e))
  -> B.ByteString
  -> m (Array B Ix1 (Image r cs e), Maybe B.ByteString)
decodeGIFs f converter bs = do
  jpImgs <- either (throwM . DecodeError) pure $ JP.decodeGifImages bs
  imgs <- P.traverse (convertEither f showJP converter) jpImgs
  pure (fromList Seq imgs, Nothing)
{-# INLINE decodeGIFs #-}


decodeGIFsWithDelays
  :: (ColorSpace cs e, MonadThrow m)
  => Sequence GIF
  -> (JP.DynamicImage -> Maybe (Image S cs e))
  -> B.ByteString
  -> m (Array B Ix1 (JP.GifDelay, Image S cs e), Maybe B.ByteString)
decodeGIFsWithDelays f converter bs = do
  jpImgsLs <- either (throwM . DecodeError) pure $ JP.decodeGifImages bs
  delays <- either (throwM . DecodeError) pure $ JP.getDelaysGifImages bs
  imgs <- P.traverse (convertEither f showJP converter) jpImgsLs
  pure (fromList Seq (P.zip delays imgs), Nothing)
{-# INLINE decodeGIFsWithDelays #-}


encodeGIF :: forall r cs e . (ColorSpace cs e, Source r Ix2 (Pixel cs e))
          => WriteOptionsGIF
          -> Image r cs e
          -> Maybe (Either String BL.ByteString)
encodeGIF (WriteOptionsGIF pal) img =
  uncurry JP.encodeGifImageWithPalette <$>
  msum
    [ do Refl <- eqT :: Maybe (cs :~: Y)
         jImg <-
           msum
             [ do Refl <- eqT :: Maybe (e :~: Word8)
                  pure $ toJPImageY8 img
             , pure $ toJPImageY8 $ M.map toWord8 img
             ]
         return (jImg, JP.greyPalette)
    , do Refl <- eqT :: Maybe (cs :~: RGB)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                palettizeRGB pal img
           , palettizeRGB pal $ M.map toWord8 img
           ]
    ]


encodeGIFs :: forall r cs e . (ColorSpace cs e, Source r Ix2 (Pixel cs e))
           => WriteOptionsSequenceGIF
           -> Array B Ix1 (JP.GifDelay, Image r cs e)
           -> Maybe (Either String BL.ByteString)
encodeGIFs (WriteOptionsSequenceGIF pal looping) arr = do
  palImgsLs <-
    msum
      [ do Refl <- eqT :: Maybe (cs :~: Y)
           msum
             [ do Refl <- eqT :: Maybe (e :~: Word8)
                  pure $ P.map ((, JP.greyPalette) . toJPImageY8) imgsLs
             , pure $
               P.map ((, JP.greyPalette) . toJPImageY8 . M.map toWord8) imgsLs
             ]
      , do Refl <- eqT :: Maybe (cs :~: RGB)
           msum
             [ do Refl <- eqT :: Maybe (e :~: Word8)
                  P.mapM (palettizeRGB pal) imgsLs
             , P.mapM (palettizeRGB pal . M.map toWord8) imgsLs
             ]
      ]
  let palDelImgsLs = P.zipWith (\(i, p) d -> (p, d, i)) palImgsLs delaysLs
  pure $ JP.encodeGifImages looping palDelImgsLs
  where
    delaysLs = toList delays
    imgsLs = toList imgs
    (delays, imgs) = M.unzip arr
{-# INLINE encodeGIFs #-}


palettizeRGB :: forall r e . (ColorSpace RGB e, Source r Ix2 (Pixel RGB e))
          => JP.PaletteOptions
          -> Image r RGB e
          -> Maybe (JP.Image JP.Pixel8, JP.Palette)
palettizeRGB pal img =
  msum
    [ do Refl <- eqT :: Maybe (e :~: Word8)
         pure $ palettize' img
    , pure $ palettize' $ M.map toWord8 img
    ]
  where
    palettize' :: forall r' . Source r' Ix2 (Pixel RGB Word8) =>
                  Image r' RGB Word8 -> (JP.Image JP.Pixel8, JP.Palette)
    palettize' = JP.palettize pal . toJPImageRGB8
    {-# INLINE palettize' #-}
{-# INLINE palettizeRGB #-}


--------------------------------------------------------------------------------
-- HDR Format ------------------------------------------------------------------
--------------------------------------------------------------------------------


-- | High-dynamic-range image with @.hdr@ or @.pic@ extension.
data HDR = HDR deriving Show

instance FileFormat HDR where

  ext _ = ".hdr"

  exts _ = [".hdr", ".pic"]

instance (ColorSpace cs e, Source r Ix2 (Pixel cs e)) =>
         Writable HDR (Image r cs e) where
  encodeM f _ img = fromMaybeEncode f (toProxy img) $ encodeHDR img


instance (ColorSpace cs e, ToRGB cs e, Source r Ix2 (Pixel cs e)) =>
         Writable (Auto HDR) (Image r cs e) where
  encodeM f _ =
    encodeAuto f encodeHDR toPixelRGB toPixelRGB toPixelRGB toPixelRGB


instance ColorSpace cs e => Readable HDR (Image S cs e) where
  decodeM f _ = fromEitherDecode_ f showJP fromDynamicImage . JP.decodeHDR

instance (Mutable r Ix2 (Pixel cs e), ColorSpace cs e) =>
         Readable (Auto HDR) (Image r cs e) where
  decodeM f _ = fromEitherDecode_ f showJP fromAnyDynamicImage . JP.decodeHDR




encodeHDR :: forall r cs e. (ColorSpace cs e, Source r Ix2 (Pixel cs e))
          => Image r cs e -> Maybe BL.ByteString
encodeHDR img = do
  Refl <- eqT :: Maybe (cs :~: RGB)
  msum
    [ do Refl <- eqT :: Maybe (e :~: Float)
         pure $ JP.encodeHDR $ toJPImageRGBF img
    , pure $ JP.encodeHDR $ toJPImageRGBF $ M.map toFloat img
    ]




--------------------------------------------------------------------------------
-- JPG Format ------------------------------------------------------------------
--------------------------------------------------------------------------------

newtype WriteOptionsJPG = WriteOptionsJPG { woGetQualityJPG :: Word8 } deriving Show

-- | Set the image quality, supplied value will be clamped to @[0, 100]@
-- range. This setting directly affects the Jpeg compression level.
woSetQualityJPG :: Word8 -> WriteOptionsJPG -> WriteOptionsJPG
woSetQualityJPG q opts = opts { woGetQualityJPG = min 100 (max 0 q) }

instance Default WriteOptionsJPG where
  def = WriteOptionsJPG 100

-- | Joint Photographic Experts Group image with @.jpg@ or @.jpeg@ extension.
data JPG = JPG deriving Show

instance FileFormat JPG where
  type WriteOptions JPG = WriteOptionsJPG

  ext _ = ".jpg"

  exts _ = [".jpg", ".jpeg"]

instance (ColorSpace cs e, Source r Ix2 (Pixel cs e)) =>
         Writable JPG (Image r cs e) where
  encodeM f opts img = fromMaybeEncode f (toProxy img) $ encodeJPG opts img


instance (ColorSpace cs e, ToYCbCr cs e, Source r Ix2 (Pixel cs e)) =>
         Writable (Auto JPG) (Image r cs e) where
  encodeM f opt =
    encodeAuto f (encodeJPG opt) toPixelYCbCr toPixelYCbCr toPixelYCbCr toPixelYCbCr


instance ColorSpace cs e => Readable JPG (Image S cs e) where
  decodeM f _ = fromEitherDecode_ f showJP fromDynamicImage . JP.decodeJpeg

instance (Mutable r Ix2 (Pixel cs e), ColorSpace cs e) =>
         Readable (Auto JPG) (Image r cs e) where
  decodeM f _ = fromEitherDecode_ f showJP fromAnyDynamicImage . JP.decodeJpeg




encodeJPG :: forall r cs e. (ColorSpace cs e, Source r Ix2 (Pixel cs e))
          => WriteOptionsJPG -> Image r cs e -> Maybe BL.ByteString
encodeJPG (WriteOptionsJPG q) img =
  msum
    [ do Refl <- eqT :: Maybe (cs :~: Y)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ encJPG $ toJPImageY8 img
           , pure $ encJPG $ toJPImageY8 $ M.map toWord8 img
           ]
    , do Refl <- eqT :: Maybe (cs :~: RGB)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ encJPG $ toJPImageRGB8 img
           , pure $ encJPG $ toJPImageRGB8 $ M.map toWord8 img
           ]
    , do Refl <- eqT :: Maybe (cs :~: CMYK)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ encJPG $ toJPImageCMYK8 img
           , pure $ encJPG $ toJPImageCMYK8 $ M.map toWord8 img
           ]
    , do Refl <- eqT :: Maybe (cs :~: YCbCr)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ encJPG $ toJPImageYCbCr8 img
           , pure $ encJPG $ toJPImageYCbCr8 $ M.map toWord8 img
           ]
    ]
  where
    encJPG :: forall px . JP.JpgEncodable px => JP.Image px -> BL.ByteString
    encJPG = JP.encodeDirectJpegAtQualityWithMetadata q mempty
    {-# INLINE encJPG #-}



--------------------------------------------------------------------------------
-- TGA Format ------------------------------------------------------------------
--------------------------------------------------------------------------------


-- | Truevision Graphics Adapter image with .tga extension.
data TGA = TGA deriving Show

instance FileFormat TGA where

  ext _ = ".tga"
  {-# INLINE ext #-}



instance (ColorSpace cs e, Source r Ix2 (Pixel cs e)) =>
         Writable TGA (Image r cs e) where
  encodeM f _ img = fromMaybeEncode f (toProxy img) $ encodeTGA img

instance (ColorSpace cs e, ToRGBA cs e, Source r Ix2 (Pixel cs e)) =>
         Writable (Auto TGA) (Image r cs e) where
  encodeM f _ = encodeAuto f encodeTGA id toPixelRGBA toPixelRGB toPixelRGBA


instance ColorSpace cs e => Readable TGA (Image S cs e) where
  decodeM f _ = fromEitherDecode_ f showJP fromDynamicImage . JP.decodeTga


instance (Mutable r Ix2 (Pixel cs e), ColorSpace cs e) =>
         Readable (Auto TGA) (Image r cs e) where
  decodeM f _ = fromEitherDecode_ f showJP fromAnyDynamicImage . JP.decodeTga


encodeTGA :: forall r cs e . (ColorSpace cs e, Source r Ix2 (Pixel cs e))
          => Image r cs e -> Maybe BL.ByteString
encodeTGA img =
  msum
    [ do Refl <- eqT :: Maybe (cs :~: Y)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ JP.encodeTga $ toJPImageY8 img
           , pure $ JP.encodeTga $ toJPImageY8 $ M.map toWord8 img
           ]
    , do Refl <- eqT :: Maybe (cs :~: RGB)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ JP.encodeTga $ toJPImageRGB8 img
           , pure $ JP.encodeTga $ toJPImageRGB8 $ M.map toWord8 img
           ]
    , do Refl <- eqT :: Maybe (cs :~: RGBA)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ JP.encodeTga $ toJPImageRGBA8 img
           , pure $ JP.encodeTga $ toJPImageRGBA8 $ M.map toWord8 img
           ]
    ]


--------------------------------------------------------------------------------
-- TIF Format ------------------------------------------------------------------
--------------------------------------------------------------------------------


-- | Tagged Image File Format image with @.tif@ or @.tiff@ extension.
data TIF = TIF deriving Show

instance FileFormat TIF where

  ext _ = ".tif"
  {-# INLINE ext #-}

  exts _ = [".tif", ".tiff"]
  {-# INLINE exts #-}


instance (ColorSpace cs e, Source r Ix2 (Pixel cs e)) =>
         Writable TIF (Image r cs e) where
  encodeM f _ img = fromMaybeEncode f (toProxy img) $ encodeTIF img


instance (ColorSpace cs e, ToRGBA cs e, Source r Ix2 (Pixel cs e)) =>
         Writable (Auto TIF) (Image r cs e) where
  encodeM f _ = encodeAuto f encodeTIF id id id toPixelRGBA


instance ColorSpace cs e => Readable TIF (Image S cs e) where
  decodeM f _ = fromEitherDecode_ f showJP fromDynamicImage . JP.decodeTiff

instance (Mutable r Ix2 (Pixel cs e), ColorSpace cs e) =>
         Readable (Auto TIF) (Image r cs e) where
  decodeM f _ = fromEitherDecode_ f showJP fromAnyDynamicImage . JP.decodeTiff




encodeTIF :: forall r cs e. (ColorSpace cs e, Source r Ix2 (Pixel cs e))
          => Image r cs e -> Maybe BL.ByteString
encodeTIF img =
  msum
    [ do Refl <- eqT :: Maybe (cs :~: Y)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ JP.encodeTiff $ toJPImageY8 img
           , do Refl <- eqT :: Maybe (e :~: Word16)
                pure $ JP.encodeTiff $ toJPImageY16 img
#if MIN_VERSION_JuicyPixels(3,3,0)
           , do Refl <- eqT :: Maybe (e :~: Word32)
                pure $ JP.encodeTiff $ toJPImageY32 img
           , do Refl <- eqT :: Maybe (e :~: Float)
                pure $ JP.encodeTiff $ toJPImageYF img
#endif
           , pure $ JP.encodeTiff $ toJPImageY16 $ M.map toWord16 img
           ]
    , do Refl <- eqT :: Maybe (cs :~: YA)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ JP.encodeTiff $ toJPImageYA8 img
           , do Refl <- eqT :: Maybe (e :~: Word16)
                pure $ JP.encodeTiff $ toJPImageYA16 img
           , pure $ JP.encodeTiff $ toJPImageYA16 $ M.map toWord16 img
           ]
    , do Refl <- eqT :: Maybe (cs :~: RGB)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ JP.encodeTiff $ toJPImageRGB8 img
           , do Refl <- eqT :: Maybe (e :~: Word16)
                pure $ JP.encodeTiff $ toJPImageRGB16 img
           , pure $ JP.encodeTiff $ toJPImageRGB16 $ M.map toWord16 img
           ]
    , do Refl <- eqT :: Maybe (cs :~: RGBA)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ JP.encodeTiff $ toJPImageRGBA8 img
           , do Refl <- eqT :: Maybe (e :~: Word16)
                pure $ JP.encodeTiff $ toJPImageRGBA16 img
           , pure $ JP.encodeTiff $ toJPImageRGBA16 $ M.map toWord16 img
           ]
    , do Refl <- eqT :: Maybe (cs :~: YCbCr)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ JP.encodeTiff $ toJPImageYCbCr8 img
           , pure $ JP.encodeTiff $ toJPImageYCbCr8 $ M.map toWord8 img
           ]
    , do Refl <- eqT :: Maybe (cs :~: CMYK)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ JP.encodeTiff $ toJPImageCMYK8 img
           , do Refl <- eqT :: Maybe (e :~: Word16)
                pure $ JP.encodeTiff $ toJPImageCMYK16 img
             -- for CMYK default is 8bit, instead of 16bit, since many viewers and editors
             -- don't support it.
           , pure $ JP.encodeTiff $ toJPImageCMYK8 $ M.map toWord8 img
           ]
    ]




--------------------------------------------------------------------------------
-- Common encoding/decoding functions ------------------------------------------
--------------------------------------------------------------------------------


fromDynamicImage :: forall cs e . ColorSpace cs e
                 => JP.DynamicImage -> Maybe (Image S cs e)
fromDynamicImage jpDynImg =
  case jpDynImg of
    JP.ImageY8 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel Y Word8)
      fromJPImageUnsafe jimg
    JP.ImageY16 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel Y Word16)
      fromJPImageUnsafe jimg
#if MIN_VERSION_JuicyPixels(3,3,0)
    JP.ImageY32 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel Y Word32)
      fromJPImageUnsafe jimg
#endif
    JP.ImageYF jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel Y Float)
      fromJPImageUnsafe jimg
    JP.ImageYA8 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel YA Word8)
      fromJPImageUnsafe jimg
    JP.ImageYA16 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel YA Word16)
      fromJPImageUnsafe jimg
    JP.ImageRGB8 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel RGB Word8)
      fromJPImageUnsafe jimg
    JP.ImageRGB16 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel RGB Word16)
      fromJPImageUnsafe jimg
    JP.ImageRGBF jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel RGB Float)
      fromJPImageUnsafe jimg
    JP.ImageRGBA8 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel RGBA Word8)
      fromJPImageUnsafe jimg
    JP.ImageRGBA16 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel RGBA Word16)
      fromJPImageUnsafe jimg
    JP.ImageYCbCr8 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel YCbCr Word8)
      fromJPImageUnsafe jimg
    JP.ImageCMYK8 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel CMYK Word8)
      fromJPImageUnsafe jimg
    JP.ImageCMYK16 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel CMYK Word16)
      fromJPImageUnsafe jimg



fromAnyDynamicImage :: (Mutable r Ix2 (Pixel cs e), ColorSpace cs e) =>
                       JP.DynamicImage -> Maybe (Image r cs e)
fromAnyDynamicImage jpDynImg =
  case jpDynImg of
    JP.ImageY8 jimg     -> (fromJPImageUnsafe jimg :: Maybe (Image S Y Word8))     >>= toAnyCS
    JP.ImageY16 jimg    -> (fromJPImageUnsafe jimg :: Maybe (Image S Y Word16))    >>= toAnyCS
#if MIN_VERSION_JuicyPixels(3,3,0)
    JP.ImageY32 jimg    -> (fromJPImageUnsafe jimg :: Maybe (Image S Y Word32))    >>= toAnyCS
#endif
    JP.ImageYF jimg     -> (fromJPImageUnsafe jimg :: Maybe (Image S Y Float))     >>= toAnyCS
    JP.ImageYA8 jimg    -> (fromJPImageUnsafe jimg :: Maybe (Image S YA Word8))    >>= toAnyCS
    JP.ImageYA16 jimg   -> (fromJPImageUnsafe jimg :: Maybe (Image S YA Word16))   >>= toAnyCS
    JP.ImageRGB8 jimg   -> (fromJPImageUnsafe jimg :: Maybe (Image S RGB Word8))   >>= toAnyCS
    JP.ImageRGB16 jimg  -> (fromJPImageUnsafe jimg :: Maybe (Image S RGB Word16))  >>= toAnyCS
    JP.ImageRGBF jimg   -> (fromJPImageUnsafe jimg :: Maybe (Image S RGB Float))   >>= toAnyCS
    JP.ImageRGBA8 jimg  -> (fromJPImageUnsafe jimg :: Maybe (Image S RGBA Word8))  >>= toAnyCS
    JP.ImageRGBA16 jimg -> (fromJPImageUnsafe jimg :: Maybe (Image S RGBA Word16)) >>= toAnyCS
    JP.ImageYCbCr8 jimg -> (fromJPImageUnsafe jimg :: Maybe (Image S YCbCr Word8)) >>= toAnyCS
    JP.ImageCMYK8 jimg  -> (fromJPImageUnsafe jimg :: Maybe (Image S CMYK Word8))  >>= toAnyCS
    JP.ImageCMYK16 jimg -> (fromJPImageUnsafe jimg :: Maybe (Image S CMYK Word16)) >>= toAnyCS



showJP :: JP.DynamicImage -> String
showJP (JP.ImageY8     _) = "Image S Y Word8"
showJP (JP.ImageY16    _) = "Image S Y Word16"
#if MIN_VERSION_JuicyPixels(3,3,0)
showJP (JP.ImageY32    _) = "Image S Y Word32"
#endif
showJP (JP.ImageYF     _) = "Image S Y Float"
showJP (JP.ImageYA8    _) = "Image S YA Word8"
showJP (JP.ImageYA16   _) = "Image S YA Word16"
showJP (JP.ImageRGB8   _) = "Image S RGB Word8"
showJP (JP.ImageRGB16  _) = "Image S RGB Word16"
showJP (JP.ImageRGBF   _) = "Image S RGB Float"
showJP (JP.ImageRGBA8  _) = "Image S RGBA Word8"
showJP (JP.ImageRGBA16 _) = "Image S RGBA Word16"
showJP (JP.ImageYCbCr8 _) = "Image S YCbCr Word8"
showJP (JP.ImageCMYK8  _) = "Image S CMYK Word8"
showJP (JP.ImageCMYK16 _) = "Image S CMYK Word16"


-- Encoding

toJPImageUnsafe
  :: forall r cs a . (JP.Pixel a, Source r Ix2 (Pixel cs (JP.PixelBaseComponent a)),
                      ColorSpace cs (JP.PixelBaseComponent a))
  => Image r cs (JP.PixelBaseComponent a)
  -> JP.Image a
toJPImageUnsafe img = JP.Image n m $ V.unsafeCast $ toVector arrS
  where
    !arrS = computeSource img :: Image S cs (JP.PixelBaseComponent a)
    Sz (m :. n) = size img
{-# INLINE toJPImageUnsafe #-}

toJPImageY8 :: Source r Ix2 (Pixel Y Word8) => Image r Y Word8 -> JP.Image TypesJP.Pixel8
toJPImageY8 = toJPImageUnsafe
{-# INLINE toJPImageY8 #-}

toJPImageY16 :: Source r Ix2 (Pixel Y Word16) => Image r Y Word16 -> JP.Image TypesJP.Pixel16
toJPImageY16 = toJPImageUnsafe
{-# INLINE toJPImageY16 #-}

#if MIN_VERSION_JuicyPixels(3,3,0)
toJPImageY32 :: Source r Ix2 (Pixel Y Word32) => Image r Y Word32 -> JP.Image TypesJP.Pixel32
toJPImageY32 = toJPImageUnsafe
{-# INLINE toJPImageY32 #-}
#endif

toJPImageYA8 :: Source r Ix2 (Pixel YA Word8) => Image r YA Word8 -> JP.Image JP.PixelYA8
toJPImageYA8 = toJPImageUnsafe
{-# INLINE toJPImageYA8 #-}

toJPImageYA16 :: Source r Ix2 (Pixel YA Word16) => Image r YA Word16 -> JP.Image JP.PixelYA16
toJPImageYA16 = toJPImageUnsafe
{-# INLINE toJPImageYA16 #-}

toJPImageYF :: Source r Ix2 (Pixel Y Float) => Image r Y Float -> JP.Image JP.PixelF
toJPImageYF = toJPImageUnsafe
{-# INLINE toJPImageYF #-}

toJPImageRGB8 :: Source r Ix2 (Pixel RGB Word8) => Image r RGB Word8 -> JP.Image JP.PixelRGB8
toJPImageRGB8 = toJPImageUnsafe
{-# INLINE toJPImageRGB8 #-}

toJPImageRGBA8 :: Source r Ix2 (Pixel RGBA Word8) => Image r RGBA Word8 -> JP.Image JP.PixelRGBA8
toJPImageRGBA8 = toJPImageUnsafe
{-# INLINE toJPImageRGBA8 #-}

toJPImageRGB16 :: Source r Ix2 (Pixel RGB Word16) => Image r RGB Word16 -> JP.Image JP.PixelRGB16
toJPImageRGB16 = toJPImageUnsafe
{-# INLINE toJPImageRGB16 #-}

toJPImageRGBA16 :: Source r Ix2 (Pixel RGBA Word16) => Image r RGBA Word16 -> JP.Image JP.PixelRGBA16
toJPImageRGBA16 = toJPImageUnsafe
{-# INLINE toJPImageRGBA16 #-}

toJPImageRGBF :: Source r Ix2 (Pixel RGB Float) => Image r RGB Float -> JP.Image JP.PixelRGBF
toJPImageRGBF = toJPImageUnsafe
{-# INLINE toJPImageRGBF #-}

toJPImageYCbCr8 :: Source r Ix2 (Pixel YCbCr Word8) => Image r YCbCr Word8 -> JP.Image JP.PixelYCbCr8
toJPImageYCbCr8 = toJPImageUnsafe
{-# INLINE toJPImageYCbCr8 #-}

toJPImageCMYK8 :: Source r Ix2 (Pixel CMYK Word8) => Image r CMYK Word8 -> JP.Image JP.PixelCMYK8
toJPImageCMYK8 = toJPImageUnsafe
{-# INLINE toJPImageCMYK8 #-}

toJPImageCMYK16 :: Source r Ix2 (Pixel CMYK Word16) => Image r CMYK Word16 -> JP.Image JP.PixelCMYK16
toJPImageCMYK16 = toJPImageUnsafe
{-# INLINE toJPImageCMYK16 #-}




-- General decoding and helper functions

fromJPImageUnsafe :: forall jpx cs e . (Storable (Pixel cs e), Storable e, JP.Pixel jpx) =>
                     JP.Image jpx -> Maybe (Image S cs e)
fromJPImageUnsafe (JP.Image n m !v) = do
  let numberOfComponentsFromSize = sizeOf (undefined :: Pixel cs e) `div` sizeOf (undefined :: e)
      numComponentsPerPixel = JP.componentCount (undefined :: jpx)
  unless (numComponentsPerPixel == numberOfComponentsFromSize) $
    error $
    concat
      [ "Mismatched sizes beteen JuicyPixels: "
      , show numComponentsPerPixel
      , " and massiv: "
      , show numberOfComponentsFromSize
      ]
  guard (n * m * numComponentsPerPixel == V.length v)
  fromVectorM Par (Sz (m :. n)) $ V.unsafeCast v
{-# INLINE fromJPImageUnsafe #-}

