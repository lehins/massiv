{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Massiv.Array.IO.Image.JuicyPixels.TIF
-- Copyright   : (c) Alexey Kuleshevich 2019-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.IO.Image.JuicyPixels.TIF
  ( TIF(..)
  , decodeTIF
  , decodeWithMetadataTIF
  , decodeAutoTIF
  , decodeAutoWithMetadataTIF
  , encodeTIF
  , encodeAutoTIF
  ) where

import qualified Codec.Picture as JP
import qualified Codec.Picture.Metadata as JP
import qualified Codec.Picture.Tiff as JP
import Control.Monad (msum)
import Data.Bifunctor (first)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL (ByteString)
import Data.Massiv.Array as A
import Data.Massiv.Array.IO.Base
import Data.Massiv.Array.IO.Image.JuicyPixels.Base
import Data.Maybe (fromMaybe)
import Data.Typeable
import qualified Graphics.Pixel as CM
import Graphics.Pixel.ColorSpace
import Prelude as P



--------------------------------------------------------------------------------
-- TIF Format ------------------------------------------------------------------
--------------------------------------------------------------------------------

-- TODOs:
--  * Check on reading in YCbCr
--  * Check on "except for Y32 which is truncated to 16 bits" in `JP.decodeTiff` doc.


-- | Tagged Image File Format image with @.tif@ or @.tiff@ extension.
data TIF = TIF deriving Show

instance FileFormat TIF where
  type Metadata TIF = JP.Metadatas
  ext _ = ".tif"
  exts _ = [".tif", ".tiff"]

instance Writable TIF (Image S CM.Y Word8) where
  encodeM TIF _ img = pure $ JP.encodeTiff (toJPImageY8 img)

instance Writable TIF (Image S CM.Y Word16) where
  encodeM TIF _ img = pure $ JP.encodeTiff (toJPImageY16 img)

instance Writable TIF (Image S CM.Y Word32) where
  encodeM TIF _ img = pure $ JP.encodeTiff (toJPImageY32 img)

instance Writable TIF (Image S CM.Y Float) where
  encodeM TIF _ img = pure $ JP.encodeTiff (toJPImageYF img)

instance Writable TIF (Image S (Alpha CM.Y) Word8) where
  encodeM TIF _ img = pure $ JP.encodeTiff (toJPImageYA8 img)

instance Writable TIF (Image S (Alpha CM.Y) Word16) where
  encodeM TIF _ img = pure $ JP.encodeTiff (toJPImageYA16 img)

instance Writable TIF (Image S CM.RGB Word8) where
  encodeM TIF _ img = pure $ JP.encodeTiff (toJPImageRGB8 img)

instance Writable TIF (Image S CM.RGB Word16) where
  encodeM TIF _ img = pure $ JP.encodeTiff (toJPImageRGB16 img)

instance Writable TIF (Image S (Alpha CM.RGB) Word8) where
  encodeM TIF _ img = pure $ JP.encodeTiff (toJPImageRGBA8 img)

instance Writable TIF (Image S (Alpha CM.RGB) Word16) where
  encodeM TIF _ img = pure $ JP.encodeTiff (toJPImageRGBA16 img)

instance Writable TIF (Image S CM.YCbCr Word8) where
  encodeM TIF _ img = pure $ JP.encodeTiff (toJPImageYCbCr8 img)

instance Writable TIF (Image S CM.CMYK Word8) where
  encodeM TIF _ img = pure $ JP.encodeTiff (toJPImageCMYK8 img)

instance Writable TIF (Image S CM.CMYK Word16) where
  encodeM TIF _ img = pure $ JP.encodeTiff (toJPImageCMYK16 img)

instance Writable TIF (Image S (Y D65) Word8) where
  encodeM f opts = encodeM f opts . toImageBaseModel

instance Writable TIF (Image S (Y D65) Word16) where
  encodeM f opts = encodeM f opts . toImageBaseModel

instance Writable TIF (Image S (Y D65) Word32) where
  encodeM f opts = encodeM f opts . toImageBaseModel

instance Writable TIF (Image S (Y D65) Float) where
  encodeM f opts = encodeM f opts . toImageBaseModel

instance Writable TIF (Image S (Alpha (Y D65)) Word8) where
  encodeM f opts = encodeM f opts . toImageBaseModel

instance Writable TIF (Image S (Alpha (Y D65)) Word16) where
  encodeM f opts = encodeM f opts . toImageBaseModel

instance Writable TIF (Image S SRGB Word8) where
  encodeM f opts = encodeM f opts . toImageBaseModel

instance Writable TIF (Image S SRGB Word16) where
  encodeM f opts = encodeM f opts . toImageBaseModel

instance Writable TIF (Image S (Alpha SRGB) Word8) where
  encodeM f opts = encodeM f opts . toImageBaseModel

instance Writable TIF (Image S (Alpha SRGB) Word16) where
  encodeM f opts = encodeM f opts . toImageBaseModel

instance Writable TIF (Image S (YCbCr SRGB) Word8) where
  encodeM f opts = encodeM f opts . toImageBaseModel

instance Writable TIF (Image S (CMYK SRGB) Word8) where
  encodeM f opts = encodeM f opts . toImageBaseModel

instance Writable TIF (Image S (CMYK SRGB) Word16) where
  encodeM f opts = encodeM f opts . toImageBaseModel

instance (ColorSpace cs i e, ColorSpace (BaseSpace cs) i e, Source r Ix2 (Pixel cs e)) =>
         Writable (Auto TIF) (Image r cs e) where
  encodeM f _ = pure . encodeAutoTIF f


instance Readable TIF (Image S CM.Y Word8) where
  decodeWithMetadataM = decodeWithMetadataTIF

instance Readable TIF (Image S CM.Y Word16) where
  decodeWithMetadataM = decodeWithMetadataTIF

instance Readable TIF (Image S CM.Y Word32) where
  decodeWithMetadataM = decodeWithMetadataTIF

instance Readable TIF (Image S CM.Y Float) where
  decodeWithMetadataM = decodeWithMetadataTIF

instance Readable TIF (Image S (Alpha CM.Y) Word8) where
  decodeWithMetadataM = decodeWithMetadataTIF

instance Readable TIF (Image S (Alpha CM.Y) Word16) where
  decodeWithMetadataM = decodeWithMetadataTIF

instance Readable TIF (Image S CM.RGB Word8) where
  decodeWithMetadataM = decodeWithMetadataTIF

instance Readable TIF (Image S CM.RGB Word16) where
  decodeWithMetadataM = decodeWithMetadataTIF

instance Readable TIF (Image S (Alpha CM.RGB) Word8) where
  decodeWithMetadataM = decodeWithMetadataTIF

instance Readable TIF (Image S (Alpha CM.RGB) Word16) where
  decodeWithMetadataM = decodeWithMetadataTIF

instance Readable TIF (Image S CM.CMYK Word8) where
  decodeWithMetadataM = decodeWithMetadataTIF

instance Readable TIF (Image S CM.CMYK Word16) where
  decodeWithMetadataM = decodeWithMetadataTIF


instance Readable TIF (Image S (Y D65) Word8) where
  decodeWithMetadataM f = fmap (first fromImageBaseModel) . decodeWithMetadataM f

instance Readable TIF (Image S (Y D65) Word16) where
  decodeWithMetadataM f = fmap (first fromImageBaseModel) . decodeWithMetadataM f

instance Readable TIF (Image S (Y D65) Word32) where
  decodeWithMetadataM f = fmap (first fromImageBaseModel) . decodeWithMetadataM f

instance Readable TIF (Image S (Y D65) Float) where
  decodeWithMetadataM f = fmap (first fromImageBaseModel) . decodeWithMetadataM f

instance Readable TIF (Image S (Alpha (Y D65)) Word8) where
  decodeWithMetadataM f = fmap (first fromImageBaseModel) . decodeWithMetadataM f

instance Readable TIF (Image S (Alpha (Y D65)) Word16) where
  decodeWithMetadataM f = fmap (first fromImageBaseModel) . decodeWithMetadataM f

instance Readable TIF (Image S SRGB Word8) where
  decodeWithMetadataM f = fmap (first fromImageBaseModel) . decodeWithMetadataM f

instance Readable TIF (Image S SRGB Word16) where
  decodeWithMetadataM f = fmap (first fromImageBaseModel) . decodeWithMetadataM f

instance Readable TIF (Image S (Alpha SRGB) Word8) where
  decodeWithMetadataM f = fmap (first fromImageBaseModel) . decodeWithMetadataM f

instance Readable TIF (Image S (Alpha SRGB) Word16) where
  decodeWithMetadataM f = fmap (first fromImageBaseModel) . decodeWithMetadataM f

instance Readable TIF (Image S (CMYK SRGB) Word8) where
  decodeWithMetadataM f = fmap (first fromImageBaseModel) . decodeWithMetadataM f

instance Readable TIF (Image S (CMYK SRGB) Word16) where
  decodeWithMetadataM f = fmap (first fromImageBaseModel) . decodeWithMetadataM f

-- | Decode a Tiff Image
decodeTIF :: (ColorModel cs e, MonadThrow m) => TIF -> B.ByteString -> m (Image S cs e)
decodeTIF f bs = convertWith f (JP.decodeTiff bs)

-- | Decode a Tiff Image
decodeWithMetadataTIF ::
     (ColorModel cs e, MonadThrow m) => TIF -> B.ByteString -> m (Image S cs e, JP.Metadatas)
decodeWithMetadataTIF f bs = convertWithMetadata f (JP.decodeTiffWithMetadata bs)


-- | Decode a Tiff Image
decodeAutoTIF ::
     (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e, MonadThrow m)
  => Auto TIF
  -> B.ByteString
  -> m (Image r cs e)
decodeAutoTIF f bs = convertAutoWith f (JP.decodeTiff bs)

-- | Decode a Tiff Image
decodeAutoWithMetadataTIF ::
     (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e, MonadThrow m)
  => Auto TIF
  -> B.ByteString
  -> m (Image r cs e, JP.Metadatas)
decodeAutoWithMetadataTIF f bs = convertAutoWithMetadata f (JP.decodeTiffWithMetadata bs)

instance (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e) =>
         Readable (Auto TIF) (Image r cs e) where
  decodeM = decodeAutoTIF
  decodeWithMetadataM = decodeAutoWithMetadataTIF

encodeTIF ::
     forall r cs e m.
     (ColorModel cs e, Source r Ix2 (Pixel cs e), MonadThrow m)
  => TIF
  -> Image r cs e
  -> m BL.ByteString
encodeTIF f img =
  fromMaybeEncode f (Proxy :: Proxy (Image r cs e)) $
  msum
    [ do Refl <- eqT :: Maybe (cs :~: CM.Y)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ JP.encodeTiff $ toJPImageY8 img
           , do Refl <- eqT :: Maybe (e :~: Word16)
                pure $ JP.encodeTiff $ toJPImageY16 img
           , do Refl <- eqT :: Maybe (e :~: Word32)
                pure $ JP.encodeTiff $ toJPImageY32 img
           , do Refl <- eqT :: Maybe (e :~: Float)
                pure $ JP.encodeTiff $ toJPImageYF img
           ]
    , do Refl <- eqT :: Maybe (cs :~: Alpha CM.Y)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ JP.encodeTiff $ toJPImageYA8 img
           , do Refl <- eqT :: Maybe (e :~: Word16)
                pure $ JP.encodeTiff $ toJPImageYA16 img
           ]
    , do Refl <- eqT :: Maybe (cs :~: CM.RGB)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ JP.encodeTiff $ toJPImageRGB8 img
           , do Refl <- eqT :: Maybe (e :~: Word16)
                pure $ JP.encodeTiff $ toJPImageRGB16 img
           ]
    , do Refl <- eqT :: Maybe (cs :~: Alpha CM.RGB)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ JP.encodeTiff $ toJPImageRGBA8 img
           , do Refl <- eqT :: Maybe (e :~: Word16)
                pure $ JP.encodeTiff $ toJPImageRGBA16 img
           ]
    , do Refl <- eqT :: Maybe (cs :~: CM.YCbCr)
         Refl <- eqT :: Maybe (e :~: Word8)
         pure $ JP.encodeTiff $ toJPImageYCbCr8 img
    , do Refl <- eqT :: Maybe (cs :~: CM.CMYK)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ JP.encodeTiff $ toJPImageCMYK8 img
           , do Refl <- eqT :: Maybe (e :~: Word16)
                pure $ JP.encodeTiff $ toJPImageCMYK16 img
           ]
    ]



encodeAutoTIF ::
     forall r cs i e. (ColorSpace (BaseSpace cs) i e, ColorSpace cs i e, Source r Ix2 (Pixel cs e))
  => Auto TIF
  -> Image r cs e
  -> BL.ByteString
encodeAutoTIF _ img =
  fromMaybe (toTiff toJPImageRGB8 toSRGB8 img) $
  msum
    [ do Refl <- eqT :: Maybe (BaseModel cs :~: CM.Y)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Bit)
                pure $ toTiff toJPImageY8 (toPixel8 . toPixelBaseModel) img
           , do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ toTiff toJPImageY8 toPixelBaseModel img
           , do Refl <- eqT :: Maybe (e :~: Word16)
                pure $ toTiff toJPImageY16 toPixelBaseModel img
           , do Refl <- eqT :: Maybe (e :~: Word32)
                pure $ toTiff toJPImageY32 toPixelBaseModel img
           , do Refl <- eqT :: Maybe (e :~: Float)
                pure $ toTiff toJPImageYF toPixelBaseModel img
           , pure $ toTiff toJPImageY16 (toPixel16 . toPixelBaseModel) img
           ]
    , do Refl <- eqT :: Maybe (BaseModel cs :~: Alpha CM.Y)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ toTiff toJPImageYA8 toPixelBaseModel img
           , do Refl <- eqT :: Maybe (e :~: Word16)
                pure $ toTiff toJPImageYA16 toPixelBaseModel img
           , pure $ toTiff toJPImageYA16 (toPixel16 . toPixelBaseModel) img
           ]
    , do Refl <- eqT :: Maybe (BaseModel cs :~: CM.YCbCr)
         pure $ toTiff toJPImageYCbCr8 toYCbCr8 img
    , do Refl <- eqT :: Maybe (BaseModel cs :~: CM.CMYK)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word16)
                pure $ toTiff toJPImageCMYK16 toCMYK16 img
             -- for CMYK default is 8bit, instead of 16bit, since many viewers and editors
             -- don't support the latter.
           , pure $ toTiff toJPImageCMYK8 toCMYK8 img
           ]
    , do Refl <- eqT :: Maybe (BaseModel cs :~: CM.RGB)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ toTiff toJPImageRGB8 toSRGB8 img
           , pure $ toTiff toJPImageRGB16 toSRGB16 img
           ]
    , do Refl <- eqT :: Maybe (cs :~: Alpha (Opaque cs))
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ toTiff toJPImageRGBA8 toSRGBA8 img
           , pure $ toTiff toJPImageRGBA16 toSRGBA16 img
           ]
    ]
  where
    toTiff ::
         (JP.TiffSaveable px, Source r ix a)
      => (Array D ix b -> JP.Image px)
      -> (a -> b)
      -> Array r ix a
      -> BL.ByteString
    toTiff toJP adjustPixel = JP.encodeTiff . toJP . A.map adjustPixel
