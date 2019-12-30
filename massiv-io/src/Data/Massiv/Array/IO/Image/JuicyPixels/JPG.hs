{-# LANGUAGE NamedFieldPuns #-}
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
-- Module      : Data.Massiv.Array.IO.Image.JuicyPixels.JPG
-- Copyright   : (c) Alexey Kuleshevich 2019-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.IO.Image.JuicyPixels.JPG
  ( JPG(..)
  , JpegOptions(..)
  , decodeJPG
  , decodeWithMetadataJPG
  , decodeAutoJPG
  , decodeAutoWithMetadataJPG
  , encodeJPG
  , encodeAutoJPG
  ) where

import Prelude as P
import Data.Maybe (fromMaybe)
import qualified Codec.Picture as JP
import qualified Codec.Picture.Metadata as JP
import qualified Codec.Picture.Jpg as JP
import Control.Monad (msum)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL (ByteString)
import Data.Massiv.Array as A
import Data.Massiv.Array.IO.Base
import Data.Typeable
import Graphics.Color.Model.Alpha
import qualified Graphics.Color.Model.CMYK as CM
import qualified Graphics.Color.Model.RGB as CM
import qualified Graphics.Color.Model.Y as CM
import qualified Graphics.Color.Model.YCbCr as CM
import Graphics.Color.Pixel
import Graphics.Color.Algebra.Binary
import Data.Massiv.Array.IO.Image.JuicyPixels.Base

--------------------------------------------------------------------------------
-- JPG Format ------------------------------------------------------------------
--------------------------------------------------------------------------------

-- TODOs: Check if JP is capable of writing Jpegs with PixelYA8 (No instance for
-- JpegEncodable, but it can read 'em)

data JpegOptions = JpegOptions
  { jpegQuality  :: !Word8
  , jpegMetadata :: !JP.Metadatas
  } deriving (Show)

instance Default JpegOptions where
  def = JpegOptions 100 mempty


-- | Joint Photographic Experts Group image with @.jpg@ or @.jpeg@ extension.
data JPG = JPG deriving Show

instance FileFormat JPG where
  type WriteOptions JPG = JpegOptions
  type Metadata JPG = JP.Metadatas
  ext _ = ".jpg"
  exts _ = [".jpg", ".jpeg"]

instance Writable JPG (Image S CM.Y Word8) where
  encodeM JPG JpegOptions {jpegQuality, jpegMetadata} =
    pure . JP.encodeDirectJpegAtQualityWithMetadata jpegQuality jpegMetadata . toJPImageY8

instance Writable JPG (Image S CM.RGB Word8) where
  encodeM JPG JpegOptions {jpegQuality, jpegMetadata} =
    pure . JP.encodeDirectJpegAtQualityWithMetadata jpegQuality jpegMetadata . toJPImageRGB8

instance Writable JPG (Image S CM.YCbCr Word8) where
  encodeM JPG JpegOptions {jpegQuality, jpegMetadata} =
    pure . JP.encodeJpegAtQualityWithMetadata jpegQuality jpegMetadata . toJPImageYCbCr8

instance Writable JPG (Image S CM.CMYK Word8) where
  encodeM JPG JpegOptions {jpegQuality, jpegMetadata} =
    pure . JP.encodeDirectJpegAtQualityWithMetadata jpegQuality jpegMetadata . toJPImageCMYK8

instance (ColorSpace cs i e, ColorSpace (BaseSpace cs) i e, Source r Ix2 (Pixel cs e)) =>
         Writable (Auto JPG) (Image r cs e) where
  encodeM _ opts = pure . encodeAutoJPG opts


instance Readable JPG (Image S CM.Y Word8) where
  decodeM f _ = decodeJPG f
  decodeWithMetadataM f _ = decodeWithMetadataJPG f

instance Readable JPG (Image S (Alpha CM.Y) Word8) where
  decodeM f _ = decodeJPG f
  decodeWithMetadataM f _ = decodeWithMetadataJPG f

instance Readable JPG (Image S CM.RGB Word8) where
  decodeM f _ = decodeJPG f
  decodeWithMetadataM f _ = decodeWithMetadataJPG f

instance Readable JPG (Image S CM.CMYK Word8) where
  decodeM f _ = decodeJPG f
  decodeWithMetadataM f _ = decodeWithMetadataJPG f

instance Readable JPG (Image S CM.YCbCr Word8) where
  decodeM f _ = decodeJPG f
  decodeWithMetadataM f _ = decodeWithMetadataJPG f

-- | Decode a Jpeg Image
decodeJPG :: (ColorModel cs e, MonadThrow m) => JPG -> B.ByteString -> m (Image S cs e)
decodeJPG f bs = convertWith f (JP.decodeJpeg bs)

-- | Decode a Jpeg Image
decodeWithMetadataJPG ::
     (ColorModel cs e, MonadThrow m) => JPG -> B.ByteString -> m (Image S cs e, JP.Metadatas)
decodeWithMetadataJPG f bs = convertWithMetadata f (JP.decodeJpegWithMetadata bs)


-- | Decode a Jpeg Image
decodeAutoJPG ::
     (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e, MonadThrow m)
  => Auto JPG
  -> B.ByteString
  -> m (Image r cs e)
decodeAutoJPG f bs = convertAutoWith f (JP.decodeJpeg bs)

-- | Decode a Jpeg Image
decodeAutoWithMetadataJPG ::
     (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e, MonadThrow m)
  => Auto JPG
  -> B.ByteString
  -> m (Image r cs e, JP.Metadatas)
decodeAutoWithMetadataJPG f bs = convertAutoWithMetadata f (JP.decodeJpegWithMetadata bs)

instance (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e) =>
         Readable (Auto JPG) (Image r cs e) where
  decodeM f _ = decodeAutoJPG f
  decodeWithMetadataM f _ = decodeAutoWithMetadataJPG f

encodeJPG ::
     forall r cs e m.
     (ColorModel cs e, Source r Ix2 (Pixel cs e), MonadThrow m)
  => JPG
  -> JpegOptions
  -> Image r cs e
  -> m BL.ByteString
encodeJPG f JpegOptions {jpegQuality, jpegMetadata} img =
  fromMaybeEncode f (Proxy :: Proxy (Image r cs e)) $ do
    Refl <- eqT :: Maybe (e :~: Word8)
    msum
      [ do Refl <- eqT :: Maybe (cs :~: CM.Y)
           pure $
             JP.encodeDirectJpegAtQualityWithMetadata jpegQuality jpegMetadata $ toJPImageY8 img
      , do Refl <- eqT :: Maybe (cs :~: CM.RGB)
           pure $
             JP.encodeDirectJpegAtQualityWithMetadata jpegQuality jpegMetadata $ toJPImageRGB8 img
      , do Refl <- eqT :: Maybe (cs :~: CM.YCbCr)
           pure $ JP.encodeJpegAtQualityWithMetadata jpegQuality jpegMetadata $ toJPImageYCbCr8 img
      , do Refl <- eqT :: Maybe (cs :~: CM.CMYK)
           pure $
             JP.encodeDirectJpegAtQualityWithMetadata jpegQuality jpegMetadata $ toJPImageCMYK8 img
      ]



encodeAutoJPG ::
     forall r cs i e. (ColorSpace (BaseSpace cs) i e, ColorSpace cs i e, Source r Ix2 (Pixel cs e))
  => JpegOptions
  -> Image r cs e
  -> BL.ByteString
encodeAutoJPG JpegOptions {jpegQuality, jpegMetadata} img =
  fromMaybe (toJpeg toJPImageRGB8 (toPixelBaseModel . toSRGB8) img) $
  msum
    [ do Refl <- eqT :: Maybe (BaseModel cs :~: CM.Y)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Bit)
                pure $ toJpeg toJPImageY8 (toPixel8 . toPixelBaseModel) img
           , pure $ toJpeg toJPImageY8 (toPixel8 . toPixelBaseModel) img
           ]
    , do Refl <- eqT :: Maybe (BaseModel cs :~: CM.YCbCr)
         pure $
           JP.encodeJpegAtQualityWithMetadata jpegQuality jpegMetadata $
           toJPImageYCbCr8 $ A.map (toPixel8 . toPixelBaseModel) img
    , do Refl <- eqT :: Maybe (BaseModel cs :~: CM.CMYK)
         pure $ toJpeg toJPImageCMYK8 (toPixel8 . toPixelBaseModel) img
    , do Refl <- eqT :: Maybe (BaseModel (BaseSpace cs) :~: CM.RGB)
         pure $ toJpeg toJPImageRGB8 (toPixel8 . toPixelBaseModel . toPixelBaseSpace) img
    ]
  where
    toSRGB8 = convertPixel :: Pixel cs e -> Pixel SRGB Word8
    toJpeg ::
         (JP.JpgEncodable px, Source r ix a)
      => (Array D ix b -> JP.Image px)
      -> (a -> b)
      -> Array r ix a
      -> BL.ByteString
    toJpeg toJP adjustPixel =
      JP.encodeDirectJpegAtQualityWithMetadata jpegQuality jpegMetadata . toJP . A.map adjustPixel
