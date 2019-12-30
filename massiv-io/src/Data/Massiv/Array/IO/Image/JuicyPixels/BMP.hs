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
-- Module      : Data.Massiv.Array.IO.Image.JuicyPixels.BMP
-- Copyright   : (c) Alexey Kuleshevich 2019-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.IO.Image.JuicyPixels.BMP
  ( BMP(..)
  , BitmapOptions(..)
  , decodeBMP
  , decodeWithMetadataBMP
  , decodeAutoBMP
  , decodeAutoWithMetadataBMP
  , encodeBMP
  , encodeAutoBMP
  ) where

import Prelude as P
import Data.Maybe (fromMaybe)
import qualified Codec.Picture as JP
import qualified Codec.Picture.Metadata as JP
import qualified Codec.Picture.Bitmap as JP
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
-- BMP Format ------------------------------------------------------------------
--------------------------------------------------------------------------------

newtype BitmapOptions = BitmapOptions
  { bitmapMetadata :: JP.Metadatas
  } deriving (Show)

instance Default BitmapOptions where
  def = BitmapOptions mempty

-- | Bitmap image with @.bmp@ extension.
data BMP = BMP deriving Show

instance FileFormat BMP where
  type WriteOptions BMP = BitmapOptions
  type Metadata BMP = JP.Metadatas

  ext _ = ".bmp"

instance Writable BMP (Image S CM.Y Word8) where
  encodeM BMP BitmapOptions {bitmapMetadata} =
    pure . JP.encodeBitmapWithMetadata bitmapMetadata . toJPImageY8

instance Writable BMP (Image S CM.RGB Word8) where
  encodeM BMP BitmapOptions {bitmapMetadata} =
    pure . JP.encodeBitmapWithMetadata bitmapMetadata . toJPImageRGB8

instance Writable BMP (Image S (Alpha CM.RGB) Word8) where
  encodeM BMP BitmapOptions {bitmapMetadata} =
    pure . JP.encodeBitmapWithMetadata bitmapMetadata . toJPImageRGBA8

instance (ColorSpace cs i e, ColorSpace (BaseSpace cs) i e, Source r Ix2 (Pixel cs e)) =>
         Writable (Auto BMP) (Image r cs e) where
  encodeM _ opts = pure . encodeAutoBMP opts


instance Readable BMP (Image S CM.Y Word8) where
  decodeM f _ = decodeBMP f
  decodeWithMetadataM f _ = decodeWithMetadataBMP f

instance Readable BMP (Image S CM.RGB Word8) where
  decodeM f _ = decodeBMP f
  decodeWithMetadataM f _ = decodeWithMetadataBMP f

instance Readable BMP (Image S (Alpha CM.RGB) Word8) where
  decodeM f _ = decodeBMP f
  decodeWithMetadataM f _ = decodeWithMetadataBMP f


-- | Decode a Bitmap Image
decodeBMP :: (ColorModel cs e, MonadThrow m) => BMP -> B.ByteString -> m (Image S cs e)
decodeBMP f bs = convertWith f (JP.decodeBitmap bs)

-- | Decode a Bitmap Image
decodeWithMetadataBMP ::
     (ColorModel cs e, MonadThrow m) => BMP -> B.ByteString -> m (Image S cs e, JP.Metadatas)
decodeWithMetadataBMP f bs = convertWithMetadata f (JP.decodeBitmapWithMetadata bs)


-- | Decode a Bitmap Image
decodeAutoBMP ::
     (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e, MonadThrow m)
  => Auto BMP
  -> B.ByteString
  -> m (Image r cs e)
decodeAutoBMP f bs = convertAutoWith f (JP.decodeBitmap bs)

-- | Decode a Bitmap Image
decodeAutoWithMetadataBMP ::
     (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e, MonadThrow m)
  => Auto BMP
  -> B.ByteString
  -> m (Image r cs e, JP.Metadatas)
decodeAutoWithMetadataBMP f bs = convertAutoWithMetadata f (JP.decodeBitmapWithMetadata bs)

instance (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e) =>
         Readable (Auto BMP) (Image r cs e) where
  decodeM f _ = decodeAutoBMP f
  decodeWithMetadataM f _ = decodeAutoWithMetadataBMP f

encodeBMP ::
     forall r cs e m.
     (ColorModel cs e, Source r Ix2 (Pixel cs e), MonadThrow m)
  => BMP
  -> BitmapOptions
  -> Image r cs e
  -> m BL.ByteString
encodeBMP f BitmapOptions {bitmapMetadata} img =
  fromMaybeEncode f (Proxy :: Proxy (Image r cs e)) $ do
    Refl <- eqT :: Maybe (e :~: Word8)
    msum
      [ do Refl <- eqT :: Maybe (cs :~: CM.Y)
           pure $ JP.encodeBitmapWithMetadata bitmapMetadata $ toJPImageY8 img
      , do Refl <- eqT :: Maybe (cs :~: CM.RGB)
           pure $ JP.encodeBitmapWithMetadata bitmapMetadata $ toJPImageRGB8 img
      , do Refl <- eqT :: Maybe (cs :~: Alpha CM.RGB)
           pure $ JP.encodeBitmapWithMetadata bitmapMetadata $ toJPImageRGBA8 img
      ]



encodeAutoBMP ::
     forall r cs i e. (ColorSpace (BaseSpace cs) i e, ColorSpace cs i e, Source r Ix2 (Pixel cs e))
  => BitmapOptions
  -> Image r cs e
  -> BL.ByteString
encodeAutoBMP BitmapOptions {bitmapMetadata} img =
  fromMaybe (toBitmap toJPImageRGB8 (toPixelBaseModel . toSRGB8) img) $
  msum
    [ do Refl <- eqT :: Maybe (BaseModel cs :~: CM.Y)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Bit)
                pure $ toBitmap toJPImageY8 (toPixel8 . toPixelBaseModel) img
           , pure $ toBitmap toJPImageY8 (toPixel8 . toPixelBaseModel) img
           ]
    , do Refl <- eqT :: Maybe (BaseModel (BaseSpace cs) :~: CM.RGB)
         pure $ toBitmap toJPImageRGB8 (toPixel8 . toPixelBaseModel . toPixelBaseSpace) img
    , do Refl <- eqT :: Maybe (BaseModel (BaseSpace cs) :~: Alpha CM.RGB)
         pure $ toBitmap toJPImageRGBA8 (toPixel8 . toPixelBaseModel . toPixelBaseSpace) img
    , do Refl <- eqT :: Maybe (cs :~: Alpha (Opaque cs))
         pure $ toBitmap toJPImageRGBA8 (toPixelBaseModel . toSRGBA8) img
    ]
  where
    toSRGB8 = convertPixel :: Pixel cs e -> Pixel SRGB Word8
    toSRGBA8 = convertPixel :: Pixel cs e -> Pixel (Alpha SRGB) Word8
    toBitmap ::
         (JP.BmpEncodable px, Source r ix a)
      => (Array D ix b -> JP.Image px)
      -> (a -> b)
      -> Array r ix a
      -> BL.ByteString
    toBitmap toJP adjustPixel =
      JP.encodeBitmapWithMetadata bitmapMetadata . toJP . A.map adjustPixel
