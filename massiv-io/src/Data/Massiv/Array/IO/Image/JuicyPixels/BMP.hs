{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
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

import qualified Codec.Picture as JP
import qualified Codec.Picture.Bitmap as JP
import qualified Codec.Picture.Metadata as JP
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


instance Writable BMP (Image S Y' Word8) where
  encodeM f opts = encodeM f opts . demoteLumaImage

instance Writable BMP (Image S (Y D65) Word8) where
  encodeM f opts = encodeM f opts . toImageBaseModel

instance Writable BMP (Image S SRGB Word8) where
  encodeM f opts = encodeM f opts . toImageBaseModel

instance Writable BMP (Image S (Alpha SRGB) Word8) where
  encodeM f opts = encodeM f opts . toImageBaseModel

instance (ColorSpace cs i e, ColorSpace (BaseSpace cs) i e, Source r Ix2 (Pixel cs e)) =>
         Writable (Auto BMP) (Image r cs e) where
  encodeM f opts = pure . encodeAutoBMP f opts


instance Readable BMP (Image S CM.Y Word8) where
  decodeWithMetadataM = decodeWithMetadataBMP

instance Readable BMP (Image S CM.RGB Word8) where
  decodeWithMetadataM = decodeWithMetadataBMP

instance Readable BMP (Image S (Alpha CM.RGB) Word8) where
  decodeWithMetadataM = decodeWithMetadataBMP

instance Readable BMP (Image S Y' Word8) where
  decodeWithMetadataM f = fmap (first promoteLumaImage) . decodeWithMetadataM f

instance Readable BMP (Image S (Y D65) Word8) where
  decodeWithMetadataM f = fmap (first fromImageBaseModel) . decodeWithMetadataM f

instance Readable BMP (Image S SRGB Word8) where
  decodeWithMetadataM f = fmap (first fromImageBaseModel) . decodeWithMetadataM f

instance Readable BMP (Image S (Alpha SRGB) Word8) where
  decodeWithMetadataM f = fmap (first fromImageBaseModel) . decodeWithMetadataM f

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
  decodeM = decodeAutoBMP
  decodeWithMetadataM = decodeAutoWithMetadataBMP

encodeBMP ::
     forall cs e m. (ColorModel cs e, MonadThrow m)
  => BMP
  -> BitmapOptions
  -> Image S cs e
  -> m BL.ByteString
encodeBMP f BitmapOptions {bitmapMetadata} img =
  fromMaybeEncode f (Proxy :: Proxy (Image S cs e)) $ do
    Refl <- eqT :: Maybe (e :~: Word8)
    msum
      [ JP.encodeBitmapWithMetadata bitmapMetadata <$> maybeJPImageY8 img
      , JP.encodeBitmapWithMetadata bitmapMetadata <$> maybeJPImageRGB8 img
      , do Refl <- eqT :: Maybe (cs :~: Alpha (Opaque cs))
           JP.encodeBitmapWithMetadata bitmapMetadata <$> maybeJPImageRGBA8 img
      ]



encodeAutoBMP ::
     forall r cs i e. (ColorSpace (BaseSpace cs) i e, ColorSpace cs i e, Source r Ix2 (Pixel cs e))
  => Auto BMP
  -> BitmapOptions
  -> Image r cs e
  -> BL.ByteString
encodeAutoBMP _ BitmapOptions {bitmapMetadata} img =
  fromMaybe (toBitmap toJPImageRGB8 toSRGB8 img) $
  msum
    [ do Refl <- eqT :: Maybe (BaseModel cs :~: CM.Y)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Bit)
                pure $ toBitmap toJPImageY8 (toPixel8 . toPixelBaseModel) img
           , pure $ toBitmap toJPImageY8 (toPixel8 . toPixelBaseModel) img
           ]
    , do Refl <- eqT :: Maybe (cs :~: Alpha (Opaque cs))
         pure $ toBitmap toJPImageRGBA8 toSRGBA8 img
    ]
  where
    toBitmap ::
         (JP.BmpEncodable px, Source r ix a)
      => (Array D ix b -> JP.Image px)
      -> (a -> b)
      -> Array r ix a
      -> BL.ByteString
    toBitmap toJP adjustPixel =
      JP.encodeBitmapWithMetadata bitmapMetadata . toJP . A.map adjustPixel
