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
-- Module      : Data.Massiv.Array.IO.Image.JuicyPixels.TGA
-- Copyright   : (c) Alexey Kuleshevich 2019-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.IO.Image.JuicyPixels.TGA
  ( TGA(..)
  , decodeTGA
  , decodeWithMetadataTGA
  , decodeAutoTGA
  , decodeAutoWithMetadataTGA
  , encodeTGA
  , encodeAutoTGA
  ) where

import qualified Codec.Picture as JP
import qualified Codec.Picture.Metadata as JP
import qualified Codec.Picture.Tga as JP
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
-- TGA Format ------------------------------------------------------------------
--------------------------------------------------------------------------------


-- | Truevision Graphics Adapter image with .tga extension.
data TGA = TGA deriving Show

instance FileFormat TGA where
  type Metadata TGA = JP.Metadatas
  ext _ = ".tga"

instance Writable TGA (Image S CM.Y Word8) where
  encodeM TGA _ img = pure $ JP.encodeTga (toJPImageY8 img)

instance Writable TGA (Image S CM.RGB Word8) where
  encodeM TGA _ img = pure $ JP.encodeTga (toJPImageRGB8 img)

instance Writable TGA (Image S (Alpha CM.RGB) Word8) where
  encodeM TGA _ img = pure $ JP.encodeTga (toJPImageRGBA8 img)


instance Writable TGA (Image S Y' Word8) where
  encodeM f opts = encodeM f opts . demoteLumaImage

instance Writable TGA (Image S (Y D65) Word8) where
  encodeM f opts = encodeM f opts . toImageBaseModel

instance Writable TGA (Image S SRGB Word8) where
  encodeM f opts = encodeM f opts . toImageBaseModel

instance Writable TGA (Image S (Alpha SRGB) Word8) where
  encodeM f opts = encodeM f opts . toImageBaseModel


instance (ColorSpace cs i e, ColorSpace (BaseSpace cs) i e, Source r Ix2 (Pixel cs e)) =>
         Writable (Auto TGA) (Image r cs e) where
  encodeM f _ = pure . encodeAutoTGA f


instance Readable TGA (Image S CM.Y Word8) where
  decodeWithMetadataM = decodeWithMetadataTGA

instance Readable TGA (Image S CM.RGB Word8) where
  decodeWithMetadataM = decodeWithMetadataTGA

instance Readable TGA (Image S (Alpha CM.RGB) Word8) where
  decodeWithMetadataM = decodeWithMetadataTGA


instance Readable TGA (Image S Y' Word8) where
  decodeWithMetadataM f = fmap (first promoteLumaImage) . decodeWithMetadataM f

instance Readable TGA (Image S (Y D65) Word8) where
  decodeWithMetadataM f = fmap (first fromImageBaseModel) . decodeWithMetadataM f

instance Readable TGA (Image S SRGB Word8) where
  decodeWithMetadataM f = fmap (first fromImageBaseModel) . decodeWithMetadataM f

instance Readable TGA (Image S (Alpha SRGB) Word8) where
  decodeWithMetadataM f = fmap (first fromImageBaseModel) . decodeWithMetadataM f

-- | Decode a Tga Image
decodeTGA :: (ColorModel cs e, MonadThrow m) => TGA -> B.ByteString -> m (Image S cs e)
decodeTGA f bs = convertWith f (JP.decodeTga bs)

-- | Decode a Tga Image
decodeWithMetadataTGA ::
     (ColorModel cs e, MonadThrow m) => TGA -> B.ByteString -> m (Image S cs e, JP.Metadatas)
decodeWithMetadataTGA f bs = convertWithMetadata f (JP.decodeTgaWithMetadata bs)


-- | Decode a Tga Image
decodeAutoTGA ::
     (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e, MonadThrow m)
  => Auto TGA
  -> B.ByteString
  -> m (Image r cs e)
decodeAutoTGA f bs = convertAutoWith f (JP.decodeTga bs)

-- | Decode a Tga Image
decodeAutoWithMetadataTGA ::
     (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e, MonadThrow m)
  => Auto TGA
  -> B.ByteString
  -> m (Image r cs e, JP.Metadatas)
decodeAutoWithMetadataTGA f bs = convertAutoWithMetadata f (JP.decodeTgaWithMetadata bs)


instance (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e) =>
         Readable (Auto TGA) (Image r cs e) where
  decodeWithMetadataM = decodeAutoWithMetadataTGA

encodeTGA ::
     forall cs e m. (ColorModel cs e, MonadThrow m)
  => TGA
  -> Image S cs e
  -> m BL.ByteString
encodeTGA f img =
  fromMaybeEncode f (Proxy :: Proxy (Image S cs e)) $ do
    Refl <- eqT :: Maybe (e :~: Word8)
    msum
      [ JP.encodeTga <$> maybeJPImageY8 img
      , JP.encodeTga <$> maybeJPImageRGB8 img
      , do Refl <- eqT :: Maybe (cs :~: Alpha (Opaque cs))
           JP.encodeTga <$> maybeJPImageRGBA8 img
      ]


encodeAutoTGA ::
     forall r cs i e.
     ( ColorSpace (BaseSpace cs) i e
     , ColorSpace cs i e
     , Source r Ix2 (Pixel cs e)
     )
  => Auto TGA
  -> Image r cs e
  -> BL.ByteString
encodeAutoTGA _ img =
  fromMaybe (toTga toJPImageRGB8 toSRGB8 img) $
  msum
    [ do Refl <- eqT :: Maybe (BaseModel cs :~: CM.Y)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Bit)
                pure $ toTga toJPImageY8 (toPixel8 . toPixelBaseModel) img
           , do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ toTga toJPImageY8 toPixelBaseModel img
           , pure $ toTga toJPImageY8 (toPixel8 . toPixelBaseModel) img
           ]
    , do Refl <- eqT :: Maybe (cs :~: Alpha (Opaque cs))
         pure $ toTga toJPImageRGBA8 toSRGBA8 img
    ]
  where
    toTga ::
         (JP.TgaSaveable px, Source r ix a)
      => (Array D ix b -> JP.Image px)
      -> (a -> b)
      -> Array r ix a
      -> BL.ByteString
    toTga toJP adjustPixel = JP.encodeTga . toJP . A.map adjustPixel
