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

import Prelude as P
import Data.Maybe (fromMaybe)
import qualified Codec.Picture as JP
import qualified Codec.Picture.Metadata as JP
import qualified Codec.Picture.Tga as JP
import Control.Monad (msum)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL (ByteString)
import Data.Massiv.Array as A
import Data.Massiv.Array.IO.Base
import Data.Typeable
import Graphics.Color.Model.Alpha
import qualified Graphics.Color.Model.RGB as CM
import qualified Graphics.Color.Model.Y as CM
import Graphics.Color.Pixel
import Graphics.Color.Algebra.Binary
import Data.Massiv.Array.IO.Image.JuicyPixels.Base


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


instance (ColorSpace cs i e, ColorSpace (BaseSpace cs) i e, Source r Ix2 (Pixel cs e)) =>
         Writable (Auto TGA) (Image r cs e) where
  encodeM _ _ = pure . encodeAutoTGA


instance Readable TGA (Image S CM.Y Word8) where
  decodeM f _ = decodeTGA f
  decodeWithMetadataM f _ = decodeWithMetadataTGA f

instance Readable TGA (Image S CM.RGB Word8) where
  decodeM f _ = decodeTGA f
  decodeWithMetadataM f _ = decodeWithMetadataTGA f

instance Readable TGA (Image S (Alpha CM.RGB) Word8) where
  decodeM f _ = decodeTGA f
  decodeWithMetadataM f _ = decodeWithMetadataTGA f

-- | Decode a Tga Image
decodeTGA :: (ColorModel cs e, MonadThrow m) => TGA -> B.ByteString -> m (Image S cs e)
decodeTGA f bs = fst <$> decodeWithMetadataTGA f bs
  --convertWith f (JP.decodeTga bs)

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
  decodeWithMetadataM f _ = decodeAutoWithMetadataTGA f

encodeTGA ::
     forall r cs e m.
     (ColorModel cs e, Source r Ix2 (Pixel cs e), MonadThrow m)
  => TGA
  -> Image r cs e
  -> m BL.ByteString
encodeTGA f img =
  fromMaybeEncode f (Proxy :: Proxy (Image r cs e)) $
  msum
    [ do Refl <- eqT :: Maybe (cs :~: CM.Y)
         Refl <- eqT :: Maybe (e :~: Word8)
         pure $ JP.encodeTga $ toJPImageY8 img
    , do Refl <- eqT :: Maybe (cs :~: CM.RGB)
         Refl <- eqT :: Maybe (e :~: Word8)
         pure $ JP.encodeTga $ toJPImageRGB8 img
    , do Refl <- eqT :: Maybe (cs :~: Alpha CM.RGB)
         Refl <- eqT :: Maybe (e :~: Word8)
         pure $ JP.encodeTga $ toJPImageRGBA8 img
    ]


encodeAutoTGA ::
     forall r cs i e.
     ( ColorSpace (BaseSpace cs) i e
     , ColorSpace cs i e
     , Source r Ix2 (Pixel cs e)
     )
  => Image r cs e
  -> BL.ByteString
encodeAutoTGA img =
  fromMaybe (toTga toJPImageRGB8 (toPixelBaseModel . toSRGB8) img) $
  msum
    [ do Refl <- eqT :: Maybe (BaseModel cs :~: CM.Y)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Bit)
                pure $ toTga toJPImageY8 (toPixel8 . toPixelBaseModel) img
           , do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ toTga toJPImageY8 toPixelBaseModel img
           , pure $ toTga toJPImageY8 (toPixel8 . toPixelBaseModel) img
           ]
    , do Refl <- eqT :: Maybe (BaseModel (BaseSpace cs) :~: CM.RGB)
         pure $ toTga toJPImageRGB8 (toPixel8 . toPixelBaseModel . toPixelBaseSpace) img
    , do Refl <- eqT :: Maybe (BaseModel (BaseSpace cs) :~: Alpha CM.RGB)
         pure $ toTga toJPImageRGBA8 (toPixel8 . toPixelBaseModel . toPixelBaseSpace) img
    , do Refl <- eqT :: Maybe (cs :~: Alpha (Opaque cs))
         pure $ toTga toJPImageRGBA8 (toPixelBaseModel . toSRGBA8) img
    ]
  where
    toSRGB8 = convertPixel :: Pixel cs e -> Pixel SRGB Word8
    toSRGBA8 = convertPixel :: Pixel cs e -> Pixel (Alpha SRGB) Word8
    toTga ::
         (JP.TgaSaveable px, Source r ix a)
      => (Array D ix b -> JP.Image px)
      -> (a -> b)
      -> Array r ix a
      -> BL.ByteString
    toTga toJP adjustPixel = JP.encodeTga . toJP . A.map adjustPixel
