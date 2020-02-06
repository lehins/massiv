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
-- Module      : Data.Massiv.Array.IO.Image.JuicyPixels.HDR
-- Copyright   : (c) Alexey Kuleshevich 2019-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.IO.Image.JuicyPixels.HDR
  ( HDR(..)
  , HdrOptions(..)
  , decodeHDR
  , decodeWithMetadataHDR
  , decodeAutoHDR
  , decodeAutoWithMetadataHDR
  , encodeHDR
  , encodeAutoHDR
  ) where

import qualified Codec.Picture as JP
import qualified Codec.Picture.HDR as JP
import qualified Codec.Picture.Metadata as JP
import Data.Bifunctor (first)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL (ByteString)
import Data.Massiv.Array as A
import Data.Massiv.Array.IO.Base
import Data.Massiv.Array.IO.Image.JuicyPixels.Base
import Data.Typeable
import qualified Graphics.Pixel as CM
import Graphics.Pixel.ColorSpace
import Prelude as P

--------------------------------------------------------------------------------
-- HDR Format ------------------------------------------------------------------
--------------------------------------------------------------------------------

newtype HdrOptions = HdrOptions
  { hdrUseLightRLE :: Bool
  -- ^ Use light RLE compression. Causes problems in some viewers. See:
  -- `JP.encodeRLENewStyleHDR`
  } deriving (Show)

instance Default HdrOptions where
  def = HdrOptions False


-- | High-dynamic-range image with @.hdr@ or @.pic@ extension.
data HDR = HDR deriving Show

instance FileFormat HDR where
  type WriteOptions HDR = HdrOptions
  type Metadata HDR = JP.Metadatas
  ext _ = ".hdr"
  exts _ = [".hdr", ".pic"]

getHdrEncoder
  :: HdrOptions -> JP.Image JP.PixelRGBF -> BL.ByteString
getHdrEncoder HdrOptions {hdrUseLightRLE}
  | hdrUseLightRLE = JP.encodeRLENewStyleHDR
  | otherwise = JP.encodeHDR

instance Writable HDR (Image S CM.RGB Float) where
  encodeM HDR opts = pure . getHdrEncoder opts . toJPImageRGBF

instance Writable HDR (Image S SRGB Float) where
  encodeM f opts = encodeM f opts . toImageBaseModel

instance (ColorSpace cs i e, ColorSpace (BaseSpace cs) i e, Source r Ix2 (Pixel cs e)) =>
         Writable (Auto HDR) (Image r cs e) where
  encodeM f opts = pure . encodeAutoHDR f opts


instance Readable HDR (Image S CM.RGB Float) where
  decodeWithMetadataM = decodeWithMetadataHDR

instance Readable HDR (Image S SRGB Float) where
  decodeWithMetadataM f = fmap (first fromImageBaseModel) . decodeWithMetadataM f

-- | Decode a HDR Image
decodeHDR :: (ColorModel cs e, MonadThrow m) => HDR -> B.ByteString -> m (Image S cs e)
decodeHDR f bs = convertWith f (JP.decodeHDR bs)

-- | Decode a HDR Image
decodeWithMetadataHDR ::
     (ColorModel cs e, MonadThrow m) => HDR -> B.ByteString -> m (Image S cs e, JP.Metadatas)
decodeWithMetadataHDR f bs = convertWithMetadata f (JP.decodeHDRWithMetadata bs)


-- | Decode a HDR Image
decodeAutoHDR ::
     (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e, MonadThrow m)
  => Auto HDR
  -> B.ByteString
  -> m (Image r cs e)
decodeAutoHDR f bs = convertAutoWith f (JP.decodeHDR bs)

-- | Decode a HDR Image
decodeAutoWithMetadataHDR ::
     (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e, MonadThrow m)
  => Auto HDR
  -> B.ByteString
  -> m (Image r cs e, JP.Metadatas)
decodeAutoWithMetadataHDR f bs = convertAutoWithMetadata f (JP.decodeHDRWithMetadata bs)

instance (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e) =>
         Readable (Auto HDR) (Image r cs e) where
  decodeM = decodeAutoHDR
  decodeWithMetadataM = decodeAutoWithMetadataHDR

encodeHDR ::
     forall r cs e m.
     (ColorModel cs e, Source r Ix2 (Pixel cs e), MonadThrow m)
  => HDR
  -> HdrOptions
  -> Image r cs e
  -> m BL.ByteString
encodeHDR f opts img =
  fromMaybeEncode f (Proxy :: Proxy (Image r cs e)) $ do
    Refl <- eqT :: Maybe (e :~: Float)
    Refl <- eqT :: Maybe (cs :~: CM.RGB)
    pure $ getHdrEncoder opts $ toJPImageRGBF img



encodeAutoHDR ::
     forall r cs i e. (ColorSpace (BaseSpace cs) i e, ColorSpace cs i e, Source r Ix2 (Pixel cs e))
  => Auto HDR
  -> HdrOptions
  -> Image r cs e
  -> BL.ByteString
encodeAutoHDR _ opts = toHdr (toPixelBaseModel . toSRGBF)
  where
    toSRGBF = convertPixel :: Pixel cs e -> Pixel SRGB Float
    toHdr :: Source r Ix2 a => (a -> Pixel CM.RGB Float) -> Array r Ix2 a -> BL.ByteString
    toHdr adjustPixel = getHdrEncoder opts . toJPImageRGBF . A.map adjustPixel
