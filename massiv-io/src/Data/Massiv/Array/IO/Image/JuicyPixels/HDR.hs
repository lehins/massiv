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

import Prelude as P
import Data.Maybe (fromMaybe)
import qualified Codec.Picture as JP
import qualified Codec.Picture.Metadata as JP
import qualified Codec.Picture.HDR as JP
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL (ByteString)
import Data.Massiv.Array as A
import Data.Massiv.Array.IO.Base
import Data.Typeable
import Graphics.Color.Model.Alpha
import qualified Graphics.Color.Model.RGB as CM
import Graphics.Color.Pixel
import Data.Massiv.Array.IO.Image.JuicyPixels.Base

--------------------------------------------------------------------------------
-- HDR Format ------------------------------------------------------------------
--------------------------------------------------------------------------------

-- TODOs: Check if JP is capable of writing Jpegs with PixelYA8 (No instance for
-- JpegEncodable, but it can read 'em)

newtype HdrOptions = HdrOptions
  { hdrUseLightRLE :: Bool
  -- ^ User light RLE compression. Causes problems in some viewers. See:
  -- `JP.encodeRLENewStyleHDR`
  } deriving (Show)

instance Default HdrOptions where
  def = HdrOptions False


-- | Joint Photographic Experts Group image with @.jpg@ or @.jpeg@ extension.
data HDR = HDR deriving Show

instance FileFormat HDR where
  type WriteOptions HDR = HdrOptions
  type Metadata HDR = JP.Metadatas
  ext _ = ".jpg"
  exts _ = [".jpg", ".jpeg"]

getHdrEncoder
  :: HdrOptions -> JP.Image JP.PixelRGBF -> BL.ByteString
getHdrEncoder HdrOptions {hdrUseLightRLE}
  | hdrUseLightRLE = JP.encodeRLENewStyleHDR
  | otherwise = JP.encodeHDR

instance Writable HDR (Image S CM.RGB Float) where
  encodeM HDR opts = pure . getHdrEncoder opts . toJPImageRGBF

instance (ColorSpace cs i e, ColorSpace (BaseSpace cs) i e, Source r Ix2 (Pixel cs e)) =>
         Writable (Auto HDR) (Image r cs e) where
  encodeM _ opts = pure . encodeAutoHDR opts


instance Readable HDR (Image S CM.RGB Float) where
  decodeM f _ = decodeHDR f
  decodeWithMetadataM f _ = decodeWithMetadataHDR f

-- | Decode a Jpeg Image
decodeHDR :: (ColorModel cs e, MonadThrow m) => HDR -> B.ByteString -> m (Image S cs e)
decodeHDR f bs = convertWith f (JP.decodeJpeg bs)

-- | Decode a Jpeg Image
decodeWithMetadataHDR ::
     (ColorModel cs e, MonadThrow m) => HDR -> B.ByteString -> m (Image S cs e, JP.Metadatas)
decodeWithMetadataHDR f bs = convertWithMetadata f (JP.decodeHDRWithMetadata bs)


-- | Decode a Jpeg Image
decodeAutoHDR ::
     (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e, MonadThrow m)
  => Auto HDR
  -> B.ByteString
  -> m (Image r cs e)
decodeAutoHDR f bs = convertAutoWith f (JP.decodeJpeg bs)

-- | Decode a Jpeg Image
decodeAutoWithMetadataHDR ::
     (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e, MonadThrow m)
  => Auto HDR
  -> B.ByteString
  -> m (Image r cs e, JP.Metadatas)
decodeAutoWithMetadataHDR f bs = convertAutoWithMetadata f (JP.decodeHDRWithMetadata bs)

instance (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e) =>
         Readable (Auto HDR) (Image r cs e) where
  decodeM f _ = decodeAutoHDR f
  decodeWithMetadataM f _ = decodeAutoWithMetadataHDR f

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
  => HdrOptions
  -> Image r cs e
  -> BL.ByteString
encodeAutoHDR opts img =
  fromMaybe (toHdr (toPixelBaseModel . toSRGBF) img) $ do
    Refl <- eqT :: Maybe (BaseModel (BaseSpace cs) :~: CM.RGB)
    pure $ toHdr (toPixelF . toPixelBaseModel . toPixelBaseSpace) img
  where
    toSRGBF = convertPixel :: Pixel cs e -> Pixel SRGB Float
    toHdr :: Source r Ix2 a => (a -> Pixel CM.RGB Float) -> Array r Ix2 a -> BL.ByteString
    toHdr adjustPixel = getHdrEncoder opts . toJPImageRGBF . A.map adjustPixel
