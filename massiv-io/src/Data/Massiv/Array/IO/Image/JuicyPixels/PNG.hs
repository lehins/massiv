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
-- Module      : Data.Massiv.Array.IO.Image.JuicyPixels.PNG
-- Copyright   : (c) Alexey Kuleshevich 2019-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.IO.Image.JuicyPixels.PNG
  ( PNG(..)
  , decodePNG
  , decodeWithMetadataPNG
  , decodeAutoPNG
  , decodeAutoWithMetadataPNG
  , encodePNG
  , encodeAutoPNG
  ) where

import Prelude as P
import Data.Maybe (fromMaybe)
import qualified Codec.Picture as JP
import qualified Codec.Picture.Metadata as JP
import qualified Codec.Picture.Png as JP
import Control.Monad (msum)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL (ByteString)
import Data.Massiv.Array as A
import Data.Massiv.Array.IO.Base
import Data.Typeable
import Graphics.Pixel.ColorSpace
import qualified Graphics.Pixel as CM
import Data.Massiv.Array.IO.Image.JuicyPixels.Base

--------------------------------------------------------------------------------
-- PNG Format ------------------------------------------------------------------
--------------------------------------------------------------------------------


-- | Portable Network Graphics image with @.png@ extension.
data PNG = PNG deriving Show

instance FileFormat PNG where
  type Metadata PNG = JP.Metadatas
  ext _ = ".png"

instance Writable PNG (Image S CM.Y Word8) where
  encodeM PNG _ img = pure $ JP.encodePng (toJPImageY8 img)

instance Writable PNG (Image S CM.Y Word16) where
  encodeM PNG _ img = pure $ JP.encodePng (toJPImageY16 img)

instance Writable PNG (Image S (Alpha CM.Y) Word8) where
  encodeM PNG _ img = pure $ JP.encodePng (toJPImageYA8 img)

instance Writable PNG (Image S (Alpha CM.Y) Word16) where
  encodeM PNG _ img = pure $ JP.encodePng (toJPImageYA16 img)

instance Writable PNG (Image S CM.RGB Word8) where
  encodeM PNG _ img = pure $ JP.encodePng (toJPImageRGB8 img)

instance Writable PNG (Image S CM.RGB Word16) where
  encodeM PNG _ img = pure $ JP.encodePng (toJPImageRGB16 img)

instance Writable PNG (Image S (Alpha CM.RGB) Word8) where
  encodeM PNG _ img = pure $ JP.encodePng (toJPImageRGBA8 img)

instance Writable PNG (Image S (Alpha CM.RGB) Word16) where
  encodeM PNG _ img = pure $ JP.encodePng (toJPImageRGBA16 img)

instance (ColorSpace cs i e, ColorSpace (BaseSpace cs) i e, Source r Ix2 (Pixel cs e)) =>
         Writable (Auto PNG) (Image r cs e) where
  encodeM _ _ = pure . encodeAutoPNG


instance Readable PNG (Image S CM.Y Word8) where
  decodeM = decodePNG
  decodeWithMetadataM = decodeWithMetadataPNG

instance Readable PNG (Image S CM.Y Word16) where
  decodeM = decodePNG
  decodeWithMetadataM = decodeWithMetadataPNG

instance Readable PNG (Image S (Alpha CM.Y) Word8) where
  decodeM = decodePNG
  decodeWithMetadataM = decodeWithMetadataPNG

instance Readable PNG (Image S (Alpha CM.Y) Word16) where
  decodeM = decodePNG
  decodeWithMetadataM = decodeWithMetadataPNG

instance Readable PNG (Image S CM.RGB Word8) where
  decodeM = decodePNG
  decodeWithMetadataM = decodeWithMetadataPNG

instance Readable PNG (Image S CM.RGB Word16) where
  decodeM = decodePNG
  decodeWithMetadataM = decodeWithMetadataPNG

instance Readable PNG (Image S (Alpha CM.RGB) Word8) where
  decodeM = decodePNG
  decodeWithMetadataM = decodeWithMetadataPNG

instance Readable PNG (Image S (Alpha CM.RGB) Word16) where
  decodeM = decodePNG
  decodeWithMetadataM = decodeWithMetadataPNG

-- | Decode a Png Image
decodePNG :: (ColorModel cs e, MonadThrow m) => PNG -> B.ByteString -> m (Image S cs e)
decodePNG f bs = convertWith f (JP.decodePng bs)

-- | Decode a Png Image
decodeWithMetadataPNG ::
     (ColorModel cs e, MonadThrow m) => PNG -> B.ByteString -> m (Image S cs e, JP.Metadatas)
decodeWithMetadataPNG f bs = convertWithMetadata f (JP.decodePngWithMetadata bs)


-- | Decode a Png Image
decodeAutoPNG ::
     (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e, MonadThrow m)
  => Auto PNG
  -> B.ByteString
  -> m (Image r cs e)
decodeAutoPNG f bs = convertAutoWith f (JP.decodePng bs)

-- | Decode a Png Image
decodeAutoWithMetadataPNG ::
     (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e, MonadThrow m)
  => Auto PNG
  -> B.ByteString
  -> m (Image r cs e, JP.Metadatas)
decodeAutoWithMetadataPNG f bs = convertAutoWithMetadata f (JP.decodePngWithMetadata bs)

instance (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e) =>
         Readable (Auto PNG) (Image r cs e) where
  decodeM = decodeAutoPNG
  decodeWithMetadataM = decodeAutoWithMetadataPNG

encodePNG ::
     forall r cs e m.
     (ColorModel cs e, Source r Ix2 (Pixel cs e), MonadThrow m)
  => PNG
  -> Image r cs e
  -> m BL.ByteString
encodePNG f img =
  fromMaybeEncode f (Proxy :: Proxy (Image r cs e)) $
  msum
    [ do Refl <- eqT :: Maybe (cs :~: CM.Y)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ JP.encodePng $ toJPImageY8 img
           , do Refl <- eqT :: Maybe (e :~: Word16)
                pure $ JP.encodePng $ toJPImageY16 img
           ]
    , do Refl <- eqT :: Maybe (cs :~: Alpha CM.Y)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ JP.encodePng $ toJPImageYA8 img
           , do Refl <- eqT :: Maybe (e :~: Word16)
                pure $ JP.encodePng $ toJPImageYA16 img
           ]
    , do Refl <- eqT :: Maybe (cs :~: CM.RGB)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ JP.encodePng $ toJPImageRGB8 img
           , do Refl <- eqT :: Maybe (e :~: Word16)
                pure $ JP.encodePng $ toJPImageRGB16 img
           ]
    , do Refl <- eqT :: Maybe (cs :~: Alpha CM.RGB)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ JP.encodePng $ toJPImageRGBA8 img
           , do Refl <- eqT :: Maybe (e :~: Word16)
                pure $ JP.encodePng $ toJPImageRGBA16 img
           ]
    ]


encodeAutoPNG ::
     forall r cs i e. (ColorSpace (BaseSpace cs) i e, ColorSpace cs i e, Source r Ix2 (Pixel cs e))
  => Image r cs e
  -> BL.ByteString
encodeAutoPNG img =
  fromMaybe (toPng toJPImageRGB8 (toPixelBaseModel . toSRGB8) img) $
  msum
    [ do Refl <- eqT :: Maybe (BaseModel cs :~: CM.Y)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Bit)
                pure $ toPng toJPImageY8 (toPixel8 . toPixelBaseModel) img
           , do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ toPng toJPImageY8 toPixelBaseModel img
           -- , do Refl <- eqT :: Maybe (e :~: Word16)
           --      pure $ toPng toJPImageY16 toPixelBaseModel img
           , pure $ toPng toJPImageY16 (toPixel16 . toPixelBaseModel) img
           ]
    , do Refl <- eqT :: Maybe (BaseModel cs :~: Alpha CM.Y)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ toPng toJPImageYA8 toPixelBaseModel img
           -- , do Refl <- eqT :: Maybe (e :~: Word16)
           --      pure $ toPng toJPImageYA16 toPixelBaseModel img
           , pure $ toPng toJPImageYA16 (toPixel16 . toPixelBaseModel) img
           ]
    , do Refl <- eqT :: Maybe (BaseModel (BaseSpace cs) :~: CM.RGB)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ toPng toJPImageRGB8 (toPixelBaseModel . toPixelBaseSpace) img
           -- , do Refl <- eqT :: Maybe (e :~: Word16)
           --      pure $ toPng toJPImageRGB16 (toPixelBaseModel . toPixelBaseSpace) img
           , pure $ toPng toJPImageRGB16 (toPixel16 . toPixelBaseModel . toPixelBaseSpace) img
           ]
    , do Refl <- eqT :: Maybe (BaseModel (BaseSpace cs) :~: Alpha CM.RGB)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ toPng toJPImageRGBA8 (toPixelBaseModel . toPixelBaseSpace) img
           -- , do Refl <- eqT :: Maybe (e :~: Word16)
           --      pure $ toPng toJPImageRGBA16 (toPixelBaseModel . toPixelBaseSpace) img
           , pure $ toPng toJPImageRGBA16 (toPixel16 . toPixelBaseModel . toPixelBaseSpace) img
           ]
    , do Refl <- eqT :: Maybe (cs :~: Alpha (Opaque cs))
         pure $ toPng toJPImageRGBA8 (toPixelBaseModel . toSRGBA8) img
    ]
  where
    toSRGB8 = convertPixel :: Pixel cs e -> Pixel SRGB Word8
    toSRGBA8 = convertPixel :: Pixel cs e -> Pixel (Alpha SRGB) Word8
    toPng ::
         (JP.PngSavable px, Source r ix a)
      => (Array D ix b -> JP.Image px)
      -> (a -> b)
      -> Array r ix a
      -> BL.ByteString
    toPng toJP adjustPixel = JP.encodePng . toJP . A.map adjustPixel
