{-# LANGUAGE LambdaCase #-}
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
-- Module      : Data.Massiv.Array.IO.Image.JuicyPixels.GIF
-- Copyright   : (c) Alexey Kuleshevich 2019-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.IO.Image.JuicyPixels.GIF
  ( GIF(..)
  , GifOptions(..)
  , JP.PaletteOptions(..)
  , JP.PaletteCreationMethod(..)

  -- , JP.GifDisposalMethod(..)
  -- ,
  , decodeGIF
  , decodeWithMetadataGIF
  , decodeAutoGIF
  , decodeAutoWithMetadataGIF
  , encodeGIF
  , encodeAutoGIF
  -- Sequence
  , decodeSequenceGIF
  , decodeSequenceWithMetadataGIF
  , decodeAutoSequenceGIF
  , decodeAutoSequenceWithMetadataGIF
  ) where

import Prelude as P
import qualified Codec.Picture as JP
import qualified Codec.Picture.Metadata as JP
import qualified Codec.Picture.Gif as JP
import qualified Codec.Picture.ColorQuant as JP
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL (ByteString)
import Data.Massiv.Array as A
import Data.Massiv.Array.IO.Base
import Data.Typeable
import Graphics.Pixel.ColorSpace
import qualified Graphics.Pixel as CM
import Data.Massiv.Array.IO.Image.JuicyPixels.Base

--------------------------------------------------------------------------------
-- GIF Format ------------------------------------------------------------------
--------------------------------------------------------------------------------


newtype GifOptions = GifOptions
  { gifPaletteOptions :: JP.PaletteOptions
  }

instance Default GifOptions where
  def = GifOptions JP.defaultPaletteOptions


-- | Graphics Interchange Format image with @.gif@ extension.
data GIF = GIF deriving Show

instance FileFormat GIF where
  type WriteOptions GIF = GifOptions
  type Metadata GIF = JP.Metadatas
  ext _ = ".gif"

instance Writable GIF (Image S CM.Y Word8) where
  encodeM GIF _ =  pure . JP.encodeGifImage . toJPImageY8

instance Writable GIF (Image S CM.RGB Word8) where
  encodeM GIF = encodePalettizedRGB

encodePalettizedRGB ::
     (MonadThrow m, Source r Ix2 (Pixel CM.RGB Word8))
  => GifOptions
  -> Image r CM.RGB Word8
  -> m BL.ByteString
encodePalettizedRGB GifOptions {gifPaletteOptions} =
  encodeError .
  uncurry JP.encodeGifImageWithPalette . JP.palettize gifPaletteOptions . toJPImageRGB8

instance (ColorSpace cs i e, ColorSpace (BaseSpace cs) i e, Source r Ix2 (Pixel cs e)) =>
         Writable (Auto GIF) (Image r cs e) where
  encodeM _ = encodeAutoGIF


instance Readable GIF (Image S CM.RGB Word8) where
  decodeM = decodeGIF
  decodeWithMetadataM = decodeWithMetadataGIF

instance Readable GIF (Image S (Alpha CM.RGB) Word8) where
  decodeM = decodeGIF
  decodeWithMetadataM = decodeWithMetadataGIF

-- | Decode a Gif Image
decodeGIF :: (ColorModel cs e, MonadThrow m) => GIF -> B.ByteString -> m (Image S cs e)
decodeGIF f bs = convertWith f (JP.decodeGif bs)

-- | Decode a Gif Image
decodeWithMetadataGIF ::
     (ColorModel cs e, MonadThrow m) => GIF -> B.ByteString -> m (Image S cs e, JP.Metadatas)
decodeWithMetadataGIF f bs = convertWithMetadata f (JP.decodeGifWithMetadata bs)


-- | Decode a Gif Image
decodeAutoGIF ::
     (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e, MonadThrow m)
  => Auto GIF
  -> B.ByteString
  -> m (Image r cs e)
decodeAutoGIF f bs = convertAutoWith f (JP.decodeGif bs)

-- | Decode a Gif Image
decodeAutoWithMetadataGIF ::
     (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e, MonadThrow m)
  => Auto GIF
  -> B.ByteString
  -> m (Image r cs e, JP.Metadatas)
decodeAutoWithMetadataGIF f bs = convertAutoWithMetadata f (JP.decodeGifWithMetadata bs)


instance (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e) =>
         Readable (Auto GIF) (Image r cs e) where
  decodeM = decodeAutoGIF
  decodeWithMetadataM = decodeAutoWithMetadataGIF

encodeGIF ::
     forall r cs e m.
     (ColorModel cs e, Source r Ix2 (Pixel cs e), MonadThrow m)
  => GIF
  -> GifOptions
  -> Image r cs e
  -> m BL.ByteString
encodeGIF f opts img =
  fallbackEncodePalettizedRGB $ do
    Refl <- eqT :: Maybe (cs :~: CM.Y)
    Refl <- eqT :: Maybe (e :~: Word8)
    pure $ JP.encodeGifImage $ toJPImageY8 img
  where
    fallbackEncodePalettizedRGB =
      \case
        Just bs -> pure bs
        Nothing
          | Just Refl <- (eqT :: Maybe (e :~: Word8))
          , Just Refl <- (eqT :: Maybe (cs :~: CM.RGB)) -> encodePalettizedRGB opts img
        Nothing -> fromMaybeEncode f (Proxy :: Proxy (Image r cs e)) Nothing


encodeAutoGIF ::
     forall r cs i e m.
     (ColorSpace (BaseSpace cs) i e, ColorSpace cs i e, Source r Ix2 (Pixel cs e), MonadThrow m)
  => GifOptions
  -> Image r cs e
  -> m BL.ByteString
encodeAutoGIF opts img =
  fallbackEncodePalettizedRGB $ do
    Refl <- eqT :: Maybe (BaseModel cs :~: CM.Y)
    pure $ JP.encodeGifImage $ toJPImageY8 $ A.map (toPixel8 . toPixelBaseModel) img
  where
    toSRGB8 = convertPixel :: Pixel cs e -> Pixel SRGB Word8
    fallbackEncodePalettizedRGB =
      \case
        Just bs -> pure bs
        Nothing
          | Just Refl <- (eqT :: Maybe (BaseModel (BaseSpace cs) :~: CM.RGB)) ->
            encodePalettizedRGB opts $ A.map (toPixel8 . toPixelBaseModel . toPixelBaseSpace) img
        Nothing -> encodePalettizedRGB opts $ A.map (toPixelBaseModel . toSRGB8) img


instance FileFormat (Sequence GIF) where
  type WriteOptions (Sequence GIF) = WriteOptions GIF
  type Metadata (Sequence GIF) = [JP.GifDelay]
  ext _ = ext GIF

instance Readable (Sequence GIF) [Image S CM.RGB Word8] where
  decodeM = decodeSequenceGIF
  decodeWithMetadataM = decodeSequenceWithMetadataGIF

instance Readable (Sequence GIF) [Image S (Alpha CM.RGB) Word8] where
  decodeM = decodeSequenceGIF
  decodeWithMetadataM = decodeSequenceWithMetadataGIF

instance (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e) =>
         Readable (Auto (Sequence GIF)) [Image r cs e] where
  decodeM = decodeAutoSequenceGIF
  decodeWithMetadataM = decodeAutoSequenceWithMetadataGIF


-- | Decode a sequence of Gif images
decodeSequenceGIF ::
     (ColorModel cs e, MonadThrow m) => Sequence GIF -> B.ByteString -> m [Image S cs e]
decodeSequenceGIF f bs = convertSequenceWith f (JP.decodeGifImages bs)

-- | Decode a sequence of Gif images
decodeSequenceWithMetadataGIF ::
     (ColorModel cs e, MonadThrow m)
  => Sequence GIF
  -> B.ByteString
  -> m ([Image S cs e], [JP.GifDelay])
decodeSequenceWithMetadataGIF = decodeSeqMetadata decodeSequenceGIF

-- | Decode a sequence of Gif images
decodeAutoSequenceGIF ::
     (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e, MonadThrow m)
  => Auto (Sequence GIF)
  -> B.ByteString
  -> m [Image r cs e]
decodeAutoSequenceGIF f bs = convertAutoSequenceWith f (JP.decodeGifImages bs)

-- | Decode a sequence of Gif images
decodeAutoSequenceWithMetadataGIF ::
     (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e, MonadThrow m)
  => Auto (Sequence GIF)
  -> B.ByteString
  -> m ([Image r cs e], [JP.GifDelay])
decodeAutoSequenceWithMetadataGIF = decodeSeqMetadata decodeAutoSequenceGIF

decodeSeqMetadata ::
     MonadThrow m => (t -> B.ByteString -> m a) -> t -> B.ByteString -> m (a, [JP.GifDelay])
decodeSeqMetadata decode f bs = do
  imgs <- decode f bs
  delays <- decodeError $ JP.getDelaysGifImages bs
  pure (imgs, delays)
