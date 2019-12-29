{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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
  , decodeAutoTIF
  , encodeTIF
  , encodeAutoTIF
  ) where

import Prelude as P
import Data.Maybe (fromMaybe)
import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as TypesJP
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
-- TIF Format ------------------------------------------------------------------
--------------------------------------------------------------------------------


-- | Tagged Image File Format image with @.tif@ or @.tiff@ extension.
data TIF = TIF deriving Show

instance FileFormat TIF where

  ext _ = ".tif"
  {-# INLINE ext #-}

  exts _ = [".tif", ".tiff"]
  {-# INLINE exts #-}

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

instance (ColorSpace cs i e, ColorSpace (BaseSpace cs) i e, Source r Ix2 (Pixel cs e)) =>
         Writable (Auto TIF) (Image r cs e) where
  encodeM _ _ = pure . encodeAutoTIF


instance Readable TIF (Image S CM.Y Word8) where
  decodeM f _ bs = (, Nothing) <$> decodeTIF f bs

instance Readable TIF (Image S CM.Y Word16) where
  decodeM f _ bs = (, Nothing) <$> decodeTIF f bs

instance Readable TIF (Image S CM.Y Word32) where
  decodeM f _ bs = (, Nothing) <$> decodeTIF f bs

instance Readable TIF (Image S CM.Y Float) where
  decodeM f _ bs = (, Nothing) <$> decodeTIF f bs

instance Readable TIF (Image S (Alpha CM.Y) Word8) where
  decodeM f _ bs = (, Nothing) <$> decodeTIF f bs

instance Readable TIF (Image S (Alpha CM.Y) Word16) where
  decodeM f _ bs = (, Nothing) <$> decodeTIF f bs

instance Readable TIF (Image S CM.RGB Word8) where
  decodeM f _ bs = (, Nothing) <$> decodeTIF f bs

instance Readable TIF (Image S CM.RGB Word16) where
  decodeM f _ bs = (, Nothing) <$> decodeTIF f bs

instance Readable TIF (Image S CM.RGB Float) where
  decodeM f _ bs = (, Nothing) <$> decodeTIF f bs

instance Readable TIF (Image S (Alpha CM.RGB) Word8) where
  decodeM f _ bs = (, Nothing) <$> decodeTIF f bs

instance Readable TIF (Image S (Alpha CM.RGB) Word16) where
  decodeM f _ bs = (, Nothing) <$> decodeTIF f bs

instance Readable TIF (Image S CM.CMYK Word8) where
  decodeM f _ bs = (, Nothing) <$> decodeTIF f bs

instance Readable TIF (Image S CM.CMYK Word16) where
  decodeM f _ bs = (, Nothing) <$> decodeTIF f bs

-- | Decode a Tiff Image
decodeTIF :: (ColorModel cs e, MonadThrow m) => TIF -> B.ByteString -> m (Image S cs e)
decodeTIF TIF bs =
  case JP.decodeTiff bs of
    Left err -> throwM $ DecodeError err
    Right jp -> fromEitherDecode TIF showJP fromDynamicImage jp

-- | Decode a Tiff Image
decodeAutoTIF ::
     (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e, MonadThrow m)
  => Auto TIF
  -> B.ByteString
  -> m (Image r cs e)
decodeAutoTIF (Auto TIF) bs =
  case JP.decodeTiff bs of
    Left err -> throwM $ DecodeError err
    Right jp -> fromEitherDecode (Auto TIF) showJP fromDynamicImageAuto jp


instance (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e) =>
         Readable (Auto TIF) (Image r cs e) where
  decodeM f _ bs = (, Nothing) <$> decodeAutoTIF f bs

encodeTIF ::
     forall r cs e m.
     (ColorModel cs e, Source r Ix2 (Pixel cs e), MonadThrow m)
  => Image r cs e
  -> m BL.ByteString
encodeTIF img =
  fromMaybeEncode TIF (Proxy :: Proxy (Image r cs e)) $
  msum
    [ do Refl <- eqT :: Maybe (cs :~: CM.Y)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ JP.encodeTiff $ toJPImageY8 img
           , do Refl <- eqT :: Maybe (e :~: Word16)
                pure $ JP.encodeTiff $ toJPImageY16 img
           , do Refl <- eqT :: Maybe (e :~: Word32)
                pure $ JP.encodeTiff $ toJPImageY32 img
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
  => Image r cs e
  -> BL.ByteString
encodeAutoTIF img =
  fromMaybe (toTiff toJPImageRGB8 (toPixelBaseModel . toSRGB8) img) $
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
         pure $ toTiff toJPImageYCbCr8 (toPixel8 . toPixelBaseModel) img
    , do Refl <- eqT :: Maybe (BaseModel cs :~: CM.CMYK)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ toTiff toJPImageCMYK8 toPixelBaseModel img
           , do Refl <- eqT :: Maybe (e :~: Word16)
                pure $ toTiff toJPImageCMYK16 toPixelBaseModel img
             -- for CMYK default is 8bit, instead of 16bit, since many viewers and editors
             -- don't support it.
           , pure $ toTiff toJPImageCMYK8 (toPixel8 . toPixelBaseModel) img
           ]
    , do Refl <- eqT :: Maybe (BaseModel (BaseSpace cs) :~: CM.RGB)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ toTiff toJPImageRGB8 (toPixelBaseModel . toPixelBaseSpace) img
           , do Refl <- eqT :: Maybe (e :~: Word16)
                pure $ toTiff toJPImageRGB16 (toPixelBaseModel . toPixelBaseSpace) img
           ]
    , do Refl <- eqT :: Maybe (BaseModel (BaseSpace cs) :~: Alpha CM.RGB)
         msum
           [ do Refl <- eqT :: Maybe (e :~: Word8)
                pure $ toTiff toJPImageRGBA8 (toPixelBaseModel . toPixelBaseSpace) img
           , do Refl <- eqT :: Maybe (e :~: Word16)
                pure $ toTiff toJPImageRGBA16 (toPixelBaseModel . toPixelBaseSpace) img
           ]
    ]
  where
    toSRGB8 = convertPixel :: Pixel cs e -> Pixel SRGB Word8
    toTiff ::
         (JP.TiffSaveable px, Source r ix a)
      => (Array D ix b -> TypesJP.Image px)
      -> (a -> b)
      -> Array r ix a
      -> BL.ByteString
    toTiff toJP adjustPixel = JP.encodeTiff . toJP . A.map adjustPixel
