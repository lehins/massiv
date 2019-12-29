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
-- Module      : Data.Massiv.Array.IO.Image.JuicyPixels.Base
-- Copyright   : (c) Alexey Kuleshevich 2019-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.IO.Image.JuicyPixels.Base
  ( showJP
  , toJPImageY8
  , toJPImageY16
  , toJPImageY32
  , toJPImageYA8
  , toJPImageYA16
  , toJPImageYF
  , toJPImageRGB8
  , toJPImageRGB16
  , toJPImageRGBA8
  , toJPImageRGBA16
  , toJPImageRGBF
  , toJPImageYCbCr8
  , toJPImageCMYK8
  , toJPImageCMYK16
  , fromDynamicImage
  , fromDynamicImageAuto
  ) where

import Prelude as P
import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as TypesJP
import Control.Monad (guard, unless)
import Data.Massiv.Array as A
import Data.Massiv.Array.IO.Base
import Data.Massiv.Array.Manifest.Vector
import Data.Typeable
import qualified Data.Vector.Storable as V
import Foreign.Storable (Storable(sizeOf))
import Graphics.Color.Model.Alpha
import qualified Graphics.Color.Model.CMYK as CM
import qualified Graphics.Color.Model.RGB as CM
import qualified Graphics.Color.Model.Y as CM
import qualified Graphics.Color.Model.YCbCr as CM
import Graphics.Color.Pixel
import Graphics.Color.Space.RGB.Alternative.CMYK
import Graphics.Color.Space.RGB.Alternative.YCbCr

--------------------------------------------------------------------------------
-- Common encoding/decoding functions ------------------------------------------
--------------------------------------------------------------------------------


fromDynamicImage ::
     forall cs e. ColorModel cs e
  => JP.DynamicImage
  -> Maybe (Image S cs e)
fromDynamicImage jpDynImg =
  case jpDynImg of
    JP.ImageY8 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel CM.Y Word8)
      fromJPImageUnsafe jimg
    JP.ImageY16 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel CM.Y Word16)
      fromJPImageUnsafe jimg
    JP.ImageY32 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel CM.Y Word32)
      fromJPImageUnsafe jimg
    JP.ImageYF jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel CM.Y Float)
      fromJPImageUnsafe jimg
    JP.ImageYA8 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel (Alpha CM.Y) Word8)
      fromJPImageUnsafe jimg
    JP.ImageYA16 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel (Alpha CM.Y) Word16)
      fromJPImageUnsafe jimg
    JP.ImageRGB8 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel CM.RGB Word8)
      fromJPImageUnsafe jimg
    JP.ImageRGB16 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel CM.RGB Word16)
      fromJPImageUnsafe jimg
    JP.ImageRGBF jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel CM.RGB Float)
      fromJPImageUnsafe jimg
    JP.ImageRGBA8 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel (Alpha CM.RGB) Word8)
      fromJPImageUnsafe jimg
    JP.ImageRGBA16 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel (Alpha CM.RGB) Word16)
      fromJPImageUnsafe jimg
    JP.ImageYCbCr8 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel CM.YCbCr Word8)
      fromJPImageUnsafe jimg
    JP.ImageCMYK8 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel CM.CMYK Word8)
      fromJPImageUnsafe jimg
    JP.ImageCMYK16 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel CM.CMYK Word16)
      fromJPImageUnsafe jimg


fromDynamicImageAuto ::
     forall r cs i e. (Mutable r Ix2 (Pixel cs e), ColorSpace cs i e)
  => JP.DynamicImage
  -> Maybe (Image r cs e)
fromDynamicImageAuto jpDynImg =
  case jpDynImg of
    JP.ImageY8 jimg ->
      convertImage <$> (fromJPImageUnsafe jimg :: Maybe (Image S (Y D65) Word8))
    JP.ImageY16 jimg ->
      convertImage <$> (fromJPImageUnsafe jimg :: Maybe (Image S (Y D65) Word16))
    JP.ImageY32 jimg ->
      convertImage <$> (fromJPImageUnsafe jimg :: Maybe (Image S (Y D65) Word32))
    JP.ImageYF jimg ->
      convertImage <$> (fromJPImageUnsafe jimg :: Maybe (Image S (Y D65) Float))
    JP.ImageYA8 jimg ->
      convertImage <$> (fromJPImageUnsafe jimg :: Maybe (Image S (Alpha (Y D65)) Word8))
    JP.ImageYA16 jimg ->
      convertImage <$> (fromJPImageUnsafe jimg :: Maybe (Image S (Alpha (Y D65)) Word16))
    JP.ImageRGB8 jimg ->
      convertImage <$> (fromJPImageUnsafe jimg :: Maybe (Image S SRGB Word8))
    JP.ImageRGB16 jimg ->
      convertImage <$> (fromJPImageUnsafe jimg :: Maybe (Image S SRGB Word16))
    JP.ImageRGBF jimg ->
      convertImage <$> (fromJPImageUnsafe jimg :: Maybe (Image S SRGB Float))
    JP.ImageRGBA8 jimg ->
      convertImage <$> (fromJPImageUnsafe jimg :: Maybe (Image S (Alpha SRGB) Word8))
    JP.ImageRGBA16 jimg ->
      convertImage <$> (fromJPImageUnsafe jimg :: Maybe (Image S (Alpha SRGB) Word16))
    JP.ImageYCbCr8 jimg ->
      convertImage <$> (fromJPImageUnsafe jimg :: Maybe (Image S (YCbCr SRGB) Word8))
    JP.ImageCMYK8 jimg ->
      convertImage <$> (fromJPImageUnsafe jimg :: Maybe (Image S (CMYK SRGB) Word8))
    JP.ImageCMYK16 jimg ->
      convertImage <$> (fromJPImageUnsafe jimg :: Maybe (Image S (CMYK SRGB) Word16))



showJP :: JP.DynamicImage -> String
showJP (JP.ImageY8     _) = "Image S Y Word8"
showJP (JP.ImageY16    _) = "Image S Y Word16"
showJP (JP.ImageY32    _) = "Image S Y Word32"
showJP (JP.ImageYF     _) = "Image S Y Float"
showJP (JP.ImageYA8    _) = "Image S YA Word8"
showJP (JP.ImageYA16   _) = "Image S YA Word16"
showJP (JP.ImageRGB8   _) = "Image S RGB Word8"
showJP (JP.ImageRGB16  _) = "Image S RGB Word16"
showJP (JP.ImageRGBF   _) = "Image S RGB Float"
showJP (JP.ImageRGBA8  _) = "Image S RGBA Word8"
showJP (JP.ImageRGBA16 _) = "Image S RGBA Word16"
showJP (JP.ImageYCbCr8 _) = "Image S YCbCr Word8"
showJP (JP.ImageCMYK8  _) = "Image S CMYK Word8"
showJP (JP.ImageCMYK16 _) = "Image S CMYK Word16"


-- Encoding

toJPImageUnsafe
  :: forall r cs a . (JP.Pixel a, Source r Ix2 (Pixel cs (JP.PixelBaseComponent a)),
                      ColorModel cs (JP.PixelBaseComponent a))
  => Image r cs (JP.PixelBaseComponent a)
  -> JP.Image a
toJPImageUnsafe img = JP.Image n m $ V.unsafeCast $ toVector arrS
  where
    !arrS = computeSource img :: Image S cs (JP.PixelBaseComponent a)
    Sz (m :. n) = size img
{-# INLINE toJPImageUnsafe #-}

toJPImageY8 :: Source r Ix2 (Pixel CM.Y Word8) => Image r CM.Y Word8 -> JP.Image TypesJP.Pixel8
toJPImageY8 = toJPImageUnsafe
{-# INLINE toJPImageY8 #-}

toJPImageY16 :: Source r Ix2 (Pixel CM.Y Word16) => Image r CM.Y Word16 -> JP.Image TypesJP.Pixel16
toJPImageY16 = toJPImageUnsafe
{-# INLINE toJPImageY16 #-}

toJPImageY32 :: Source r Ix2 (Pixel CM.Y Word32) => Image r CM.Y Word32 -> JP.Image TypesJP.Pixel32
toJPImageY32 = toJPImageUnsafe
{-# INLINE toJPImageY32 #-}

toJPImageYF :: Source r Ix2 (Pixel CM.Y Float) => Image r CM.Y Float -> JP.Image JP.PixelF
toJPImageYF = toJPImageUnsafe
{-# INLINE toJPImageYF #-}

toJPImageYA8 ::
     Source r Ix2 (Pixel (Alpha CM.Y) Word8) => Image r (Alpha CM.Y) Word8 -> JP.Image JP.PixelYA8
toJPImageYA8 = toJPImageUnsafe
{-# INLINE toJPImageYA8 #-}

toJPImageYA16 ::
     Source r Ix2 (Pixel (Alpha CM.Y) Word16)
  => Image r (Alpha CM.Y) Word16
  -> JP.Image JP.PixelYA16
toJPImageYA16 = toJPImageUnsafe
{-# INLINE toJPImageYA16 #-}

toJPImageRGB8 :: Source r Ix2 (Pixel CM.RGB Word8) => Image r CM.RGB Word8 -> JP.Image JP.PixelRGB8
toJPImageRGB8 = toJPImageUnsafe
{-# INLINE toJPImageRGB8 #-}

toJPImageRGB16 ::
     Source r Ix2 (Pixel CM.RGB Word16) => Image r CM.RGB Word16 -> JP.Image JP.PixelRGB16
toJPImageRGB16 = toJPImageUnsafe
{-# INLINE toJPImageRGB16 #-}

toJPImageRGBF :: Source r Ix2 (Pixel CM.RGB Float) => Image r CM.RGB Float -> JP.Image JP.PixelRGBF
toJPImageRGBF = toJPImageUnsafe
{-# INLINE toJPImageRGBF #-}

toJPImageRGBA8 ::
     Source r Ix2 (Pixel (Alpha CM.RGB) Word8)
  => Image r (Alpha CM.RGB) Word8
  -> JP.Image JP.PixelRGBA8
toJPImageRGBA8 = toJPImageUnsafe
{-# INLINE toJPImageRGBA8 #-}

toJPImageRGBA16 ::
     Source r Ix2 (Pixel (Alpha CM.RGB) Word16)
  => Image r (Alpha CM.RGB) Word16
  -> JP.Image JP.PixelRGBA16
toJPImageRGBA16 = toJPImageUnsafe
{-# INLINE toJPImageRGBA16 #-}


toJPImageYCbCr8 ::
     Source r Ix2 (Pixel CM.YCbCr Word8) => Image r CM.YCbCr Word8 -> JP.Image JP.PixelYCbCr8
toJPImageYCbCr8 = toJPImageUnsafe
{-# INLINE toJPImageYCbCr8 #-}

toJPImageCMYK8 ::
     Source r Ix2 (Pixel CM.CMYK Word8) => Image r CM.CMYK Word8 -> JP.Image JP.PixelCMYK8
toJPImageCMYK8 = toJPImageUnsafe
{-# INLINE toJPImageCMYK8 #-}

toJPImageCMYK16 ::
     Source r Ix2 (Pixel CM.CMYK Word16) => Image r CM.CMYK Word16 -> JP.Image JP.PixelCMYK16
toJPImageCMYK16 = toJPImageUnsafe
{-# INLINE toJPImageCMYK16 #-}


-- General decoding and helper functions

fromJPImageUnsafe :: forall jpx cs e . (Storable (Pixel cs e), Storable e, JP.Pixel jpx) =>
                     JP.Image jpx -> Maybe (Image S cs e)
fromJPImageUnsafe (JP.Image n m !v) = do
  let numberOfComponentsFromSize = sizeOf (undefined :: Pixel cs e) `div` sizeOf (undefined :: e)
      numComponentsPerPixel = JP.componentCount (undefined :: jpx)
  unless (numComponentsPerPixel == numberOfComponentsFromSize) $
    error $
    concat
      [ "Mismatched sizes beteen JuicyPixels: "
      , show numComponentsPerPixel
      , " and massiv: "
      , show numberOfComponentsFromSize
      ]
  guard (n * m * numComponentsPerPixel == V.length v)
  fromVectorM Par (Sz (m :. n)) $ V.unsafeCast v
{-# INLINE fromJPImageUnsafe #-}

