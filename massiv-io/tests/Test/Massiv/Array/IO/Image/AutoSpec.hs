{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Test.Massiv.Array.IO.Image.AutoSpec (spec) where

import Data.Massiv.Array
import Data.Massiv.Array.IO hiding (showsType)
import Test.Massiv.Core
import Test.Massiv.Array.IO.Image.Common

spec :: Spec
spec =
  describe "Auto" $
  describe "Encode/Decode" $ do
    specEncodeDecodeNoErrorAuto BMP
    specEncodeDecodeNoErrorAuto GIF
    --specEncodeDecodeNoErrorAuto HDR -- Get "Invalid sanline size" from JuicyPixels
    specEncodeDecodeNoErrorAuto JPG
    specEncodeDecodeNoErrorAuto PNG
    specEncodeDecodeNoErrorAuto TGA
    specEncodeDecodeNoErrorAuto TIF


specEncodeDecodeNoErrorAuto ::
     ( Show f
     , Readable (Auto f) (Image S (Y D65) Word8)
     , Readable (Auto f) (Image S (Y D65) Word16)
     , Readable (Auto f) (Image S (Y D65) Word32)
     , Readable (Auto f) (Image S (Y D65) Word64)
     , Readable (Auto f) (Image S (CMYK SRGB) Word8)
     , Readable (Auto f) (Image S (CMYK SRGB) Word16)
     , Readable (Auto f) (Image S (CMYK SRGB) Word32)
     , Readable (Auto f) (Image S (CMYK SRGB) Word64)
     , Readable (Auto f) (Image S (CMYK AdobeRGB) Word8)
     , Readable (Auto f) (Image S (CMYK AdobeRGB) Word16)
     , Readable (Auto f) (Image S (CMYK AdobeRGB) Word32)
     , Readable (Auto f) (Image S (CMYK AdobeRGB) Word64)
     , Readable (Auto f) (Image S (YCbCr SRGB) Word8)
     , Readable (Auto f) (Image S (YCbCr SRGB) Word16)
     , Readable (Auto f) (Image S (YCbCr SRGB) Word32)
     , Readable (Auto f) (Image S (YCbCr SRGB) Word64)
     -- AdobeRGB doesn't have Luma instance
     -- , Readable (Auto f) (Image S (YCbCr AdobeRGB) Word8)
     -- , Readable (Auto f) (Image S (YCbCr AdobeRGB) Word16)
     -- , Readable (Auto f) (Image S (YCbCr AdobeRGB) Word32)
     -- , Readable (Auto f) (Image S (YCbCr AdobeRGB) Word64)
     , Readable (Auto f) (Image S (Alpha (Y D65)) Word8)
     , Readable (Auto f) (Image S (Alpha (Y D65)) Word16)
     , Readable (Auto f) (Image S (Alpha (Y D65)) Word32)
     , Readable (Auto f) (Image S (Alpha (Y D65)) Word64)
     , Readable (Auto f) (Image S (Alpha SRGB) Word8)
     , Readable (Auto f) (Image S (Alpha SRGB) Word16)
     , Readable (Auto f) (Image S (Alpha SRGB) Word32)
     , Readable (Auto f) (Image S (Alpha SRGB) Word64)
     , Readable (Auto f) (Image S (Alpha AdobeRGB) Word8)
     , Readable (Auto f) (Image S (Alpha AdobeRGB) Word16)
     , Readable (Auto f) (Image S (Alpha AdobeRGB) Word32)
     , Readable (Auto f) (Image S (Alpha AdobeRGB) Word64)
     , Readable (Auto f) (Image S SRGB Word8)
     , Readable (Auto f) (Image S SRGB Word16)
     , Readable (Auto f) (Image S SRGB Word32)
     , Readable (Auto f) (Image S SRGB Word64)
     , Readable (Auto f) (Image S AdobeRGB Word8)
     , Readable (Auto f) (Image S AdobeRGB Word16)
     , Readable (Auto f) (Image S AdobeRGB Word32)
     , Readable (Auto f) (Image S AdobeRGB Word64)
     , Writable (Auto f) (Image S (Y D65) Word8)
     , Writable (Auto f) (Image S (Y D65) Word16)
     , Writable (Auto f) (Image S (Y D65) Word32)
     , Writable (Auto f) (Image S (Y D65) Word64)
     , Writable (Auto f) (Image S (CMYK SRGB) Word8)
     , Writable (Auto f) (Image S (CMYK SRGB) Word16)
     , Writable (Auto f) (Image S (CMYK SRGB) Word32)
     , Writable (Auto f) (Image S (CMYK SRGB) Word64)
     , Writable (Auto f) (Image S (CMYK AdobeRGB) Word8)
     , Writable (Auto f) (Image S (CMYK AdobeRGB) Word16)
     , Writable (Auto f) (Image S (CMYK AdobeRGB) Word32)
     , Writable (Auto f) (Image S (CMYK AdobeRGB) Word64)
     , Writable (Auto f) (Image S (YCbCr SRGB) Word8)
     , Writable (Auto f) (Image S (YCbCr SRGB) Word16)
     , Writable (Auto f) (Image S (YCbCr SRGB) Word32)
     , Writable (Auto f) (Image S (YCbCr SRGB) Word64)
     -- , Writable (Auto f) (Image S (YCbCr AdobeRGB) Word8)
     -- , Writable (Auto f) (Image S (YCbCr AdobeRGB) Word16)
     -- , Writable (Auto f) (Image S (YCbCr AdobeRGB) Word32)
     -- , Writable (Auto f) (Image S (YCbCr AdobeRGB) Word64)
     , Writable (Auto f) (Image S (Alpha (Y D65)) Word8)
     , Writable (Auto f) (Image S (Alpha (Y D65)) Word16)
     , Writable (Auto f) (Image S (Alpha (Y D65)) Word32)
     , Writable (Auto f) (Image S (Alpha (Y D65)) Word64)
     , Writable (Auto f) (Image S (Alpha SRGB) Word8)
     , Writable (Auto f) (Image S (Alpha SRGB) Word16)
     , Writable (Auto f) (Image S (Alpha SRGB) Word32)
     , Writable (Auto f) (Image S (Alpha SRGB) Word64)
     , Writable (Auto f) (Image S (Alpha AdobeRGB) Word8)
     , Writable (Auto f) (Image S (Alpha AdobeRGB) Word16)
     , Writable (Auto f) (Image S (Alpha AdobeRGB) Word32)
     , Writable (Auto f) (Image S (Alpha AdobeRGB) Word64)
     , Writable (Auto f) (Image S SRGB Word8)
     , Writable (Auto f) (Image S SRGB Word16)
     , Writable (Auto f) (Image S SRGB Word32)
     , Writable (Auto f) (Image S SRGB Word64)
     , Writable (Auto f) (Image S AdobeRGB Word8)
     , Writable (Auto f) (Image S AdobeRGB Word16)
     , Writable (Auto f) (Image S AdobeRGB Word32)
     , Writable (Auto f) (Image S AdobeRGB Word64)
     )
  => f
  -> Spec
specEncodeDecodeNoErrorAuto f =
    describe (show f) $ do
      specEncodeDecodeAutoNoError @(Y D65) @Word8 f
      specEncodeDecodeAutoNoError @(Y D65) @Word16 f
      specEncodeDecodeAutoNoError @(Y D65) @Word32 f
      specEncodeDecodeAutoNoError @(Y D65) @Word64 f
      specEncodeDecodeAutoNoError @(Alpha (Y D65)) @Word8 f
      specEncodeDecodeAutoNoError @(Alpha (Y D65)) @Word16 f
      specEncodeDecodeAutoNoError @(Alpha (Y D65)) @Word32 f
      specEncodeDecodeAutoNoError @(Alpha (Y D65)) @Word64 f
      specEncodeDecodeAutoNoError @SRGB @Word8 f
      specEncodeDecodeAutoNoError @SRGB @Word16 f
      specEncodeDecodeAutoNoError @SRGB @Word32 f
      specEncodeDecodeAutoNoError @SRGB @Word64 f
      specEncodeDecodeAutoNoError @(Alpha SRGB) @Word8 f
      specEncodeDecodeAutoNoError @(Alpha SRGB) @Word16 f
      specEncodeDecodeAutoNoError @(Alpha SRGB) @Word32 f
      specEncodeDecodeAutoNoError @(Alpha SRGB) @Word64 f
      specEncodeDecodeAutoNoError @(CMYK SRGB) @Word8 f
      specEncodeDecodeAutoNoError @(CMYK SRGB) @Word16 f
      specEncodeDecodeAutoNoError @(CMYK SRGB) @Word32 f
      specEncodeDecodeAutoNoError @(CMYK SRGB) @Word64 f
      specEncodeDecodeAutoNoError @(YCbCr SRGB) @Word8 f
      specEncodeDecodeAutoNoError @(YCbCr SRGB) @Word16 f
      specEncodeDecodeAutoNoError @(YCbCr SRGB) @Word32 f
      specEncodeDecodeAutoNoError @(YCbCr SRGB) @Word64 f
      specEncodeDecodeAutoNoError @AdobeRGB @Word8 f
      specEncodeDecodeAutoNoError @AdobeRGB @Word16 f
      specEncodeDecodeAutoNoError @AdobeRGB @Word32 f
      specEncodeDecodeAutoNoError @AdobeRGB @Word64 f
      specEncodeDecodeAutoNoError @(Alpha AdobeRGB) @Word8 f
      specEncodeDecodeAutoNoError @(Alpha AdobeRGB) @Word16 f
      specEncodeDecodeAutoNoError @(Alpha AdobeRGB) @Word32 f
      specEncodeDecodeAutoNoError @(Alpha AdobeRGB) @Word64 f
      specEncodeDecodeAutoNoError @(CMYK AdobeRGB) @Word8 f
      specEncodeDecodeAutoNoError @(CMYK AdobeRGB) @Word16 f
      specEncodeDecodeAutoNoError @(CMYK AdobeRGB) @Word32 f
      specEncodeDecodeAutoNoError @(CMYK AdobeRGB) @Word64 f
      -- specEncodeDecodeAutoNoError @(YCbCr AdobeRGB) @Word8 f
      -- specEncodeDecodeAutoNoError @(YCbCr AdobeRGB) @Word16 f
      -- specEncodeDecodeAutoNoError @(YCbCr AdobeRGB) @Word32 f
      -- specEncodeDecodeAutoNoError @(YCbCr AdobeRGB) @Word64 f
