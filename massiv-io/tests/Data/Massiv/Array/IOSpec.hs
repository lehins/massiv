{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Data.Massiv.Array.IOSpec (spec) where

import Data.Bifunctor
import Data.List.NonEmpty as NE (NonEmpty, fromList)
import Data.Massiv.Array
import Data.Massiv.Array.IO hiding (showsType)
import qualified Data.ByteString.Lazy as BL
import Test.Massiv.Core
import System.Random
import Test.Hspec.QuickCheck

elevatorGen :: (Random e, Elevator e) => Gen e
elevatorGen = choose (minValue, maxValue)

instance (Arbitrary e, Random e, Elevator e) => Arbitrary (Pixel (Y i) e) where
  arbitrary = PixelY <$> elevatorGen
instance (Arbitrary e, Random e, Elevator e) => Arbitrary (Pixel (Alpha (Y i)) e) where
  arbitrary = PixelYA <$> elevatorGen <*> elevatorGen
instance (Arbitrary e, Random e, Elevator e) => Arbitrary (Pixel SRGB e) where
  arbitrary = PixelRGB <$> elevatorGen <*> elevatorGen <*> elevatorGen
instance (Arbitrary e, Random e, Elevator e) => Arbitrary (Pixel (Alpha SRGB) e) where
  arbitrary = PixelRGBA <$> elevatorGen <*> elevatorGen <*> elevatorGen <*> elevatorGen
instance (Arbitrary e, Random e, Elevator e) => Arbitrary (Pixel AdobeRGB e) where
  arbitrary = PixelRGB <$> elevatorGen <*> elevatorGen <*> elevatorGen
instance (Arbitrary e, Random e, Elevator e) => Arbitrary (Pixel (Alpha AdobeRGB) e) where
  arbitrary = PixelRGBA <$> elevatorGen <*> elevatorGen <*> elevatorGen <*> elevatorGen
instance (Arbitrary e, Random e, Elevator e) => Arbitrary (Pixel (YCbCr cs) e) where
  arbitrary = PixelYCbCr <$> elevatorGen <*> elevatorGen <*> elevatorGen
instance (Arbitrary e, Random e, Elevator e) => Arbitrary (Pixel (Alpha (YCbCr cs)) e) where
  arbitrary = PixelYCbCrA <$> elevatorGen <*> elevatorGen <*> elevatorGen <*> elevatorGen
instance (Arbitrary e, Random e, Elevator e) => Arbitrary (Pixel (CMYK cs) e) where
  arbitrary = PixelCMYK <$> elevatorGen <*> elevatorGen <*> elevatorGen <*> elevatorGen
instance (Arbitrary e, Random e, Elevator e) => Arbitrary (Pixel (Alpha (CMYK cs)) e) where
  arbitrary =
    PixelCMYKA <$> elevatorGen <*> elevatorGen <*> elevatorGen <*> elevatorGen <*> elevatorGen

specEncodeNoError ::
     forall cs e i f. (Writable f (Image S cs e), Arbitrary (Pixel cs e), ColorSpace cs i e)
  => f
  -> Spec
specEncodeNoError f =
  prop (("Image S " ++) .
        showsColorModelName (Proxy :: Proxy (Color cs e)) . (' ':) .
        showsType @e $ "") $
  property $ \ (ArrNE img :: ArrNE S Ix2 (Pixel cs e)) -> do
    bs <- encodeM f def img
    bs' <- encodeImageM imageWriteFormats ("foo" ++ ext f) img
    bs' `shouldBe` bs

specEncodeDecodeNoError ::
     forall cs e i f.
     ( Readable f (Image S cs e)
     , Writable f (Image S cs e)
     , Arbitrary (Pixel cs e)
     , ColorSpace cs i e
     )
  => f
  -> Spec
specEncodeDecodeNoError f =
  prop (("Image S " ++) .
        showsColorModelName (Proxy :: Proxy (Color cs e)) . (' ':) .
        showsType @e $ "") $
  property $ \ (ArrNE img :: ArrNE S Ix2 (Pixel cs e)) -> do
    bs <- encodeM f def img
    img' :: Image S cs e <- decodeM f $ BL.toStrict bs
    size img' `shouldBe` size img
    bs' <- encodeImageM imageWriteFormats ("foo" ++ ext f) img
    bs' `shouldBe` bs

specEncodeDecodeAutoNoError ::
     forall cs e i f.
     ( Readable (Auto f) (Image S cs e)
     , Writable (Auto f) (Image S cs e)
     , Arbitrary (Pixel cs e)
     , ColorSpace (BaseSpace cs) i e
     , ColorSpace cs i e
     )
  => f
  -> Spec
specEncodeDecodeAutoNoError nonAutoFormat =
  prop (("Image S " ++) .
        showsColorModelName (Proxy :: Proxy (Color cs e)) . (' ':) .
        showsType @e $ "") $
  property $ \ (ArrNE img :: ArrNE S Ix2 (Pixel cs e)) -> do
    let f = Auto nonAutoFormat
    bs <- encodeM f def img
    img' :: Image S cs e <- decodeM f $ BL.toStrict bs
    size img' `shouldBe` size img
    bs' <- encodeImageM imageWriteAutoFormats ("foo" ++ ext f) img
    bs' `shouldBe` bs

_specEncodeDecodeGifSequenceNoError ::
     forall cs e.
     ( Readable (Sequence GIF) [Image S cs e]
     , Writable (Sequence GIF) (NonEmpty (GifDelay, Image S cs e))
     , Arbitrary (Pixel cs e)
     , ColorModel cs e
     )
  => Spec
_specEncodeDecodeGifSequenceNoError =
  prop (("Image S " ++) .
        showsColorModelName (Proxy :: Proxy (Color cs e)) . (' ':) .
        showsType @e $ "") $
  property $ \ (imgsNE :: NonEmptyList (GifDelay, ArrNE S Ix2 (Pixel cs e))) -> do
    let imgs = fmap (second unArr) $ NE.fromList $ getNonEmpty imgsNE
    bs <- encodeM (Sequence GIF) def imgs
    imgs' :: [Image S cs e] <- decodeM (Sequence GIF) $ BL.toStrict bs
    Prelude.length imgs' `shouldBe` Prelude.length imgs


spec :: Spec
spec = do
  describe "Readable/Writable" $ do
    describe "BMP" $ do
      specEncodeNoError @(Y D65) @Word8 BMP
      --specEncodeDecodeNoError @(Y D65) @Word8 BMP
      specEncodeDecodeNoError @SRGB @Word8 BMP
      specEncodeDecodeNoError @(Alpha SRGB) @Word8 BMP
    describe "GIF" $ do
      specEncodeNoError @(Y D65) @Word8 GIF
      specEncodeDecodeNoError @SRGB @Word8 GIF
      --specEncodeGifSequenceNoError @(Y D65) @Word8
      --describe "Sequenece" $ -- Need to ensure same size for all arrays
      -- TODO: Define (Arbitrary ArrListNE) (either slice 3d, or gen list of functions)
      --  specEncodeDecodeGifSequenceNoError @SRGB @Word8
      -- TODO: read RGBA8, write Y8
    -- getting 'DecodeError "Invalid sanline size"' with seed=2023820902
    -- describe "HDR" $ do
    --   specEncodeDecodeNoError @SRGB @Float HDR
    describe "JPG" $ do
      specEncodeDecodeNoError @(Y D65) @Word8 JPG
      specEncodeDecodeNoError @SRGB @Word8 JPG
      specEncodeDecodeNoError @(CMYK SRGB) @Word8 JPG
      specEncodeDecodeNoError @(YCbCr SRGB) @Word8 JPG
      -- TODO: read YA8
    describe "PNG" $ do
      specEncodeDecodeNoError @(Y D65) @Word8 PNG
      specEncodeDecodeNoError @(Y D65) @Word16 PNG
      specEncodeDecodeNoError @(Alpha (Y D65)) @Word8 PNG
      specEncodeDecodeNoError @(Alpha (Y D65)) @Word16 PNG
      specEncodeDecodeNoError @SRGB @Word8 PNG
      specEncodeDecodeNoError @SRGB @Word16 PNG
      specEncodeDecodeNoError @(Alpha SRGB) @Word8 PNG
      specEncodeDecodeNoError @(Alpha SRGB) @Word16 PNG
    describe "TGA" $ do
      specEncodeDecodeNoError @(Y D65) @Word8 TGA
      specEncodeDecodeNoError @SRGB @Word8 TGA
      specEncodeDecodeNoError @(Alpha SRGB) @Word8 TGA
    describe "TIF" $ do
      specEncodeDecodeNoError @(Y D65) @Word8 TIF
      specEncodeDecodeNoError @(Y D65) @Word16 TIF
      specEncodeDecodeNoError @(Y D65) @Word32 TIF
      specEncodeDecodeNoError @(Y D65) @Float TIF
      specEncodeDecodeNoError @(Alpha (Y D65)) @Word8 TIF
      specEncodeDecodeNoError @(Alpha (Y D65)) @Word16 TIF
      specEncodeDecodeNoError @SRGB @Word8 TIF
      specEncodeDecodeNoError @SRGB @Word16 TIF
      specEncodeDecodeNoError @(Alpha SRGB) @Word8 TIF
      specEncodeDecodeNoError @(Alpha SRGB) @Word16 TIF
      specEncodeDecodeNoError @(CMYK SRGB) @Word8 TIF
      specEncodeDecodeNoError @(CMYK SRGB) @Word16 TIF
      specEncodeNoError @(YCbCr SRGB) @Word8 TIF
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
    describe ("Auto " ++ show f) $ do
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
