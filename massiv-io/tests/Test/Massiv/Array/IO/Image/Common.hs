{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Test.Massiv.Array.IO.Image.Common where

import Data.List.NonEmpty as NE (NonEmpty(..))
import Data.Massiv.Array
import Data.Massiv.Array.IO hiding (showsType)
import qualified Data.ByteString.Lazy as BL
import Test.Massiv.Core
import System.Random
import Test.Hspec.QuickCheck

elevatorGen :: (Random e, Elevator e) => Gen e
elevatorGen = choose (minValue, maxValue)

instance (Arbitrary e, Random e, Elevator e) => Arbitrary (Pixel Y' e) where
  arbitrary = PixelY' <$> elevatorGen
instance (Arbitrary e, Random e, Elevator e) => Arbitrary (Pixel (Alpha Y') e) where
  arbitrary = PixelY'A <$> elevatorGen <*> elevatorGen
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
     forall cs e f. (Writable f (Image S cs e), Arbitrary (Pixel cs e), ColorModel cs e)
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
     forall cs e f.
     ( Readable f (Image S cs e)
     , Writable f (Image S cs e)
     , Arbitrary (Pixel cs e)
     , ColorModel cs e
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

specEncodeDecodeGifSequenceNoError ::
     forall cs e.
     ( Readable (Sequence GIF) [Image S cs e]
     , Writable (Sequence GIF) (NonEmpty (GifDelay, Image S cs e))
     , Arbitrary (Pixel cs e)
     , ColorModel cs e
     )
  => Spec
specEncodeDecodeGifSequenceNoError =
  prop (("Image S " ++) .
        showsColorModelName (Proxy :: Proxy (Color cs e)) . (' ':) .
        showsType @e $ "") $
  property $ forAll (genNonEmptyImagesWithGifDelay @cs @e) $ \ imgs -> do
    bs <- encodeM (Sequence GIF) def imgs
    imgs' :: [Image S cs e] <- decodeM (Sequence GIF) $ BL.toStrict bs
    Prelude.length imgs' `shouldBe` Prelude.length imgs

genNonEmptyImagesWithGifDelay ::
     forall cs e. (Storable (Color cs e), Arbitrary (Pixel cs e))
  => Gen (NonEmpty (GifDelay, Image S cs e))
genNonEmptyImagesWithGifDelay = do
  arrs <- toSameSizeNE <$> (arbitrary :: Gen (ArrNE D Ix3 (Pixel cs e)))
  Prelude.mapM (\i -> (,) <$> arbitrary <*> pure (compute i)) arrs

toSameSizeNE :: OuterSlice r ix e => ArrNE r ix e -> NonEmpty (Elt r ix e)
toSameSizeNE (ArrNE arr) = (arr !>) <$> (0 :| [1 .. unSz (fst (unconsSz (size arr))) - 1])
