module Test.Massiv.Array.Numeric.IntegralSpec
  ( spec
  ) where

import Data.Massiv.Array as A
import Data.Massiv.Array.Numeric.Integral
import Test.Massiv.Core

gaussian :: Float -> Float
gaussian x = exp (x ^ (2 :: Int))

spec :: Spec
spec = do
  let (a, b) = (0, 2)
      integrator rule = rule Seq N (\ scale -> gaussian . scale) a b (Sz1 1)
  describe "Integral Approximation" $ do
    it "Midpoint Rule" $ do
      integrator midpointRule 4 ! 0 `shouldBe` 14.485613
      integrator midpointRule 8 ! 0 `shouldBe` 15.905677
      integrator midpointRule 16 ! 0 `shouldBe` 16.311854
      integrator midpointRule 32 ! 0 `shouldBe` 16.417171
      integrator midpointRule 64 ! 0 `shouldBe` 16.443748
      integrator midpointRule 128 ! 0 `shouldBe` 16.450407
    it "Trapezoid Rule" $ do
      integrator trapezoidRule 4 ! 0 `shouldBe` 20.644558
      integrator trapezoidRule 8 ! 0 `shouldBe` 17.565086
      integrator trapezoidRule 16 ! 0 `shouldBe` 16.735381
      integrator trapezoidRule 32 ! 0 `shouldBe` 16.523618
      integrator trapezoidRule 64 ! 0 `shouldBe` 16.470394
      integrator trapezoidRule 128 ! 0 `shouldBe` 16.457073
    it "Simspon's Rule" $ do
      integrator simpsonsRule 4 ! 0 `shouldBe` 17.353626
      integrator simpsonsRule 8 ! 0 `shouldBe` 16.538595
      integrator simpsonsRule 16 ! 0 `shouldBe` 16.458815
      integrator simpsonsRule 32 ! 0 `shouldBe` 16.453030
      integrator simpsonsRule 64 ! 0 `shouldBe` 16.452653
      integrator simpsonsRule 128 ! 0 `shouldBe` 16.452629
