module Examples.SortRowsSpec (spec) where

import Data.Massiv.Array as A
import Examples.SortRows
import Test.Hspec
import Test.QuickCheck

isGrowingStencil :: A.Stencil Ix1 Int Bool
isGrowingStencil = A.makeStencil 2 1 $ \get -> get (-1) <= get 0

prop_Sorted :: Comp -> Positive Int -> Positive Int -> Property
prop_Sorted comp (Positive m) (Positive n) =
  forAllShrink (infiniteList :: Gen [Int]) shrink $ \xs ->
    let sz = Sz (m :. n)
        arr = resize' sz (A.fromList comp (Prelude.take (totalElem sz) xs) :: Array U Ix1 Int)
        sortedArr = sortRows arr
     in A.and $ makeArrayR D Seq (Sz1 m) $ \i ->
          A.and $ A.computeAs U (A.mapStencil (Fill minBound) isGrowingStencil (sortedArr !> i))

spec :: Spec
spec = describe "Sorting Properties" $ do
  it "Ix2 Rows Sorted Seq" $ property (prop_Sorted Seq)
  it "Ix2 Rows Sorted Par" $ property (prop_Sorted Par)
