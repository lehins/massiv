{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Data.Massiv.Array.Ops.SortSpec (spec) where

import Data.List as L
import Data.Massiv.Array as A
import Test.Massiv.Core


prop_IsSorted :: (b -> b) -> ([Int] -> b) -> (b -> [Int]) -> [Int] -> Property
prop_IsSorted sortWith from to xs =
  to (sortWith (from xs)) === sort xs

spec :: Spec
spec =
  describe "QuickSort" $ do
    it "Seq" $ property $ prop_IsSorted (quicksort @P) (A.fromList Seq) A.toList
    it "Par" $ property $ prop_IsSorted (quicksort @P) (A.fromList (ParN 4)) A.toList
