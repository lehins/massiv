{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Test.Massiv.Array.Ops.SortSpec (spec) where

import Data.List as L
import Data.Massiv.Array as A
import Test.Massiv.Core as A
import Data.Foldable as F
import Data.Map.Strict as M

prop_IsSorted :: (b -> b) -> ([Int] -> b) -> (b -> [Int]) -> [Int] -> Property
prop_IsSorted sortWith from to xs =
  to (sortWith (from xs)) === sort xs

tallyMap :: Array P Ix1 Word -> Map Word Int
tallyMap arr = F.foldr' addCount M.empty $ toManifest arr
  where
    addCount :: Word -> Map Word Int -> Map Word Int
    addCount !el !counter = M.insertWith (+) el 1 counter


spec :: Spec
spec = do
  describe "QuickSort" $ do
    it "Seq" $ property $ prop_IsSorted (quicksort @P) (A.fromList Seq) A.toList
    it "Par" $ property $ prop_IsSorted (quicksort @P) (A.fromList (ParN 4)) A.toList
  describe "Tally" $
    prop "Same as Map" $ \ arr ->
       M.toList (tallyMap arr) === F.toList (tally arr)
