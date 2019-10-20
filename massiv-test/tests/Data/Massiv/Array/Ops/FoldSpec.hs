{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Massiv.Array.Ops.FoldSpec (spec) where

import qualified Data.Foldable as F
import Data.Massiv.Array as A
import Data.Semigroup
import Prelude hiding (map, product, sum)
import Test.Massiv.Core



prop_SumSEqSumP :: Index ix => Array D ix Int -> Bool
prop_SumSEqSumP arr = sum arr == sum (setComp Par arr)


prop_ProdSEqProdP :: Index ix => Array D ix Int -> Bool
prop_ProdSEqProdP arr = product arr == product (setComp Par arr)


foldOpsProp ::
     (Source P ix Int)
  => Fun Int Bool
  -> ArrTinyNE P ix Int
  -> Property
foldOpsProp f (ArrTinyNE arr) =
  (A.maximum' arr === getMax (foldMono Max arr)) .&&.
  (A.minimum' arr === getMin (foldSemi Min maxBound arr)) .&&.
  (A.sum arr === F.sum ls) .&&.
  (A.product (A.map ((+ 0.1) . (fromIntegral :: Int -> Double)) arr) ===
   getProduct (foldMono (Product . (+ 0.1) . fromIntegral) arr)) .&&.
  (A.all (apply f) arr === F.all (apply f) ls) .&&.
  (A.any (apply f) arr === F.any (apply f) ls) .&&.
  (A.or (A.map (apply f) arr) === F.or (fmap (apply f) ls)) .&&.
  (A.and (A.map (apply f) arr) === F.and (fmap (apply f) ls))
  where
    ls = toList arr


prop_NestedFoldP :: Array D Ix1 (Array D Ix1 Int) -> Bool
prop_NestedFoldP arr = sum (setComp Par (map sum $ setComp Par arr)) == sum (map sum arr)


specFold ::
     forall ix. (Arbitrary ix, Index ix, Show (Array D ix Int), Show (Array P ix Int))
  => String
  -> Spec
specFold dimStr =
  describe dimStr $ do
    it "sumS Eq sumP" $ property $ prop_SumSEqSumP @ix
    it "prodS Eq prodP" $ property $ prop_ProdSEqProdP @ix
    it "foldOps" $ property $ foldOpsProp @ix


prop_foldOuterSliceToList ::
     (Elt P ix Int ~ Array M (Lower ix) Int, OuterSlice P ix Int, Index (Lower ix))
  => ArrTiny P ix Int
  -> Property
prop_foldOuterSliceToList (ArrTiny arr) =
  foldOuterSlice A.toList arr === A.fold (A.map pure arr)


spec :: Spec
spec = do
  specFold @Ix1 "Ix1"
  specFold @Ix2 "Ix2"
  specFold @Ix3 "Ix3"
  it "Nested Parallel Fold" $ property prop_NestedFoldP
  describe "Foldable Props" $ do
    prop "Ix2" $ prop_foldOuterSliceToList @Ix2
    prop "Ix3" $ prop_foldOuterSliceToList @Ix3
    prop "Ix4" $ prop_foldOuterSliceToList @Ix4
  describe "Exceptions" $ do
    let emptySelector :: forall ix . Index ix => SizeException -> Bool
        emptySelector = (== SizeEmptyException (Sz (zeroIndex :: ix)))
    it "maximumM" $ maximumM (A.empty :: Array D Ix1 Int) `shouldThrow` emptySelector @Ix1
    it "minimumM" $ minimumM (A.empty :: Array D Ix2 Int) `shouldThrow` emptySelector @Ix2
    it "maximum'" $ (pure $! maximum' (A.empty :: Array D Ix3 Int)) `shouldThrow` emptySelector @Ix3
    it "minimum'" $ (pure $! minimum' (A.empty :: Array D Ix4 Int)) `shouldThrow` emptySelector @Ix4
