{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Array.Massiv.Ops.FoldSpec (spec) where

import Prelude hiding (map)
import           Data.Array.Massiv
import           Data.Array.Massiv.DelayedSpec ()
import           Test.Hspec
import           Test.QuickCheck


prop_SumSEqSumP :: Index ix => proxy ix -> Array D ix Int -> Bool
prop_SumSEqSumP _ arr = sumS arr == sumP arr


prop_ProdSEqProdP :: Index ix => proxy ix -> Array D ix Int -> Bool
prop_ProdSEqProdP _ arr = productS arr == productP arr

prop_NestedFoldP :: Array D DIM1 (Array D DIM1 Int) -> Bool
prop_NestedFoldP arr = sumP (map sumP arr) == sumS (map sumS arr)


specFold :: (Arbitrary ix, CoArbitrary ix, Index ix) => proxy ix -> String -> Spec
specFold proxy dimStr = do
  describe dimStr $ do
    it "sumS Eq sumP" $ property $ prop_SumSEqSumP proxy
    it "prodS Eq prodP" $ property $ prop_ProdSEqProdP proxy

spec :: Spec
spec = do
  specFold (Nothing :: Maybe DIM1) "DIM1"
  specFold (Nothing :: Maybe DIM2) "DIM2"
  it "Nested Parallel Fold" $ property prop_NestedFoldP
  -- describe "DIM2" $ do
  --   specShapeN (Nothing :: Maybe (D, DIM2, Int))
  --   specSliceN (Nothing :: Maybe (D, DIM2, Int))
  --   specSliceDim2
  -- describe "DIM3" $ do
  --   specShapeN (Nothing :: Maybe (D, DIM3, Int))
  --   specSliceN (Nothing :: Maybe (D, DIM3, Int))
