{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Array.Massiv.Ops.FoldSpec (spec) where

import           Data.Array.Massiv
import           Data.Array.Massiv.CommonSpec (ArrP(..))
import           Data.List.NonEmpty           (NonEmpty, toList)
import           Prelude                      hiding (map, product, sum)
import qualified Prelude                      as P (sum)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic


prop_SumSEqSumP :: Index ix => proxy ix -> Array D ix Int -> Bool
prop_SumSEqSumP _ arr = sum arr == sum (setComp Par arr)


prop_ProdSEqProdP :: Index ix => proxy ix -> Array D ix Int -> Bool
prop_ProdSEqProdP _ arr = product arr == product (setComp Par arr)

prop_NestedFoldP :: Array D DIM1 (Array D DIM1 Int) -> Bool
prop_NestedFoldP arr = sum (setComp Par (map sum $ setComp Par arr)) == sum (map sum arr)

prop_FoldrOnP :: NonEmpty Int -> ArrP D DIM1 Int -> Property
prop_FoldrOnP wIds (ArrP arr) = length arr > length wIds ==> monadicIO $ do
  res <- run $ ifoldrOnP (toList wIds) (:) [] (\ _ -> (+)) 0 arr
  if length arr `mod` length wIds == 0
    then assert (length res == length wIds)
    else assert (length res == length wIds + 1)
  assert (P.sum res == sum arr)

prop_FoldlOnP :: NonEmpty Int -> ArrP D DIM1 Int -> Property
prop_FoldlOnP wIds (ArrP arr) = length arr > length wIds ==> monadicIO $ do
  res <- run $ ifoldlOnP (toList wIds) (flip (:)) [] (\ a _ x -> a + x) 0 arr
  if length arr `mod` length wIds == 0
    then assert (length res == length wIds)
    else assert (length res == length wIds + 1)
  assert (P.sum res == sum arr)


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
  it "FoldrOnP" $ property $ prop_FoldrOnP
  it "FoldlOnP" $ property $ prop_FoldlOnP
