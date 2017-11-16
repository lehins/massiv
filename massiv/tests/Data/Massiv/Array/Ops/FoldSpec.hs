{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Massiv.Array.Ops.FoldSpec (spec) where

import           Data.Massiv.CoreArbitrary
import           Prelude                   hiding (map, product, sum)
import qualified Prelude                   as P (length, sum)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic


prop_SumSEqSumP :: Index ix => proxy ix -> Array D ix Int -> Bool
prop_SumSEqSumP _ arr = sum arr == sum (setComp Par arr)


prop_ProdSEqProdP :: Index ix => proxy ix -> Array D ix Int -> Bool
prop_ProdSEqProdP _ arr = product arr == product (setComp Par arr)

prop_NestedFoldP :: Array D Ix1 (Array D Ix1 Int) -> Bool
prop_NestedFoldP arr = sum (setComp Par (map sum $ setComp Par arr)) == sum (map sum arr)

prop_FoldrOnP :: Int -> [Int] -> ArrP D Ix1 Int -> Property
prop_FoldrOnP wId wIds (ArrP arr) =
  P.length arr > P.length wIds ==> monadicIO $ do
    res <- run $ ifoldrOnP wIdsNE (:) [] (\_ -> (+)) 0 arr
    if P.length arr `mod` P.length wIdsNE == 0
      then assert (P.length res == P.length wIdsNE)
      else assert (P.length res == P.length wIdsNE + 1)
    assert (P.sum res == sum arr)
  where
    wIdsNE = wId : wIds

prop_FoldlOnP :: Int -> [Int] -> ArrP D Ix1 Int -> Property
prop_FoldlOnP wId wIds (ArrP arr) =
  P.length arr > P.length wIds ==> monadicIO $ do
    res <- run $ ifoldlOnP wIdsNE (flip (:)) [] (\a _ x -> a + x) 0 arr
    if P.length arr `mod` P.length wIdsNE == 0
      then assert (P.length res == P.length wIdsNE)
      else assert (P.length res == P.length wIdsNE + 1)
    assert (P.sum res == sum arr)
  where
    wIdsNE = wId : wIds


specFold ::
     (Arbitrary ix, CoArbitrary ix, Index ix, Show (Array D ix Int))
  => proxy ix
  -> String
  -> Spec
specFold proxy dimStr = do
  describe dimStr $ do
    it "sumS Eq sumP" $ property $ prop_SumSEqSumP proxy
    it "prodS Eq prodP" $ property $ prop_ProdSEqProdP proxy

spec :: Spec
spec = do
  specFold (Nothing :: Maybe Ix1) "Ix1"
  specFold (Nothing :: Maybe Ix2) "Ix2"
  it "Nested Parallel Fold" $ property prop_NestedFoldP
  it "FoldrOnP" $ property $ prop_FoldrOnP
  it "FoldlOnP" $ property $ prop_FoldlOnP
