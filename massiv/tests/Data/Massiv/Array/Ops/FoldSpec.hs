{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Massiv.Array.Ops.FoldSpec (spec) where

import qualified Data.Foldable             as F
import           Data.Massiv.CoreArbitrary as A
import           Data.Semigroup
import           Prelude                   hiding (map, product, sum)


prop_SumSEqSumP :: Index ix => proxy ix -> Array D ix Int -> Bool
prop_SumSEqSumP _ arr = sum arr == sum (setComp Par arr)


prop_ProdSEqProdP :: Index ix => proxy ix -> Array D ix Int -> Bool
prop_ProdSEqProdP _ arr = product arr == product (setComp Par arr)

prop_NestedFoldP :: Array D Ix1 (Array D Ix1 Int) -> Bool
prop_NestedFoldP arr = sum (setComp Par (map sum $ setComp Par arr)) == sum (map sum arr)

-- prop_FoldrOnP :: Int -> [Int] -> ArrP D Ix1 Int -> Property
-- prop_FoldrOnP wId wIds (ArrP arr) =
--   P.length arr > P.length wIds ==> monadicIO $ do
--     res <- run $ ifoldrOnP wIdsNE (\_ -> (+)) 0 (:) [] arr
--     if P.length arr `mod` P.length wIdsNE == 0
--       then assert (P.length res == P.length wIdsNE)
--       else assert (P.length res == P.length wIdsNE + 1)
--     assert (P.sum res == sum arr)
--   where
--     wIdsNE = wId : wIds

-- prop_FoldlOnP :: Int -> [Int] -> ArrP D Ix1 Int -> Property
-- prop_FoldlOnP wId wIds (ArrP arr) =
--   P.length arr > P.length wIds ==> monadicIO $ do
--     res <- run $ ifoldlOnP wIdsNE (\a _ x -> a + x) 0 (flip (:)) [] arr
--     if P.length arr `mod` P.length wIdsNE == 0
--       then assert (P.length res == P.length wIdsNE)
--       else assert (P.length res == P.length wIdsNE + 1)
--     assert (P.sum res == sum arr)
--   where
--     wIdsNE = wId : wIds


specFold ::
     (Arbitrary ix, CoArbitrary ix, Index ix, Show (Array D ix Int))
  => proxy ix
  -> String
  -> Spec
specFold proxy dimStr = do
  describe dimStr $ do
    it "sumS Eq sumP" $ property $ prop_SumSEqSumP proxy
    it "prodS Eq prodP" $ property $ prop_ProdSEqProdP proxy

foldOpsProp :: (Source P ix Int) => proxy ix -> Fun Int Bool -> ArrTiny1 P ix Int -> Property
foldOpsProp _ f (ArrTiny1 arr) =
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

spec :: Spec
spec = do
  specFold (Nothing :: Maybe Ix1) "Ix1"
  specFold (Nothing :: Maybe Ix2) "Ix2"
  it "Nested Parallel Fold" $ property prop_NestedFoldP
  -- it "FoldrOnP" $ property $ prop_FoldrOnP
  -- it "FoldlOnP" $ property $ prop_FoldlOnP
  describe "Foldable Props" $ do
    it "Ix1" $ property $ foldOpsProp (Nothing :: Maybe Ix1)
    it "Ix2" $ property $ foldOpsProp (Nothing :: Maybe Ix2)
    it "Ix3" $ property $ foldOpsProp (Nothing :: Maybe Ix3)
