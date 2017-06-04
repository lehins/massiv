{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Array.Massiv.Ops.TransformSpec (spec) where

import           Data.Array.Massiv            as M
import           Data.Array.Massiv.CommonSpec (Arr (..), ArrIx (..))
import           Data.Typeable                (Typeable)
import           Test.Hspec
import           Test.QuickCheck



prop_ExtractAppend
  :: (Eq e, Shape r ix e, Arbitrary (ArrIx r ix e))
  => proxy (r, ix, e) -> Int -> ArrIx r ix e -> Bool
prop_ExtractAppend _ dim (ArrIx arr ix) =
  maybe False ((delay arr ==) . uncurry (append' dim')) $ M.splitAt dim' ix arr
  where
    dim' = (dim `mod` rank ix) + 1


prop_transposeOuterInner :: Arr D DIM2 Int -> Property
prop_transposeOuterInner (Arr arr) = transposeOuter arr === transpose arr


specN :: (Eq e, Shape r ix e, Typeable e, Arbitrary (ArrIx r ix e))
  => proxy (r, ix, e) -> Spec
specN r = do
  it "ExtractAppend" $ property $ prop_ExtractAppend r


spec :: Spec
spec = do
  it "transposeOuterInner" $ property prop_transposeOuterInner
  describe "Delayed" $ do
    describe "DIM1" $ specN (Nothing :: Maybe (D, DIM1, Int))
    describe "DIM2" $ specN (Nothing :: Maybe (D, DIM2, Int))
    describe "DIM3" $ specN (Nothing :: Maybe (D, DIM3, Int))
    describe "DIM4" $ specN (Nothing :: Maybe (D, DIM4, Int))
  describe "Unboxed" $ do
    describe "DIM1" $ specN (Nothing :: Maybe (U, DIM1, Int))
    describe "DIM2" $ specN (Nothing :: Maybe (U, DIM2, Int))
    describe "DIM3" $ specN (Nothing :: Maybe (U, DIM3, Int))
    describe "DIM4" $ specN (Nothing :: Maybe (U, DIM4, Int))
