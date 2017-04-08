{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Array.MassivSpec (spec) where

import           Data.Array.Massiv
import           Data.Array.Massiv.DelayedSpec()
import           Data.Array.Massiv.CommonSpec (ArrIx (..))
import           Test.Hspec
import           Test.QuickCheck






prop_ExtractAppend
  :: (Eq e, Shape r ix e, Arbitrary (ArrIx r ix e))
  => proxy (r, ix, e) -> Int -> Int -> ArrIx r ix e -> Property
prop_ExtractAppend _ thresh dim (ArrIx arr ix) =
  totalElem sz < thresh ==> maybe True (delay arr ==) $ do
    i <- getIndex ix dim'
    eIx <- setIndex sz dim' i
    sIx' <- setIndex zeroIndex dim' i
    arr1 <- extractFromTo zeroIndex eIx arr
    arr2 <- extractFromTo sIx' sz arr
    append dim' arr1 arr2
  where
    sz = size arr
    dim' = (dim `mod` (rank ix)) + 1


massivSpecN
  :: (Eq e, Unbox e, Shape r ix e, Load r ix, Arbitrary (ArrIx r ix e))
  => proxy (r, ix, e) -> Spec
massivSpecN r = do
  describe "ExtractAppend" $ do
    it "ExtractAppend" $ property $ prop_ExtractAppend r 100000

spec :: Spec
spec = do
  describe "DIM1" $ do
    massivSpecN (Nothing :: Maybe (D, DIM1, Int))
  describe "DIM2" $ do
    massivSpecN (Nothing :: Maybe (D, DIM2, Int))
  describe "DIM3" $ do
    massivSpecN (Nothing :: Maybe (D, DIM3, Int))
  describe "DIM4" $ do
    massivSpecN (Nothing :: Maybe (D, DIM4, Int))
