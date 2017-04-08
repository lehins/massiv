{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Array.MassivSpec (spec) where

import           Data.Array.Massiv
import           Data.Array.Massiv.DelayedSpec()
import           Data.Array.Massiv.CommonSpec (ArrIx (..))
import           Data.Array.Massiv.ManifestSpec ()
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


massivSpecNDelayed
  :: (Eq e, Shape D ix e, Arbitrary (ArrIx D ix e))
  => proxy (D, ix, e) -> Spec
massivSpecNDelayed r = do
  it "ExtractAppend" $ property $ prop_ExtractAppend r 100000
  it "ExtractAppend" $ property $ prop_ExtractAppend r 100000

massivSpecNManifest
  :: (Eq e, Shape M ix e, Arbitrary (ArrIx M ix e))
  => proxy (M, ix, e) -> Spec
massivSpecNManifest r = do
  it "ExtractAppend" $ property $ prop_ExtractAppend r 100000


spec :: Spec
spec = do
  describe "Delayed" $ do
    describe "DIM1" $ do
      massivSpecNDelayed (Nothing :: Maybe (D, DIM1, Int))
    describe "DIM2 - Delayed" $ do
      massivSpecNDelayed (Nothing :: Maybe (D, DIM2, Int))
    describe "DIM3 - Delayed" $ do
      massivSpecNDelayed (Nothing :: Maybe (D, DIM3, Int))
    describe "DIM4 - Delayed" $ do
      massivSpecNDelayed (Nothing :: Maybe (D, DIM4, Int))
  describe "Manifest" $ do
    describe "DIM1 - Manifest" $ do
      massivSpecNManifest (Nothing :: Maybe (M, DIM1, Int))
    describe "DIM2 - Manifest" $ do
      massivSpecNManifest (Nothing :: Maybe (M, DIM2, Int))
    describe "DIM3 - Manifest" $ do
      massivSpecNManifest (Nothing :: Maybe (M, DIM3, Int))
    describe "DIM4 - Manifest" $ do
      massivSpecNManifest (Nothing :: Maybe (M, DIM4, Int))
