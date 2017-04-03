{-# LANGUAGE FlexibleContexts #-}
module Data.Array.Massiv.Common.IndexSpec (spec) where

import           Data.Array.Massiv.Common.Index
import           Test.Hspec
import           Test.QuickCheck

newtype Sz ix = Sz ix deriving Show

instance Functor Sz where
  fmap f (Sz sz) = Sz (f sz)

data Ix ix = Ix (Sz ix) ix deriving Show

instance (Index ix, Arbitrary ix) => Arbitrary (Sz ix) where
  arbitrary = Sz <$> liftIndex ((+1) . abs) <$> arbitrary

instance (Index ix, Arbitrary ix) => Arbitrary (Ix ix) where
  arbitrary = do
    Sz sz <- arbitrary
    -- Make sure index is within bounds:
    Ix (Sz sz) <$> flip (liftIndex2 mod) sz <$> arbitrary


instance Arbitrary e => Arbitrary (Border e) where
  arbitrary =
    oneof
      [ Fill <$> arbitrary
      , return Wrap
      , return Edge
      , return Reflect
      , return Continue
      ]


prop_IsSafeIx :: (Index ix) => proxy ix -> Ix ix -> Bool
prop_IsSafeIx _ (Ix (Sz sz) ix) = isSafeIndex sz ix

prop_RepairSafeIx :: Index ix => proxy ix -> Ix ix -> Bool
prop_RepairSafeIx _ (Ix (Sz sz) ix) =
  ix == repairIndex sz ix (error "Impossible") (error "Impossible")

prop_UnconsCons :: (Index (Lower ix), Index ix) => proxy ix -> ix -> Bool
prop_UnconsCons _ ix = ix == uncurry consDim (unconsDim ix)

prop_UnsnocSnoc :: (Index (Lower ix), Index ix) => proxy ix -> ix -> Bool
prop_UnsnocSnoc _ ix = ix == uncurry snocDim (unsnocDim ix)

prop_ToFromLinearIndex :: (Index (Lower ix), Index ix) => proxy ix -> Ix ix -> Property
prop_ToFromLinearIndex _ (Ix (Sz sz) ix) =
  isSafeIndex sz ix ==> ix == fromLinearIndex sz (toLinearIndex sz ix)

prop_FromToLinearIndex :: (Index (Lower ix), Index ix) => proxy ix -> Sz ix -> Int -> Property
prop_FromToLinearIndex _ (Sz sz) i =
  totalElem sz >= i ==> i == toLinearIndex sz (fromLinearIndex sz i)


-- NOTE: takes a bit too long for DIM5
prop_CountElements :: Index ix => proxy ix -> Sz ix -> Bool
prop_CountElements _ (Sz sz) = totalElem sz == iter zeroIndex sz 0 (\ _ acc -> (acc + 1))

prop_IterMonotonic :: (Ord ix, Index ix) => proxy ix -> Sz ix -> Bool
prop_IterMonotonic _ (Sz sz) =
  fst $
  iter (liftIndex succ zeroIndex) sz (True, zeroIndex) $ \ curIx (prevMono, prevIx) ->
    (prevMono && prevIx < curIx, curIx)

specNDim :: (Index (Lower ix), Index ix, Ord ix, Arbitrary ix) => proxy ix -> Spec
specNDim proxy = do
  describe "Safety" $ do
    it "isSafeIndex" $ property $ prop_IsSafeIx proxy
    it "RepairSafeIx" $ property $ prop_RepairSafeIx proxy
  describe "Higher/Lower" $ do
    it "UnconsCons" $ property $ prop_UnconsCons proxy
    it "UnsnocSnoc" $ property $ prop_UnsnocSnoc proxy
  describe "Linear" $ do
    it "ToFromLinearIndex" $ property $ prop_ToFromLinearIndex proxy
    it "FromToLinearIndex" $ property $ prop_FromToLinearIndex proxy
  describe "Iterator" $ do
    it "Monotonic" $ property $ prop_IterMonotonic proxy
    it "CountElements" $ property $ prop_CountElements proxy


spec :: Spec
spec = do
  describe "DIM1" $ specNDim (Nothing :: Maybe DIM1)
  describe "DIM2" $ specNDim (Nothing :: Maybe DIM2)
  describe "DIM3" $ specNDim (Nothing :: Maybe DIM3)
  describe "DIM4" $ specNDim (Nothing :: Maybe DIM4)
  describe "DIM5" $ specNDim (Nothing :: Maybe DIM5)
