{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Array.Massiv.Common.IndexSpec (Sz(..), Ix(..), spec) where

import           Control.Monad
import           Data.Array.Massiv.Common.Index
import           Data.Functor.Identity
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

prop_ToFromLinearIndex :: Index ix => proxy ix -> Ix ix -> Property
prop_ToFromLinearIndex _ (Ix (Sz sz) ix) =
  isSafeIndex sz ix ==> ix == fromLinearIndex sz (toLinearIndex sz ix)

prop_FromToLinearIndex :: Index ix => proxy ix -> Sz ix -> Int -> Property
prop_FromToLinearIndex _ (Sz sz) i =
  totalElem sz >= i ==> i == toLinearIndex sz (fromLinearIndex sz i)

prop_CountElements :: Index ix => proxy ix -> Int -> Sz ix -> Property
prop_CountElements _ thresh (Sz sz) =
  totalElem sz < thresh ==> totalElem sz == iter zeroIndex sz 1 (<) 0 (\ _ acc -> (acc + 1))

prop_IterMonotonic :: (Ord ix, Index ix) => proxy ix -> Int -> Sz ix -> Property
prop_IterMonotonic _ thresh (Sz sz) =
  totalElem sz < thresh ==> fst $
  iter (liftIndex succ zeroIndex) sz 1 (<) (True, zeroIndex) $ \ curIx (prevMono, prevIx) ->
    let isMono = prevMono && prevIx < curIx in isMono `seq` (isMono, curIx)

prop_IterMonotonicM :: (Ord ix, Index ix) => proxy ix -> Int -> Sz ix -> Property
prop_IterMonotonicM _ thresh (Sz sz) =
  totalElem sz < thresh ==> fst $
  runIdentity $
  iterM (liftIndex succ zeroIndex) sz 1 (<) (True, zeroIndex) $ \curIx (prevMono, prevIx) ->
    let isMono = prevMono && prevIx < curIx
    in return $ isMono `seq` (isMono, curIx)


prop_IterMonotonicBackwards :: (Ord ix, Index ix) => proxy ix -> Int -> Sz ix -> Property
prop_IterMonotonicBackwards _ thresh (Sz sz) =
  totalElem sz < thresh ==> fst $
  iter (liftIndex pred sz) zeroIndex (-1) (>=) (True, sz) $ \ curIx (prevMono, prevIx) ->
    let isMono = prevMono && prevIx > curIx in isMono `seq` (isMono, curIx)

prop_IterMonotonicBackwardsM :: (Ord ix, Index ix) => proxy ix -> Int -> Sz ix -> Property
prop_IterMonotonicBackwardsM _ thresh (Sz sz) =
  totalElem sz < thresh ==> fst $ runIdentity $
  iterM (liftIndex pred sz) zeroIndex (-1) (>=) (True, sz) $ \ curIx (prevMono, prevIx) ->
    let isMono = prevMono && prevIx > curIx in return $ isMono `seq` (isMono, curIx)

prop_LiftLift2 :: Index ix => proxy ix -> ix -> Int -> Bool
prop_LiftLift2 _ ix delta = liftIndex2 (+) ix (liftIndex (+delta) zeroIndex) ==
                            liftIndex (+delta) ix


instance Show (DIM1 -> Double) where
  show _ = "Index Func: DIM1 -> Double"


prop_BorderRepairSafe :: Index ix => proxy ix -> Border ix -> Sz ix -> ix -> Property
prop_BorderRepairSafe _ border@(Fill defIx) (Sz sz) ix =
  not (isSafeIndex sz ix) ==> handleBorderIndex border sz id ix == defIx
prop_BorderRepairSafe _ border (Sz sz) ix =
  not (isSafeIndex sz ix) ==> isSafeIndex sz (handleBorderIndex border sz id ix)


prop_UnconsGetDrop :: (Arbitrary ix, Index (Lower ix), Index ix) => proxy ix -> ix -> Bool
prop_UnconsGetDrop _ ix =
  Just (unconsDim ix) == do
    i <- getIndex ix 1
    ixL <- dropIndex ix 1
    return (i, ixL)

prop_UnsnocGetDrop :: (Arbitrary ix, Index (Lower ix), Index ix) => proxy ix -> ix -> Bool
prop_UnsnocGetDrop _ ix =
  Just (unsnocDim ix) == do
    i <- getIndex ix (rank ix)
    ixL <- dropIndex ix (rank ix)
    return (ixL, i)

prop_SetAll :: (Arbitrary ix, Index ix) => proxy ix -> ix -> Int -> Bool
prop_SetAll _ ix i =
  foldM (\cix d -> setIndex cix d i) ix [1 .. rank ix] ==
  Just (liftIndex (+ i) zeroIndex)


prop_SetGet :: (Arbitrary ix, Index ix) => proxy ix -> ix -> Int -> Bool
prop_SetGet _ ix n = Just n == (setIndex ix valDim n >>= (`getIndex` valDim))
  where
    valDim = (1 + (n `mod` rank ix))


prop_BorderDIM1 :: (Positive Int) -> Border Double -> (DIM1 -> Double) -> Sz DIM1 -> DIM1 -> Bool
prop_BorderDIM1 (Positive period) border getVal (Sz sz) ix =
  if isSafeIndex sz ix
    then getVal ix == val
    else case border of
           Fill defVal -> defVal == val
           Wrap ->
             val ==
             handleBorderIndex
               border
               sz
               getVal
               (liftIndex2 (+) (liftIndex (* period) sz) ix)
           Edge ->
             if ix < 0
               then val == getVal (liftIndex (max 0) ix)
               else val ==
                    getVal (liftIndex2 min (liftIndex (subtract 1) sz) ix)
           Reflect ->
             val ==
             handleBorderIndex
               border
               sz
               getVal
               (liftIndex2 (+) (liftIndex (* (2 * signum ix * period)) sz) ix)
           Continue ->
             val ==
             handleBorderIndex
               Reflect
               sz
               getVal
               (if ix < 0
                  then ix - 1
                  else ix + 1)
  where
    val = handleBorderIndex border sz getVal ix

specDimN :: (Index ix, Ord ix, CoArbitrary ix, Arbitrary ix) => proxy ix -> Spec
specDimN proxy = do
  describe "Safety" $ do
    it "isSafeIndex" $ property $ prop_IsSafeIx proxy
    it "RepairSafeIx" $ property $ prop_RepairSafeIx proxy
  describe "Lifting" $ do
    it "Lift/Lift2" $ property $ prop_LiftLift2 proxy
  describe "Linear" $ do
    it "ToFromLinearIndex" $ property $ prop_ToFromLinearIndex proxy
    it "FromToLinearIndex" $ property $ prop_FromToLinearIndex proxy
  describe "Iterator" $ do
    it "CountElements" $ property $ prop_CountElements proxy (2000000)
    it "Monotonic" $ property $ prop_IterMonotonic proxy (2000000)
    it "MonotonicBackwards" $ property $ prop_IterMonotonicBackwards proxy (2000000)
    it "MonotonicM" $ property $ prop_IterMonotonicM proxy (2000000)
    it "MonotonicBackwardsM" $ property $ prop_IterMonotonicBackwardsM proxy (2000000)
  describe "Border" $ do
    it "BorderRepairSafe" $ property $ prop_BorderRepairSafe proxy
  describe "SetGetDrop" $ do
    it "SetAll" $ property $ prop_SetAll proxy
    it "SetGet" $ property $ prop_SetGet proxy

specDim2AndUp
  :: (Index ix, Index (Lower ix), Ord ix, CoArbitrary ix, Arbitrary ix)
  => proxy ix -> Spec
specDim2AndUp proxy = do
  describe "Higher/Lower" $ do
    it "UnconsCons" $ property $ prop_UnconsCons proxy
    it "UnsnocSnoc" $ property $ prop_UnsnocSnoc proxy
    it "UnconsGetDrop" $ property $ prop_UnconsGetDrop proxy
    it "UnsnocGetDrop" $ property $ prop_UnsnocGetDrop proxy


spec :: Spec
spec = do
  describe "DIM1" $ do
    specDimN (Nothing :: Maybe DIM1)
    it "BorderIndex" $ property $ prop_BorderDIM1
  describe "DIM2" $ do
    specDimN (Nothing :: Maybe DIM2)
    specDim2AndUp (Nothing :: Maybe DIM2)
  describe "DIM3" $ do
    specDimN (Nothing :: Maybe DIM3)
    specDim2AndUp (Nothing :: Maybe DIM3)
  describe "DIM4" $ do
    specDimN (Nothing :: Maybe DIM4)
    specDim2AndUp (Nothing :: Maybe DIM4)
  describe "DIM5" $ do
    specDimN (Nothing :: Maybe DIM5)
    specDim2AndUp (Nothing :: Maybe DIM5)
