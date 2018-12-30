{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Data.Massiv.Core.IndexSpec (Sz(..), SzZ(..), SzIx(..), DimIx(..), spec) where

import           Control.Monad
import           Data.Massiv.Core.Index
import           Data.Functor.Identity
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Function

-- | Size that will result in a non-empty array
newtype Sz ix = Sz ix deriving Show

-- | Size that can have zero elements
newtype SzZ ix = SzZ ix deriving Show

-- | Dimension that is always within bounds of an index
newtype DimIx ix = DimIx Dim deriving Show

instance Functor Sz where
  fmap f (Sz sz) = Sz (f sz)

instance Functor SzZ where
  fmap f (SzZ sz) = SzZ (f sz)

data SzIx ix = SzIx (Sz ix) ix deriving Show

instance (Index ix, Arbitrary ix) => Arbitrary (Sz ix) where
  arbitrary = do
    sz <- liftIndex ((+1) . abs) <$> arbitrary
    if totalElem sz > 200000
      then arbitrary
      else return $ Sz sz

instance (Index ix, Arbitrary ix) => Arbitrary (SzZ ix) where
  arbitrary = do
    sz <- liftIndex abs <$> arbitrary
    if totalElem sz > 200000
      then arbitrary
      else return $ SzZ sz

instance (Index ix, Arbitrary ix) => Arbitrary (Stride ix) where
  arbitrary = do
    Positive (Small x) <- arbitrary
    Stride <$> liftIndex ((+1) . (`mod` x)) <$> arbitrary

instance (Index ix, Arbitrary ix) => Arbitrary (SzIx ix) where
  arbitrary = do
    Sz sz <- arbitrary
    -- Make sure index is within bounds:
    SzIx (Sz sz) <$> flip (liftIndex2 mod) sz <$> arbitrary


instance Arbitrary e => Arbitrary (Border e) where
  arbitrary =
    oneof
      [ Fill <$> arbitrary
      , return Wrap
      , return Edge
      , return Reflect
      , return Continue
      ]


instance Index ix => Arbitrary (DimIx ix) where
  arbitrary = do
    n <- arbitrary
    return $ DimIx (1 + (Dim n `mod` (dimensions (undefined :: ix))))

arbitraryIntIx :: Gen Int
arbitraryIntIx = sized (\s -> resize (floor $ (sqrt :: Double -> Double) $ fromIntegral s) arbitrary)
  -- Generators are quadratic in QuickCheck.


instance Arbitrary Ix2 where
  arbitrary = (:.) <$> arbitraryIntIx <*> arbitraryIntIx

instance Arbitrary Ix3 where
  arbitrary = (:>) <$> arbitraryIntIx <*> ((:.) <$> arbitraryIntIx <*> arbitraryIntIx)

instance Arbitrary Ix4 where
  arbitrary = (:>) <$> arbitraryIntIx <*> arbitrary

instance Arbitrary Ix5 where
  arbitrary = (:>) <$> arbitraryIntIx <*> arbitrary

instance CoArbitrary Ix2 where
  coarbitrary (i :. j) = coarbitrary i . coarbitrary j

instance CoArbitrary Ix3 where
  coarbitrary (i :> ix) = coarbitrary i . coarbitrary ix

instance CoArbitrary Ix4 where
  coarbitrary (i :> ix) = coarbitrary i . coarbitrary ix

instance CoArbitrary Ix5 where
  coarbitrary (i :> ix) = coarbitrary i . coarbitrary ix

instance Function Ix2 where
  function = functionMap fromIx2 toIx2

instance Function Ix3 where
  function = functionMap fromIx3 toIx3

instance Function Ix4 where
  function = functionMap fromIx4 toIx4

instance Function Ix5 where
  function = functionMap fromIx5 toIx5



-- instance Arbitrary Ix2 where
--   arbitrary = (:.) <$> arbitrary <*> arbitrary

-- instance (Arbitrary (Ix (n - 1))) => Arbitrary (IxN n) where
--   arbitrary = (:>) <$> arbitrary <*> arbitrary

-- instance CoArbitrary Ix2 where
--   coarbitrary (i :. j) = coarbitrary i . coarbitrary j

-- instance CoArbitrary (Ix (n - 1)) => CoArbitrary (IxN n) where
--   coarbitrary (i :> ix) = coarbitrary i . coarbitrary ix



prop_IsSafeIx :: Index ix => proxy ix -> SzIx ix -> Bool
prop_IsSafeIx _ (SzIx (Sz sz) ix) = isSafeIndex sz ix

prop_RepairSafeIx :: Index ix => proxy ix -> SzIx ix -> Bool
prop_RepairSafeIx _ (SzIx (Sz sz) ix) =
  ix == repairIndex sz ix (error "Impossible") (error "Impossible")

prop_UnconsCons :: Index ix => proxy ix -> ix -> Bool
prop_UnconsCons _ ix = ix == uncurry consDim (unconsDim ix)

prop_UnsnocSnoc :: Index ix => proxy ix -> ix -> Bool
prop_UnsnocSnoc _ ix = ix == uncurry snocDim (unsnocDim ix)

prop_ToFromLinearIndex :: Index ix => proxy ix -> SzIx ix -> Property
prop_ToFromLinearIndex _ (SzIx (Sz sz) ix) =
  isSafeIndex sz ix ==> ix == fromLinearIndex sz (toLinearIndex sz ix)

prop_FromToLinearIndex :: Index ix => proxy ix -> Sz ix -> Int -> Property
prop_FromToLinearIndex _ (Sz sz) i =
  totalElem sz >= i ==> i == toLinearIndex sz (fromLinearIndex sz i)

prop_CountElements :: Index ix => proxy ix -> Int -> Sz ix -> Property
prop_CountElements _ thresh (Sz sz) =
  totalElem sz < thresh ==> totalElem sz ==
  iter zeroIndex sz (pureIndex 1) (<) 0 (\_ acc -> (acc + 1))

prop_IterMonotonic :: Index ix => proxy ix -> Int -> Sz ix -> Property
prop_IterMonotonic _ thresh (Sz sz) =
  totalElem sz < thresh ==> fst $
  iter (liftIndex succ zeroIndex) sz (pureIndex 1) (<) (True, zeroIndex) $ \curIx (prevMono, prevIx) ->
    let isMono = prevMono && prevIx < curIx
     in isMono `seq` (isMono, curIx)


prop_IterMonotonic' :: Index ix => proxy ix -> Int -> Sz ix -> Property
prop_IterMonotonic' _ thresh (Sz sz) =
  totalElem sz <
  thresh ==>
  if isM
    then isM
    else error (show a)
  where
    (isM, a, _) =
      iter (liftIndex succ zeroIndex) sz (pureIndex 1) (<) (True, [], zeroIndex) $
      \ curIx (prevMono, acc, prevIx) ->
        let nAcc = (prevIx, curIx, prevIx < curIx) : acc
            isMono = prevMono && prevIx < curIx
        in isMono `seq` (isMono, nAcc, curIx)


prop_IterMonotonicBackwards' :: Index ix => proxy ix -> Int -> Sz ix -> Property
prop_IterMonotonicBackwards' _ thresh (Sz sz) =
  totalElem sz <
  thresh ==>
  if isM
    then isM
    else error (show a)
  where
    (isM, a, _) =
      iter (liftIndex pred sz) zeroIndex (pureIndex (-1)) (>=) (True, [], sz) $
      \ curIx (prevMono, acc, prevIx) ->
      let isMono = prevMono && prevIx > curIx
          nAcc = (prevIx, curIx, prevIx > curIx) : acc
      in isMono `seq` (isMono, nAcc, curIx)

prop_IterMonotonicM :: Index ix => proxy ix -> Int -> Sz ix -> Property
prop_IterMonotonicM _ thresh (Sz sz) =
  totalElem sz < thresh ==> fst $
  runIdentity $
  iterM (liftIndex succ zeroIndex) sz (pureIndex 1) (<) (True, zeroIndex) $ \curIx (prevMono, prevIx) ->
    let isMono = prevMono && prevIx < curIx
    in return $ isMono `seq` (isMono, curIx)


prop_IterMonotonicBackwards :: Index ix => proxy ix -> Int -> Sz ix -> Property
prop_IterMonotonicBackwards _ thresh (Sz sz) =
  totalElem sz < thresh ==> fst $
  iter (liftIndex pred sz) zeroIndex (pureIndex (-1)) (>=) (True, sz) $ \ curIx (prevMono, prevIx) ->
    let isMono = prevMono && prevIx > curIx in isMono `seq` (isMono, curIx)

prop_IterMonotonicBackwardsM :: Index ix => proxy ix -> Int -> Sz ix -> Property
prop_IterMonotonicBackwardsM _ thresh (Sz sz) =
  totalElem sz < thresh ==> fst $ runIdentity $
  iterM (liftIndex pred sz) zeroIndex (pureIndex (-1)) (>=) (True, sz) $ \ curIx (prevMono, prevIx) ->
    let isMono = prevMono && prevIx > curIx in return $ isMono `seq` (isMono, curIx)

prop_LiftLift2 :: Index ix => proxy ix -> ix -> Int -> Bool
prop_LiftLift2 _ ix delta = liftIndex2 (+) ix (liftIndex (+delta) zeroIndex) ==
                            liftIndex (+delta) ix


instance Show (Ix1 -> Double) where
  show _ = "Index Func: Ix1 -> Double"


prop_BorderRepairSafe :: Index ix => proxy ix -> Border ix -> Sz ix -> ix -> Property
prop_BorderRepairSafe _ border@(Fill defIx) (Sz sz) ix =
  not (isSafeIndex sz ix) ==> handleBorderIndex border sz id ix == defIx
prop_BorderRepairSafe _ border (Sz sz) ix =
  not (isSafeIndex sz ix) ==> isSafeIndex sz (handleBorderIndex border sz id ix)


prop_GetDropInsert :: Index ix => proxy ix -> NonNegative Int -> ix -> Property
prop_GetDropInsert _ (NonNegative d) ix =
  expected === do
    i <- getDim ix dim
    ixL <- dropDim ix dim
    insertDim ixL dim i
  where expected = if d >= 1 && dim <= dimensions ix then Just ix else Nothing
        dim = Dim d

prop_PullOutInsert :: Index ix => proxy ix -> NonNegative Int -> ix -> Property
prop_PullOutInsert _ (NonNegative d) ix =
  expected === do
    (i, ixL) <- pullOutDim ix dim
    insertDim ixL dim i
  where expected = if d >= 1 && dim <= dimensions ix then Just ix else Nothing
        dim = Dim d

prop_UnconsGetDrop :: (Index (Lower ix), Index ix) => proxy ix -> ix -> Property
prop_UnconsGetDrop _ ix =
  Just (unconsDim ix) === do
    i <- getDim ix (dimensions ix)
    ixL <- dropDim ix (dimensions ix)
    return (i, ixL)

prop_UnsnocGetDrop :: (Index (Lower ix), Index ix) => proxy ix -> ix -> Property
prop_UnsnocGetDrop _ ix =
  Just (unsnocDim ix) === do
    i <- getDim ix 1
    ixL <- dropDim ix 1
    return (ixL, i)

prop_SetAll :: Index ix => proxy ix -> ix -> Int -> Bool
prop_SetAll _ ix i =
  foldM (\cix d -> setDim cix d i) ix ([1 .. dimensions ix] :: [Dim]) ==
  Just (pureIndex i)


prop_SetGet :: Index ix => proxy ix -> ix -> DimIx ix -> Int -> Bool
prop_SetGet _ ix (DimIx dim) n = Just n == (setDim ix dim n >>= (`getDim` dim))


prop_BorderIx1 :: Positive Int -> Border Double -> (Ix1 -> Double) -> Sz Ix1 -> Ix1 -> Bool
prop_BorderIx1 (Positive period) border getVal (Sz sz) ix =
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
    it "GetDropInsert" $ property $ prop_GetDropInsert proxy
    it "PullOutInsert" $ property $ prop_PullOutInsert proxy

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
  describe "Tuple based indices" $ do
    describe "Ix1T" $ do
      specDimN (Nothing :: Maybe Ix1T)
      it "BorderIndex" $ property $ prop_BorderIx1
    describe "Ix2T" $ do
      specDimN (Nothing :: Maybe Ix2T)
      specDim2AndUp (Nothing :: Maybe Ix2T)
    describe "Ix3T" $ do
      specDimN (Nothing :: Maybe Ix3T)
      specDim2AndUp (Nothing :: Maybe Ix3T)
    describe "Ix4T" $ do
      specDimN (Nothing :: Maybe Ix4T)
      specDim2AndUp (Nothing :: Maybe Ix4T)
    describe "Ix5T" $ do
      specDimN (Nothing :: Maybe Ix5T)
      specDim2AndUp (Nothing :: Maybe Ix5T)
  describe "Specialized indices" $ do
    describe "Ix2" $ do
      -- These can be used to quickly debug monotonicity
      it "Monotonic'" $
        property $ prop_IterMonotonic' (Nothing :: Maybe Ix2) (20000)
      it "MonotonicBackwards'" $
        property $ prop_IterMonotonicBackwards' (Nothing :: Maybe Ix2) (20000)
      specDimN (Nothing :: Maybe Ix2)
      specDim2AndUp (Nothing :: Maybe Ix2)
    describe "Ix3" $ do
      specDimN (Nothing :: Maybe Ix3)
      specDim2AndUp (Nothing :: Maybe Ix3)
    describe "Ix4" $ do
      specDimN (Nothing :: Maybe Ix4)
      specDim2AndUp (Nothing :: Maybe Ix4)
    describe "Ix5" $ do
      specDimN (Nothing :: Maybe Ix5)
      specDim2AndUp (Nothing :: Maybe Ix5)
