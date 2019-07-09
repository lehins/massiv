{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Test.Massiv.Core.Index
  ( DimIx(..)
  , SzNE(..)
  , SzIx(..)
  , ixToList
  , arbitraryIx1
  , toIx
  -- * Specs
  -- ** Index
  , specIx1
  , ixSpec
  , ix2UpSpec
  , ixNumSpec
  -- ** Size
  , szNumSpec
  , szSpec
  -- * Re-exports
  , module Data.Massiv.Core.Index
  ) where

import Data.IORef
import Control.DeepSeq
import Control.Exception (throw)
import Control.Monad
import Data.Foldable as F
import Data.Functor.Identity
import Data.Massiv.Array.Unsafe (Sz(SafeSz))
import Data.Massiv.Core.Index
import Data.Proxy
import Data.Typeable
import Test.Hspec
import Test.Massiv.Utils
import Test.QuickCheck
import Test.QuickCheck.Function


-- | Dimension that is always within bounds of an index
newtype DimIx ix = DimIx Dim deriving Show

deriving instance Arbitrary Dim

-- | Size that will result in a non-empty array.
--
-- prop > \ (neSz :: Sz1) -> totalElem (unSzNE neSz) > 0
-- prop > \ (neSz :: Sz2) -> totalElem (unSzNE neSz) > 0
-- prop > \ (neSz :: Sz3) -> totalElem (unSzNE neSz) > 0
-- prop > \ (neSz :: Sz4) -> totalElem (unSzNE neSz) > 0
-- prop > \ (neSz :: Sz5) -> totalElem (unSzNE neSz) > 0
newtype SzNE ix = SzNE
  { unSzNE :: Sz ix
  } deriving (Show)


-- | Non-empty size together with an index that is within bounds of that index.
data SzIx ix = SzIx (Sz ix) ix deriving Show

instance (Index ix, Arbitrary ix) => Arbitrary (Sz ix) where
  arbitrary = do
    sz <- Sz . liftIndex abs <$> arbitrary
    if totalElem sz > 200000
      then arbitrary
      else return sz

instance (Index ix, Arbitrary ix) => Arbitrary (SzNE ix) where
  arbitrary = SzNE . Sz . liftIndex (+1) . unSz <$> arbitrary

instance (Index ix, Arbitrary ix) => Arbitrary (Stride ix) where
  arbitrary = do
    Positive (Small x) <- arbitrary
    Stride . liftIndex ((+1) . (`mod` min 6 x)) <$> arbitrary

instance (Index ix, Arbitrary ix) => Arbitrary (SzIx ix) where
  arbitrary = do
    SzNE sz <- arbitrary
    -- Make sure index is within bounds:
    SzIx sz . flip (liftIndex2 mod) (unSz sz) <$> arbitrary


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
    return $ DimIx (1 + (Dim n `mod` dimensions (Proxy :: Proxy ix)))

-- | Generators are quadratic in QuickCheck. Unlike built-in Arbitrary instance for `Int`,
-- this one generates smaller integers
--
-- @since 0.1.0
arbitraryIx1 :: Gen Int
arbitraryIx1 = sized (\s -> resize (floor $ (sqrt :: Double -> Double) $ fromIntegral s) arbitrary)

-- | Convert an index to a list
--
-- @since 0.1.0
ixToList :: Index ix => ix -> [Int]
ixToList = reverse . foldlIndex (flip (:)) []

-- | A fairly slow way to convert from one arbitrary index to another of the same dimension
--
-- @since 0.1.0
toIx ::
     forall ix' ix. (Dimensions ix' ~ Dimensions ix, Index ix', Index ix)
  => ix
  -> ix'
toIx ix = F.foldl' setEachIndex zeroIndex [1 .. dimensions (Sz ix)]
  where
    setEachIndex ix' d = setDim' ix' d (getDim' ix d)

instance Arbitrary Ix0 where
  arbitrary = pure Ix0

instance Arbitrary Ix2 where
  arbitrary = (:.) <$> arbitraryIx1 <*> arbitraryIx1

instance Arbitrary Ix3 where
  arbitrary = (:>) <$> arbitraryIx1 <*> ((:.) <$> arbitraryIx1 <*> arbitraryIx1)

instance Arbitrary Ix4 where
  arbitrary = (:>) <$> arbitraryIx1 <*> arbitrary

instance Arbitrary Ix5 where
  arbitrary = (:>) <$> arbitraryIx1 <*> arbitrary

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


prop_IsSafeIndex :: Index ix => SzIx ix -> Bool
prop_IsSafeIndex (SzIx sz ix) = isSafeIndex sz ix

prop_RepairSafeIx :: Index ix => SzIx ix -> Property
prop_RepairSafeIx (SzIx sz ix) =
  ix === repairIndex sz ix (errorImpossible "below zero") (errorImpossible "above zero")
  where
    errorImpossible msg sz1 ix1 =
      error $ "Impossible <" ++ msg ++ ">: " ++ show sz1 ++ " and " ++ show ix1

prop_UnconsCons :: Index ix => ix -> Property
prop_UnconsCons ix = ix === uncurry consDim (unconsDim ix)

prop_UnsnocSnoc :: Index ix => ix -> Property
prop_UnsnocSnoc ix = ix === uncurry snocDim (unsnocDim ix)

prop_ToFromLinearIndex :: Index ix => SzIx ix -> Property
prop_ToFromLinearIndex (SzIx sz ix) =
  isSafeIndex sz ix ==> ix == fromLinearIndex sz (toLinearIndex sz ix)

prop_FromToLinearIndex :: Index ix => SzNE ix -> NonNegative Int -> Property
prop_FromToLinearIndex (SzNE sz) (NonNegative i) =
  totalElem sz >= i ==> i == toLinearIndex sz (fromLinearIndex sz i)

prop_CountElements :: Index ix => Int -> Sz ix -> Property
prop_CountElements thresh sz =
  totalElem sz < thresh ==> totalElem sz ==
  iter zeroIndex (unSz sz) (pureIndex 1) (<) 0 (const (+ 1))

prop_IterMonotonic :: Index ix => Int -> Sz ix -> Property
prop_IterMonotonic thresh sz =
  totalElem sz < thresh ==> fst $
  iter (liftIndex succ zeroIndex) (unSz sz) (pureIndex 1) (<) (True, zeroIndex) mono
  where
    mono curIx (prevMono, prevIx) =
      let isMono = prevMono && prevIx < curIx
       in isMono `seq` (isMono, curIx)

prop_IterMonotonicM :: Index ix => Int -> Sz ix -> Property
prop_IterMonotonicM thresh sz =
  totalElem sz < thresh ==> fst $
  runIdentity $ iterM (liftIndex succ zeroIndex) (unSz sz) (pureIndex 1) (<) (True, zeroIndex) mono
  where
    mono curIx (prevMono, prevIx) =
      let isMono = prevMono && prevIx < curIx
       in return $ isMono `seq` (isMono, curIx)


prop_IterMonotonicBackwards :: Index ix => Int -> Sz ix -> Property
prop_IterMonotonicBackwards thresh sz@(Sz szix) =
  totalElem sz < thresh ==> fst $
  iter (liftIndex pred szix) zeroIndex (pureIndex (-1)) (>=) (True, szix) mono
  where
    mono curIx (prevMono, prevIx) =
      let isMono = prevMono && prevIx > curIx
       in isMono `seq` (isMono, curIx)

prop_IterMonotonicBackwardsM :: Index ix => Int -> Sz ix -> Property
prop_IterMonotonicBackwardsM thresh sz@(Sz szix) =
  totalElem sz < thresh ==> fst $
  runIdentity $ iterM (liftIndex pred szix) zeroIndex (pureIndex (-1)) (>=) (True, szix) mono
  where
    mono curIx (prevMono, prevIx) =
      let isMono = prevMono && prevIx > curIx
       in return $ isMono `seq` (isMono, curIx)

prop_LiftLift2 :: Index ix => ix -> Int -> Bool
prop_LiftLift2 ix delta = liftIndex2 (+) ix (liftIndex (+delta) zeroIndex) ==
                            liftIndex (+delta) ix


prop_BorderRepairSafe :: Index ix => Border ix -> SzNE ix -> ix -> Property
prop_BorderRepairSafe border@(Fill defIx) (SzNE sz) ix =
  not (isSafeIndex sz ix) ==> handleBorderIndex border sz id ix == defIx
prop_BorderRepairSafe border (SzNE sz) ix =
  not (isSafeIndex sz ix) ==> isSafeIndex sz (handleBorderIndex border sz id ix)


prop_GetDropInsert :: Index ix => DimIx ix -> ix -> Property
prop_GetDropInsert (DimIx dim) ix =
  property $
  flip shouldReturn ix $ do
    i <- getDimM ix dim
    ixL <- dropDimM ix dim
    insertDimM ixL dim i

prop_PullOutInsert :: Index ix => DimIx ix -> ix -> Property
prop_PullOutInsert (DimIx dim) ix =
  property $
  flip shouldReturn ix $ do
    (i, ixL) <- pullOutDimM ix dim
    insertDimM ixL dim i

prop_getDimException :: (Typeable ix, Index ix) => Dim -> ix -> Property
prop_getDimException d ix =
  (d <= 0 || d > dimensions (Just ix)) ==>
  assertExceptionIO (== IndexDimensionException ix d) (getDimM ix d)

prop_setDimException :: (Typeable ix, Index ix) => Dim -> ix -> Int -> Property
prop_setDimException d ix i =
  (d <= 0 || d > dimensions (Just ix)) ==>
  assertExceptionIO (== IndexDimensionException ix d) (setDimM ix d i)

prop_PullOutDimException :: (Typeable ix, Index ix) => Dim -> ix -> Property
prop_PullOutDimException d ix =
  (d <= 0 || d > dimensions (Just ix)) ==>
  assertExceptionIO (== IndexDimensionException ix d) (pullOutDimM ix d)

prop_InsertDimException ::
     forall ix. (Typeable (Lower ix), Index ix)
  => Dim
  -> Lower ix
  -> Int
  -> Property
prop_InsertDimException d ix i =
  (d <= 0 || d > dimensions resIO) ==> assertExceptionIO (== IndexDimensionException ix d) resIO
  where
    resIO = insertDimM ix d i :: IO ix


prop_UnconsGetDrop :: (Index (Lower ix), Index ix) => ix -> Property
prop_UnconsGetDrop ix =
  property $
  flip shouldReturn (unconsDim ix) $ do
    i <- getDimM ix (dimensions (Just ix))
    ixL <- dropDimM ix (dimensions (Just ix))
    return (i, ixL)

prop_UnsnocGetDrop :: (Index (Lower ix), Index ix) => ix -> Property
prop_UnsnocGetDrop ix =
  property $
  flip shouldReturn (unsnocDim ix) $ do
    i <- getDimM ix 1
    ixL <- dropDimM ix 1
    return (ixL, i)

prop_SetAll :: Index ix => ix -> Property
prop_SetAll ix = property $ do
  replaceDims dims `shouldReturn` ix
  replaceDims (reverse dims) `shouldReturn` ix
  where
    replaceDims = foldM (\cix d -> getDimM ix d >>= setDimM cix d) zeroIndex
    dims = [1 .. dimensions (Just ix)] :: [Dim]


prop_SetGet :: Index ix => ix -> DimIx ix -> Int -> Property
prop_SetGet ix (DimIx dim) n = n === getDim' (setDim' ix dim n) dim



prop_BorderIx1 :: Positive Int -> Border Char -> Fun Ix1 Char -> SzNE Ix1 -> Ix1 -> Property
prop_BorderIx1 (Positive period) border getVal (SzNE sz) ix =
  if isSafeIndex sz ix
    then val === apply getVal ix
    else case border of
           Fill defVal -> val === defVal
           Wrap ->
             val ===
             handleBorderIndex
               border
               sz
               (apply getVal)
               (liftIndex2 (+) (liftIndex (* period) (unSz sz)) ix)
           Edge ->
             if ix < 0
               then val === apply getVal (liftIndex (max 0) ix)
               else val === apply getVal (liftIndex2 min (liftIndex (subtract 1) (unSz sz)) ix)
           Reflect ->
             val ===
             handleBorderIndex
               border
               sz
               (apply getVal)
               (liftIndex2 (+) (liftIndex (* (2 * signum ix * period)) (unSz sz)) ix)
           Continue ->
             val ===
             handleBorderIndex
               Reflect
               sz
               (apply getVal)
               (if ix < 0
                  then ix - 1
                  else ix + 1)
  where
    val = handleBorderIndex border sz (apply getVal) ix


prop_BinaryNumIx ::
  (Num ix, Index ix) => (forall n . Num n => n -> n -> n) -> ix -> ix -> Property
prop_BinaryNumIx f ix1 ix2 = zipWith f (ixToList ix1) (ixToList ix2) === ixToList (f ix1 ix2)

prop_UnaryNumIx ::
  (Num ix, Index ix) => (forall n . Num n => n -> n) -> ix -> Property
prop_UnaryNumIx f ix = map f (ixToList ix) === ixToList (f ix)

prop_BinaryNumSz ::
  (Num ix, Index ix) => (forall n . Num n => n -> n -> n) -> Sz ix -> Sz ix -> Property
prop_BinaryNumSz f sz1 sz2 =
  zipWith f' (ixToList (unSz sz1)) (ixToList (unSz sz2)) === ixToList (unSz (f sz1 sz2))
  where
    f' x y = max 0 (f x y)

prop_UnaryNumSz ::
  (Num ix, Index ix) => (forall n . Num n => n -> n) -> Sz ix -> Property
prop_UnaryNumSz f sz = map f' (ixToList (unSz sz)) === ixToList (unSz (f sz))
  where
    f' = max 0 . f



prop_IterLinearM :: Index ix => Sz ix -> NonNegative Int -> Positive Int -> Property
prop_IterLinearM sz (NonNegative start) (Positive increment) = property $ do
  xs <- iterLinearM sz start (totalElem sz) increment (<) [] $ \i ix acc -> do
    toLinearIndex sz ix `shouldBe` i
    pure (i:acc)
  reverse xs `shouldBe` [start, start + increment .. totalElem sz - 1]

prop_IterLinearM_ :: Index ix => Sz ix -> NonNegative Int -> Positive Int -> Property
prop_IterLinearM_ sz (NonNegative start) (Positive increment) = property $ do
  ref <- newIORef []
  iterLinearM_ sz start (totalElem sz) increment (<) $ \i ix -> do
    toLinearIndex sz ix `shouldBe` i
    modifyIORef' ref (i:)
  xs <- readIORef ref
  reverse xs `shouldBe` [start, start + increment .. totalElem sz - 1]

-----------
-- Specs --
-----------

specIx1 :: Spec
specIx1 = describe "Ix1" $ do
  ixSpec @Ix1
  ixNumSpec @Ix1
  it "Border" $ property prop_BorderIx1


ixSpec ::
     forall ix. (Typeable (Lower ix), Arbitrary (Lower ix), Typeable ix, Index ix, Arbitrary ix)
  => Spec
ixSpec = do
  describe "Safety" $ do
    it "IsSafeIndex" $ property $ prop_IsSafeIndex @ix
    it "RepairSafeIx" $ property $ prop_RepairSafeIx @ix
  describe "Lifting" $
    it "Lift/Lift2" $ property $ prop_LiftLift2 @ix
  describe "Linear" $ do
    it "ToFromLinearIndex" $ property $ prop_ToFromLinearIndex @ix
    it "FromToLinearIndex" $ property $ prop_FromToLinearIndex @ix
  describe "Iterator" $ do
    it "CountElements" $ property $ prop_CountElements @ix 2000000
    it "Monotonic" $ property $ prop_IterMonotonic @ix 2000000
    it "MonotonicBackwards" $ property $ prop_IterMonotonicBackwards @ix 2000000
    it "MonotonicM" $ property $ prop_IterMonotonicM @ix 2000000
    it "MonotonicBackwardsM" $ property $ prop_IterMonotonicBackwardsM @ix 2000000
  describe "Border" $
    it "BorderRepairSafe" $ property $ prop_BorderRepairSafe @ix
  describe "SetGetDrop" $ do
    it "SetAll" $ property $ prop_SetAll @ix
    it "SetGet" $ property $ prop_SetGet @ix
    it "GetDropInsert" $ property $ prop_GetDropInsert @ix
    it "PullOutInsert" $ property $ prop_PullOutInsert @ix
    it "UnconsCons" $ property $ prop_UnconsCons @ix
    it "UnsnocSnoc" $ property $ prop_UnsnocSnoc @ix
  describe "IndexDimensionException" $ do
    it "getDimException" $ property $ prop_getDimException @ix
    it "setDimException" $ property $ prop_setDimException @ix
    it "PullOutDimException" $ property $ prop_PullOutDimException @ix
    it "InsertDimException" $ property $ prop_InsertDimException @ix
  describe "Dimension" $ do
    it "GetInnerDimension" $ property $ \(ix :: ix) -> lastDim ix === getDimension ix Dim1
    it "GetOuterDimension" $ property $
      \(ix :: ix) -> headDim ix === getDimension ix (DimN :: Dimension (Dimensions ix))
    it "SetInnerDimension" $ property $
      \(ix :: ix) i -> snocDim (initDim ix) i === setDimension ix Dim1 i
    it "SetOuterDimension" $ property $
      \(ix :: ix) i -> consDim i (tailDim ix) === setDimension ix (DimN :: Dimension (Dimensions ix)) i
    it "DropInnerDimension" $ property $ \(ix :: ix) -> initDim ix === dropDimension ix Dim1
    it "DropOuterDimension" $ property $
      \(ix :: ix) -> tailDim ix === dropDimension ix (DimN :: Dimension (Dimensions ix))
    it "InsertInnerDimension" $ property $
      \(ixl :: Lower ix) i -> (snocDim ixl i :: ix) === insertDimension ixl Dim1 i
    it "InsertOuterDimension" $ property $
      \(ixl :: Lower ix) i -> (consDim i ixl :: ix) ===
                               insertDimension ixl (DimN :: Dimension (Dimensions ix)) i
    it "PullOutInnerDimension" $ property $
      \(ix :: ix) -> unsnocDim ix === uncurry (flip (,)) (pullOutDimension ix Dim1)
    it "PullInnerOuterDimension" $ property $
      \(ix :: ix) -> unconsDim ix ===
                               pullOutDimension ix (DimN :: Dimension (Dimensions ix))

  describe "NFData" $ do
    it "rnf" $ property $ \ (ix :: ix) -> rnf ix `shouldBe` ()
    it "throws exception" $ property $ \ (DimIx d :: DimIx ix) (ix :: ix) ->
      assertException (== ExpectedException) (setDim' ix d (throw ExpectedException))


ix2UpSpec ::
     forall ix. (Index ix, Index (Lower ix), Arbitrary ix, Arbitrary (Lower ix), Typeable (Lower ix))
  => Spec
ix2UpSpec =
  describe "Higher/Lower" $ do
    it "UnconsGetDrop" $ property $ prop_UnconsGetDrop @ix
    it "UnsnocGetDrop" $ property $ prop_UnsnocGetDrop @ix


-- | Spec that validates the Num instance for any `Index ix => ix`
ixNumSpec :: forall ix . (Typeable ix, Num ix, Index ix, Arbitrary ix) => Spec
ixNumSpec = do
  describe ("Num (" ++ showsTypeRep (typeRep (Proxy :: Proxy ix)) ")") $ do
    it "(+)" $ property $ prop_BinaryNumIx @ix (+)
    it "(-)" $ property $ prop_BinaryNumIx @ix (-)
    it "(*)" $ property $ prop_BinaryNumIx @ix (*)
    it "negate" $ property $ prop_UnaryNumIx @ix negate
    it "abs" $ property $ prop_UnaryNumIx @ix abs
    it "signum" $ property $ prop_UnaryNumIx @ix signum
    it "fromInteger" $ property $ \ (i :: Int) ->
      (fromIntegral i :: ix) === liftIndex (const i) zeroIndex
  describe "Constants" $ do
    it "zeroIndex" $ (zeroIndex :: ix) `shouldBe` 0
    it "oneIndex" $ (oneIndex :: ix) `shouldBe` 1

-- | Spec that validates the Num instance for any `Index ix => Sz ix`
szNumSpec :: forall ix . (Typeable ix, Num ix, Index ix, Arbitrary ix) => Spec
szNumSpec = do
  describe ("Num (" ++ showsTypeRep (typeRep (Proxy :: Proxy (Sz ix))) ")") $ do
    it "(+)" $ property $ prop_BinaryNumSz @ix (+)
    it "(-)" $ property $ prop_BinaryNumSz @ix (-)
    it "(*)" $ property $ prop_BinaryNumSz @ix (*)
    it "negate" $ property $ prop_UnaryNumSz @ix negate
    it "abs" $ property $ prop_UnaryNumSz @ix abs
    it "signum" $ property $ prop_UnaryNumSz @ix signum
    it "fromInteger" $ property $ \ (i :: Int) ->
      (fromIntegral i :: Sz ix) === SafeSz (pureIndex (max 0 i))
    it "fromIx" $ property $ \ (ix :: ix) -> unSz (Sz ix) === liftIndex (max 0) ix
  describe "Constants" $ do
    it "zeroSz" $ (zeroSz :: Sz ix) `shouldBe` 0
    it "oneSz" $ (oneSz :: Sz ix) `shouldBe` 1


prop_PullOutInsertSize :: Index ix => DimIx ix -> Sz ix -> Property
prop_PullOutInsertSize (DimIx dim) sz =
  either throw (sz ===) $ do
    (i, szL) <- pullOutSzM sz dim
    insertSzM szL dim i


szSpec ::
     forall ix. (Index ix, Arbitrary ix)
  => Spec
szSpec = do
  describe "Higher/Lower" $ do
    it "UnconsCons" $ property $ \ (sz :: Sz ix) -> sz === uncurry consSz (unconsSz sz)
    it "UnsnocSnoc" $ property $ \ (sz :: Sz ix) -> sz === uncurry snocSz (unsnocSz sz)
    it "PullOutInsert" $ property $ prop_PullOutInsertSize @ix
    it "SetSzInnerSnoc" $ property $
      \ (sz :: Sz ix) i -> setSzM sz 1 i `shouldReturn` snocSz (fst $ unsnocSz sz) i
  describe "Number of Elements" $ do
    it "TotalElem" $ property $
      \(sz :: Sz ix) -> totalElem sz === foldlIndex (*) 1 (unSz sz)
    it "IsNonEmpty" $ property $
      \(sz :: Sz ix) -> isNonEmpty sz === foldlIndex (\a x -> a && x > 0) True (unSz sz)
  describe "Iterators" $ do
    it "IterLinearM" $ property $ prop_IterLinearM @ix
    it "IterLinearM_" $ property $ prop_IterLinearM_ @ix
