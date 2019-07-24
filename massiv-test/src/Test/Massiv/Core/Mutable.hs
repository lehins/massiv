{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Massiv.Core.Mutable
  ( -- * Spec for Mutable instance
    unsafeMutableSpec
  , prop_UnsafeNewMsize
  , prop_UnsafeThawFreeze
  , prop_UnsafeInitializeNew
  , prop_UnsafeArrayLinearCopy
  -- ** Properties that aren't valid for boxed
  , unsafeMutableUnboxedSpec
  , prop_UnsafeInitialize
  ) where

import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe
import Test.Massiv.Core.Common
import Test.Massiv.Utils



prop_UnsafeNewMsize ::
     forall r ix e.
     (Arbitrary ix, Mutable r ix e)
  => Property
prop_UnsafeNewMsize = property $ \ sz -> do
  marr :: MArray RealWorld r ix e <- unsafeNew sz
  sz `shouldBe` msize marr

prop_UnsafeNewLinearWriteRead ::
     forall r ix e.
     (Eq e, Show e, Mutable r ix e, Arbitrary ix, Arbitrary e)
  => Property
prop_UnsafeNewLinearWriteRead = property $ \ (SzIx sz ix) e1 e2 -> do
  marr :: MArray RealWorld r ix e <- unsafeNew sz
  let i = toLinearIndex sz ix
  unsafeLinearWrite marr i e1
  unsafeLinearRead marr i `shouldReturn` e1
  unsafeLinearModify marr (\ !_ -> pure e2) i `shouldReturn` e1
  unsafeLinearRead marr i `shouldReturn` e2


prop_UnsafeThawFreeze ::
     forall r ix e.
     (Eq (Array r ix e), Show (Array r ix e), Mutable r ix e)
  => Array r ix e -> Property
prop_UnsafeThawFreeze arr = arr === runST (unsafeFreeze (getComp arr) =<< unsafeThaw arr)


prop_UnsafeInitializeNew ::
     forall r ix e.
     ( Eq (Array r ix e)
     , Show (Array r ix e)
     , Show e
     , Arbitrary e
     , Arbitrary ix
     , Mutable r ix e
     , Construct r ix e
     )
  => Property
prop_UnsafeInitializeNew =
  property $ \comp sz e ->
    (A.replicate comp sz e :: Array r ix e) ===
    runST (unsafeFreeze comp =<< initializeNew (Just e) sz)

prop_UnsafeInitialize ::
     forall r ix e.
     ( Eq (Array r ix e)
     , Show (Array r ix e)
     , Arbitrary ix
     , Mutable r ix e
     )
  => Property
prop_UnsafeInitialize =
  property $ \comp sz ->
    runST $ do
      marr1 :: MArray s r ix e <- unsafeNew sz
      initialize marr1
      marr2 :: MArray s r ix e <- initializeNew Nothing sz
      (===) <$> unsafeFreeze comp marr1 <*> unsafeFreeze comp marr2


prop_UnsafeLinearCopy ::
     forall r ix e. (Eq (Array r ix e), Show (Array r ix e), Mutable r ix e)
  => Array r ix e
  -> Property
prop_UnsafeLinearCopy arr =
  (arr, arr) ===
  runST
    (do let sz = size arr
        marrs <- thawS arr
        marrd <- unsafeNew sz
        unsafeLinearCopy marrs 0 marrd 0 (Sz (totalElem sz))
        arrd <- unsafeFreeze (getComp arr) marrd
        arrs <- unsafeFreeze (getComp arr) marrs
        pure (arrs, arrd))

prop_UnsafeLinearCopyPart ::
     forall r ix e.
     ( Eq (Array r ix e)
     , Show (Array r ix e)
     , Eq (Array (R r) Ix1 e)
     , Show (Array (R r) Ix1 e)
     , Mutable r ix e
     , Mutable r Ix1 e
     , Extract r Ix1 e
     , Resize r ix
     )
  => ArrIx r ix e
  -> NonNegative Ix1
  -> Ix1
  -> Property
prop_UnsafeLinearCopyPart (ArrIx arr ix) (NonNegative delta) toOffset =
  arr === arrs .&&. extract' i k (flatten arr) === extract' j k arrd
  where
    sz = size arr
    i = toLinearIndex sz ix
    j = max 0 (i + toOffset)
    k = Sz (totalElem sz - i - delta)
    sz' = Sz (j + unSz k)
    (arrs, arrd) =
      runST $ do
        marrs <- thawS arr -- make sure that the source does not get modified
        marrd <- unsafeNew sz'
        unsafeLinearCopy marrs i marrd j k
        (,) <$> unsafeFreeze (getComp arr) marrs <*> unsafeFreeze (getComp arr) marrd


prop_UnsafeArrayLinearCopy ::
     forall r ix e. (Eq (Array r ix e), Show (Array r ix e), Mutable r ix e)
  => Array r ix e
  -> Property
prop_UnsafeArrayLinearCopy arr =
  arr ===
  runST
    (do let sz = size arr
        marr <- unsafeNew sz
        unsafeArrayLinearCopy arr 0 marr 0 (Sz (totalElem sz))
        unsafeFreeze (getComp arr) marr)


prop_UnsafeArrayLinearCopyPart ::
     forall r ix e.
     ( Eq (Array (R r) Ix1 e)
     , Show (Array (R r) Ix1 e)
     , Mutable r ix e
     , Mutable r Ix1 e
     , Extract r Ix1 e
     , Resize r ix
     )
  => ArrIx r ix e
  -> NonNegative Ix1
  -> Ix1
  -> Property
prop_UnsafeArrayLinearCopyPart (ArrIx arr ix) (NonNegative delta) toOffset =
  extract' i k (flatten arr) === extract' j k arr'
  where
    sz = size arr
    i = toLinearIndex sz ix
    j = max 0 (i + toOffset)
    k = Sz (totalElem sz - i - delta)
    sz' = Sz (j + unSz k)
    arr' =
      runST $ do
        marr <- unsafeNew sz'
        unsafeArrayLinearCopy arr i marr j k
        unsafeFreeze (getComp arr) marr

prop_UnsafeLinearSet ::
     forall r ix e.
     ( Eq (Array (R r) Ix1 e)
     , Show (Array (R r) Ix1 e)
     , Mutable r ix e
     , Construct r ix e
     , Extract r Ix1 e
     , Resize r ix
     )
  => Comp
  -> SzIx ix
  -> NonNegative Ix1
  -> e
  -> Property
prop_UnsafeLinearSet comp (SzIx sz ix) (NonNegative delta) e =
  extract' i k (flatten (A.replicate Seq sz e :: Array r ix e)) ===
  extract' i k (flatten (arrd :: Array r ix e))
  where
    i = toLinearIndex sz ix
    k = Sz (totalElem sz - i - delta)
    arrd =
      runST $ do
        marrd <- unsafeNew sz
        unsafeLinearSet marrd i (unSz k) e
        unsafeFreeze comp marrd

prop_UnsafeLinearShrink ::
     forall r ix e.
     ( Eq (Array (R r) Ix1 e)
     , Show (Array (R r) Ix1 e)
     , Mutable r ix e
     , Extract r Ix1 e
     , Resize r ix
     )
  => ArrIx r ix e
  -> Property
prop_UnsafeLinearShrink (ArrIx arr ix) =
  extract' 0 k (flatten arr) === extract' 0 k (flatten arr')
  where
    sz = size arr
    sz' = Sz (liftIndex2 (-) (unSz sz) ix)
    k = Sz (totalElem sz')
    arr' =
      runST $ do
        marr <- thawS arr
        marr' <- unsafeLinearShrink marr sz'
        unsafeFreeze (getComp arr) marr'

prop_UnsafeLinearGrow ::
     forall r ix e.
     ( Eq (Array r ix e)
     , Show (Array r ix e)
     , Eq (Array (R r) Ix1 e)
     , Show (Array (R r) Ix1 e)
     , Mutable r ix e
     , Extract r Ix1 e
     , Resize r ix
     )
  => ArrIx r ix e
  -> e
  -> Property
prop_UnsafeLinearGrow (ArrIx arr ix) e =
  extract' 0 k (flatten arr) === extract' 0 k (flatten arrGrown) .&&.
  arrCopied === arrGrown
  where
    sz = size arr
    sz' = Sz (liftIndex2 (+) (unSz sz) ix)
    k = Sz (totalElem sz)
    (arrCopied, arrGrown) =
      runST $ do
        marrCopied <- unsafeNew sz'
        unsafeArrayLinearCopy arr 0 marrCopied 0 k
        marr <- thawS arr
        marrGrown <- unsafeLinearGrow marr sz'
        when (sz' /= sz) $ do
          unsafeLinearSet marrGrown (totalElem sz) (totalElem sz' - totalElem sz) e
          unsafeLinearSet marrCopied (totalElem sz) (totalElem sz' - totalElem sz) e
        (,) <$> unsafeFreeze (getComp arr) marrCopied <*> unsafeFreeze (getComp arr) marrGrown


unsafeMutableSpec ::
     forall r ix e.
     ( Eq (Array (R r) Ix1 e)
     , Show (Array (R r) Ix1 e)
     , Eq (Array r ix e)
     , Show (Array r ix e)
     , Mutable r ix e
     , Mutable r Ix1 e
     , Construct r ix e
     , Show e
     , Eq e
     , Arbitrary e
     , Arbitrary ix
     , Typeable e
     , Typeable ix
     , Extract r Ix1 e
     , Resize r ix
     )
  => Spec
unsafeMutableSpec =
  describe ("Mutable (" ++ showsArrayType @r @ix @e ") (Unsafe)") $ do
    it "UnsafeNewMsize" $ prop_UnsafeNewMsize @r @ix @e
    it "UnsafeNewLinearWriteRead" $ prop_UnsafeNewLinearWriteRead @r @ix @e
    it "UnsafeThawFreeze" $ property $ prop_UnsafeThawFreeze @r @ix @e
    it "UnsafeInitializeNew" $ prop_UnsafeInitializeNew @r @ix @e
    it "UnsafeLinearSet" $ property $ prop_UnsafeLinearSet @r @ix @e
    it "UnsafeLinearCopy" $ property $ prop_UnsafeLinearCopy @r @ix @e
    it "UnsafeLinearCopyPart" $ property $ prop_UnsafeLinearCopyPart @r @ix @e
    it "UnsafeArrayLinearCopy" $ property $ prop_UnsafeArrayLinearCopy @r @ix @e
    it "UnsafeArrayLinearCopyPart" $ property $ prop_UnsafeArrayLinearCopyPart @r @ix @e
    it "UnsafeLinearShrink" $ property $ prop_UnsafeLinearShrink @r @ix @e
    it "UnsafeLinearGrow" $ property $ prop_UnsafeLinearGrow @r @ix @e

unsafeMutableUnboxedSpec ::
     forall r ix e.
     (Typeable e, Typeable ix, Eq (Array r ix e), Show (Array r ix e), Arbitrary ix, Mutable r ix e)
  => Spec
unsafeMutableUnboxedSpec =
  describe ("Mutable Unboxed (" ++ showsArrayType @r @ix @e ") (Unsafe)") $
    it "UnsafeInitialize" $ prop_UnsafeInitialize @r @ix @e
