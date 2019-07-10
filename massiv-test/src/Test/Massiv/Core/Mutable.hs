{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Massiv.Core.Mutable
  ( mutableSpec
  , prop_UnsafeNewMsize
  , prop_UnsafeThawFreeze
  , prop_UnsafeInititalizeNew
  , prop_UnsafeArrayLinearCopy
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


prop_UnsafeThawFreeze ::
     forall r ix e.
     (Eq (Array r ix e), Show (Array r ix e), Mutable r ix e)
  => Array r ix e -> Property
prop_UnsafeThawFreeze arr = arr === runST (unsafeFreeze (getComp arr) =<< unsafeThaw arr)


prop_UnsafeInititalizeNew ::
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
prop_UnsafeInititalizeNew =
  property $ \comp sz e ->
    (A.replicate comp sz e :: Array r ix e) ===
    runST (unsafeFreeze comp =<< initializeNew (Just e) sz)


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
     , Eq (Array (EltRepr r Ix1) Ix1 e)
     , Show (Array (EltRepr r Ix1) Ix1 e)
     , Mutable r ix e
     , Mutable r Ix1 e
     , Extract r Ix1 e
     , Resize r ix
     )
  => ArrIx r ix e
  -> NonNegative Int
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
     ( Eq (Array (EltRepr r Ix1) Ix1 e)
     , Show (Array (EltRepr r Ix1) Ix1 e)
     , Mutable r ix e
     , Mutable r Ix1 e
     , Extract r Ix1 e
     , Resize r ix
     )
  => ArrIx r ix e
  -> NonNegative Int
  -> Int
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




mutableSpec ::
     forall r ix e.
     ( Eq (Array (EltRepr r Ix1) Ix1 e)
     , Show (Array (EltRepr r Ix1) Ix1 e)
     , Eq (Array r ix e)
     , Show (Array r ix e)
     , Mutable r ix e
     , Mutable r Ix1 e
     , Construct r ix e
     , Show e
     , Arbitrary e
     , Arbitrary ix
     , Extract r Ix1 e
     , Resize r ix
     )
  => Spec
mutableSpec = do
  describe "Mutable (Unsafe)" $ do
    it "UnsafeNewMsize" $ prop_UnsafeNewMsize @r @ix @e
    it "UnsafeThawFreeze" $ property $ prop_UnsafeThawFreeze @r @ix @e
    it "UnsafeInititalizeNew" $ prop_UnsafeInititalizeNew @r @ix @e
    it "UnsafeLinearCopy" $ property $ prop_UnsafeLinearCopy @r @ix @e
    it "UnsafeLinearCopyPart" $ property $ prop_UnsafeLinearCopyPart @r @ix @e
    it "UnsafeArrayLinearCopy" $ property $ prop_UnsafeArrayLinearCopy @r @ix @e
    it "UnsafeArrayLinearCopyPart" $ property $ prop_UnsafeArrayLinearCopyPart @r @ix @e
