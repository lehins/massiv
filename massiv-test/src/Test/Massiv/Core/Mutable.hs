{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Massiv.Core.Mutable (
  -- * Spec for Mutable instance
  unsafeMutableSpec,
  prop_UnsafeNewMsize,
  prop_UnsafeThawFreeze,
  prop_UnsafeInitializeNew,
  prop_UnsafeArrayLinearCopy,

  -- ** Properties that aren't valid for boxed
  unsafeMutableUnboxedSpec,
  prop_UnsafeInitialize,
) where

import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe
import Test.Massiv.Core.Common
import Test.Massiv.Utils

prop_UnsafeNewMsize
  :: forall r ix e
   . (Arbitrary ix, Index ix, Manifest r e)
  => Property
prop_UnsafeNewMsize = property $ \sz -> do
  marr :: MArray RealWorld r ix e <- unsafeNew sz
  sz `shouldBe` sizeOfMArray marr

prop_UnsafeNewLinearWriteRead
  :: forall r ix e
   . (Eq e, Show e, Manifest r e, Index ix, Arbitrary ix, Arbitrary e)
  => Property
prop_UnsafeNewLinearWriteRead = property $ \(SzIx sz ix) e1 e2 -> do
  marr :: MArray RealWorld r ix e <- unsafeNew sz
  let i = toLinearIndex sz ix
  unsafeLinearWrite marr i e1
  unsafeLinearRead marr i `shouldReturn` e1
  unsafeLinearModify marr (\ !_ -> pure e2) i `shouldReturn` e1
  unsafeLinearRead marr i `shouldReturn` e2

prop_UnsafeThawFreeze
  :: forall r ix e
   . (Eq (Array r ix e), Show (Array r ix e), Index ix, Manifest r e)
  => Array r ix e
  -> Property
prop_UnsafeThawFreeze arr = arr === runST (unsafeFreeze (getComp arr) =<< unsafeThaw arr)

prop_UnsafeInitializeNew
  :: forall r ix e
   . ( Eq (Array r ix e)
     , Show (Array r ix e)
     , Show e
     , Arbitrary e
     , Arbitrary ix
     , Index ix
     , Manifest r e
     )
  => Property
prop_UnsafeInitializeNew =
  property $ \comp sz e ->
    (compute (A.replicate comp sz e :: Array DL ix e) :: Array r ix e)
      === runST (unsafeFreeze comp =<< initializeNew (Just e) sz)

prop_UnsafeInitialize
  :: forall r ix e
   . ( Eq (Array r ix e)
     , Show (Array r ix e)
     , Arbitrary ix
     , Index ix
     , Manifest r e
     )
  => Property
prop_UnsafeInitialize =
  property $ \comp sz ->
    runST $ do
      marr1 :: MArray s r ix e <- unsafeNew sz
      initialize marr1
      marr2 :: MArray s r ix e <- initializeNew Nothing sz
      (===) <$> unsafeFreeze comp marr1 <*> unsafeFreeze comp marr2

prop_UnsafeLinearCopy
  :: forall r ix e
   . (Eq (Array r ix e), Show (Array r ix e), Index ix, Manifest r e)
  => Array r ix e
  -> Property
prop_UnsafeLinearCopy arr =
  (arr, arr)
    === runST
      ( do
          let sz = size arr
          marrs <- thawS arr
          marrd <- unsafeNew sz
          unsafeLinearCopy marrs 0 marrd 0 (Sz (totalElem sz))
          arrd <- unsafeFreeze (getComp arr) marrd
          arrs <- unsafeFreeze (getComp arr) marrs
          pure (arrs, arrd)
      )

prop_UnsafeLinearCopyPart
  :: forall r ix e
   . ( Eq (Vector r e)
     , Show (Vector r e)
     , Eq (Array r ix e)
     , Show (Array r ix e)
     , Manifest r e
     , Index ix
     )
  => ArrIx r ix e
  -> NonNegative Ix1
  -> Ix1
  -> Property
prop_UnsafeLinearCopyPart (ArrIx arr ix) (NonNegative delta) toOffset =
  arr === arrs .&&. slice' i k (flatten arr) === slice' j k arrd
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

prop_UnsafeArrayLinearCopy
  :: forall r ix e
   . (Eq (Array r ix e), Show (Array r ix e), Index ix, Manifest r e)
  => Array r ix e
  -> Property
prop_UnsafeArrayLinearCopy arr =
  arr
    === runST
      ( do
          let sz = size arr
          marr <- unsafeNew sz
          unsafeArrayLinearCopy arr 0 marr 0 (Sz (totalElem sz))
          unsafeFreeze (getComp arr) marr
      )

prop_UnsafeArrayLinearCopyPart
  :: forall r ix e
   . (Eq (Vector r e), Show (Vector r e), Index ix, Manifest r e)
  => ArrIx r ix e
  -> NonNegative Ix1
  -> Ix1
  -> Property
prop_UnsafeArrayLinearCopyPart (ArrIx arr ix) (NonNegative delta) toOffset =
  slice' i k (flatten arr) === slice' j k arr'
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

prop_UnsafeLinearSet
  :: forall r ix e
   . ( Eq (Vector r e)
     , Show (Vector r e)
     , Index ix
     , Manifest r e
     )
  => Comp
  -> SzIx ix
  -> NonNegative Ix1
  -> e
  -> Property
prop_UnsafeLinearSet comp (SzIx sz ix) (NonNegative delta) e =
  compute (A.replicate Seq k e :: Array DL Ix1 e)
    === slice' i k (flatten (arrd :: Array r ix e))
  where
    i = toLinearIndex sz ix
    k = Sz (totalElem sz - i - delta)
    arrd =
      runST $ do
        marrd <- unsafeNew sz
        unsafeLinearSet marrd i k e
        unsafeFreeze comp marrd

prop_UnsafeLinearShrink
  :: forall r ix e
   . ( Eq (Vector r e)
     , Show (Vector r e)
     , Manifest r e
     , Index ix
     )
  => ArrIx r ix e
  -> Property
prop_UnsafeLinearShrink (ArrIx arr ix) =
  slice' 0 k (flatten arr) === slice' 0 k (flatten arr')
  where
    sz = size arr
    sz' = Sz (liftIndex2 (-) (unSz sz) ix)
    k = Sz (totalElem sz')
    arr' =
      runST $ do
        marr <- thawS arr
        marr' <- unsafeLinearShrink marr sz'
        unsafeFreeze (getComp arr) marr'

prop_UnsafeLinearGrow
  :: forall r ix e
   . ( Eq (Array r ix e)
     , Show (Array r ix e)
     , Eq (Vector r e)
     , Show (Vector r e)
     , Manifest r e
     , Index ix
     )
  => ArrIx r ix e
  -> e
  -> Property
prop_UnsafeLinearGrow (ArrIx arr ix) e =
  slice' 0 k (flatten arr)
    === slice' 0 k (flatten arrGrown)
    .&&. arrCopied
    === arrGrown
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
          unsafeLinearSet marrGrown (totalElem sz) (Sz (totalElem sz' - totalElem sz)) e
          unsafeLinearSet marrCopied (totalElem sz) (Sz (totalElem sz' - totalElem sz)) e
        (,) <$> unsafeFreeze (getComp arr) marrCopied <*> unsafeFreeze (getComp arr) marrGrown

prop_UnsafeLinearSliceMArray
  :: forall r ix e
   . (HasCallStack, Index ix, Manifest r e, Eq (Vector r e), Show (Vector r e))
  => Array r ix e
  -> Property
prop_UnsafeLinearSliceMArray arr =
  forAll genLinearRegion $ \(i, k) ->
    propIO $ do
      marr <- thawS arr
      unsafeFreeze Seq (unsafeLinearSliceMArray i k marr) `shouldReturn` unsafeLinearSlice i k arr
  where
    n = totalElem (size arr)
    genLinearRegion = do
      k <- chooseInt (0, n)
      i <- chooseInt (0, n - k)
      pure (i, Sz k)

unsafeMutableSpec
  :: forall r ix e
   . ( Eq (Vector r e)
     , Show (Vector r e)
     , Eq (Array r ix e)
     , Show (Array r ix e)
     , Manifest r e
     , Show e
     , Eq e
     , Load r ix e
     , Arbitrary (Array r ix e)
     , Arbitrary e
     , Arbitrary ix
     , Typeable e
     )
  => Spec
unsafeMutableSpec =
  describe ("Manifest (" ++ showsArrayType @r @ix @e ") (Unsafe)") $ do
    prop "UnsafeNewMsize" $ prop_UnsafeNewMsize @r @ix @e
    prop "UnsafeNewLinearWriteRead" $ prop_UnsafeNewLinearWriteRead @r @ix @e
    prop "UnsafeThawFreeze" $ prop_UnsafeThawFreeze @r @ix @e
    prop "UnsafeInitializeNew" $ prop_UnsafeInitializeNew @r @ix @e
    prop "UnsafeLinearSet" $ prop_UnsafeLinearSet @r @ix @e
    prop "UnsafeLinearCopy" $ prop_UnsafeLinearCopy @r @ix @e
    prop "UnsafeLinearCopyPart" $ prop_UnsafeLinearCopyPart @r @ix @e
    prop "UnsafeArrayLinearCopy" $ prop_UnsafeArrayLinearCopy @r @ix @e
    prop "UnsafeArrayLinearCopyPart" $ prop_UnsafeArrayLinearCopyPart @r @ix @e
    prop "UnsafeLinearShrink" $ prop_UnsafeLinearShrink @r @ix @e
    prop "UnsafeLinearGrow" $ prop_UnsafeLinearGrow @r @ix @e
    prop "UnsafeLinearSliceMArray" $ prop_UnsafeLinearSliceMArray @r @ix @e

unsafeMutableUnboxedSpec
  :: forall r ix e
   . ( Typeable e
     , Eq (Array r ix e)
     , Show (Array r ix e)
     , Index ix
     , Arbitrary ix
     , Manifest r e
     )
  => Spec
unsafeMutableUnboxedSpec =
  describe ("Manifest Unboxed (" ++ showsArrayType @r @ix @e ") (Unsafe)") $
    it "UnsafeInitialize" $
      prop_UnsafeInitialize @r @ix @e
