{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Data.Massiv.Array.MutableSpec (spec) where

import Control.Concurrent.Async
import Control.Monad (when)
import Control.Monad.ST
import Data.Functor.Identity
import Data.List as L
import Data.Massiv.Array.Mutable.Atomic
import Data.Massiv.Array.Unsafe
import Data.Massiv.CoreArbitrary as A
import Data.Proxy


prop_MapMapM :: (Show (Array r ix Int), Eq (Array r ix Int), Mutable r ix Int) =>
                r -> Proxy ix -> Fun Int Int -> ArrTiny D ix Int -> Property
prop_MapMapM r _ f (ArrTiny arr) =
  computeAs r (A.map (apply f) arr) === runIdentity (A.mapMR r (return . apply f) arr)

prop_iMapiMapM :: (Show (Array r ix Int), Eq (Array r ix Int), Mutable r ix Int) =>
                r -> Proxy ix -> Fun (ix, Int) Int -> ArrTiny D ix Int -> Property
prop_iMapiMapM r _ f (ArrTiny arr) =
  computeAs r (A.imap (curry (apply f)) arr) ===
  runIdentity (A.imapMR r (\ix e -> return $ apply f (ix, e)) arr)


prop_generateMakeST ::
     (Show (Array r ix Int), Eq (Array r ix Int), Mutable r ix Int)
  => r
  -> Proxy ix
  -> Arr r ix Int
  -> Property
prop_generateMakeST _ _ (Arr arr) =
  arr === runST (generateArrayS (getComp arr) (size arr) (return . evaluate' arr))

prop_generateMakeIO :: (Show (Array r ix Int), Eq (Array r ix Int), Mutable r ix Int) =>
                             r -> Proxy ix -> Arr r ix Int -> Property
prop_generateMakeIO _ _ (Arr arr) = monadicIO $ do
  arr' <- run $ generateArray (getComp arr) (size arr) (evaluateM arr)
  return (arr === arr')

prop_shrinkIO ::
     ( Mutable r ix Int
     , Resize r ix
     , Source r Ix1 Int
     )
  => r
  -> Proxy ix
  -> ArrIx r ix Int
  -> Property
prop_shrinkIO _ _ (ArrIx arr ix) =
  monadicIO $
  run $ do
    marr <- thaw arr
    sarr <- unsafeFreeze (getComp arr) =<< unsafeLinearShrink marr (Sz ix)
    pure (A.foldlS (.&&.) (property True) $ A.zipWith (===) (flatten arr) (flatten sarr))

prop_growShrinkIO ::
     ( Show (Array r ix Int)
     , Eq (Array r ix Int)
     , Mutable r ix Int
     , Extract r ix Int
     , Num ix
     , Load (EltRepr r ix) ix Int
     )
  => r
  -> Proxy ix
  -> Arr r ix Int
  -> NonNegative Int
  -> Property
prop_growShrinkIO _ _ (Arr arr) (NonNegative delta) =
  monadicIO $
  run $ do
    marr <- thaw arr
    let sz = size arr
    k <- getDimM (unSz sz) (dimensions sz)
    -- increase the outer most dimension, just so the structure doesn't change
    newSz <- Sz <$> setDimM (unSz sz) (dimensions sz) (k + delta)
    gMarr <- unsafeLinearGrow marr newSz
    -- Make sure we can write into the newly allocated area
    when (delta > 0) $ write' gMarr (unSz newSz - 1) delta
    garr <- compute . extract' 0 sz <$> unsafeFreeze (getComp arr) gMarr
    sarr <- freeze (getComp arr) =<< unsafeLinearShrink gMarr sz
    pure (garr === arr .&&. sarr === arr)

prop_atomicModifyIntArrayMany :: ArrIx P Ix2 Int -> Array B Ix1 Int -> Property
prop_atomicModifyIntArrayMany (ArrIx arr ix) barr =
  monadicIO $ do
    xs <-
      run $ do
        marr <- thaw arr
        mbarr' <- mapConcurrently (atomicModifyIntArray marr ix . const) barr
        x <- A.read' marr ix
        let xs = maybe (error "atomicModifyIntArray read'") toList (Prelude.sequenceA mbarr')
        pure (x : xs)
    return (L.sort (index' arr ix : toList barr) === L.sort xs)



prop_atomicReadIntArrayMany :: Array P Ix2 Int -> Array B Ix1 Ix2 -> Property
prop_atomicReadIntArrayMany arr bix =
  monadicIO $
  run $ do
    marr <- thaw arr
    as :: Array N Ix1 (Maybe Int) <- forM bix (A.read marr)
    as' <- forM bix (atomicReadIntArray marr)
    pure (as === as')


prop_atomicWriteIntArrayMany :: Array P Ix2 Int -> Array B Ix1 Ix2 -> Fun Ix2 Int -> Property
prop_atomicWriteIntArrayMany arr bix f =
  monadicIO $
  run $ do
    marr <- thaw arr
    marr' <- unsafeThaw arr
    bs :: Array N Ix1 Bool <- forM bix (\ix -> write marr ix (apply f ix))
    bs' <- forM bix (\ix -> atomicWriteIntArray marr' ix (apply f ix))
    arrRes <- unsafeFreeze (getComp arr) marr
    arrRes' <- unsafeFreeze (getComp arr) marr'
    pure (bs === bs' .&&. arrRes === arrRes')



prop_unfoldrList :: Sz1 -> Fun Word (Int, Word) -> Word -> Property
prop_unfoldrList sz1 f i =
  conjoin $
  L.zipWith
    (===)
    (A.toList (runST $ unfoldrPrimM_ @P Seq sz1 (pure . apply f) i))
    (L.unfoldr (Just . apply f) i)

prop_unfoldrReverseUnfoldl :: Sz1 -> Fun Word (Int, Word) -> Word -> Property
prop_unfoldrReverseUnfoldl sz1 f i =
  runST (unfoldrPrimM_ @P Seq sz1 (pure . apply f) i) ===
  rev (runST (unfoldlPrimM_ @P Seq sz1 (pure . swapTuple . apply f) i))
    where swapTuple (x, y) = (y, x)
          rev a = computeAs P $ backpermute' sz1 (\ix1 -> unSz sz1 - ix1 - 1) a



mutableSpec ::
     ( Show r
     , Show (Array r Ix1 Int)
     , Show (Array r Ix2 Int)
     , Show (Array r Ix3 Int)
     , Eq (Array r Ix1 Int)
     , Eq (Array r Ix2 Int)
     , Eq (Array r Ix3 Int)
     , Mutable r Ix1 Int
     , Mutable r Ix2 Int
     , Mutable r Ix3 Int
     , Construct r Ix1 Int
     , Construct r Ix2 Int
     , Construct r Ix3 Int
     , Extract r Ix1 Int
     , Extract r Ix2 Int
     , Extract r Ix3 Int
     , Resize r Ix1
     , Resize r Ix2
     , Resize r Ix3
     , Load (EltRepr r Ix1) Ix1 Int
     , Load (EltRepr r Ix2) Ix2 Int
     , Load (EltRepr r Ix3) Ix3 Int
     )
  => r
  -> SpecWith ()
mutableSpec r =
  describe (show r) $ do
    describe "map == mapM" $ do
      it "Ix1" $ property $ prop_MapMapM r (Proxy :: Proxy Ix1)
      it "Ix2" $ property $ prop_MapMapM r (Proxy :: Proxy Ix2)
      it "Ix3" $ property $ prop_MapMapM r (Proxy :: Proxy Ix3)
    describe "imap == imapM" $ do
      it "Ix1" $ property $ prop_iMapiMapM r (Proxy :: Proxy Ix1)
      it "Ix2T" $ property $ prop_iMapiMapM r (Proxy :: Proxy Ix2)
      it "Ix3T" $ property $ prop_iMapiMapM r (Proxy :: Proxy Ix3)
    describe "makeArray == generateArrayST" $ do
      it "Ix1" $ property $ prop_generateMakeST r (Proxy :: Proxy Ix1)
      it "Ix2" $ property $ prop_generateMakeST r (Proxy :: Proxy Ix2)
      it "Ix3" $ property $ prop_generateMakeST r (Proxy :: Proxy Ix3)
    describe "makeArray == generateArrayIO" $ do
      it "Ix1" $ property $ prop_generateMakeIO r (Proxy :: Proxy Ix1)
      it "Ix2" $ property $ prop_generateMakeIO r (Proxy :: Proxy Ix2)
      it "Ix3" $ property $ prop_generateMakeIO r (Proxy :: Proxy Ix3)
    describe "shrink" $ do
      it "Ix1" $ property $ prop_shrinkIO r (Proxy :: Proxy Ix1)
      it "Ix2" $ property $ prop_shrinkIO r (Proxy :: Proxy Ix2)
      it "Ix3" $ property $ prop_shrinkIO r (Proxy :: Proxy Ix3)
    describe "grow+shrink" $ do
      it "Ix1" $ property $ prop_growShrinkIO r (Proxy :: Proxy Ix1)
      it "Ix2" $ property $ prop_growShrinkIO r (Proxy :: Proxy Ix2)
      it "Ix3" $ property $ prop_growShrinkIO r (Proxy :: Proxy Ix3)

generateSpec :: Spec
generateSpec = do
  mutableSpec P
  mutableSpec S
  mutableSpec U
  mutableSpec B
  mutableSpec N


spec :: Spec
spec = do
  describe "GenerateM" generateSpec
  describe "AtomicIntArraySpec" $ do
    it "atomicReadIntArrayMany" $ property prop_atomicReadIntArrayMany
    it "atomicWriteIntArrayMany" $ property prop_atomicWriteIntArrayMany
    it "atomicModifyIntArrayMany" $ property prop_atomicModifyIntArrayMany
  describe "Unfolding" $ do
    it "unfoldrList" $ property prop_unfoldrList
    it "unfoldrReverseUnfoldl" $ property prop_unfoldrReverseUnfoldl
