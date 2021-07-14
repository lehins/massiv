{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Massiv.Array.Mutable
  ( -- * Spec for safe Mutable instance
    mutableSpec
  , prop_GenerateArray
  , prop_iMapiMapM
  , prop_Shrink
  , prop_GrowShrink
  , prop_unfoldrList
  , prop_unfoldrReverseUnfoldl
  , prop_toStreamArrayMutable
  -- * Atomic ops spec
  , atomicIntSpec
  ) where

import Control.Scheduler
import Data.Bits
import Data.Functor.Identity
import Data.List as L
import Data.Massiv.Array as A
import Data.Massiv.Array.Mutable.Atomic
import Data.Massiv.Array.Unsafe
import qualified Data.Massiv.Vector.Stream as S
import Test.Massiv.Core.Common
import Test.Massiv.Utils as T
import UnliftIO.Async


-- prop_MapMapM :: forall r ix(Show (Array r ix Word), Eq (Array r ix Word), Mutable r ix) =>
--                 Fun Word Word -> ArrTiny D ix Word -> Property
-- prop_MapMapM r _ f (ArrTiny arr) =
--   computeAs r (A.map (apply f) arr) === runIdentity (A.mapMR r (return . apply f) arr)

prop_iMapiMapM ::
     forall r ix e. (Show (Array r ix e), Eq (Array r ix e), Mutable r e, Index ix)
  => Fun (ix, e) e
  -> Array D ix e
  -> Property
prop_iMapiMapM f arr =
  (compute (A.imap (curry (apply f)) arr) :: Array r ix e) ===
  runIdentity (A.imapM (\ix e -> pure $ apply f (ix, e)) arr)

prop_GenerateArray ::
     forall r ix e.
     ( Show (Array r ix e)
     , Eq (Array r ix e)
     , Mutable r e
     , Load r ix e
     , Show e
     , Arbitrary e
     , Arbitrary ix
     , Function ix
     , CoArbitrary ix
     )
  => Property
prop_GenerateArray =
  property $ \comp sz f' -> do
    let arr = makeArray comp sz f :: Array r ix e
        arrST = runST (generateArrayS (size arr) (return . evaluate' arr))
        f = apply f'
    arrST `shouldBe` arr
    arrIO <- generateArray (getComp arr) (size arr) (evaluateM arr)
    arrIO `shouldBe` arr

prop_Shrink ::
     forall r ix e.
     (Show (Array r ix e), Mutable r e, Load r ix e, Arbitrary ix, Arbitrary e, Eq e)
  => Property
prop_Shrink  =
  property $ \ (ArrIx arr ix) -> runST $ do
    marr :: MArray r ix e s <- thawS arr
    sarr <- unsafeFreeze (getComp arr) =<< unsafeLinearShrink marr (Sz ix)
    pure (A.foldlS (.&&.) (property True) $ A.zipWith (==) (flatten arr) (flatten sarr))

-- TODO: Improve runtime speed!
prop_GrowShrink ::
     forall r ix e.
     ( Eq (Array r ix e)
     , Show (Array r ix e)
     , Load r ix e
     , Mutable r e
     , Arbitrary ix
     , Arbitrary e
     , Show e
     )
  => Property
prop_GrowShrink =
  property $ \ (ArrNE arr) (NonNegative delta) e -> runST $ do
    let sz = size (arr :: Array r ix e)
        k = getDim' (unSz sz) (dimensions sz)
        -- increase the outer most dimension, just so the structure doesn't change
        newSz = Sz $ setDim' (unSz sz) (dimensions sz) (k + delta)
    marr <- thawS arr
    grownMarr <- unsafeLinearGrow marr newSz
    -- Make sure we can write into the newly allocated area
    when (delta > 0) $ void $ write grownMarr (liftIndex pred (unSz newSz)) e
    garr <- compute . extract' zeroIndex sz <$> unsafeFreeze (getComp arr) grownMarr
    sarr <- freezeS =<< unsafeLinearShrink grownMarr sz
    pure (garr === arr .&&. sarr === arr)



prop_unfoldrList ::
     forall r ix e.
     ( Show (Array r Ix1 e)
     , Eq (Array r Ix1 e)
     , Index ix
     , Arbitrary ix
     , Arbitrary e
     , Show e
     , Mutable r e
     )
  => Property
prop_unfoldrList =
  property $ \comp sz f (i :: Word) ->
    let xs = runST (unfoldrPrimM_ sz (pure . apply f) i) :: Array r ix e
        ys = A.fromList comp (L.take (totalElem sz) (L.unfoldr (Just . apply f) i))
     in flatten xs === ys


prop_unfoldrReverseUnfoldl ::
     forall r ix e.
     ( Show (Array r ix e)
     , Eq (Array r ix e)
     , Index ix
     , Arbitrary ix
     , Arbitrary e
     , Show e
     , Mutable r e
     )
  => Property
prop_unfoldrReverseUnfoldl =
  property $ \ sz f (i :: Word) ->
    let swapTuple (x, y) = (y, x)
        rev a =
          compute @r (backpermute' sz (liftIndex pred . liftIndex2 (-) (unSz sz)) a)
     in do a1 :: Array r ix e <- unfoldrPrimM_ @r sz (pure . apply f) i
           a2 <- unfoldlPrimM_ @r sz (pure . swapTuple . apply f) i
           rev a1 `shouldBe` a2

prop_toStreamArrayMutable ::
     forall r ix e. (Mutable r e, Index ix, Show (Array r ix e), Eq (Array r ix e))
  => Array r ix e
  -> Property
prop_toStreamArrayMutable arr =
  arr === S.unstreamExact (size arr) (S.stepsStream (toSteps (toStreamArray arr)))

prop_WithMArray ::
     forall r ix e. (HasCallStack, Index ix, Mutable r e, Eq (Array r ix e), Show (Array r ix e))
  => Array r ix e
  -> Fun e e
  -> Fun e e
  -> Property
prop_WithMArray arr f g =
  expectProp $ do
    let g' :: Monad m => e -> m e
        g' = pure . applyFun g
    let arr' = compute $ A.map (applyFun g . applyFun f) arr
    arr1 <-
      withMArray_ arr $ \scheduler marr -> scheduleWork_ scheduler $ forPrimM marr (g' . applyFun f)
    arr1 `shouldBe` arr'
    arr2 <-
      withLoadMArray_ (A.map (applyFun f) arr) $ \scheduler marr ->
        scheduleWork_ scheduler $ forPrimM marr g'
    arr2 `shouldBe` arr'
    arr3 <- withMArrayS_ arr $ \marr -> forPrimM marr (g' . applyFun f)
    arr3 `shouldBe` arr'
    arr4 <- withLoadMArrayS_ (A.map (applyFun f) arr) $ \marr -> forPrimM marr g'
    arr4 `shouldBe` arr'
    let arr5 = withMArrayST_ arr $ \marr -> forPrimM marr (g' . applyFun f)
    arr5 `shouldBe` arr'
    let arr6 = withLoadMArrayST_ (A.map (applyFun f) arr) $ \marr -> forPrimM marr g'
    arr6 `shouldBe` arr'

mutableSpec ::
     forall r ix e.
     ( Show (Array D ix e)
     , Show (Array r ix e)
     , Show (Array r Ix1 e)
     , Eq (Array r Ix1 e)
     , Load r ix e
     , Eq (Array r ix e)
     , Typeable e
     , Show e
     , Eq e
     , Mutable r e
     , CoArbitrary ix
     , Arbitrary e
     , CoArbitrary e
     , Arbitrary ix
     , Typeable ix
     , Function ix
     , Function e
     )
  => Spec
mutableSpec = do
  describe ("Mutable (" ++ showsArrayType @r @ix @e ") (Safe)") $ do
    prop "GenerateArray" $ prop_GenerateArray @r @ix @e
    prop "Shrink" $ prop_Shrink @r @ix @e
    prop "GrowShrink" $ prop_GrowShrink @r @ix @e
    prop "map == mapM" $ prop_iMapiMapM @r @ix @e
    prop "withMArray" $ prop_WithMArray @r @ix @e
  describe "Unfolding" $ do
    it "unfoldrList" $ prop_unfoldrList @r @ix @e
    it "unfoldrReverseUnfoldl" $ prop_unfoldrReverseUnfoldl @r @ix @e
  describe "Stream" $
    prop "toStreamArrayMutable" $ prop_toStreamArrayMutable @r @ix @e

-- | Try to write many elements into the same array cell concurrently, while keeping the
-- previous element for each write. With atomic writes, not a single element should be lost.
prop_atomicModifyIntArrayMany ::
     forall ix. (Show (Array P ix Int), Arbitrary ix, Index ix)
  => Property
prop_atomicModifyIntArrayMany =
  property $ \(ArrIx arr ix) (ys :: Array B Ix1 Int)  -> do
    marr <- thaw arr
    atomicModifyIntArray marr (liftIndex (subtract 1 . negate) ix) succ `shouldReturn` Nothing
    mys <- mapConcurrently (atomicModifyIntArray marr ix . const) ys
    x <- A.readM marr (ix :: ix)
    let xs = x : fromMaybe (error "atomicModifyIntArray") (Prelude.sequenceA (toList mys))
    y <- indexM arr ix
    L.sort (y : toList ys) `shouldBe` L.sort xs

prop_atomicReadIntArray ::
     forall ix. (Show (Array P ix Int), Arbitrary ix, Index ix)
  => Property
prop_atomicReadIntArray =
  property $ \arr (ix :: ix) -> do
    marr <- unsafeThaw arr
    mx <- A.read marr ix
    atomicReadIntArray marr ix `shouldReturn` mx

prop_atomicWriteIntArray ::
     forall ix. (Show (Array P ix Int), Arbitrary ix, Index ix)
  => Property
prop_atomicWriteIntArray =
  property $ \arr (ix :: ix) (e :: Int) -> do
    marr <- unsafeThaw arr
    mx <- A.read marr ix
    atomicWriteIntArray marr ix e `shouldReturn` isJust mx
    T.forM_ mx $ \ _ ->
      A.read marr ix `shouldReturn` Just e

prop_atomicOpIntArray ::
     forall ix. (Show (Array P ix Int), Arbitrary ix, Index ix)
  => (Int -> Int -> Int)
  -> (forall s m. Primal s m =>
                    MArray P ix Int s -> ix -> Int -> m (Maybe Int))
  -> Property
prop_atomicOpIntArray f atomicAction =
  property $ \arr (ix :: ix) (e :: Int) -> do
    marr <- unsafeThaw arr
    mx <- A.read marr ix
    atomicAction marr ix e `shouldReturn` mx
    T.forM_ mx $ \x -> A.readM marr ix `shouldReturn` f x e

prop_casIntArray ::
     forall ix. (Show (Array P ix Int), Arbitrary ix, Index ix)
  => Property
prop_casIntArray =
  property $ \arr (ix :: ix) (e :: Int) -> do
    marr <- unsafeThaw arr
    mx <- A.read marr ix
    case mx of
      Nothing -> casIntArray marr ix e e `shouldReturn` Nothing
      Just x -> do
        casIntArray marr ix x e `shouldReturn` mx
        A.readM marr ix `shouldReturn` e


atomicIntSpec ::
     forall ix. (Show (Array P ix Int), Arbitrary ix, Index ix)
  => Spec
atomicIntSpec =
  describe "Atomic Int Operations" $ do
    it "atomicModifyIntArrayMany" $ prop_atomicModifyIntArrayMany @ix
    it "atomicReadIntArray" $ prop_atomicReadIntArray @ix
    it "atomicWriteIntArray" $ prop_atomicWriteIntArray @ix
    it "atomicAddIntArray" $ prop_atomicOpIntArray @ix (+) atomicAddIntArray
    it "atomicSubIntArray" $ prop_atomicOpIntArray @ix (-) atomicSubIntArray
    it "atomicAndIntArray" $ prop_atomicOpIntArray @ix (.&.) atomicAndIntArray
    it "atomicNandIntArray" $
      prop_atomicOpIntArray @ix (\x y -> complement (x .&. y)) atomicNandIntArray
    it "atomicOrIntArray" $ prop_atomicOpIntArray @ix (.|.) atomicOrIntArray
    it "atomicXorIntArray" $ prop_atomicOpIntArray @ix xor atomicXorIntArray
    it "casIntArray" $ prop_casIntArray @ix
