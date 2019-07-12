{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Massiv.Array.Mutable
  ( -- * Spec for safe Mutable instance
    mutableSpec
  , prop_GenerateArray
  , prop_iMapiMapM
  -- * Atomic ops spec
  , atomicIntSpec
  ) where

import Control.Concurrent.Async
import Data.Functor.Identity
import Data.List as L
import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe
import Data.Massiv.Array.Mutable.Atomic
import Test.Massiv.Core.Common
import Test.Massiv.Utils


-- prop_MapMapM :: forall r ix(Show (Array r ix Word), Eq (Array r ix Word), Mutable r ix Word) =>
--                 Fun Word Word -> ArrTiny D ix Word -> Property
-- prop_MapMapM r _ f (ArrTiny arr) =
--   computeAs r (A.map (apply f) arr) === runIdentity (A.mapMR r (return . apply f) arr)

prop_iMapiMapM ::
     forall r ix e. (Show (Array r ix e), Eq (Array r ix e), Mutable r ix e)
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
     , Mutable r ix e
     , Construct r ix e
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
        arrST = runST (generateArrayS (getComp arr) (size arr) (return . evaluate' arr))
        f = apply f'
    arr `shouldBe` arrST
    arrIO <- generateArray (getComp arr) (size arr) (return . evaluate' arr)
    arr `shouldBe` arrIO

prop_Shrink ::
     forall r ix e.
     ( Show (Array r ix e)
     , Mutable r ix e
     , Construct r ix e
     , Resize r ix
     , Source r Ix1 e
     , Arbitrary ix
     , Arbitrary e
     , Eq e
     )
  => Property
prop_Shrink  =
  property $ \ (ArrIx arr ix) -> runST $ do
    marr :: MArray s r ix e <- thawS arr
    sarr <- unsafeFreeze (getComp arr) =<< unsafeLinearShrink marr (Sz ix)
    pure (A.foldlS (.&&.) (property True) $ A.zipWith (==) (flatten arr) (flatten sarr))

prop_GrowShrink ::
     forall r ix e.
     ( Eq (Array r ix e)
     , Show (Array r ix e)
     , Load (EltRepr r ix) ix e
     , Mutable r ix e
     , Construct r ix e
     , Extract r ix e
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
  forall r ix e . (Arbitrary ix, Arbitrary e, Eq e, Show e, Mutable r ix e) => Property
prop_unfoldrList = property $ \ sz f (i :: Word) ->
  conjoin $
  L.zipWith
    (===)
    (A.toList (runST (unfoldrPrimM_ Seq sz (pure . apply f) i) :: Array r ix e))
    (L.unfoldr (Just . apply f) i)

prop_unfoldrReverseUnfoldl ::
     forall r ix e.
     ( Show (Array r ix e)
     , Eq (Array r ix e)
     , Arbitrary ix
     , Arbitrary e
     , Show e
     , Source r ix e
     , Mutable r ix e
     )
  => Property
prop_unfoldrReverseUnfoldl =
  property $ \sz f (i :: Word) ->
    let swapTuple (x, y) = (y, x)
        rev a =
          compute @r (backpermute' sz (\ix1 -> liftIndex pred $ liftIndex2 (-) (unSz sz) ix1) a)
     in do a1 :: Array r ix e <- unfoldrPrimM_ @r Seq sz (pure . apply f) i
           a2 <- unfoldlPrimM_ @r Seq sz (pure . swapTuple . apply f) i
           rev a1 `shouldBe` a2


mutableSpec ::
     forall r ix e.
     ( Show (Array D ix e)
     , Show (Array r ix e)
     , Load (EltRepr r ix) ix e
     , Eq (Array r ix e)
     , Typeable e
     , Show e
     , Eq e
     , Mutable r ix e
     , Construct r ix e
     , Extract r ix e
     , Source r Ix1 e
     , Resize r ix
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
  describe ("Mutable " ++ showsArrayType @r @ix @e " (Safe)") $ do
    it "GenerateArray" $ property $ prop_GenerateArray @r @ix @e
    it "Shrink" $ property $ prop_Shrink @r @ix @e
    it "GrowShrink" $ property $ prop_GrowShrink @r @ix @e
    it "map == mapM" $ property $ prop_iMapiMapM @r @ix @e
  describe "Unfolding" $ do
    it "unfoldrList" $ prop_unfoldrList @r @ix @e
    it "unfoldrReverseUnfoldl" $ prop_unfoldrReverseUnfoldl @r @ix @e



-- | Try to write many elements into the same array cell concurrently, while keeping the
-- previous element for each write. With atomic writes, not a single element should be lost.
prop_atomicModifyIntArrayMany ::
     forall ix. (Show (Array P ix Int), Arbitrary ix, Index ix)
  => Property
prop_atomicModifyIntArrayMany =
  property $ \(ArrIx arr ix) (ys :: Array B Ix1 Int)  -> do
    marr <- thaw arr
    mys <- mapConcurrently (atomicModifyIntArray marr ix . const) ys
    my <- A.read marr (ix :: ix)
    let xs = fromMaybe (error "atomicModifyIntArray read'") $ Prelude.sequenceA (my : toList mys)
    y <- indexM arr ix
    L.sort (y : toList ys) `shouldBe` L.sort xs


atomicIntSpec ::
     forall ix. (Show (Array P ix Int), Arbitrary ix, Index ix)
  => Spec
atomicIntSpec = do
  describe "Atomic Int Operations" $ do
    it "atomicModifyIntArrayMany" $ prop_atomicModifyIntArrayMany @ix
