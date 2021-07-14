{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Massiv.Array.Manifest.Internal
-- Copyright   : (c) Alexey Kuleshevich 2018-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Manifest.Internal
  ( Manifest(..)
  , Array(..)
  , compute
  , computeS
  , computeP
  , computeIO
  , computePrimM
  , computeAs
  , computeProxy
  , computeSource
  , computeWithStride
  , computeWithStrideAs
  , clone
  , convert
  , convertAs
  , convertProxy
  , gcastArr
  , loadArrayM
  , loadArrayWithSetM
  , loadArrayWithStrideM
  , fromRaggedArrayM
  , fromRaggedArray'
  , sizeofArray
  , sizeofMutableArray
  , iterateUntil
  , iterateUntilM
  ) where

import Control.Monad.ST
import Control.Scheduler
import Data.Massiv.Array.Delayed.Pull
import Data.Massiv.Array.Mutable
import Data.Massiv.Array.Mutable.Internal (unsafeCreateArray_)
import Data.Massiv.Core.Common
import Data.Massiv.Core.List
import Data.Maybe (fromMaybe)
import Data.Typeable
import System.IO.Unsafe (unsafePerformIO)

#if MIN_VERSION_primitive(0,6,2)
import Data.Primitive.Array (sizeofArray, sizeofMutableArray)

#else
import qualified Data.Primitive.Array as A (Array(..), MutableArray(..))
import GHC.Exts (sizeofArray#, sizeofMutableArray#)

sizeofArray :: A.Array a -> Int
sizeofArray (A.Array a) = I# (sizeofArray# a)
{-# INLINE sizeofArray #-}

sizeofMutableArray :: A.MutableArray s a -> Int
sizeofMutableArray (A.MutableArray ma) = I# (sizeofMutableArray# ma)
{-# INLINE sizeofMutableArray #-}
#endif


-- | Load an array into memory.
--
-- @since 0.3.0
loadArrayM ::
     forall r ix e m s. (Load r ix e, UnliftPrimal s m)
  => Scheduler s ()
  -> Array r ix e -- ^ Array that is being loaded
  -> (Int -> e -> m ()) -- ^ Function that writes an element into target array
  -> m ()
loadArrayM scheduler arr uWrite =
  withRunInST $ \run -> loadArrayST scheduler arr (\i -> run . uWrite i)
{-# INLINE loadArrayM #-}

-- | Load an array into memory, just like `loadArrayM`. Except it also accepts a
-- function that is potentially optimized for setting many cells in a region to the same
-- value
--
-- @since 0.5.8
loadArrayWithSetM ::
     forall r ix e m s. (Load r ix e, UnliftPrimal s m)
  => Scheduler s ()
  -> Array r ix e -- ^ Array that is being loaded
  -> (Ix1 -> e -> m ()) -- ^ Function that writes an element into target array
  -> (Ix1 -> Sz1 -> e -> m ()) -- ^ Function that efficiently sets a region of an array
                               -- to the supplied value target array
  -> m ()
loadArrayWithSetM scheduler arr uWrite uSet =
  withRunInST $ \run ->
    loadArrayWithSetST scheduler arr (\i -> run . uWrite i) (\i sz -> run . uSet i sz)
{-# INLINE loadArrayWithSetM #-}


-- | Load an array into memory with stride. Default implementation requires an instance of
-- `Source`.
loadArrayWithStrideM
  :: (StrideLoad r ix e, UnliftPrimal s m) =>
     Scheduler s ()
  -> Stride ix -- ^ Stride to use
  -> Sz ix -- ^ Size of the target array affected by the stride.
  -> Array r ix e -- ^ Array that is being loaded
  -> (Int -> e -> m ()) -- ^ Function that writes an element into target array
  -> m ()
loadArrayWithStrideM scheduler stride resultSize arr f =
  withRunInST $ \run ->
    loadArrayWithStrideST scheduler stride resultSize arr (\i -> run . f i)
{-# INLINE loadArrayWithStrideM #-}


-- | Ensure that Array is computed, i.e. represented with concrete elements in memory, hence is the
-- `Mutable` type class restriction. Use `setComp` if you'd like to change computation strategy
-- before calling @compute@
--
-- @since 0.1.0
compute :: forall r ix e r' . (Mutable r e, Load r' ix e) => Array r' ix e -> Array r ix e
compute !arr = unsafePerformIO $ computeIO arr
{-# INLINE compute #-}

-- | Compute array sequentially disregarding predefined computation strategy. Very much
-- the same as `computePrimM`, but executed in `ST`, thus pure.
--
-- @since 0.1.0
computeS :: forall r ix e r' . (Mutable r e, Load r' ix e) => Array r' ix e -> Array r ix e
computeS !arr = runST $ computePrimM arr
{-# INLINE computeS #-}


-- | Compute array in parallel using all cores disregarding predefined computation
-- strategy. Computation stategy of the resulting array will match the source, despite
-- that it is diregarded.
--
-- @since 0.5.4
computeP ::
     forall r ix e r'. (Mutable r e, Load r' ix e)
  => Array r' ix e
  -> Array r ix e
computeP arr = setComp (getComp arr) $ compute (setComp Par arr)
{-# INLINE computeP #-}

-- | Very similar to `compute`, but computes an array inside the `IO` monad. Despite being
-- deterministic and referentially transparent, because this is an `IO` action it
-- can be very useful for enforcing the order of evaluation. Should be a prefered way of
-- computing an array during benchmarking.
--
-- @since 0.4.5
computeIO ::
     forall r ix e r' m. (Mutable r e, Load r' ix e, Primal RW m)
  => Array r' ix e
  -> m (Array r ix e)
computeIO arr = liftIO (loadArray arr >>= unsafeFreeze (getComp arr))
{-# INLINE computeIO #-}

-- | Compute an array in `PrimMonad` sequentially disregarding predefined computation
-- strategy.
--
-- @since 0.4.5
computePrimM ::
     forall r ix e r' m s. (Mutable r e, Load r' ix e, Primal s m)
  => Array r' ix e
  -> m (Array r ix e)
computePrimM arr = loadArrayS arr >>= unsafeFreeze (getComp arr)
{-# INLINE computePrimM #-}


-- | Just as `compute`, but let's you supply resulting representation type as an argument.
--
-- ====__Examples__
--
-- >>> import Data.Massiv.Array
-- >>> computeAs P $ range Seq (Ix1 0) 10
-- Array P Seq (Sz1 10)
--   [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
--
computeAs :: (Mutable r e, Load r' ix e) => r -> Array r' ix e -> Array r ix e
computeAs _ = compute
{-# INLINE computeAs #-}


-- | Same as `compute` and `computeAs`, but let's you supply resulting representation type as a proxy
-- argument.
--
-- ==== __Examples__
--
-- Useful only really for cases when representation constructor or @TypeApplications@ extension
-- aren't desireable for some reason:
--
-- >>> import Data.Proxy
-- >>> import Data.Massiv.Array
-- >>> computeProxy (Proxy :: Proxy P) $ (^ (2 :: Int)) <$> range Seq (Ix1 0) 10
-- Array P Seq (Sz1 10)
--   [ 0, 1, 4, 9, 16, 25, 36, 49, 64, 81 ]
--
-- @since 0.1.1
computeProxy :: (Mutable r e, Load r' ix e) => proxy r -> Array r' ix e -> Array r ix e
computeProxy _ = compute
{-# INLINE computeProxy #-}


-- | This is just like `convert`, but restricted to `Source` arrays. Will be a noop if
-- resulting type is the same as the input.
--
-- @since 0.1.0
computeSource :: forall r ix e r' . (Mutable r e, Source r' e, Index ix)
              => Array r' ix e -> Array r ix e
computeSource arr = maybe (compute $ delay arr) (\Refl -> arr) (eqT :: Maybe (r' :~: r))
{-# INLINE computeSource #-}


-- | /O(n)/ - Make an exact immutable copy of an Array.
--
-- @since 0.1.0
clone :: (Mutable r e, Index ix) => Array r ix e -> Array r ix e
clone arr = unsafePerformIO $ thaw arr >>= unsafeFreeze (getComp arr)
{-# INLINE clone #-}


-- | /O(1)/ - Cast over Array representation
gcastArr :: forall r ix e r' . (Typeable r, Typeable r')
       => Array r' ix e -> Maybe (Array r ix e)
gcastArr arr = fmap (\Refl -> arr) (eqT :: Maybe (r :~: r'))


-- | /O(n)/ - conversion between array types. A full copy will occur, unless when the source and
-- result arrays are of the same representation, in which case it is an /O(1)/ operation.
--
-- @since 0.1.0
convert :: forall r ix e r' . (Mutable r e, Load r' ix e)
        => Array r' ix e -> Array r ix e
convert arr = fromMaybe (compute arr) (gcastArr arr)
{-# INLINE convert #-}

-- | Same as `convert`, but let's you supply resulting representation type as an argument.
--
-- @since 0.1.0
convertAs :: (Mutable r e, Load r' ix e)
          => r -> Array r' ix e -> Array r ix e
convertAs _ = convert
{-# INLINE convertAs #-}


-- | Same as `convert` and `convertAs`, but let's you supply resulting representation type as a
-- proxy argument.
--
-- @since 0.1.1
convertProxy :: (Mutable r e, Load r' ix e)
             => proxy r -> Array r' ix e -> Array r ix e
convertProxy _ = convert
{-# INLINE convertProxy #-}


-- | Convert a ragged array into a common array with rectangular shape. Throws `ShapeException`
-- whenever supplied ragged array does not have a rectangular shape.
--
-- @since 0.4.0
fromRaggedArrayM ::
     forall r ix e r' m . (Mutable r e, Ragged r' ix e, Raises m)
  => Array r' ix e
  -> m (Array r ix e)
fromRaggedArrayM arr =
  let sz = outerSize arr
   in either (\(e :: ShapeException) -> raiseM e) pure $
      unsafePerformIO $ do
        marr <- unsafeNew sz
        traverse (\_ -> unsafeFreeze (getComp arr) marr) =<<
          try (withMassivScheduler_ (getComp arr) $ \scheduler ->
                 liftST $ loadRagged scheduler (unsafeLinearWrite marr) 0 (totalElem sz) sz arr)
{-# INLINE fromRaggedArrayM #-}


-- | Same as `fromRaggedArrayM`, but will throw a pure exception if its shape is not
-- rectangular.
--
-- @since 0.1.1
fromRaggedArray' ::
     forall r ix e r'. (HasCallStack, Mutable r e, Ragged r' ix e)
  => Array r' ix e
  -> Array r ix e
fromRaggedArray' = raiseLeftImprecise . fromRaggedArrayM
{-# INLINE fromRaggedArray' #-}


-- | Same as `compute`, but with `Stride`.
--
-- /O(n div k)/ - Where @n@ is number of elements in the source array and @k@ is number of
-- elements in the stride.
--
-- @since 0.3.0
computeWithStride ::
     forall r ix e r'. (Mutable r e, StrideLoad r' ix e)
  => Stride ix
  -> Array r' ix e
  -> Array r ix e
computeWithStride stride !arr =
  unsafePerformIO $ do
    let !sz = strideSize stride (size arr)
    unsafeCreateArray_ (getComp arr) sz $ \scheduler marr ->
      loadArrayWithStrideM scheduler stride sz arr (unsafeLinearWrite marr)
{-# INLINE computeWithStride #-}


-- | Same as `computeWithStride`, but with ability to specify resulting array representation.
--
-- @since 0.3.0
computeWithStrideAs ::
     (Mutable r e, StrideLoad r' ix e) => r -> Stride ix -> Array r' ix e -> Array r ix e
computeWithStrideAs _ = computeWithStride
{-# INLINE computeWithStrideAs #-}



-- | Efficiently iterate a function until a convergence condition is satisfied. If the
-- size of array doesn't change between iterations then no more than two new arrays will be
-- allocated, regardless of the number of iterations. If the size does change from one
-- iteration to another, an attempt will be made to grow/shrink the intermediate mutable
-- array instead of allocating a new one.
--
-- ====__Example__
--
-- >>> import Data.Massiv.Array
-- >>> a = computeAs P $ makeLoadArrayS (Sz2 8 8) (0 :: Int) $ \ w -> () <$ w (0 :. 0) 1
-- >>> a
-- Array P Seq (Sz (8 :. 8))
--   [ [ 1, 0, 0, 0, 0, 0, 0, 0 ]
--   , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
--   , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
--   , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
--   , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
--   , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
--   , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
--   , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
--   ]
-- >>> nextPascalRow cur above = if cur == 0 then above else cur
-- >>> pascal = makeStencil (Sz2 2 2) 1 $ \ get -> nextPascalRow (get (0 :. 0)) (get (-1 :. -1) + get (-1 :. 0))
-- >>> iterateUntil (\_ _ a -> (a ! (7 :. 7)) /= 0) (\ _ -> mapStencil (Fill 0) pascal) a
-- Array P Seq (Sz (8 :. 8))
--   [ [ 1, 0, 0, 0, 0, 0, 0, 0 ]
--   , [ 1, 1, 0, 0, 0, 0, 0, 0 ]
--   , [ 1, 2, 1, 0, 0, 0, 0, 0 ]
--   , [ 1, 3, 3, 1, 0, 0, 0, 0 ]
--   , [ 1, 4, 6, 4, 1, 0, 0, 0 ]
--   , [ 1, 5, 10, 10, 5, 1, 0, 0 ]
--   , [ 1, 6, 15, 20, 15, 6, 1, 0 ]
--   , [ 1, 7, 21, 35, 35, 21, 7, 1 ]
--   ]
--
-- @since 0.3.6
iterateUntil ::
     (Size r', Load r' ix e, Mutable r e)
  => (Int -> Array r ix e -> Array r ix e -> Bool)
  -- ^ Convergence condition. Accepts current iteration counter, array at the previous
  -- state and at the current state.
  -> (Int -> Array r ix e -> Array r' ix e)
  -- ^ A modifying function to apply at each iteration. The size of resulting array may
  -- differ if necessary
  -> Array r ix e -- ^ Initial source array
  -> Array r ix e
iterateUntil convergence iteration initArr0
  | convergence 0 initArr0 initArr1 = initArr1
  | otherwise =
    unsafePerformIO $ do
      let loadArr = iteration 1 initArr1
      marr <- unsafeNew (size loadArr)
      iterateLoop
        (\n a comp marr' -> convergence n a <$> unsafeFreeze comp marr')
        iteration
        1
        initArr1
        loadArr
        (asArr initArr0 marr)
  where
    !initArr1 = compute $ iteration 0 initArr0
    asArr :: Array r ix e -> MArray r ix e s -> MArray r ix e s
    asArr _ = id
{-# INLINE iterateUntil #-}

-- | Monadic version of `iterateUntil` where at each iteration mutable version of an array
-- is available.
--
-- @since 0.3.6
iterateUntilM ::
     (Size r', Load r' ix e, Mutable r e, Primal RW m)
  => (Int -> Array r ix e -> MArray r ix e RW -> m Bool)
  -- ^ Convergence condition. Accepts current iteration counter, pure array at previous
  -- state and a mutable at the current state, therefore after each iteration its contents
  -- can be modifed if necessary.
  -> (Int -> Array r ix e -> Array r' ix e)
  -- ^ A modifying function to apply at each iteration.  The size of resulting array may
  -- differ if necessary.
  -> Array r ix e -- ^ Initial source array
  -> m (Array r ix e)
iterateUntilM convergence iteration initArr0 = do
  let loadArr0 = iteration 0 initArr0
  initMArr1 <- unsafeNew (size loadArr0)
  computeInto initMArr1 loadArr0
  shouldStop <- convergence 0 initArr0 initMArr1
  initArr1 <- unsafeFreeze (getComp loadArr0) initMArr1
  if shouldStop
    then pure initArr1
    else do
      let loadArr1 = iteration 1 initArr1
      marr <- unsafeNew (size loadArr1)
      iterateLoop (\n a _ -> convergence n a) iteration 1 initArr1 loadArr1 marr
{-# INLINE iterateUntilM #-}


iterateLoop ::
     (Size r', Load r' ix e, Mutable r e, Primal RW m)
  => (Int -> Array r ix e -> Comp -> MArray r ix e RW -> m Bool)
  -> (Int -> Array r ix e -> Array r' ix e)
  -> Int
  -> Array r ix e
  -> Array r' ix e
  -> MArray r ix e RW
  -> m (Array r ix e)
iterateLoop convergence iteration = go
  where
    go !n !arr !loadArr !marr = do
      let !sz = size loadArr
          !k = totalElem sz
          !mk = totalElem (msize marr)
          !comp = getComp loadArr
      marr' <-
        if k == mk
          then pure marr
          else if k < mk
                 then unsafeLinearShrink marr sz
                 else unsafeLinearGrow marr sz
      computeInto marr' loadArr
      shouldStop <- convergence n arr comp marr'
      arr' <- unsafeFreeze comp marr'
      if shouldStop
        then pure arr'
        else do
          nextMArr <- unsafeThaw arr
          go (n + 1) arr' (iteration (n + 1) arr') nextMArr
{-# INLINE iterateLoop #-}


