{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- |
-- Module      : Data.Massiv.Array.Mutable
-- Copyright   : (c) Alexey Kuleshevich 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Mutable
  ( -- * Element-wise mutation
    read
  , read'
  , write
  , write'
  , modify
  , modify'
  , swap
  , swap'
  -- ** Operate over `MArray`
  , Mutable
  , MArray
  , msize
  -- *** Convert
  , new
  , thaw
  , freeze
  -- *** Create
  , createArray_
  , createArray
  , createArrayST_
  , createArrayST
  -- *** Generate
  , generateArray
  , generateArrayIO
  -- *** Unfold
  , unfoldlPrim_
  , unfoldlPrim
  -- *** Modify
  , withMArray
  , withMArrayST
  -- ** Computation
  , RealWorld
  , computeInto
  ) where

import           Prelude                             hiding (mapM, read)

import           Control.Monad                       (unless)
import           Control.Monad.Primitive             (PrimMonad (..))
import           Control.Monad.ST
import           Data.Massiv.Array.Manifest.Internal
import           Data.Massiv.Array.Unsafe
import           Data.Massiv.Core.Common
import           Data.Massiv.Core.Scheduler

-- | Initialize a new mutable array. Negative size will result in an empty array.
new :: (Mutable r ix e, PrimMonad m) => ix -> m (MArray (PrimState m) r ix e)
new sz = unsafeNewZero (liftIndex (max 0) sz)
{-# INLINE new #-}

-- | /O(n)/ - Yield a mutable copy of the immutable array
thaw :: (Mutable r ix e, PrimMonad m) => Array r ix e -> m (MArray (PrimState m) r ix e)
thaw = unsafeThaw . clone
{-# INLINE thaw #-}

-- | /O(n)/ - Yield an immutable copy of the mutable array
freeze :: (Mutable r ix e, PrimMonad m) => Comp -> MArray (PrimState m) r ix e -> m (Array r ix e)
freeze comp marr = clone <$> unsafeFreeze comp marr
{-# INLINE freeze #-}


-- | Create a new array by supplying an action that will fill the new blank mutable array. Use
-- `createArray` if you'd like to keep the result of the filling function.
--
-- ====__Examples__
--
-- >>> createArray_ Seq (Ix1 2) (\ marr -> write marr 0 10 >> write marr 1 11) :: IO (Array P Ix1 Int)
-- (Array P Seq (2)
--   [ 10,11 ])
--
-- @since 0.2.6
--
createArray_ ::
     (Mutable r ix e, PrimMonad m)
  => Comp -- ^ Computation strategy to use after `MArray` gets frozen and onward.
  -> ix -- ^ Size of the newly created array
  -> (MArray (PrimState m) r ix e -> m a)
  -- ^ An action that should fill all elements of the brand new mutable array
  -> m (Array r ix e)
createArray_ comp sz action = fmap snd $ createArray comp sz action
{-# INLINE createArray_ #-}

-- | Just like `createArray_`, but together with `Array` it returns the result of the filling action.
--
-- @since 0.2.6
--
createArray ::
     (Mutable r ix e, PrimMonad m)
  => Comp -- ^ Computation strategy to use after `MArray` gets frozen and onward.
  -> ix -- ^ Size of the newly created array
  -> (MArray (PrimState m) r ix e -> m a)
  -- ^ An action that should fill all elements of the brand new mutable array
  -> m (a, Array r ix e)
createArray comp sz action = do
  marr <- new sz
  a <- action marr
  arr <- unsafeFreeze comp marr
  return (a, arr)
{-# INLINE createArray #-}

-- | Just like `createArray_`, but restricted to `ST`.
--
-- @since 0.2.6
--
createArrayST_ ::
     Mutable r ix e => Comp -> ix -> (forall s. MArray s r ix e -> ST s a) -> Array r ix e
createArrayST_ comp sz action = runST $ createArray_ comp sz action
{-# INLINE createArrayST_ #-}


-- | Just like `createArray`, but restricted to `ST`.
--
-- @since 0.2.6
--
createArrayST ::
     Mutable r ix e => Comp -> ix -> (forall s. MArray s r ix e -> ST s a) -> (a, Array r ix e)
createArrayST comp sz action = runST $ createArray comp sz action
{-# INLINE createArrayST #-}


-- | Sequentially generate a pure array. Much like `makeArray` creates a pure array this function
-- will use `Mutable` interface to generate a pure `Array` in the end, except that computation
-- strategy is ignored. Element producing function no longer has to be pure but is a stateful
-- action, since it is restricted to `PrimMonad` and allows for sharing the state between
-- computation of each element, which could be arbitrary effects if that monad is `IO`.
--
-- @since 0.2.6
--
-- ====__Examples__
--
-- >>> import Data.IORef
-- >>> ref <- newIORef (0 :: Int)
-- >>> generateArray Seq (Ix1 6) (\ i -> modifyIORef' ref (+i) >> print i >> pure i) :: IO (Array U Ix1 Int)
-- 0
-- 1
-- 2
-- 3
-- 4
-- 5
-- (Array U Seq (6)
--   [ 0,1,2,3,4,5 ])
-- >>> readIORef ref
-- 15
--
generateArray ::
     (Mutable r ix e, PrimMonad m)
  => Comp -- ^ Computation strategy (ingored during generation)
  -> ix -- ^ Resulting size of the array
  -> (ix -> m e) -- ^ Element producing generator
  -> m (Array r ix e)
generateArray comp sz' gen = do
  let sz = liftIndex (max 0) sz'
  marr <- unsafeNew sz
  iterM_ zeroIndex (msize marr) (pureIndex 1) (<) $ \ix -> gen ix >>= write marr ix
  unsafeFreeze comp marr
{-# INLINE generateArray #-}


-- | Just like `generateArray`, except this generator __will__ respect the supplied computation
-- strategy, and for that reason it is restricted to `IO`.
--
-- @since 0.2.6
generateArrayIO ::
     (Mutable r ix e)
  => Comp
  -> ix
  -> (ix -> IO e)
  -> IO (Array r ix e)
generateArrayIO comp sz' gen = do
  case comp of
    Seq -> generateArray comp sz' gen
    ParOn wids -> do
      let sz = liftIndex (max 0) sz'
      marr <- unsafeNew sz
      withScheduler_ wids $ \scheduler ->
        splitLinearlyWithM_
          (numWorkers scheduler)
          (scheduleWork scheduler)
          (totalElem sz)
          (gen . fromLinearIndex sz)
          (unsafeLinearWrite marr)
      unsafeFreeze comp marr
{-# INLINE generateArrayIO #-}

-- | Sequentially unfold an array from the left.
--
-- @since 0.2.6
--
-- ====__Examples__
--
-- Create an array with Fibonacci numbers while performing and `IO` action on the accumulator for
-- each element of the array.
--
-- >>> unfoldlPrim_ Seq  (Ix1 10) (\a@(f0, f1) _ -> let fn = f0 + f1 in print a >> return ((f1, fn), f0)) (0, 1) :: IO (Array P Ix1 Int)
-- (0,1)
-- (1,1)
-- (1,2)
-- (2,3)
-- (3,5)
-- (5,8)
-- (8,13)
-- (13,21)
-- (21,34)
-- (34,55)
-- (Array P Seq (10)
--   [ 0,1,1,2,3,5,8,13,21,34 ])
--
unfoldlPrim_ ::
     (Mutable r ix e, PrimMonad m)
  => Comp -- ^ Computation strategy (ignored during initial creation)
  -> ix -- ^ Size of the desired array
  -> (a -> ix -> m (a, e)) -- ^ Unfolding action
  -> a -- ^ Initial accumulator
  -> m (Array r ix e)
unfoldlPrim_ comp sz gen acc0 = fmap snd $ unfoldlPrim comp sz gen acc0
{-# INLINE unfoldlPrim_ #-}


-- | Just like `unfoldlPrim`, but also returns the final value of the accumulator.
--
-- @since 0.2.6
--
unfoldlPrim ::
     (Mutable r ix e, PrimMonad m)
  => Comp -- ^ Computation strategy (ignored during initial creation)
  -> ix -- ^ Size of the desired array
  -> (a -> ix -> m (a, e)) -- ^ Unfolding action
  -> a -- ^ Initial accumulator
  -> m (a, Array r ix e)
unfoldlPrim comp sz gen acc0 =
  createArray comp sz $ \marr ->
    let sz' = msize marr
     in iterLinearM sz' 0 (totalElem sz') 1 (<) acc0 $ \i ix acc -> do
          (acc', e) <- gen acc ix
          unsafeLinearWrite marr i e
          return acc'
{-# INLINE unfoldlPrim #-}


-- | Create a copy of a pure array, mutate it in place and return its frozen version.
--
-- @since 0.2.2
withMArray ::
     (Mutable r ix e, PrimMonad m)
  => Array r ix e
  -> (MArray (PrimState m) r ix e -> m a)
  -> m (Array r ix e)
withMArray arr action = do
  marr <- thaw arr
  _ <- action marr
  unsafeFreeze (getComp arr) marr
{-# INLINE withMArray #-}


-- | Same as `withMArray` but in `ST`.
--
-- @since 0.2.2
withMArrayST ::
     Mutable r ix e
  => Array r ix e
  -> (forall s . MArray s r ix e -> ST s a)
  -> Array r ix e
withMArrayST arr f = runST $ withMArray arr f
{-# INLINE withMArrayST #-}

-- | /O(1)/ - Lookup an element in the mutable array. Return `Nothing` when index is out of bounds.
read :: (Mutable r ix e, PrimMonad m) =>
        MArray (PrimState m) r ix e -> ix -> m (Maybe e)
read marr ix =
  if isSafeIndex (msize marr) ix
  then Just <$> unsafeRead marr ix
  else return Nothing
{-# INLINE read #-}


-- | /O(1)/ - Same as `read`, but throws an error if index is out of bounds.
read' :: (Mutable r ix e, PrimMonad m) =>
        MArray (PrimState m) r ix e -> ix -> m e
read' marr ix = do
  mval <- read marr ix
  case mval of
    Just e  -> return e
    Nothing -> errorIx "Data.Massiv.Array.Mutable.read'" (msize marr) ix
{-# INLINE read' #-}


-- | /O(1)/ - Write an element into the cell of a mutable array. Returns `False` when index is out
-- of bounds.
write :: (Mutable r ix e, PrimMonad m) =>
        MArray (PrimState m) r ix e -> ix -> e -> m Bool
write marr ix e =
  if isSafeIndex (msize marr) ix
  then unsafeWrite marr ix e >> return True
  else return False
{-# INLINE write #-}


-- | /O(1)/ - Same as `write`, but throws an error if index is out of bounds.
write' :: (Mutable r ix e, PrimMonad m) =>
        MArray (PrimState m) r ix e -> ix -> e -> m ()
write' marr ix e =
  write marr ix e >>= (`unless` errorIx "Data.Massiv.Array.Mutable.write'" (msize marr) ix)
{-# INLINE write' #-}


-- | /O(1)/ - Modify an element in the cell of a mutable array with a supplied function. Returns
-- `False` when index is out of bounds.
modify :: (Mutable r ix e, PrimMonad m) =>
        MArray (PrimState m) r ix e -> (e -> e) -> ix -> m Bool
modify marr f ix =
  if isSafeIndex (msize marr) ix
  then do
    val <- unsafeRead marr ix
    unsafeWrite marr ix $ f val
    return True
  else return False
{-# INLINE modify #-}


-- | /O(1)/ - Same as `modify`, but throws an error if index is out of bounds.
modify' :: (Mutable r ix e, PrimMonad m) =>
        MArray (PrimState m) r ix e -> (e -> e) -> ix -> m ()
modify' marr f ix =
  modify marr f ix >>= (`unless` errorIx "Data.Massiv.Array.Mutable.modify'" (msize marr) ix)
{-# INLINE modify' #-}


-- | /O(1)/ - Swap two elements in a mutable array by supplying their indices. Returns `False` when
-- either one of the indices is out of bounds.
swap :: (Mutable r ix e, PrimMonad m) =>
        MArray (PrimState m) r ix e -> ix -> ix -> m Bool
swap marr ix1 ix2 = do
  let sz = msize marr
  if isSafeIndex sz ix1 && isSafeIndex sz ix2
  then do
    val1 <- unsafeRead marr ix1
    val2 <- unsafeRead marr ix2
    unsafeWrite marr ix1 val2
    unsafeWrite marr ix2 val1
    return True
  else return False
{-# INLINE swap #-}


-- | /O(1)/ - Same as `swap`, but throws an error if index is out of bounds.
swap' :: (Mutable r ix e, PrimMonad m) =>
        MArray (PrimState m) r ix e -> ix -> ix -> m ()
swap' marr ix1 ix2 = do
  success <- swap marr ix1 ix2
  unless success $
    errorIx "Data.Massiv.Array.Mutable.swap'" (msize marr) $
    if isSafeIndex (msize marr) ix1
      then ix2
      else ix1
{-# INLINE swap' #-}

