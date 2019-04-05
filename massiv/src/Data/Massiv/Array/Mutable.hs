{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Massiv.Array.Mutable
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Mutable
  ( -- ** Size
    msize
    -- ** Element-wise mutation
  , read
  , read'
  , write
  , write'
  , modify
  , modify'
  , swap
  , swap'
  -- ** Operate over `MArray`
  -- *** Immutable conversion
  , new
  , thaw
  , thawS
  , freeze
  , freezeS
  -- *** Create mutable
  , makeMArray
  , makeMArrayLinear
  , makeMArrayS
  , makeMArrayLinearS
  -- *** Create pure
  , createArray_
  , createArray
  , createArrayS_
  , createArrayS
  , createArrayST_
  , createArrayST
  -- *** Generate
  , generateArray
  , generateArrayLinear
  , generateArrayS
  , generateArrayLinearS
  -- *** Unfold
  , unfoldrPrimM_
  , iunfoldrPrimM_
  , unfoldrPrimM
  , iunfoldrPrimM
  , unfoldlPrimM_
  , iunfoldlPrimM_
  , unfoldlPrimM
  , iunfoldlPrimM
  -- *** Mapping
  , forPrimM_
  , iforPrimM_
  , iforLinearPrimM_
  -- *** Modify
  , withMArray
  , withMArrayST
  -- *** Initialize
  , initialize
  , initializeNew
  -- ** Computation
  , Mutable
  , MArray
  , RealWorld
  , computeInto
  , loadArray
  , loadArrayS
  ) where

import Control.Scheduler
import Control.Monad (unless)
import Control.Monad.ST
import Data.Massiv.Core.Common
import Prelude hiding (mapM, read)

-- | /O(n)/ - Initialize a new mutable array. All elements will be set to some default value. For
-- boxed arrays in will be a thunk with `Uninitialized` exception, while for others it will be
-- simply zeros.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> marr <- new (Sz2 2 6) :: IO (MArray RealWorld P Ix2 Int)
-- >>> freeze Seq marr
-- Array P Seq (Sz (2 :. 6))
--   [ [ 0, 0, 0, 0, 0, 0 ]
--   , [ 0, 0, 0, 0, 0, 0 ]
--   ]
--
-- Or using @TypeApplications@:
--
-- >>> :set -XTypeApplications
-- >>> new @P @Ix2 @Int (Sz2 2 6) >>= freezeS
-- Array P Seq (Sz (2 :. 6))
--   [ [ 0, 0, 0, 0, 0, 0 ]
--   , [ 0, 0, 0, 0, 0, 0 ]
--   ]
-- >>> new @B @_ @Int (Sz2 2 6) >>= (`read'` 1)
-- *** Exception: Uninitialized
--
-- @since 0.1.0
new ::
     forall r ix e m. (Mutable r ix e, PrimMonad m)
  => Sz ix
  -> m (MArray (PrimState m) r ix e)
new = initializeNew Nothing
{-# INLINE new #-}

-- | /O(n)/ - Make a mutable copy of a pure array. Keep in mind that both `freeze` and `thaw` trigger a
-- copy of the full array.
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Array
-- >>> :set -XTypeApplications
-- >>> arr <- fromListsM @U @Ix2 @Double Par [[12,21],[13,31]]
-- >>> marr <- thaw arr
-- >>> modify marr (+ 10) (1 :. 0)
-- True
-- >>> freeze Par marr
-- Array U Par (Sz (2 :. 2))
--   [ [ 12.0, 21.0 ]
--   , [ 23.0, 31.0 ]
--   ]
--
-- @since 0.1.0
thaw :: forall r ix e m. (Mutable r ix e, MonadIO m) => Array r ix e -> m (MArray RealWorld r ix e)
thaw arr = liftIO $ makeMArrayLinear (getComp arr) (size arr) (pure . unsafeLinearIndexM arr)
-- TODO: use faster memcpy
{-# INLINE thaw #-}

-- | Same as `thaw`, but restrict computation to sequential only.
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Array
-- >>> :set -XOverloadedLists
-- >>> thawS @P @Ix1 @Double [1..10]
-- >>> marr <- thawS @P @Ix1 @Double [1..10]
-- >>> write' marr 5 100
-- >>> freezeS marr
-- Array P Seq (Sz1 10)
--   [ 1.0, 2.0, 3.0, 4.0, 5.0, 100.0, 7.0, 8.0, 9.0, 10.0 ]
--
-- @since 0.3.0
thawS ::
     forall r ix e m. (Mutable r ix e, PrimMonad m)
  => Array r ix e
  -> m (MArray (PrimState m) r ix e)
thawS arr = makeMArrayLinearS (size arr) (pure . unsafeLinearIndexM arr)
-- TODO: use faster memcpy
{-# INLINE thawS #-}


-- TODO: implement and benchmark parallel `thawIO` and `freezeIO` with memcpy

-- | /O(n)/ - Yield an immutable copy of the mutable array. Note that mutable representations
-- have to be the same.
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Array
-- >>> marr <- new @P @_ @Int (Sz2 2 6)
-- >>> forM_ (range Seq 0 (Ix2 1 4)) $ \ix -> write marr ix 9
-- >>> freeze Seq marr
-- Array P Seq (Sz (2 :. 6))
--   [ [ 9, 9, 9, 9, 0, 0 ]
--   , [ 0, 0, 0, 0, 0, 0 ]
--   ]
--
-- @since 0.1.0
freeze ::
     forall r ix e m. (Mutable r ix e, MonadIO m)
  => Comp
  -> MArray RealWorld r ix e
  -> m (Array r ix e)
freeze comp marr = liftIO $ generateArrayLinear comp (msize marr) (unsafeLinearRead marr)
{-# INLINE freeze #-}


-- | Same as `freeze`, but do the copy of supplied muable array sequentially. Also, unlike `freeze`
-- that has to be done in `IO`, `freezeS` can be used with `ST`.
--
-- @since 0.3.0
freezeS ::
     forall r ix e m. (Mutable r ix e, PrimMonad m)
  => MArray (PrimState m) r ix e
  -> m (Array r ix e)
freezeS marr = generateArrayLinearS Seq (msize marr) (unsafeLinearRead marr)
{-# INLINE freezeS #-}


-- | Load sequentially a pure array into the newly created mutable array.
--
-- @since 0.3.0
loadArrayS ::
     forall r ix e r' m. (Load r' ix e, Mutable r ix e, PrimMonad m)
  => Array r' ix e
  -> m (MArray (PrimState m) r ix e)
loadArrayS arr = do
  marr <- unsafeNew (size arr)
  loadArrayM trivialScheduler_ arr (unsafeLinearWrite marr)
  pure marr
{-# INLINE loadArrayS #-}


-- | Load a pure array into the newly created mutable array, while respecting computation startegy.
--
-- @since 0.3.0
loadArray ::
     forall r ix e r' m. (Load r' ix e, Mutable r ix e, MonadIO m)
  => Array r' ix e
  -> m (MArray RealWorld r ix e)
loadArray arr =
  liftIO $ do
    marr <- unsafeNew (size arr)
    withScheduler_ (getComp arr) $ \scheduler -> loadArrayM scheduler arr (unsafeLinearWrite marr)
    pure marr
{-# INLINE loadArray #-}



-- | Compute an Array while loading the results into the supplied mutable target array. Number of
-- elements for arrays must agree, otherwise `SizeElementsMismatchException` exception is thrown.
--
-- @since 0.1.3
computeInto ::
     (Load r' ix' e, Mutable r ix e, MonadIO m)
  => MArray RealWorld r ix e -- ^ Target Array
  -> Array r' ix' e -- ^ Array to load
  -> m ()
computeInto !mArr !arr =
  liftIO $ do
    unless (totalElem (msize mArr) == totalElem (size arr)) $
      throwM $ SizeElementsMismatchException (msize mArr) (size arr)
    withScheduler_ (getComp arr) $ \scheduler -> loadArrayM scheduler arr (unsafeLinearWrite mArr)
{-# INLINE computeInto #-}


-- | Create a mutable array using an index aware generating action.
--
-- @since 0.3.0
makeMArrayS ::
     forall r ix e m. (Mutable r ix e, PrimMonad m)
  => Sz ix -- ^ Size of the create array
  -> (ix -> m e) -- ^ Element generating action
  -> m (MArray (PrimState m) r ix e)
makeMArrayS sz f = makeMArrayLinearS sz (f . fromLinearIndex sz)
{-# INLINE makeMArrayS #-}


-- | Same as `makeMArrayS`, but index supplied to the action is row-major linear index.
--
-- @since 0.3.0
makeMArrayLinearS ::
     forall r ix e m. (Mutable r ix e, PrimMonad m)
  => Sz ix
  -> (Int -> m e)
  -> m (MArray (PrimState m) r ix e)
makeMArrayLinearS sz f = do
  marr <- unsafeNew sz
  loopM_ 0 (< totalElem (msize marr)) (+ 1) (\ !i -> f i >>= unsafeLinearWrite marr i)
  return marr
{-# INLINE makeMArrayLinearS #-}

-- | Just like `makeMArrayS`, but also accepts computation strategy and runs in `IO`.
--
-- @since 0.3.0
makeMArray ::
     forall r ix e m. (PrimMonad m, MonadUnliftIO m, Mutable r ix e)
  => Comp
  -> Sz ix
  -> (ix -> m e)
  -> m (MArray (PrimState m) r ix e)
makeMArray comp sz f = makeMArrayLinear comp sz (f . fromLinearIndex sz)
{-# INLINE makeMArray #-}


-- | Just like `makeMArrayLinearS`, but also accepts computation strategy and runs in `IO`.
--
-- @since 0.3.0
makeMArrayLinear ::
     forall r ix e m. (PrimMonad m, MonadUnliftIO m, Mutable r ix e)
  => Comp
  -> Sz ix
  -> (Int -> m e)
  -> m (MArray (PrimState m) r ix e)
makeMArrayLinear comp sz f = do
  marr <- unsafeNew sz
  withScheduler_ comp $ \scheduler ->
    splitLinearlyWithM_ scheduler (totalElem sz) f (unsafeLinearWrite marr)
  return marr
{-# INLINE makeMArrayLinear #-}




-- | Create a new array by supplying an action that will fill the new blank mutable array. Use
-- `createArray` if you'd like to keep the result of the filling function.
--
-- ====__Examples__
--
-- >>> :set -XTypeApplications
-- >>> import Data.Massiv.Array
-- >>> createArray_ @P @_ @Int Seq (Sz1 2) (\ s marr -> scheduleWork s (write' marr 0 10) >> scheduleWork s (write' marr 1 11))
-- Array P Seq (Sz1 2)
--   [ 10, 11 ]
--
-- @since 0.3.0
--
createArray_ ::
     forall r ix e a m. (Mutable r ix e, PrimMonad m, MonadUnliftIO m)
  => Comp -- ^ Computation strategy to use after `MArray` gets frozen and onward.
  -> Sz ix -- ^ Size of the newly created array
  -> (Scheduler m () -> MArray (PrimState m) r ix e -> m a)
  -- ^ An action that should fill all elements of the brand new mutable array
  -> m (Array r ix e)
createArray_ comp sz action = do
  marr <- new sz
  withScheduler_ comp (`action` marr)
  unsafeFreeze comp marr
{-# INLINE createArray_ #-}

-- | Just like `createArray_`, but together with `Array` it returns results of scheduled filling
-- actions.
--
-- @since 0.3.0
--
createArray ::
     forall r ix e a m. (Mutable r ix e, PrimMonad m, MonadUnliftIO m)
  => Comp -- ^ Computation strategy to use after `MArray` gets frozen and onward.
  -> Sz ix -- ^ Size of the newly created array
  -> (Scheduler m a -> MArray (PrimState m) r ix e -> m [a])
  -- ^ An action that should fill all elements of the brand new mutable array
  -> m ([a], Array r ix e)
createArray comp sz action = do
  marr <- new sz
  a <- withScheduler comp (`action` marr)
  arr <- unsafeFreeze comp marr
  return (a, arr)
{-# INLINE createArray #-}


-- | Create a new array by supplying an action that will fill the new blank mutable array. Use
-- `createArrayS` if you'd like to keep the result of the filling function.
--
-- ====__Examples__
--
-- >>> :set -XTypeApplications
-- >>> import Data.Massiv.Array
-- >>> createArrayS_ @P @_ @Int Seq (Sz1 2) (\ marr -> write marr 0 10 >> write marr 1 12)
-- Array P Seq (Sz1 2)
--   [ 10, 12 ]
--
-- @since 0.3.0
--
createArrayS_ ::
     forall r ix e a m. (Mutable r ix e, PrimMonad m)
  => Comp -- ^ Computation strategy to use after `MArray` gets frozen and onward.
  -> Sz ix -- ^ Size of the newly created array
  -> (MArray (PrimState m) r ix e -> m a)
  -- ^ An action that should fill all elements of the brand new mutable array
  -> m (Array r ix e)
createArrayS_ comp sz action = snd <$> createArrayS comp sz action
{-# INLINE createArrayS_ #-}

-- | Just like `createArray_`, but together with `Array` it returns the result of the filling action.
--
-- @since 0.3.0
--
createArrayS ::
     forall r ix e a m. (Mutable r ix e, PrimMonad m)
  => Comp -- ^ Computation strategy to use after `MArray` gets frozen and onward.
  -> Sz ix -- ^ Size of the newly created array
  -> (MArray (PrimState m) r ix e -> m a)
  -- ^ An action that should fill all elements of the brand new mutable array
  -> m (a, Array r ix e)
createArrayS comp sz action = do
  marr <- new sz
  a <- action marr
  arr <- unsafeFreeze comp marr
  return (a, arr)
{-# INLINE createArrayS #-}

-- | Just like `createArrayS_`, but restricted to `ST`.
--
-- @since 0.3.0
--
createArrayST_ ::
     forall r ix e a. Mutable r ix e
  => Comp
  -> Sz ix
  -> (forall s. MArray s r ix e -> ST s a)
  -> Array r ix e
createArrayST_ comp sz action = runST $ createArrayS_ comp sz action
{-# INLINE createArrayST_ #-}


-- | Just like `createArrayS`, but restricted to `ST`.
--
-- @since 0.2.6
--
createArrayST ::
     forall r ix e a. Mutable r ix e
  => Comp
  -> Sz ix
  -> (forall s. MArray s r ix e -> ST s a)
  -> (a, Array r ix e)
createArrayST comp sz action = runST $ createArrayS comp sz action
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
-- >>> import Data.Massiv.Array
-- >>> import Data.IORef
-- >>> ref <- newIORef (0 :: Int)
-- >>> generateArray Seq (Sz1 6) (\ i -> modifyIORef' ref (+i) >> print i >> pure i) :: IO (Array U Ix1 Int)
-- 0
-- 1
-- 2
-- 3
-- 4
-- 5
-- Array U Seq (Sz1 6)
--   [ 0, 1, 2, 3, 4, 5 ]
-- >>> readIORef ref
-- 15
--
generateArrayS ::
     forall r ix e m. (Mutable r ix e, PrimMonad m)
  => Comp -- ^ Computation strategy (ingored during generation)
  -> Sz ix -- ^ Resulting size of the array
  -> (ix -> m e) -- ^ Element producing generator
  -> m (Array r ix e)
generateArrayS comp sz gen = generateArrayLinearS comp sz (gen . fromLinearIndex sz)
{-# INLINE generateArrayS #-}

-- | Same as `generateArray` but with action takes row-major linear index.
--
-- @since 0.3.0
generateArrayLinearS ::
     forall r ix e m. (Mutable r ix e, PrimMonad m)
  => Comp -- ^ Computation strategy (ingored during generation)
  -> Sz ix -- ^ Resulting size of the array
  -> (Int -> m e) -- ^ Element producing generator
  -> m (Array r ix e)
generateArrayLinearS comp sz gen = do
  marr <- unsafeNew sz
  loopM_ 0 (< totalElem (msize marr)) (+ 1) $ \i -> gen i >>= unsafeLinearWrite marr i
  unsafeFreeze comp marr
{-# INLINE generateArrayLinearS #-}


-- | Just like `generateArrayS`, except this generator __will__ respect the supplied computation
-- strategy, and for that reason it is restricted to `IO`.
--
-- @since 0.2.6
generateArray ::
     forall r ix e m. (MonadUnliftIO m, PrimMonad m, Mutable r ix e)
  => Comp
  -> Sz ix
  -> (ix -> m e)
  -> m (Array r ix e)
generateArray comp sz f = generateArrayLinear comp sz (f . fromLinearIndex sz)
{-# INLINE generateArray #-}

-- | Just like `generateArrayIO`, but action supplied will receive a row-major linear index.
--
-- @since 0.3.0
generateArrayLinear ::
     forall r ix e m. (MonadUnliftIO m, PrimMonad m, Mutable r ix e)
  => Comp
  -> Sz ix
  -> (Int -> m e)
  -> m (Array r ix e)
generateArrayLinear comp sz f = makeMArrayLinear comp sz f >>= unsafeFreeze comp
{-# INLINE generateArrayLinear #-}


-- | Sequentially unfold an array from the left.
--
-- ====__Examples__
--
-- Create an array with Fibonacci numbers while performing and `IO` action on the accumulator for
-- each element of the array.
--
-- >>> import Data.Massiv.Array
-- >>> unfoldrPrimM_ Seq  (Sz1 10) (\a@(f0, f1) -> let fn = f0 + f1 in print a >> return (f0, (f1, fn))) (0, 1) :: IO (Array P Ix1 Int)
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
-- Array P Seq (Sz1 10)
--   [ 0, 1, 1, 2, 3, 5, 8, 13, 21, 34 ]
--
-- @since 0.3.0
--
unfoldrPrimM_ ::
     forall r ix e a m. (Mutable r ix e, PrimMonad m)
  => Comp -- ^ Computation strategy (ignored during initial creation)
  -> Sz ix -- ^ Size of the desired array
  -> (a -> m (e, a)) -- ^ Unfolding action
  -> a -- ^ Initial accumulator
  -> m (Array r ix e)
unfoldrPrimM_ comp sz gen acc0 = snd <$> unfoldrPrimM comp sz gen acc0
{-# INLINE unfoldrPrimM_ #-}

-- | Same as `unfoldrPrimM_` but do the unfolding with index aware function.
--
-- @since 0.3.0
--
iunfoldrPrimM_ ::
     forall r ix e a m. (Mutable r ix e, PrimMonad m)
  => Comp -- ^ Computation strategy (ignored during initial creation)
  -> Sz ix -- ^ Size of the desired array
  -> (a -> ix -> m (e, a)) -- ^ Unfolding action
  -> a -- ^ Initial accumulator
  -> m (Array r ix e)
iunfoldrPrimM_ comp sz gen acc0 = snd <$> iunfoldrPrimM comp sz gen acc0
{-# INLINE iunfoldrPrimM_ #-}


-- | Just like `iunfoldrPrimM_`, but also returns the final value of the accumulator.
--
-- @since 0.3.0
iunfoldrPrimM ::
     forall r ix e a m. (Mutable r ix e, PrimMonad m)
  => Comp -- ^ Computation strategy (ignored during initial creation)
  -> Sz ix -- ^ Size of the desired array
  -> (a -> ix -> m (e, a)) -- ^ Unfolding action
  -> a -- ^ Initial accumulator
  -> m (a, Array r ix e)
iunfoldrPrimM comp sz gen acc0 =
  createArrayS comp sz $ \marr ->
    let sz' = msize marr
     in iterLinearM sz' 0 (totalElem sz') 1 (<) acc0 $ \i ix acc -> do
          (e, acc') <- gen acc ix
          unsafeLinearWrite marr i e
          return acc'
{-# INLINE iunfoldrPrimM #-}

-- | Just like `iunfoldrPrimM`, but do the unfolding with index aware function.
--
-- @since 0.3.0
unfoldrPrimM ::
     forall r ix e a m. (Mutable r ix e, PrimMonad m)
  => Comp -- ^ Computation strategy (ignored during initial creation)
  -> Sz ix -- ^ Size of the desired array
  -> (a -> m (e, a)) -- ^ Unfolding action
  -> a -- ^ Initial accumulator
  -> m (a, Array r ix e)
unfoldrPrimM comp sz gen acc0 =
  createArrayS comp sz $ \marr ->
    let sz' = msize marr
     in loopM 0 (< totalElem sz') (+1) acc0 $ \i acc -> do
          (e, acc') <- gen acc
          unsafeLinearWrite marr i e
          return acc'
{-# INLINE unfoldrPrimM #-}

-- | Sequentially unfold an array from the left.
--
-- ====__Examples__
--
-- Create an array with Fibonacci numbers starting at the end while performing and `IO` action on
-- the accumulator for each element of the array.
--
-- >>> import Data.Massiv.Array
-- >>> unfoldlPrimM_ Seq  (Sz1 10) (\a@(f0, f1) -> let fn = f0 + f1 in print a >> return ((f1, fn), f0)) (0, 1) :: IO (Array P Ix1 Int)
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
-- Array P Seq (Sz1 10)
--   [ 34, 21, 13, 8, 5, 3, 2, 1, 1, 0 ]
--
-- @since 0.3.0
--
unfoldlPrimM_ ::
     forall r ix e a m. (Mutable r ix e, PrimMonad m)
  => Comp -- ^ Computation strategy (ignored during initial creation)
  -> Sz ix -- ^ Size of the desired array
  -> (a -> m (a, e)) -- ^ Unfolding action
  -> a -- ^ Initial accumulator
  -> m (Array r ix e)
unfoldlPrimM_ comp sz gen acc0 = snd <$> unfoldlPrimM comp sz gen acc0
{-# INLINE unfoldlPrimM_ #-}

-- | Same as `unfoldlPrimM_` but do the unfolding with index aware function.
--
-- @since 0.3.0
--
iunfoldlPrimM_ ::
     forall r ix e a m. (Mutable r ix e, PrimMonad m)
  => Comp -- ^ Computation strategy (ignored during initial creation)
  -> Sz ix -- ^ Size of the desired array
  -> (a -> ix -> m (a, e)) -- ^ Unfolding action
  -> a -- ^ Initial accumulator
  -> m (Array r ix e)
iunfoldlPrimM_ comp sz gen acc0 = snd <$> iunfoldlPrimM comp sz gen acc0
{-# INLINE iunfoldlPrimM_ #-}


-- | Just like `iunfoldlPrimM_`, but also returns the final value of the accumulator.
--
-- @since 0.3.0
iunfoldlPrimM ::
     forall r ix e a m. (Mutable r ix e, PrimMonad m)
  => Comp -- ^ Computation strategy (ignored during initial creation)
  -> Sz ix -- ^ Size of the desired array
  -> (a -> ix -> m (a, e)) -- ^ Unfolding action
  -> a -- ^ Initial accumulator
  -> m (a, Array r ix e)
iunfoldlPrimM comp sz gen acc0 =
  createArrayS comp sz $ \marr ->
    let sz' = msize marr
     in iterLinearM sz' (totalElem sz' - 1) 0 (negate 1) (>=) acc0 $ \i ix acc -> do
          (acc', e) <- gen acc ix
          unsafeLinearWrite marr i e
          return acc'
{-# INLINE iunfoldlPrimM #-}

-- | Just like `iunfoldlPrimM`, but do the unfolding with index aware function.
--
-- @since 0.3.0
unfoldlPrimM ::
     forall r ix e a m. (Mutable r ix e, PrimMonad m)
  => Comp -- ^ Computation strategy (ignored during initial creation)
  -> Sz ix -- ^ Size of the desired array
  -> (a -> m (a, e)) -- ^ Unfolding action
  -> a -- ^ Initial accumulator
  -> m (a, Array r ix e)
unfoldlPrimM comp sz gen acc0 =
  createArrayS comp sz $ \marr ->
    let sz' = msize marr
     in loopDeepM 0 (< totalElem sz') (+1) acc0 $ \i acc -> do
          (acc', e) <- gen acc
          unsafeLinearWrite marr i e
          return acc'
{-# INLINE unfoldlPrimM #-}


-- | Sequentially loop over a mutable array while modifying each element with an action.
--
-- @since 0.3.0
forPrimM_ :: (Mutable r ix e, PrimMonad m) => MArray (PrimState m) r ix e -> (e -> m e) -> m ()
forPrimM_ marr f =
  loopM_ 0 (< totalElem (msize marr)) (+1) (unsafeLinearModify marr (const f))
{-# INLINE forPrimM_ #-}


-- | Sequentially loop over a mutable array while modifying each element with an index aware action.
--
-- @since 0.3.0
iforPrimM_ ::
     (Mutable r ix e, PrimMonad m) => MArray (PrimState m) r ix e -> (ix -> e -> m e) -> m ()
iforPrimM_ marr f = iforLinearPrimM_ marr (f . fromLinearIndex (msize marr))
{-# INLINE iforPrimM_ #-}


-- | Sequentially loop over a mutable array while modifying each element with an index aware action.
--
-- @since 0.3.0
iforLinearPrimM_ ::
     (Mutable r ix e, PrimMonad m) => MArray (PrimState m) r ix e -> (Int -> e -> m e) -> m ()
iforLinearPrimM_ marr f = loopM_ 0 (< totalElem (msize marr)) (+ 1) (unsafeLinearModify marr f)
{-# INLINE iforLinearPrimM_ #-}


-- | Create a copy of a pure array, mutate it in place and return its frozen version. The big
-- difference between `withMArrayS` is that it's not only gonna respect the computation strategy
-- supplied to it, but it will also pass extra argumens to the action that suppose to modify the
-- mutable copy of the source array. These two extra arguments are:
--
-- * Number of capabilities derived from the `Comp`utation strategy of the array.
--
-- * An action that can be used to schedule arbitrary number of jobs that will be executed in
--   parallel.
--
-- * And, of course, the mutable array itself.
--
-- @since 0.3.0
withMArray ::
     (Mutable r ix e, MonadUnliftIO m)
  => Array r ix e
  -> (Int -> (m () -> m ()) -> MArray RealWorld r ix e -> m a)
  -> m (Array r ix e)
withMArray arr action = do
  marr <- thaw arr
  withScheduler_ (getComp arr) $ \scheduler ->
    action (numWorkers scheduler) (scheduleWork scheduler) marr
  liftIO $ unsafeFreeze (getComp arr) marr
{-# INLINE withMArray #-}


-- | Create a copy of a pure array, mutate it in place and return its frozen version. The important
-- benefit over doing a manual `thawS` followed by a `freezeS` is that an array will be only copied
-- once.
--
-- @since 0.3.0
withMArrayS ::
     (Mutable r ix e, PrimMonad m)
  => Array r ix e
  -> (MArray (PrimState m) r ix e -> m a)
  -> m (Array r ix e)
withMArrayS arr action = do
  marr <- thawS arr
  _ <- action marr
  unsafeFreeze (getComp arr) marr
{-# INLINE withMArrayS #-}


-- | Same as `withMArrayS` but in `ST`. This is not only pure, but also the safest way to do
-- mutation to the array.
--
-- @since 0.2.2
withMArrayST ::
     Mutable r ix e
  => Array r ix e
  -> (forall s . MArray s r ix e -> ST s a)
  -> Array r ix e
withMArrayST arr f = runST $ withMArrayS arr f
{-# INLINE withMArrayST #-}


-- | /O(1)/ - Lookup an element in the mutable array. Returns `Nothing` when index is out of bounds.
read :: (Mutable r ix e, PrimMonad m) =>
        MArray (PrimState m) r ix e -> ix -> m (Maybe e)
read marr ix =
  if isSafeIndex (msize marr) ix
    then Just <$> unsafeRead marr ix
    else return Nothing
{-# INLINE read #-}


-- | /O(1)/ - Same as `read`, but lives in IO and throws `IndexOutOfBoundsException` on invalid
-- index.
read' :: (Mutable r ix e, MonadThrow m, PrimMonad m) => MArray (PrimState m) r ix e -> ix -> m e
read' marr ix = do
  mval <- read marr ix
  case mval of
    Just e  -> pure e
    Nothing -> throwM $ IndexOutOfBoundsException (msize marr) ix
{-# INLINE read' #-}


-- | /O(1)/ - Write an element into the cell of a mutable array. Returns `False` when index is out
-- of bounds.
write :: (Mutable r ix e, PrimMonad m) => MArray (PrimState m) r ix e -> ix -> e -> m Bool
write marr ix e =
  if isSafeIndex (msize marr) ix
  then unsafeWrite marr ix e >> return True
  else return False
{-# INLINE write #-}


-- | /O(1)/ - Same as `write`, but lives in IO and throws `IndexOutOfBoundsException` on invalid
-- index.
write' ::
     (Mutable r ix e, MonadThrow m, PrimMonad m) => MArray (PrimState m) r ix e -> ix -> e -> m ()
write' marr ix e = write marr ix e >>= (`unless` throwM (IndexOutOfBoundsException (msize marr) ix))
{-# INLINE write' #-}


-- | /O(1)/ - Modify an element in the cell of a mutable array with a supplied function. Returns
-- `False` when index is out of bounds.
modify :: (Mutable r ix e, PrimMonad m) => MArray (PrimState m) r ix e -> (e -> e) -> ix -> m Bool
modify marr f ix =
  if isSafeIndex (msize marr) ix
  then do
    val <- unsafeRead marr ix
    unsafeWrite marr ix $ f val
    return True
  else return False
{-# INLINE modify #-}


-- | /O(1)/ - Same as `modify`, but throws an error if index is out of bounds.
modify' :: (Mutable r ix e, MonadThrow m, PrimMonad m) =>
        MArray (PrimState m) r ix e -> (e -> e) -> ix -> m ()
modify' marr f ix =
  modify marr f ix >>= (`unless` throwM (IndexOutOfBoundsException (msize marr) ix))
{-# INLINE modify' #-}


-- | /O(1)/ - Swap two elements in a mutable array by supplying their indices. Returns `False` when
-- either one of the indices is out of bounds.
swap :: (Mutable r ix e, PrimMonad m) => MArray (PrimState m) r ix e -> ix -> ix -> m Bool
swap marr ix1 ix2 = do
  let sz = msize marr
  if isSafeIndex sz ix1 && isSafeIndex sz ix2
  then do
    val <- unsafeRead marr ix1
    unsafeRead marr ix2 >>= unsafeWrite marr ix1
    unsafeWrite marr ix2 val
    return True
  else return False
{-# INLINE swap #-}


-- | /O(1)/ - Same as `swap`, but throws an `IndexOutOfBoundsException` on invalid indices.
swap' ::
     (Mutable r ix e, MonadThrow m, PrimMonad m) => MArray (PrimState m) r ix e -> ix -> ix -> m ()
swap' marr ix1 ix2 =
  swap marr ix1 ix2 >>=
    (`unless` if isSafeIndex (msize marr) ix1
                then throwM $ IndexOutOfBoundsException (msize marr) ix2
                else throwM $ IndexOutOfBoundsException (msize marr) ix1)
{-# INLINE swap' #-}

