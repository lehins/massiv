{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Data.Massiv.Array.Mutable
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Mutable
  ( -- ** Element-wise mutation
    read
  , read'
  , write
  , write'
  , modify
  , modify'
  , swap
  , swap'
  -- ** Operate over `MArray`
  -- *** Basic conversion
  , new
  , thaw
  , thawS
  , freeze
  , freezeS
  , msize
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
  , unfoldlPrim_
  , unfoldlPrim
  -- *** Mapping
  , forPrim_
  , iforPrim_
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

import           Prelude                             hiding (mapM, read)
import           Control.Monad                       (unless)
import           Control.Monad.ST
import           Data.Massiv.Core.Common
import           Data.Massiv.Scheduler

-- | /O(n)/ - Initialize a new mutable array. All elements will be set to some default value. For
-- Boxed arrays that will be an `Uninitialized` exception, while for othere it will be zeros.
--
-- ==== __Examples__
--
-- >>> :set -XTypeApplications
-- >>> marr <- new @P @Int (Sz2 2 6)
-- >>> freezeS Seq marr
-- (Array P Seq (Sz2 (2 :. 6))
--   [ [ 0, 0, 0, 0, 0, 0 ]
--   , [ 0, 0, 0, 0, 0, 0 ]
--   ]
-- )
--
-- @since 0.1.0
new ::
     forall r e ix m. (Mutable r ix e, PrimMonad m)
  => Sz ix
  -> m (MArray (PrimState m) r ix e)
new = initializeNew Nothing
{-# INLINE new #-}

-- | /O(n)/ - Make a mutable copy of a pure array.
--
-- ==== __Example__
--
-- >>> :set -XTypeApplications
-- >>> a <- resizeM (Sz2 2 5) $ enumFromN @Int Seq 12 (Sz1 10)
-- >>> thaw @P a
-- >>> ma <- thaw @P a
-- >>> modify ma (`mod` 10) (1 :. 2)
-- True
-- >>> freeze Seq ma
-- (Array P Seq (Sz2 (2 :. 5))
--   [ [ 12, 13, 14, 15, 16 ]
--   , [ 17, 18, 9, 20, 21 ]
--   ]
-- )
--
-- @since 0.1.0
thaw :: (Mutable r ix e, MonadIO m) => Array r ix e -> m (MArray RealWorld r ix e)
thaw arr = liftIO $ makeMArrayLinear (getComp arr) (size arr) (pure . unsafeLinearIndexM arr)
-- TODO: use faster memcpy
{-# INLINE thaw #-}


thawS :: (Mutable r ix e, PrimMonad m) => Array r ix e -> m (MArray (PrimState m) r ix e)
thawS arr = makeMArrayLinearS (size arr) (pure . unsafeLinearIndexM arr)
-- TODO: use faster memcpy
{-# INLINE thawS #-}


-- TODO: implement and benchmark parallel `thawIO` and `freezeIO` with memcpy

-- | /O(n)/ - Yield an immutable copy of the mutable array. Note that mutable representations
-- have to be the same.
--
-- ==== __Example__
--
-- >>> ma <- thaw @P $ range Seq 100 (Ix1 110)
--
-- @since 0.1.0
freeze :: (Mutable r ix e, MonadIO m) => Comp -> MArray RealWorld r ix e -> m (Array r ix e)
freeze comp marr = liftIO $ generateArrayLinear comp (msize marr) (unsafeLinearRead marr)
{-# INLINE freeze #-}


-- | Same as `freeze`, but disregard the supplied computation strategy when doing a copy of the
-- mutable array and perform it sequentially. Also, unlike `freeze` that has to be done in `IO`,
-- `freezeS` can be used with `ST`.
--
-- @since 0.3.0
freezeS :: (Mutable r ix e, PrimMonad m) => Comp -> MArray (PrimState m) r ix e -> m (Array r ix e)
freezeS comp marr = generateArrayLinearS comp (msize marr) (unsafeLinearRead marr)
{-# INLINE freezeS #-}


loadArrayS ::
     forall r e ix r' m. (Load r' ix e, Mutable r ix e, PrimMonad m)
  => Array r' ix e
  -> m (MArray (PrimState m) r ix e)
loadArrayS arr = do
  marr <- unsafeNew (size arr)
  loadArrayM 1 id arr (unsafeLinearWrite marr)
  pure marr
{-# INLINE loadArrayS #-}

loadArray ::
     forall r e ix r' m. (Load r' ix e, Mutable r ix e, MonadIO m)
  => Array r' ix e
  -> m (MArray RealWorld r ix e)
loadArray arr = liftIO $ do
  marr <- unsafeNew (size arr)
  withScheduler_ (getComp arr) $ \s ->
    loadArrayM (numWorkers s) (scheduleWork s) arr (unsafeLinearWrite marr)
  pure marr
{-# INLINE loadArray #-}



-- | Compute an Array while loading the results into the supplied mutable target array. Sizes of
-- both arrays must agree, otherwise error.
--
-- @since 0.1.3
computeInto ::
     (Load r' ix e, Mutable r ix e, MonadIO m)
  => MArray RealWorld r ix e -- ^ Target Array
  -> Array r' ix e -- ^ Array to load
  -> m ()
computeInto !mArr !arr =
  liftIO $ do
    unless (msize mArr == size arr) $ throwM $ SizeMismatchException (msize mArr) (size arr)
    withScheduler_ (getComp arr) $ \scheduler ->
      loadArrayM (numWorkers scheduler) (scheduleWork scheduler) arr (unsafeLinearWrite mArr)
{-# INLINE computeInto #-}


-- | Create a mutable array using an index aware generating action.
--
-- @since 0.3.0
makeMArrayS ::
     (Mutable r ix e, PrimMonad m) => Sz ix -> (ix -> m e) -> m (MArray (PrimState m) r ix e)
makeMArrayS sz f = makeMArrayLinearS sz (f . fromLinearIndex sz)
{-# INLINE makeMArrayS #-}


-- | Same as `makeMArrayS`, but index supplied to the action is row-major linear index.
--
-- @since 0.3.0
makeMArrayLinearS ::
     (Mutable r ix e, PrimMonad m) => Sz ix -> (Int -> m e) -> m (MArray (PrimState m) r ix e)
makeMArrayLinearS sz f = do
  marr <- unsafeNew sz
  loopM_ 0 (< totalElem (msize marr)) (+ 1) (\ !i -> f i >>= unsafeLinearWrite marr i)
  return marr
{-# INLINE makeMArrayLinearS #-}

-- | Just like `makeMArrayS`, but also accepts computation strategy and runs in `IO`.
--
-- @since 0.3.0
makeMArray ::
     (PrimMonad m, MonadUnliftIO m, Mutable r ix e)
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
     (PrimMonad m, MonadUnliftIO m, Mutable r ix e)
  => Comp
  -> Sz ix
  -> (Int -> m e)
  -> m (MArray (PrimState m) r ix e)
makeMArrayLinear comp sz f = do
  marr <- unsafeNew sz
  withScheduler_ comp $ \s ->
    splitLinearlyWithM_ (numWorkers s) (scheduleWork s) (totalElem sz) f (unsafeLinearWrite marr)
  return marr
{-# INLINE makeMArrayLinear #-}




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
     (Mutable r ix e, PrimMonad m, MonadUnliftIO m)
  => Comp -- ^ Computation strategy to use after `MArray` gets frozen and onward.
  -> Sz ix -- ^ Size of the newly created array
  -> (Int -> (m () -> m ()) -> MArray (PrimState m) r ix e -> m a)
  -- ^ An action that should fill all elements of the brand new mutable array
  -> m (Array r ix e)
createArray_ comp sz action = do
  marr <- new sz
  withScheduler_ comp $ \scheduler ->
    action (numWorkers scheduler) (scheduleWork_ scheduler) marr
  unsafeFreeze comp marr
{-# INLINE createArray_ #-}

-- | Just like `createArray_`, but together with `Array` it returns the result of the filling action.
--
-- @since 0.3.0
--
createArray ::
     (Mutable r ix e, PrimMonad m, MonadUnliftIO m)
  => Comp -- ^ Computation strategy to use after `MArray` gets frozen and onward.
  -> Sz ix -- ^ Size of the newly created array
  -> (Int -> (m a -> m ()) -> MArray (PrimState m) r ix e -> m [a])
  -- ^ An action that should fill all elements of the brand new mutable array
  -> m ([a], Array r ix e)
createArray comp sz action = do
  marr <- new sz
  a <- withScheduler comp $ \scheduler ->
    action (numWorkers scheduler) (scheduleWork scheduler) marr
  arr <- unsafeFreeze comp marr
  return (a, arr)
{-# INLINE createArray #-}


-- | Create a new array by supplying an action that will fill the new blank mutable array. Use
-- `createArray` if you'd like to keep the result of the filling function.
--
-- ====__Examples__
--
-- >>> createArray_ Seq (Ix1 2) (\ marr -> write marr 0 10 >> write marr 1 11) :: IO (Array P Ix1 Int)
-- (Array P Seq (2)
--   [ 10,11 ])
--
-- @since 0.3.0
--
createArrayS_ ::
     (Mutable r ix e, PrimMonad m)
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
     (Mutable r ix e, PrimMonad m)
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
     Mutable r ix e => Comp -> Sz ix -> (forall s. MArray s r ix e -> ST s a) -> Array r ix e
createArrayST_ comp sz action = runST $ createArrayS_ comp sz action
{-# INLINE createArrayST_ #-}


-- | Just like `createArrayS`, but restricted to `ST`.
--
-- @since 0.2.6
--
createArrayST ::
     Mutable r ix e => Comp -> Sz ix -> (forall s. MArray s r ix e -> ST s a) -> (a, Array r ix e)
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
generateArrayS ::
     (Mutable r ix e, PrimMonad m)
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
     (Mutable r ix e, PrimMonad m)
  => Comp -- ^ Computation strategy (ingored during generation)
  -> Sz ix -- ^ Resulting size of the array
  -> (Int -> m e) -- ^ Element producing generator
  -> m (Array r ix e)
generateArrayLinearS comp sz gen = do
  marr <- unsafeNew sz
  loopM_ 0 (< totalElem (msize marr)) (+ 1) $ \i -> gen i >>= unsafeLinearWrite marr i
  unsafeFreeze comp marr
{-# INLINE generateArrayLinearS #-}


-- | Just like `generateArray`, except this generator __will__ respect the supplied computation
-- strategy, and for that reason it is restricted to `IO`.
--
-- @since 0.2.6
generateArray ::
     (MonadUnliftIO m, PrimMonad m, Mutable r ix e)
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
     (MonadUnliftIO m, PrimMonad m, Mutable r ix e)
  => Comp
  -> Sz ix
  -> (Int -> m e)
  -> m (Array r ix e)
generateArrayLinear comp sz f = makeMArrayLinear comp sz f >>= unsafeFreeze comp
{-# INLINE generateArrayLinear #-}


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
  -> Sz ix -- ^ Size of the desired array
  -> (a -> ix -> m (a, e)) -- ^ Unfolding action
  -> a -- ^ Initial accumulator
  -> m (Array r ix e)
unfoldlPrim_ comp sz gen acc0 = fmap snd $ unfoldlPrim comp sz gen acc0
{-# INLINE unfoldlPrim_ #-}


-- | Just like `unfoldlPrim_`, but also returns the final value of the accumulator.
--
-- @since 0.2.6
unfoldlPrim ::
     (Mutable r ix e, PrimMonad m)
  => Comp -- ^ Computation strategy (ignored during initial creation)
  -> Sz ix -- ^ Size of the desired array
  -> (a -> ix -> m (a, e)) -- ^ Unfolding action
  -> a -- ^ Initial accumulator
  -> m (a, Array r ix e)
unfoldlPrim comp sz gen acc0 =
  createArrayS comp sz $ \marr ->
    let sz' = msize marr
     in iterLinearM sz' 0 (totalElem sz') 1 (<) acc0 $ \i ix acc -> do
          (acc', e) <- gen acc ix
          unsafeLinearWrite marr i e
          return acc'
{-# INLINE unfoldlPrim #-}


-- | Sequentially loop over a mutable array while modifying each element with an action.
--
-- @since 0.3.0
forPrim_ :: (Mutable r ix e, PrimMonad m) => MArray (PrimState m) r ix e -> (e -> m e) -> m ()
forPrim_ marr f = do
  loopM_ 0 (< totalElem (msize marr)) (+1) (unsafeLinearModify marr (const f))
{-# INLINE forPrim_ #-}


-- | Sequentially loop over a mutable array while modifying each element with an index aware action.
--
-- @since 0.3.0
iforPrim_ ::
     (Mutable r ix e, PrimMonad m) => MArray (PrimState m) r ix e -> (ix -> e -> m e) -> m ()
iforPrim_ marr f = iforLinearPrim_ marr (f . fromLinearIndex (msize marr))
{-# INLINE iforPrim_ #-}


-- | Sequentially loop over a mutable array while modifying each element with an index aware action.
--
-- @since 0.3.0
iforLinearPrim_ ::
     (Mutable r ix e, PrimMonad m) => MArray (PrimState m) r ix e -> (Int -> e -> m e) -> m ()
iforLinearPrim_ marr f = loopM_ 0 (< totalElem (msize marr)) (+ 1) (unsafeLinearModify marr f)
{-# INLINE iforLinearPrim_ #-}


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
    action (numWorkers scheduler) (scheduleWork_ scheduler) marr
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


-- | /O(1)/ - Same as `read`, but lives in IO and throws an `IndexOutOfBoundsException` on invalid
-- index.
read' :: (Mutable r ix e, MonadThrow m, PrimMonad m) => MArray (PrimState m) r ix e -> ix -> m e
read' marr ix = do
  mval <- read marr ix
  case mval of
    Just e -> pure e
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


-- | /O(1)/ - Same as `write`, but lives in IO and throws an `IndexOutOfBoundsException` on invalid
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


-- | /O(1)/ - Same as `reaswap`, but throws an `IndexOutOfBoundsException` on invalid indices.
swap' ::
     (Mutable r ix e, MonadThrow m, PrimMonad m) => MArray (PrimState m) r ix e -> ix -> ix -> m ()
swap' marr ix1 ix2 = do
  swap marr ix1 ix2 >>=
    (`unless` if isSafeIndex (msize marr) ix1
                then throwM $ IndexOutOfBoundsException (msize marr) ix2
                else throwM $ IndexOutOfBoundsException (msize marr) ix1)
{-# INLINE swap' #-}

