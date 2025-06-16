{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Data.Massiv.Array.Mutable
-- Copyright   : (c) Alexey Kuleshevich 2018-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
module Data.Massiv.Array.Mutable (
  -- ** Size
  sizeOfMArray,
  msize,
  resizeMArrayM,
  flattenMArray,
  outerSliceMArrayM,
  outerSlicesMArray,

  -- ** Element-wise mutation
  read,
  readM,
  write,
  write_,
  writeM,
  modify,
  modify_,
  modifyM,
  modifyM_,
  swap,
  swap_,
  swapM,
  swapM_,
  zipSwapM_,

  -- ** Operations on @MArray@

  -- *** Immutable conversion
  thaw,
  thawS,
  freeze,
  freezeS,

  -- *** Create mutable
  newMArray,
  newMArray',
  makeMArray,
  makeMArrayLinear,
  makeMArrayS,
  makeMArrayLinearS,

  -- *** Create pure
  createArray_,
  createArray,
  createArrayS_,
  createArrayS,
  createArrayST_,
  createArrayST,

  -- *** Generate
  generateArray,
  generateArrayLinear,
  generateArrayS,
  generateArrayLinearS,
  generateSplitSeedArray,

  -- *** Stateful worker threads
  generateArrayWS,
  generateArrayLinearWS,

  -- *** Unfold
  unfoldrPrimM_,
  iunfoldrPrimM_,
  unfoldrPrimM,
  iunfoldrPrimM,
  unfoldlPrimM_,
  iunfoldlPrimM_,
  unfoldlPrimM,
  iunfoldlPrimM,

  -- *** Mapping
  forPrimM,
  forPrimM_,
  iforPrimM,
  iforPrimM_,
  iforLinearPrimM,
  iforLinearPrimM_,
  for2PrimM_,
  ifor2PrimM_,

  -- *** Modify
  withMArray,
  withMArray_,
  withLoadMArray_,
  withMArrayS,
  withLoadMArrayS,
  withMArrayS_,
  withLoadMArrayS_,
  withMArrayST,
  withLoadMArrayST,
  withMArrayST_,
  withLoadMArrayST_,

  -- *** Initialize
  initialize,
  initializeNew,

  -- ** Computation
  Manifest,
  MArray,
  RealWorld,
  computeInto,
  loadArray,
  loadArrayS,
) where

-- TODO: add fromListM, et al.

import Control.Monad (unless, void, when, (>=>))
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Scheduler
import Data.IORef
import Data.Massiv.Array.Delayed.Pull (D)
import Data.Massiv.Array.Mutable.Internal
import Data.Massiv.Core.Common
import Data.Maybe (fromMaybe)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (mapM, read)

-- | /O(1)/ - Change the size of a mutable array. Throws
-- `SizeElementsMismatchException` if total number of elements does not match
-- the supplied array.
--
-- @since 1.0.0
resizeMArrayM
  :: (Manifest r e, Index ix', Index ix, MonadThrow m)
  => Sz ix'
  -> MArray s r ix e
  -> m (MArray s r ix' e)
resizeMArrayM sz marr =
  unsafeResizeMArray sz marr <$ guardNumberOfElements (sizeOfMArray marr) sz
{-# INLINE resizeMArrayM #-}

-- | /O(1)/ - Change a mutable array to a mutable vector.
--
-- @since 1.0.0
flattenMArray :: (Manifest r e, Index ix) => MArray s r ix e -> MVector s r e
flattenMArray marr = unsafeResizeMArray (toLinearSz (sizeOfMArray marr)) marr
{-# INLINE flattenMArray #-}

-- | /O(1)/ - Slice a mutable array from the outside, while reducing its
-- dimensionality by one. Same as `Data.Massiv.Array.!?>` operator, but for
-- mutable arrays.
--
-- @since 1.0.0
outerSliceMArrayM
  :: forall r ix e m s
   . (MonadThrow m, Index (Lower ix), Index ix, Manifest r e)
  => MArray s r ix e
  -> Ix1
  -> m (MArray s r (Lower ix) e)
outerSliceMArrayM !marr !i = do
  let (k, szL) = unconsSz (sizeOfMArray marr)
  unless (isSafeIndex k i) $ throwM $ IndexOutOfBoundsException k i
  pure $ unsafeResizeMArray szL $ unsafeLinearSliceMArray (i * totalElem szL) (toLinearSz szL) marr
{-# INLINE outerSliceMArrayM #-}

-- | /O(1)/ - Take all outer slices of a mutable array and construct a delayed
-- vector out of them. In other words it applies `outerSliceMArrayM` to each
-- outer index. Same as `Data.Massiv.Array.outerSlices` function, but for
-- mutable arrays.
--
-- ====__Examples__
--
-- >>> import Data.Massiv.Array as A
-- >>> arr <- resizeM (Sz2 4 7) $ makeArrayR P Seq (Sz1 28) (+10)
-- >>> arr
-- Array P Seq (Sz (4 :. 7))
--   [ [ 10, 11, 12, 13, 14, 15, 16 ]
--   , [ 17, 18, 19, 20, 21, 22, 23 ]
--   , [ 24, 25, 26, 27, 28, 29, 30 ]
--   , [ 31, 32, 33, 34, 35, 36, 37 ]
--   ]
--
-- Here we can see we can get individual rows from a mutable matrix
--
-- >>> marr <- thawS arr
-- >>> import Control.Monad ((<=<))
-- >>> mapIO_ (print <=< freezeS)  $ outerSlicesMArray Seq marr
-- Array P Seq (Sz1 7)
--   [ 10, 11, 12, 13, 14, 15, 16 ]
-- Array P Seq (Sz1 7)
--   [ 17, 18, 19, 20, 21, 22, 23 ]
-- Array P Seq (Sz1 7)
--   [ 24, 25, 26, 27, 28, 29, 30 ]
-- Array P Seq (Sz1 7)
--   [ 31, 32, 33, 34, 35, 36, 37 ]
--
-- For the sake of example what if our goal was to mutate array in such a way
-- that rows from the top half were swapped with the bottom half:
--
-- >>> (top, bottom) <- splitAtM 1 2 $ outerSlicesMArray Seq marr
-- >>> mapIO_ (print <=< freezeS) top
-- Array P Seq (Sz1 7)
--   [ 10, 11, 12, 13, 14, 15, 16 ]
-- Array P Seq (Sz1 7)
--   [ 17, 18, 19, 20, 21, 22, 23 ]
-- >>> mapIO_ (print <=< freezeS) bottom
-- Array P Seq (Sz1 7)
--   [ 24, 25, 26, 27, 28, 29, 30 ]
-- Array P Seq (Sz1 7)
--   [ 31, 32, 33, 34, 35, 36, 37 ]
-- >>> szipWithM_ (zipSwapM_ 0) top bottom
-- >>> freezeS marr
-- Array P Seq (Sz (4 :. 7))
--   [ [ 24, 25, 26, 27, 28, 29, 30 ]
--   , [ 31, 32, 33, 34, 35, 36, 37 ]
--   , [ 10, 11, 12, 13, 14, 15, 16 ]
--   , [ 17, 18, 19, 20, 21, 22, 23 ]
--   ]
--
-- @since 1.0.0
outerSlicesMArray
  :: forall r ix e s
   . (Index (Lower ix), Index ix, Manifest r e)
  => Comp
  -> MArray s r ix e
  -> Vector D (MArray s r (Lower ix) e)
outerSlicesMArray comp marr =
  makeArray comp k (\i -> unsafeResizeMArray szL $ unsafeLinearSliceMArray (i * unSz kL) kL marr)
  where
    kL = toLinearSz szL
    (k, szL) = unconsSz $ sizeOfMArray marr
{-# INLINE outerSlicesMArray #-}

-- | /O(n)/ - Initialize a new mutable array. All elements will be set to some default value. For
-- boxed arrays it will be a thunk with `Uninitialized` exception, while for others it will be
-- simply zeros.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> marr <- newMArray' (Sz2 2 6) :: IO (MArray RealWorld P Ix2 Int)
-- >>> freeze Seq marr
-- Array P Seq (Sz (2 :. 6))
--   [ [ 0, 0, 0, 0, 0, 0 ]
--   , [ 0, 0, 0, 0, 0, 0 ]
--   ]
--
-- Or using @TypeApplications@:
--
-- >>> :seti -XTypeApplications
-- >>> newMArray' @P @Ix2 @Int (Sz2 2 6) >>= freezeS
-- Array P Seq (Sz (2 :. 6))
--   [ [ 0, 0, 0, 0, 0, 0 ]
--   , [ 0, 0, 0, 0, 0, 0 ]
--   ]
-- >>> newMArray' @B @_ @Int (Sz2 2 6) >>= freezeS
-- *** Exception: Uninitialized
--
-- @since 0.6.0
newMArray'
  :: forall r ix e m
   . (Manifest r e, Index ix, PrimMonad m)
  => Sz ix
  -> m (MArray (PrimState m) r ix e)
newMArray' sz = unsafeNew sz >>= \ma -> ma <$ initialize ma
{-# INLINE newMArray' #-}

-- | /O(n)/ - Make a mutable copy of a pure array. Keep in mind that both `freeze` and `thaw` trigger a
-- copy of the full array.
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Array
-- >>> :seti -XTypeApplications
-- >>> arr <- fromListsM @U @Ix2 @Double Par [[12,21],[13,31]]
-- >>> marr <- thaw arr
-- >>> modify marr (pure . (+ 10)) (1 :. 0)
-- Just 13.0
-- >>> freeze Par marr
-- Array U Par (Sz (2 :. 2))
--   [ [ 12.0, 21.0 ]
--   , [ 23.0, 31.0 ]
--   ]
--
-- @since 0.1.0
thaw
  :: forall r ix e m. (Manifest r e, Index ix, MonadIO m) => Array r ix e -> m (MArray RealWorld r ix e)
thaw arr =
  liftIO $ do
    let sz = size arr
        totalLength = totalElem sz
    marr <- unsafeNew sz
    withMassivScheduler_ (getComp arr) $ \scheduler ->
      splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
        loopA_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
          scheduleWork_ scheduler $ unsafeArrayLinearCopy arr start marr start (SafeSz chunkLength)
        let slackLength = totalLength - slackStart
        when (slackLength > 0) $
          scheduleWork_ scheduler $
            unsafeArrayLinearCopy arr slackStart marr slackStart (SafeSz slackLength)
    pure marr
{-# INLINE thaw #-}

-- | Same as `thaw`, but restrict computation to sequential only.
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Array
-- >>> :seti -XOverloadedLists
-- >>> thawS @P @Ix1 @Double [1..10]
-- >>> marr <- thawS @P @Ix1 @Double [1..10]
-- >>> writeM marr 5 100
-- >>> freezeS marr
-- Array P Seq (Sz1 10)
--   [ 1.0, 2.0, 3.0, 4.0, 5.0, 100.0, 7.0, 8.0, 9.0, 10.0 ]
--
-- @since 0.3.0
thawS
  :: forall r ix e m
   . (Manifest r e, Index ix, PrimMonad m)
  => Array r ix e
  -> m (MArray (PrimState m) r ix e)
thawS arr = do
  tmarr <- unsafeNew (size arr)
  unsafeArrayLinearCopy arr 0 tmarr 0 (SafeSz (totalElem (size arr)))
  pure tmarr
{-# INLINE thawS #-}

-- | /O(n)/ - Yield an immutable copy of the mutable array. Note that mutable representations
-- have to be the same.
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Array
-- >>> marr <- newMArray @P (Sz2 2 6) (0 :: Int)
-- >>> forM_ (range Seq 0 (Ix2 1 4)) $ \ix -> write marr ix 9
-- >>> freeze Seq marr
-- Array P Seq (Sz (2 :. 6))
--   [ [ 9, 9, 9, 9, 0, 0 ]
--   , [ 0, 0, 0, 0, 0, 0 ]
--   ]
--
-- @since 0.1.0
freeze
  :: forall r ix e m
   . (Manifest r e, Index ix, MonadIO m)
  => Comp
  -> MArray RealWorld r ix e
  -> m (Array r ix e)
freeze comp smarr =
  liftIO $ do
    let sz = sizeOfMArray smarr
        totalLength = totalElem sz
    tmarr <- unsafeNew sz
    withMassivScheduler_ comp $ \scheduler ->
      splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
        loopA_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
          scheduleWork_ scheduler $ unsafeLinearCopy smarr start tmarr start (SafeSz chunkLength)
        let slackLength = totalLength - slackStart
        when (slackLength > 0) $
          scheduleWork_ scheduler $
            unsafeLinearCopy smarr slackStart tmarr slackStart (SafeSz slackLength)
    unsafeFreeze comp tmarr
{-# INLINE freeze #-}

-- | Same as `freeze`, but do the copy of supplied muable array sequentially. Also, unlike `freeze`
-- that has to be done in `IO`, `freezeS` can be used with `ST`.
--
-- @since 0.3.0
freezeS
  :: forall r ix e m
   . (Manifest r e, Index ix, PrimMonad m)
  => MArray (PrimState m) r ix e
  -> m (Array r ix e)
freezeS smarr = do
  let sz = sizeOfMArray smarr
  tmarr <- unsafeNew sz
  unsafeLinearCopy smarr 0 tmarr 0 (SafeSz (totalElem sz))
  unsafeFreeze Seq tmarr
{-# INLINE freezeS #-}

unsafeNewUpper
  :: (Load r' ix e, Manifest r e, PrimMonad m) => Array r' ix e -> m (MArray (PrimState m) r Ix1 e)
unsafeNewUpper !arr = unsafeNew (fromMaybe zeroSz (maxLinearSize arr))
{-# INLINE unsafeNewUpper #-}

-- | Load sequentially a pure array into the newly created mutable array.
--
-- @since 0.3.0
loadArrayS
  :: forall r ix e r' m
   . (Load r' ix e, Manifest r e, PrimMonad m)
  => Array r' ix e
  -> m (MArray (PrimState m) r ix e)
loadArrayS arr = do
  marr <- unsafeNewUpper arr
  stToPrim $ unsafeLoadIntoST marr arr
{-# INLINE loadArrayS #-}

-- | Load a pure array into the newly created mutable array, while respecting computation startegy.
--
-- @since 0.3.0
loadArray
  :: forall r ix e r' m
   . (Load r' ix e, Manifest r e, MonadIO m)
  => Array r' ix e
  -> m (MArray RealWorld r ix e)
loadArray arr =
  liftIO $ do
    marr <- unsafeNewUpper arr
    unsafeLoadIntoIO marr arr
{-# INLINE loadArray #-}

-- | Compute an Array while loading the results into the supplied mutable target array. Number of
-- elements for arrays must agree, otherwise `SizeElementsMismatchException` exception is thrown.
--
-- @since 0.1.3
computeInto
  :: (Load r' ix' e, Manifest r e, Index ix, MonadIO m)
  => MArray RealWorld r ix e
  -- ^ Target Array
  -> Array r' ix' e
  -- ^ Array to load
  -> m ()
computeInto !mArr !arr =
  liftIO $ do
    let sz = outerSize arr
    unless (totalElem (sizeOfMArray mArr) == totalElem sz) $
      throwM $
        SizeElementsMismatchException (sizeOfMArray mArr) sz
    withMassivScheduler_ (getComp arr) $ \scheduler ->
      stToPrim $ iterArrayLinearST_ scheduler arr (unsafeLinearWrite mArr)
{-# INLINE computeInto #-}

-- | Create a mutable array using an index aware generating action.
--
-- @since 0.3.0
makeMArrayS
  :: forall r ix e m
   . (Manifest r e, Index ix, PrimMonad m)
  => Sz ix
  -- ^ Size of the create array
  -> (ix -> m e)
  -- ^ Element generating action
  -> m (MArray (PrimState m) r ix e)
makeMArrayS sz f = makeMArrayLinearS sz (f . fromLinearIndex sz)
{-# INLINE makeMArrayS #-}

-- | Same as `makeMArrayS`, but index supplied to the action is row-major linear index.
--
-- @since 0.3.0
makeMArrayLinearS
  :: forall r ix e m
   . (Manifest r e, Index ix, PrimMonad m)
  => Sz ix
  -> (Int -> m e)
  -> m (MArray (PrimState m) r ix e)
makeMArrayLinearS sz f = do
  marr <- unsafeNew sz
  loopA_ 0 (< totalElem (sizeOfMArray marr)) (+ 1) (\ !i -> f i >>= unsafeLinearWrite marr i)
  return marr
{-# INLINE makeMArrayLinearS #-}

-- | Just like `makeMArrayS`, but also accepts computation strategy and runs in `IO`.
--
-- @since 0.3.0
makeMArray
  :: forall r ix e m
   . (MonadUnliftIO m, Manifest r e, Index ix)
  => Comp
  -> Sz ix
  -> (ix -> m e)
  -> m (MArray RealWorld r ix e)
makeMArray comp sz f = makeMArrayLinear comp sz (f . fromLinearIndex sz)
{-# INLINE makeMArray #-}

-- | Just like `makeMArrayLinearS`, but also accepts computation strategy and runs in `IO`.
--
-- @since 0.3.0
makeMArrayLinear
  :: forall r ix e m
   . (MonadUnliftIO m, Manifest r e, Index ix)
  => Comp
  -> Sz ix
  -> (Int -> m e)
  -> m (MArray RealWorld r ix e)
makeMArrayLinear comp sz f = do
  marr <- liftIO $ unsafeNew sz
  withScheduler_ comp $ \scheduler ->
    withRunInIO $ \run ->
      splitLinearlyWithM_ scheduler (totalElem sz) (run . f) (unsafeLinearWrite marr)
  return marr
{-# INLINE makeMArrayLinear #-}

-- | Create a new array by supplying an action that will fill the new blank mutable array. Use
-- `createArray` if you'd like to keep the result of the filling function.
--
-- ====__Examples__
--
-- >>> :seti -XTypeApplications
-- >>> import Data.Massiv.Array
-- >>> createArray_ @P @_ @Int Seq (Sz1 2) (\ s marr -> scheduleWork s (writeM marr 0 10) >> scheduleWork s (writeM marr 1 11))
-- Array P Seq (Sz1 2)
--   [ 10, 11 ]
--
-- @since 0.3.0
createArray_
  :: forall r ix e a m
   . (Manifest r e, Index ix, MonadUnliftIO m)
  => Comp
  -- ^ Computation strategy to use after `MArray` gets frozen and onward.
  -> Sz ix
  -- ^ Size of the newly created array
  -> (Scheduler RealWorld () -> MArray RealWorld r ix e -> m a)
  -- ^ An action that should fill all elements of the brand new mutable array
  -> m (Array r ix e)
createArray_ comp sz action = do
  marr <- liftIO $ newMArray' sz
  withScheduler_ comp (`action` marr)
  liftIO $ unsafeFreeze comp marr
{-# INLINE createArray_ #-}

-- | Just like `createArray_`, but together with `Array` it returns results of scheduled filling
-- actions.
--
-- @since 0.3.0
createArray
  :: forall r ix e a m b
   . (Manifest r e, Index ix, MonadUnliftIO m)
  => Comp
  -- ^ Computation strategy to use after `MArray` gets frozen and onward.
  -> Sz ix
  -- ^ Size of the newly created array
  -> (Scheduler RealWorld a -> MArray RealWorld r ix e -> m b)
  -- ^ An action that should fill all elements of the brand new mutable array
  -> m ([a], Array r ix e)
createArray comp sz action = do
  marr <- liftIO $ newMArray' sz
  a <- withScheduler comp (`action` marr)
  arr <- liftIO $ unsafeFreeze comp marr
  return (a, arr)
{-# INLINE createArray #-}

-- | Create a new array by supplying an action that will fill the new blank mutable array. Use
-- `createArrayS` if you'd like to keep the result of the filling function.
--
-- ====__Examples__
--
-- >>> :seti -XTypeApplications
-- >>> import Data.Massiv.Array
-- >>> createArrayS_ @P @_ @Int (Sz1 2) (\ marr -> write marr 0 10 >> write marr 1 12)
-- Array P Seq (Sz1 2)
--   [ 10, 12 ]
--
-- @since 0.3.0
createArrayS_
  :: forall r ix e a m
   . (Manifest r e, Index ix, PrimMonad m)
  => Sz ix
  -- ^ Size of the newly created array
  -> (MArray (PrimState m) r ix e -> m a)
  -- ^ An action that should fill all elements of the brand new mutable array
  -> m (Array r ix e)
createArrayS_ sz action = snd <$> createArrayS sz action
{-# INLINE createArrayS_ #-}

-- | Just like `createArray_`, but together with `Array` it returns the result of the filling action.
--
-- @since 0.3.0
createArrayS
  :: forall r ix e a m
   . (Manifest r e, Index ix, PrimMonad m)
  => Sz ix
  -- ^ Size of the newly created array
  -> (MArray (PrimState m) r ix e -> m a)
  -- ^ An action that should fill all elements of the brand new mutable array
  -> m (a, Array r ix e)
createArrayS sz action = do
  marr <- newMArray' sz
  a <- action marr
  arr <- unsafeFreeze Seq marr
  return (a, arr)
{-# INLINE createArrayS #-}

-- | Just like `createArrayS_`, but restricted to `ST`.
--
-- @since 0.3.0
createArrayST_
  :: forall r ix e a
   . (Manifest r e, Index ix)
  => Sz ix
  -> (forall s. MArray s r ix e -> ST s a)
  -> Array r ix e
createArrayST_ sz action = runST $ createArrayS_ sz action
{-# INLINE createArrayST_ #-}

-- | Just like `createArrayS`, but restricted to `ST`.
--
-- @since 0.2.6
createArrayST
  :: forall r ix e a
   . (Manifest r e, Index ix)
  => Sz ix
  -> (forall s. MArray s r ix e -> ST s a)
  -> (a, Array r ix e)
createArrayST sz action = runST $ createArrayS sz action
{-# INLINE createArrayST #-}

-- | Sequentially generate a pure array. Much like `makeArray` creates a pure array this
-- function will use `Manifest` interface to generate a pure `Array` in the end, except that
-- computation strategy is set to `Seq`. Element producing function no longer has to be pure
-- but is a stateful action, becuase it is restricted to `PrimMonad` thus allows for sharing
-- the state between computation of each element.
--
-- ====__Examples__
--
-- >>> import Data.Massiv.Array
-- >>> import Data.IORef
-- >>> ref <- newIORef (0 :: Int)
-- >>> generateArrayS (Sz1 6) (\ i -> modifyIORef' ref (+i) >> print i >> pure i) :: IO (Array U Ix1 Int)
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
-- @since 0.2.6
generateArrayS
  :: forall r ix e m
   . (Manifest r e, Index ix, PrimMonad m)
  => Sz ix
  -- ^ Size of the array
  -> (ix -> m e)
  -- ^ Element producing action
  -> m (Array r ix e)
generateArrayS sz gen = generateArrayLinearS sz (gen . fromLinearIndex sz)
{-# INLINE generateArrayS #-}

-- | Same as `generateArray` but with action that accepts row-major linear index.
--
-- @since 0.3.0
generateArrayLinearS
  :: forall r ix e m
   . (Manifest r e, Index ix, PrimMonad m)
  => Sz ix
  -- ^ Resulting size of the array
  -> (Int -> m e)
  -- ^ Element producing generator
  -> m (Array r ix e)
generateArrayLinearS sz gen = do
  marr <- unsafeNew sz
  loopA_ 0 (< totalElem (sizeOfMArray marr)) (+ 1) $ \i -> gen i >>= unsafeLinearWrite marr i
  unsafeFreeze Seq marr
{-# INLINE generateArrayLinearS #-}

-- | Just like `generateArrayS`, except this generator __will__ respect the supplied computation
-- strategy, and for that reason it is restricted to `IO`.
--
-- @since 0.2.6
generateArray
  :: forall r ix e m
   . (MonadUnliftIO m, Manifest r e, Index ix)
  => Comp
  -> Sz ix
  -> (ix -> m e)
  -> m (Array r ix e)
generateArray comp sz f = generateArrayLinear comp sz (f . fromLinearIndex sz)
{-# INLINE generateArray #-}

-- | Just like `generateArray`, except generating action will receive a row-major linear
-- index.
--
-- @since 0.3.0
generateArrayLinear
  :: forall r ix e m
   . (MonadUnliftIO m, Manifest r e, Index ix)
  => Comp
  -> Sz ix
  -> (Ix1 -> m e)
  -> m (Array r ix e)
generateArrayLinear comp sz f = makeMArrayLinear comp sz f >>= liftIO . unsafeFreeze comp
{-# INLINE generateArrayLinear #-}

-- | Similar to `Data.Massiv.Array.makeSplitSeedArray`, except it will produce a
-- Manifest array and will return back the last unused seed together with all
-- final seeds produced by each scheduled job. This function can be thought of
-- as an unfolding done in parallel while iterating in a customizable manner.
--
-- @since 1.0.2
generateSplitSeedArray
  :: forall r ix e g it
   . (Iterator it, Manifest r e, Index ix)
  => it
  -- ^ Iterator
  -> g
  -- ^ Initial seed
  -> (forall s. g -> ST s (g, g))
  -- ^ An ST action that can split a seed into two independent seeds. It will
  -- be called the same number of times as the number of jobs that will get
  -- scheduled during parallelization. Eg. only once for the sequential case.
  -> Comp
  -- ^ Computation strategy.
  -> Sz ix
  -- ^ Resulting size of the array.
  -> (forall s. Ix1 -> ix -> g -> ST s (e, g))
  -- ^ An ST action that produces a value and the next seed. It takes both
  -- versions of the index, in linear and in multi-dimensional forms, as well
  -- as the current seeding value. Returns the element for the array cell
  -- together with the new seed that will be used for the next element
  -- generation
  -> (g, [g], Array r ix e)
  -- ^ Returned values are:
  --
  -- * The final split of the supplied seed.
  --
  -- * Results of scheduled jobs in the same order that they where scheduled
  --
  -- * Final array that was fully filled using the supplied action and iterator.
generateSplitSeedArray it seed splitSeed comp sz genFunc =
  unsafePerformIO $ do
    marr <- unsafeNew sz
    ref <- newIORef Nothing
    res <- withSchedulerR comp $ \scheduler -> do
      fin <- stToIO $
        iterTargetFullAccST it scheduler 0 sz seed splitSeed $ \ !i ix !g ->
          genFunc i ix g >>= \(x, g') -> g' <$ unsafeLinearWrite marr i x
      writeIORef ref $ Just fin
    mFin <- readIORef ref
    case res of
      Finished gs
        | Just fin <- mFin -> do
            arr <- unsafeFreeze comp marr
            pure (fin, gs, arr)
      -- This case does not make much sence for array filling and can only
      -- happen with a custom 'Iterator' defined outside massiv, therefore it is
      -- ok to not support it.
      _ ->
        error $
          "Parallelized array filling finished prematurely. "
            ++ "This feature is not supported by the 'generateSplitSeedArray' function."
{-# INLINE generateSplitSeedArray #-}

-- | Same as `generateArrayWS`, but use linear indexing instead.
--
-- @since 0.3.4
generateArrayLinearWS
  :: forall r ix e s m
   . (Manifest r e, Index ix, MonadUnliftIO m, PrimMonad m)
  => WorkerStates s
  -> Sz ix
  -> (Int -> s -> m e)
  -> m (Array r ix e)
generateArrayLinearWS states sz make = do
  marr <- unsafeNew sz
  withSchedulerWS_ states $ \schedulerWS ->
    splitLinearlyWithStatefulM_
      schedulerWS
      (totalElem sz)
      make
      (unsafeLinearWrite marr)
  unsafeFreeze (workerStatesComp states) marr
{-# INLINE generateArrayLinearWS #-}

-- | Use per worker thread state while generating elements of the array. Very useful for
-- things that are not thread safe.
--
-- @since 0.3.4
generateArrayWS
  :: forall r ix e s m
   . (Manifest r e, Index ix, MonadUnliftIO m, PrimMonad m)
  => WorkerStates s
  -> Sz ix
  -> (ix -> s -> m e)
  -> m (Array r ix e)
generateArrayWS states sz make = generateArrayLinearWS states sz (make . fromLinearIndex sz)
{-# INLINE generateArrayWS #-}

-- | Sequentially unfold an array from the left.
--
-- ====__Examples__
--
-- Create an array with Fibonacci numbers while performing an `IO` action at each iteration.
--
-- >>> import Data.Massiv.Array
-- >>> unfoldrPrimM_ (Sz1 10) (\(f0, f1) -> (f0, (f1, f0 + f1)) <$ print f1) (0, 1) :: IO (Array P Ix1 Int)
-- 1
-- 1
-- 2
-- 3
-- 5
-- 8
-- 13
-- 21
-- 34
-- 55
-- Array P Seq (Sz1 10)
--   [ 0, 1, 1, 2, 3, 5, 8, 13, 21, 34 ]
--
-- @since 0.3.0
unfoldrPrimM_
  :: forall r ix e a m
   . (Manifest r e, Index ix, PrimMonad m)
  => Sz ix
  -- ^ Size of the desired array
  -> (a -> m (e, a))
  -- ^ Unfolding action
  -> a
  -- ^ Initial accumulator
  -> m (Array r ix e)
unfoldrPrimM_ sz gen acc0 = snd <$> unfoldrPrimM sz gen acc0
{-# INLINE unfoldrPrimM_ #-}

-- | Same as `unfoldrPrimM_` but do the unfolding with index aware function.
--
-- @since 0.3.0
iunfoldrPrimM_
  :: forall r ix e a m
   . (Manifest r e, Index ix, PrimMonad m)
  => Sz ix
  -- ^ Size of the desired array
  -> (a -> ix -> m (e, a))
  -- ^ Unfolding action
  -> a
  -- ^ Initial accumulator
  -> m (Array r ix e)
iunfoldrPrimM_ sz gen acc0 = snd <$> iunfoldrPrimM sz gen acc0
{-# INLINE iunfoldrPrimM_ #-}

-- | Just like `iunfoldrPrimM_`, but also returns the final value of the accumulator.
--
-- @since 0.3.0
iunfoldrPrimM
  :: forall r ix e a m
   . (Manifest r e, Index ix, PrimMonad m)
  => Sz ix
  -- ^ Size of the desired array
  -> (a -> ix -> m (e, a))
  -- ^ Unfolding action
  -> a
  -- ^ Initial accumulator
  -> m (a, Array r ix e)
iunfoldrPrimM sz gen acc0 =
  unsafeCreateArrayS sz $ \marr ->
    let sz' = sizeOfMArray marr
     in iterLinearM sz' 0 (totalElem sz') 1 (<) acc0 $ \ !i ix !acc -> do
          (e, acc') <- gen acc ix
          unsafeLinearWrite marr i e
          pure acc'
{-# INLINE iunfoldrPrimM #-}

-- | Just like `iunfoldrPrimM`, but do the unfolding with index aware function.
--
-- @since 0.3.0
unfoldrPrimM
  :: forall r ix e a m
   . (Manifest r e, Index ix, PrimMonad m)
  => Sz ix
  -- ^ Size of the desired array
  -> (a -> m (e, a))
  -- ^ Unfolding action
  -> a
  -- ^ Initial accumulator
  -> m (a, Array r ix e)
unfoldrPrimM sz gen acc0 =
  unsafeCreateArrayS sz $ \marr ->
    let sz' = sizeOfMArray marr
     in loopM 0 (< totalElem sz') (+ 1) acc0 $ \ !i !acc -> do
          (e, acc') <- gen acc
          unsafeLinearWrite marr i e
          pure acc'
{-# INLINE unfoldrPrimM #-}

-- | Sequentially unfold an array from the left.
--
-- ====__Examples__
--
-- Create an array with Fibonacci numbers starting at the end while performing and `IO` action on
-- the accumulator for each element of the array.
--
-- >>> import Data.Massiv.Array
-- >>> unfoldlPrimM_ (Sz1 10) (\a@(f0, f1) -> let fn = f0 + f1 in print a >> return ((f1, fn), f0)) (0, 1) :: IO (Array P Ix1 Int)
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
unfoldlPrimM_
  :: forall r ix e a m
   . (Manifest r e, Index ix, PrimMonad m)
  => Sz ix
  -- ^ Size of the desired array
  -> (a -> m (a, e))
  -- ^ Unfolding action
  -> a
  -- ^ Initial accumulator
  -> m (Array r ix e)
unfoldlPrimM_ sz gen acc0 = snd <$> unfoldlPrimM sz gen acc0
{-# INLINE unfoldlPrimM_ #-}

-- | Same as `unfoldlPrimM_` but do the unfolding with index aware function.
--
-- @since 0.3.0
iunfoldlPrimM_
  :: forall r ix e a m
   . (Manifest r e, Index ix, PrimMonad m)
  => Sz ix
  -- ^ Size of the desired array
  -> (a -> ix -> m (a, e))
  -- ^ Unfolding action
  -> a
  -- ^ Initial accumulator
  -> m (Array r ix e)
iunfoldlPrimM_ sz gen acc0 = snd <$> iunfoldlPrimM sz gen acc0
{-# INLINE iunfoldlPrimM_ #-}

-- | Just like `iunfoldlPrimM_`, but also returns the final value of the accumulator.
--
-- @since 0.3.0
iunfoldlPrimM
  :: forall r ix e a m
   . (Manifest r e, Index ix, PrimMonad m)
  => Sz ix
  -- ^ Size of the desired array
  -> (a -> ix -> m (a, e))
  -- ^ Unfolding action
  -> a
  -- ^ Initial accumulator
  -> m (a, Array r ix e)
iunfoldlPrimM sz gen acc0 =
  unsafeCreateArrayS sz $ \marr ->
    let sz' = sizeOfMArray marr
     in iterLinearM sz' (totalElem sz' - 1) 0 (negate 1) (>=) acc0 $ \ !i ix !acc -> do
          (acc', e) <- gen acc ix
          unsafeLinearWrite marr i e
          pure acc'
{-# INLINE iunfoldlPrimM #-}

-- | Just like `iunfoldlPrimM`, but do the unfolding with index aware function.
--
-- @since 0.3.0
unfoldlPrimM
  :: forall r ix e a m
   . (Manifest r e, Index ix, PrimMonad m)
  => Sz ix
  -- ^ Size of the desired array
  -> (a -> m (a, e))
  -- ^ Unfolding action
  -> a
  -- ^ Initial accumulator
  -> m (a, Array r ix e)
unfoldlPrimM sz gen acc0 =
  unsafeCreateArrayS sz $ \marr ->
    let sz' = sizeOfMArray marr
     in loopDeepM 0 (< totalElem sz') (+ 1) acc0 $ \ !i !acc -> do
          (acc', e) <- gen acc
          unsafeLinearWrite marr i e
          pure acc'
{-# INLINE unfoldlPrimM #-}

-- | Sequentially loop over a mutable array while reading each element and applying an
-- action to it. There is no mutation to the array, unless the action itself modifies it.
--
-- @since 0.4.0
forPrimM_
  :: (Manifest r e, Index ix, PrimMonad m) => MArray (PrimState m) r ix e -> (e -> m ()) -> m ()
forPrimM_ marr f =
  loopA_ 0 (< totalElem (sizeOfMArray marr)) (+ 1) (unsafeLinearRead marr >=> f)
{-# INLINE forPrimM_ #-}

-- | Sequentially loop over a mutable array while modifying each element with an action.
--
-- @since 0.4.0
forPrimM
  :: (Manifest r e, Index ix, PrimMonad m) => MArray (PrimState m) r ix e -> (e -> m e) -> m ()
forPrimM marr f =
  loopA_ 0 (< totalElem (sizeOfMArray marr)) (+ 1) (unsafeLinearModify marr f)
{-# INLINE forPrimM #-}

-- | Sequentially loop over a mutable array while reading each element and applying an
-- index aware action to it. There is no mutation to the array, unless the
-- action itself modifies it.
--
-- @since 0.4.0
iforPrimM_
  :: (Manifest r e, Index ix, PrimMonad m) => MArray (PrimState m) r ix e -> (ix -> e -> m ()) -> m ()
iforPrimM_ marr f = iforLinearPrimM_ marr (f . fromLinearIndex (sizeOfMArray marr))
{-# INLINE iforPrimM_ #-}

-- | Sequentially loop over a mutable array while modifying each element with an index aware action.
--
-- @since 0.4.0
iforPrimM
  :: (Manifest r e, Index ix, PrimMonad m) => MArray (PrimState m) r ix e -> (ix -> e -> m e) -> m ()
iforPrimM marr f = iforLinearPrimM marr (f . fromLinearIndex (sizeOfMArray marr))
{-# INLINE iforPrimM #-}

-- | Sequentially loop over a mutable array while reading each element and applying a
-- linear index aware action to it. There is no mutation to the array, unless the action
-- itself modifies it.
--
-- @since 0.4.0
iforLinearPrimM_
  :: (Manifest r e, Index ix, PrimMonad m) => MArray (PrimState m) r ix e -> (Int -> e -> m ()) -> m ()
iforLinearPrimM_ marr f =
  loopA_ 0 (< totalElem (sizeOfMArray marr)) (+ 1) (\i -> unsafeLinearRead marr i >>= f i)
{-# INLINE iforLinearPrimM_ #-}

-- | Sequentially loop over a mutable array while modifying each element with an index aware action.
--
-- @since 0.4.0
iforLinearPrimM
  :: (Manifest r e, Index ix, PrimMonad m) => MArray (PrimState m) r ix e -> (Int -> e -> m e) -> m ()
iforLinearPrimM marr f =
  loopA_ 0 (< totalElem (sizeOfMArray marr)) (+ 1) (\i -> unsafeLinearModify marr (f i) i)
{-# INLINE iforLinearPrimM #-}

-- | Sequentially loop over the intersection of two mutable arrays while reading
-- elements from both and applying an action to it. There is no mutation to the
-- actual arrays, unless the action itself modifies either one of them.
--
-- @since 1.0.0
for2PrimM_
  :: forall r1 r2 e1 e2 ix m
   . (PrimMonad m, Index ix, Manifest r1 e1, Manifest r2 e2)
  => MArray (PrimState m) r1 ix e1
  -> MArray (PrimState m) r2 ix e2
  -> (e1 -> e2 -> m ())
  -> m ()
for2PrimM_ m1 m2 f = ifor2PrimM_ m1 m2 (const f)
{-# INLINE for2PrimM_ #-}

-- | Same as `for2PrimM_`, but with index aware action.
--
-- @since 1.0.0
ifor2PrimM_
  :: forall r1 r2 e1 e2 ix m
   . (PrimMonad m, Index ix, Manifest r1 e1, Manifest r2 e2)
  => MArray (PrimState m) r1 ix e1
  -> MArray (PrimState m) r2 ix e2
  -> (ix -> e1 -> e2 -> m ())
  -> m ()
ifor2PrimM_ m1 m2 f = do
  let sz = liftIndex2 min (unSz (sizeOfMArray m1)) (unSz (sizeOfMArray m2))
  iterA_ zeroIndex sz oneIndex (<) $ \ix -> do
    e1 <- unsafeRead m1 ix
    e2 <- unsafeRead m2 ix
    f ix e1 e2
{-# INLINE ifor2PrimM_ #-}

-- | Same as `withMArray_`, but allows to keep artifacts of scheduled tasks.
--
-- @since 0.5.0
withMArray
  :: (Manifest r e, Index ix, MonadUnliftIO m)
  => Array r ix e
  -> (Scheduler RealWorld a -> MArray RealWorld r ix e -> m b)
  -> m ([a], Array r ix e)
withMArray arr action = do
  marr <- thaw arr
  xs <- withScheduler (getComp arr) (`action` marr)
  liftIO ((,) xs <$> unsafeFreeze (getComp arr) marr)
{-# INLINE withMArray #-}

-- | Create a copy of a pure array, mutate it in place and return its frozen version. The big
-- difference between `withMArrayS` is that it's not only gonna respect the computation strategy
-- supplied to it while making a copy, but it will also pass extra argumens to the action that
-- suppose to modify the mutable copy of the source array. These two extra arguments are:
--
-- * Number of capabilities derived from the `Comp`utation strategy of the array.
--
-- * An action that can be used to schedule arbitrary number of jobs that will be executed in
--   parallel.
--
-- * And, of course, the mutable array itself.
--
-- @since 0.5.0
withMArray_
  :: (Manifest r e, Index ix, MonadUnliftIO m)
  => Array r ix e
  -> (Scheduler RealWorld () -> MArray RealWorld r ix e -> m a)
  -> m (Array r ix e)
withMArray_ arr action = do
  marr <- thaw arr
  withScheduler_ (getComp arr) (`action` marr)
  liftIO $ unsafeFreeze (getComp arr) marr
{-# INLINE withMArray_ #-}

-- | Same as `withMArray_`, but the array supplied to this function can be any loadable
-- array. For that reason it will be faster if supplied array is delayed.
--
-- @since 0.6.1
withLoadMArray_
  :: forall r ix e r' m b
   . (Load r' ix e, Manifest r e, MonadUnliftIO m)
  => Array r' ix e
  -> (Scheduler RealWorld () -> MArray RealWorld r ix e -> m b)
  -> m (Array r ix e)
withLoadMArray_ arr action = do
  marr <- loadArray arr
  withScheduler_ (getComp arr) (`action` marr)
  liftIO $ unsafeFreeze (getComp arr) marr
{-# INLINE [2] withLoadMArray_ #-}

{-# RULES
"withLoadMArray_/withMArray_" [~2] withLoadMArray_ = withMArray_
"withLoadMArrayS/withMArrayS" [~2] withLoadMArrayS = withMArrayS
"withLoadMArrayS_/withMArrayS_" [~2] withLoadMArrayS_ = withMArrayS_
  #-}

-- | Create a copy of a pure array, mutate it in place and return its frozen version. The important
-- benefit over doing a manual `thawS` followed by a `freezeS` is that an array will only be copied
-- once.
--
-- @since 0.5.0
withMArrayS
  :: (Manifest r e, Index ix, PrimMonad m)
  => Array r ix e
  -> (MArray (PrimState m) r ix e -> m a)
  -> m (a, Array r ix e)
withMArrayS arr action = do
  marr <- thawS arr
  a <- action marr
  (,) a <$> unsafeFreeze (getComp arr) marr
{-# INLINE withMArrayS #-}

-- | Same as `withMArrayS`, except it discards the value produced by the supplied action
--
-- @since 0.5.0
withMArrayS_
  :: (Manifest r e, Index ix, PrimMonad m)
  => Array r ix e
  -> (MArray (PrimState m) r ix e -> m a)
  -> m (Array r ix e)
withMArrayS_ arr action = snd <$> withMArrayS arr action
{-# INLINE withMArrayS_ #-}

-- | Same as `withMArrayS`, but will work with any loadable array.
--
-- @since 0.6.1
withLoadMArrayS
  :: forall r ix e r' m a
   . (Load r' ix e, Manifest r e, PrimMonad m)
  => Array r' ix e
  -> (MArray (PrimState m) r ix e -> m a)
  -> m (a, Array r ix e)
withLoadMArrayS arr action = do
  marr <- loadArrayS arr
  a <- action marr
  (,) a <$> unsafeFreeze (getComp arr) marr
{-# INLINE [2] withLoadMArrayS #-}

-- | Same as `withMArrayS_`, but will work with any loadable array.
--
-- @since 0.6.1
withLoadMArrayS_
  :: forall r ix e r' m a
   . (Load r' ix e, Manifest r e, PrimMonad m)
  => Array r' ix e
  -> (MArray (PrimState m) r ix e -> m a)
  -> m (Array r ix e)
withLoadMArrayS_ arr action = snd <$> withLoadMArrayS arr action
{-# INLINE [2] withLoadMArrayS_ #-}

-- | Same as `withMArrayS` but in `ST`. This is not only pure, but also the safest way to do
-- mutation to the array.
--
-- @since 0.5.0
withMArrayST
  :: (Manifest r e, Index ix)
  => Array r ix e
  -> (forall s. MArray s r ix e -> ST s a)
  -> (a, Array r ix e)
withMArrayST arr f = runST $ withMArrayS arr f
{-# INLINE withMArrayST #-}

-- | Same as `withMArrayS` but in `ST`. This is not only pure, but also the safest way to do
-- mutation to the array.
--
-- @since 0.5.0
withMArrayST_
  :: (Manifest r e, Index ix) => Array r ix e -> (forall s. MArray s r ix e -> ST s a) -> Array r ix e
withMArrayST_ arr f = runST $ withMArrayS_ arr f
{-# INLINE withMArrayST_ #-}

-- | Same as `withMArrayST`, but works with any loadable array.
--
-- @since 0.6.1
withLoadMArrayST
  :: forall r ix e r' a
   . (Load r' ix e, Manifest r e)
  => Array r' ix e
  -> (forall s. MArray s r ix e -> ST s a)
  -> (a, Array r ix e)
withLoadMArrayST arr f = runST $ withLoadMArrayS arr f
{-# INLINE [2] withLoadMArrayST #-}

-- | Same as `withMArrayST_`, but works with any loadable array.
--
-- @since 0.6.1
withLoadMArrayST_
  :: forall r ix e r' a
   . (Load r' ix e, Manifest r e)
  => Array r' ix e
  -> (forall s. MArray s r ix e -> ST s a)
  -> Array r ix e
withLoadMArrayST_ arr f = runST $ withLoadMArrayS_ arr f
{-# INLINE [2] withLoadMArrayST_ #-}

-- | /O(1)/ - Lookup an element in the mutable array. Returns `Nothing` when index is out of bounds.
--
-- @since 0.1.0
read
  :: (Manifest r e, Index ix, PrimMonad m)
  => MArray (PrimState m) r ix e
  -> ix
  -> m (Maybe e)
read marr ix =
  if isSafeIndex (sizeOfMArray marr) ix
    then Just <$> unsafeRead marr ix
    else return Nothing
{-# INLINE read #-}

-- | /O(1)/ - Same as `read`, but throws `IndexOutOfBoundsException` on an invalid index.
--
-- @since 0.4.0
readM
  :: (Manifest r e, Index ix, PrimMonad m, MonadThrow m)
  => MArray (PrimState m) r ix e
  -> ix
  -> m e
readM marr ix =
  read marr ix >>= \case
    Just e -> pure e
    Nothing -> throwM $ IndexOutOfBoundsException (sizeOfMArray marr) ix
{-# INLINE readM #-}

-- | /O(1)/ - Write an element into the cell of a mutable array. Returns `False` when index is out
-- of bounds.
--
-- @since 0.1.0
write :: (Manifest r e, Index ix, PrimMonad m) => MArray (PrimState m) r ix e -> ix -> e -> m Bool
write marr ix e =
  if isSafeIndex (sizeOfMArray marr) ix
    then unsafeWrite marr ix e >> pure True
    else pure False
{-# INLINE write #-}

-- | /O(1)/ - Write an element into the cell of a mutable array. Same as `write` function
-- in case of an out of bounds index it is noop, but unlike `write`, there is no
-- information is returned about was the writing of element successful or not.  In other
-- words, just like `writeM`, but doesn't throw an exception.
--
-- @since 0.4.4
write_ :: (Manifest r e, Index ix, PrimMonad m) => MArray (PrimState m) r ix e -> ix -> e -> m ()
write_ marr ix = when (isSafeIndex (sizeOfMArray marr) ix) . unsafeWrite marr ix
{-# INLINE write_ #-}

-- | /O(1)/ - Same as `write`, but throws `IndexOutOfBoundsException` on an invalid index.
--
-- @since 0.4.0
writeM
  :: (Manifest r e, Index ix, PrimMonad m, MonadThrow m)
  => MArray (PrimState m) r ix e -> ix -> e -> m ()
writeM marr ix e =
  write marr ix e >>= (`unless` throwM (IndexOutOfBoundsException (sizeOfMArray marr) ix))
{-# INLINE writeM #-}

-- | /O(1)/ - Modify an element in the cell of a mutable array with a supplied
-- action. Returns the previous value, if index was not out of bounds.
--
-- @since 0.1.0
modify
  :: (Manifest r e, Index ix, PrimMonad m)
  => MArray (PrimState m) r ix e
  -- ^ Array to mutate.
  -> (e -> m e)
  -- ^ Monadic action that modifies the element
  -> ix
  -- ^ Index at which to perform modification.
  -> m (Maybe e)
modify marr f ix =
  if isSafeIndex (sizeOfMArray marr) ix
    then Just <$> unsafeModify marr f ix
    else return Nothing
{-# INLINE modify #-}

-- | /O(1)/ - Same as `modify`, except that neither the previous value, nor any
-- information on whether the modification was successful are returned. In other words,
-- just like `modifyM_`, but doesn't throw an exception.
--
-- @since 0.4.4
modify_
  :: (Manifest r e, Index ix, PrimMonad m)
  => MArray (PrimState m) r ix e
  -- ^ Array to mutate.
  -> (e -> m e)
  -- ^ Monadic action that modifies the element
  -> ix
  -- ^ Index at which to perform modification.
  -> m ()
modify_ marr f ix = when (isSafeIndex (sizeOfMArray marr) ix) $ void $ unsafeModify marr f ix
{-# INLINE modify_ #-}

-- | /O(1)/ - Modify an element in the cell of a mutable array with a supplied
-- action. Throws an `IndexOutOfBoundsException` exception for invalid index and returns
-- the previous value otherwise.
--
-- @since 0.4.0
modifyM
  :: (Manifest r e, Index ix, PrimMonad m, MonadThrow m)
  => MArray (PrimState m) r ix e
  -- ^ Array to mutate.
  -> (e -> m e)
  -- ^ Monadic action that modifies the element
  -> ix
  -- ^ Index at which to perform modification.
  -> m e
modifyM marr f ix
  | isSafeIndex (sizeOfMArray marr) ix = unsafeModify marr f ix
  | otherwise = throwM (IndexOutOfBoundsException (sizeOfMArray marr) ix)
{-# INLINE modifyM #-}

-- | /O(1)/ - Same as `modifyM`, but discard the returned element
--
-- ====__Examples__
--
-- >>> :seti -XTypeApplications
-- >>> import Control.Monad.ST
-- >>> import Data.Massiv.Array
-- >>> runST $ newMArray' @P @Ix1 @Int (Sz1 3) >>= (\ma -> modifyM_ ma (pure . (+10)) 1 >> freezeS ma)
-- Array P Seq (Sz1 3)
--   [ 0, 10, 0 ]
--
-- @since 0.4.0
modifyM_
  :: (Manifest r e, Index ix, PrimMonad m, MonadThrow m)
  => MArray (PrimState m) r ix e
  -- ^ Array to mutate.
  -> (e -> m e)
  -- ^ Monadic action that modifies the element
  -> ix
  -- ^ Index at which to perform modification.
  -> m ()
modifyM_ marr f ix = void $ modifyM marr f ix
{-# INLINE modifyM_ #-}

-- | /O(1)/ - Same as `swapM`, but instead of throwing an exception returns `Nothing` when
-- either one of the indices is out of bounds and `Just` elements under those indices
-- otherwise.
--
-- @since 0.1.0
swap
  :: (Manifest r e, Index ix, PrimMonad m) => MArray (PrimState m) r ix e -> ix -> ix -> m (Maybe (e, e))
swap marr ix1 ix2 =
  let !sz = sizeOfMArray marr
   in if isSafeIndex sz ix1 && isSafeIndex sz ix2
        then Just <$> unsafeSwap marr ix1 ix2
        else pure Nothing
{-# INLINE swap #-}

-- | /O(1)/ - Same as `swap`, but instead of returning `Nothing` it does nothing. In other
-- words, it is similar to `swapM_`, but does not throw any exceptions.
--
-- @since 0.4.4
swap_ :: (Manifest r e, Index ix, PrimMonad m) => MArray (PrimState m) r ix e -> ix -> ix -> m ()
swap_ marr ix1 ix2 =
  let !sz = sizeOfMArray marr
   in when (isSafeIndex sz ix1 && isSafeIndex sz ix2) $ void $ unsafeSwap marr ix1 ix2
{-# INLINE swap_ #-}

-- | /O(1)/ - Swap two elements in a mutable array under the supplied indices. Throws an
-- `IndexOutOfBoundsException` when either one of the indices is out of bounds and
-- elements under those indices otherwise.
--
-- @since 0.4.0
swapM
  :: (Manifest r e, Index ix, PrimMonad m, MonadThrow m)
  => MArray (PrimState m) r ix e
  -> ix
  -- ^ Index for the first element, which will be returned as the first element in the
  -- tuple.
  -> ix
  -- ^ Index for the second element, which will be returned as the second element in
  -- the tuple.
  -> m (e, e)
swapM marr ix1 ix2
  | not (isSafeIndex sz ix1) = throwM $ IndexOutOfBoundsException (sizeOfMArray marr) ix1
  | not (isSafeIndex sz ix2) = throwM $ IndexOutOfBoundsException (sizeOfMArray marr) ix2
  | otherwise = unsafeSwap marr ix1 ix2
  where
    !sz = sizeOfMArray marr
{-# INLINE swapM #-}

-- | /O(1)/ - Same as `swapM`, but discard the returned elements
--
-- @since 0.4.0
swapM_
  :: (Manifest r e, Index ix, PrimMonad m, MonadThrow m)
  => MArray (PrimState m) r ix e
  -> ix
  -> ix
  -> m ()
swapM_ marr ix1 ix2 = void $ swapM marr ix1 ix2
{-# INLINE swapM_ #-}

-- | Swap elements in the intersection of two mutable arrays starting at the
-- initial index.
--
-- @since 1.0.0
zipSwapM_
  :: forall r1 r2 ix e m s
   . (MonadPrim s m, Manifest r2 e, Manifest r1 e, Index ix)
  => ix
  -> MArray s r1 ix e
  -> MArray s r2 ix e
  -> m ()
zipSwapM_ startIx m1 m2 = do
  let sz1 = sizeOfMArray m1
      sz2 = sizeOfMArray m2
      sz = liftIndex2 min (unSz sz1) (unSz sz2)
  iterA_ startIx sz oneIndex (<) $ \ix -> do
    let i1 = toLinearIndex sz1 ix
        i2 = toLinearIndex sz2 ix
    e1 <- unsafeLinearRead m1 i1
    e2 <- unsafeLinearRead m2 i2
    unsafeLinearWrite m2 i2 e1
    unsafeLinearWrite m1 i1 e2
{-# INLINE zipSwapM_ #-}

-- | Get the size of a mutable array.
--
-- @since 0.1.0
msize :: (Manifest r e, Index ix) => MArray s r ix e -> Sz ix
msize = sizeOfMArray
{-# DEPRECATED msize "In favor of `sizeOfMArray`" #-}
