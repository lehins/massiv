{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Data.Massiv.Array.Ops.Map
-- Copyright   : (c) Alexey Kuleshevich 2018-2025
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
module Data.Massiv.Array.Ops.Map (
  map,
  imap,

  -- ** Traversing

  -- *** Applicative
  traverseA,
  traverseA_,
  itraverseA,
  itraverseA_,
  sequenceA,
  sequenceA_,

  -- *** PrimMonad
  traversePrim,
  itraversePrim,

  -- ** Monadic mapping

  -- *** Sequential
  mapM,
  forM,
  imapM,
  iforM,
  mapM_,
  forM_,
  imapM_,
  iforM_,

  -- *** Parallelizable
  mapIO,
  mapWS,
  mapIO_,
  imapIO,
  imapWS,
  imapIO_,
  forIO,
  forWS,
  forIO_,
  iforIO,
  iforWS,
  iforIO_,
  imapSchedulerM_,
  iforSchedulerM_,
  iterArrayLinearM_,
  iterArrayLinearWithSetM_,
  iterArrayLinearWithStrideM_,

  -- ** Zipping
  zip,
  zip3,
  zip4,
  unzip,
  unzip3,
  unzip4,
  zipWith,
  zipWith3,
  zipWith4,
  izipWith,
  izipWith3,
  izipWith4,

  -- *** Applicative
  zipWithA,
  izipWithA,
  zipWith3A,
  izipWith3A,
) where

import Control.Monad (void)
import Control.Monad.Primitive
import Control.Scheduler
import Data.Coerce
import Data.Massiv.Array.Delayed.Pull
import Data.Massiv.Array.Manifest.List
import Data.Massiv.Array.Mutable
import Data.Massiv.Array.Ops.Construct (makeArrayA, makeArrayLinearA)
import Data.Massiv.Core.Common
import Data.Traversable (traverse)
import Prelude hiding (
  map,
  mapM,
  mapM_,
  sequenceA,
  traverse,
  unzip,
  unzip3,
  zip,
  zip3,
  zipWith,
  zipWith3,
 )

--------------------------------------------------------------------------------
-- map -------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Map a function over an array
--
-- @since 0.1.0
map :: (Index ix, Source r e') => (e' -> e) -> Array r ix e' -> Array D ix e
map f = fmap f . delay
{-# INLINE map #-}

--------------------------------------------------------------------------------
-- zip -------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Zip two arrays
--
-- @since 0.1.0
zip
  :: (Index ix, Source r1 e1, Source r2 e2)
  => Array r1 ix e1
  -> Array r2 ix e2
  -> Array D ix (e1, e2)
zip = zipWith (,)
{-# INLINE zip #-}

-- | Zip three arrays
--
-- @since 0.1.0
zip3
  :: (Index ix, Source r1 e1, Source r2 e2, Source r3 e3)
  => Array r1 ix e1
  -> Array r2 ix e2
  -> Array r3 ix e3
  -> Array D ix (e1, e2, e3)
zip3 = zipWith3 (,,)
{-# INLINE zip3 #-}

-- | Zip four arrays
--
-- @since 0.5.4
zip4
  :: (Index ix, Source r1 e1, Source r2 e2, Source r3 e3, Source r4 e4)
  => Array r1 ix e1
  -> Array r2 ix e2
  -> Array r3 ix e3
  -> Array r4 ix e4
  -> Array D ix (e1, e2, e3, e4)
zip4 = zipWith4 (,,,)
{-# INLINE zip4 #-}

-- | Unzip two arrays
--
-- @since 0.1.0
unzip :: (Index ix, Source r (e1, e2)) => Array r ix (e1, e2) -> (Array D ix e1, Array D ix e2)
unzip arr = (map fst arr, map snd arr)
{-# INLINE unzip #-}

-- | Unzip three arrays
--
-- @since 0.1.0
unzip3
  :: (Index ix, Source r (e1, e2, e3))
  => Array r ix (e1, e2, e3)
  -> (Array D ix e1, Array D ix e2, Array D ix e3)
unzip3 arr = (map (\(e, _, _) -> e) arr, map (\(_, e, _) -> e) arr, map (\(_, _, e) -> e) arr)
{-# INLINE unzip3 #-}

-- | Unzip four arrays
--
-- @since 0.5.4
unzip4
  :: (Index ix, Source r (e1, e2, e3, e4))
  => Array r ix (e1, e2, e3, e4)
  -> (Array D ix e1, Array D ix e2, Array D ix e3, Array D ix e4)
unzip4 arr =
  ( map (\(e, _, _, _) -> e) arr
  , map (\(_, e, _, _) -> e) arr
  , map (\(_, _, e, _) -> e) arr
  , map (\(_, _, _, e) -> e) arr
  )
{-# INLINE unzip4 #-}

--------------------------------------------------------------------------------
-- zipWith ---------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Zip two arrays with a function. Resulting array will be an intersection of
-- source arrays in case their dimensions do not match.
zipWith
  :: (Index ix, Source r1 e1, Source r2 e2)
  => (e1 -> e2 -> e)
  -> Array r1 ix e1
  -> Array r2 ix e2
  -> Array D ix e
zipWith f arr1 arr2 = DArray comp sz prefIndex
  where
    sz = SafeSz (liftIndex2 min (coerce (size arr1)) (coerce (size arr2)))
    comp = getComp arr1 <> getComp arr2
    prefIndex = PrefIndex (\ix -> f (unsafeIndex arr1 ix) (unsafeIndex arr2 ix))
-- Somehow checking for size equality destroys performance
--  | PrefIndexLinear gi1 <- unsafePrefIndex arr1,
--    PrefIndexLinear gi2 <- unsafePrefIndex arr2,
--    size arr1 == size arr2 =
--      PrefIndexLinear (\i -> f (gi1 i) (gi2 i))
--  | otherwise = PrefIndex (\ix -> f (unsafeIndex arr1 ix) (unsafeIndex arr2 ix))
{-# INLINE zipWith #-}

-- | Just like `zipWith`, except with an index aware function.
izipWith
  :: (Index ix, Source r1 e1, Source r2 e2)
  => (ix -> e1 -> e2 -> e)
  -> Array r1 ix e1
  -> Array r2 ix e2
  -> Array D ix e
izipWith f arr1 arr2 =
  DArray
    (getComp arr1 <> getComp arr2)
    (SafeSz (liftIndex2 min (coerce (size arr1)) (coerce (size arr2))))
    (PrefIndex (\ix -> f ix (unsafeIndex arr1 ix) (unsafeIndex arr2 ix)))
{-# INLINE izipWith #-}

-- | Just like `zipWith`, except zip three arrays with a function.
zipWith3
  :: (Index ix, Source r1 e1, Source r2 e2, Source r3 e3)
  => (e1 -> e2 -> e3 -> e)
  -> Array r1 ix e1
  -> Array r2 ix e2
  -> Array r3 ix e3
  -> Array D ix e
zipWith3 f arr1 arr2 arr3 = izipWith3 (\_ e1 e2 e3 -> f e1 e2 e3) arr1 arr2 arr3
-- See note on zipWith
--  | sz1 == size arr2 && sz1 == size arr3
--  , PrefIndexLinear gi1 <- unsafePrefIndex arr1
--  , PrefIndexLinear gi2 <- unsafePrefIndex arr2
--  , PrefIndexLinear gi3 <- unsafePrefIndex arr3 =
--    makeArrayLinear comp sz1 (\ !i -> f (gi1 i) (gi2 i) (gi3 i))
--  | otherwise = izipWith3 (\_ e1 e2 e3 -> f e1 e2 e3) arr1 arr2 arr3
-- where
--   comp = getComp arr1 <> getComp arr2 <> getComp arr3
--   sz1 = size arr1
{-# INLINE zipWith3 #-}

-- | Just like `zipWith3`, except with an index aware function.
izipWith3
  :: (Index ix, Source r1 e1, Source r2 e2, Source r3 e3)
  => (ix -> e1 -> e2 -> e3 -> e)
  -> Array r1 ix e1
  -> Array r2 ix e2
  -> Array r3 ix e3
  -> Array D ix e
izipWith3 f arr1 arr2 arr3 =
  DArray
    (getComp arr1 <> getComp arr2 <> getComp arr3)
    ( SafeSz
        ( liftIndex2
            min
            (liftIndex2 min (coerce (size arr1)) (coerce (size arr2)))
            (coerce (size arr3))
        )
    )
    (PrefIndex $ \ !ix -> f ix (unsafeIndex arr1 ix) (unsafeIndex arr2 ix) (unsafeIndex arr3 ix))
{-# INLINE izipWith3 #-}

-- | Just like `zipWith`, except zip four arrays with a function.
--
-- @since 0.5.4
zipWith4
  :: (Index ix, Source r1 e1, Source r2 e2, Source r3 e3, Source r4 e4)
  => (e1 -> e2 -> e3 -> e4 -> e)
  -> Array r1 ix e1
  -> Array r2 ix e2
  -> Array r3 ix e3
  -> Array r4 ix e4
  -> Array D ix e
zipWith4 f arr1 arr2 arr3 arr4 =
  izipWith4 (\_ e1 e2 e3 e4 -> f e1 e2 e3 e4) arr1 arr2 arr3 arr4
-- See note on zipWith
--  | sz1 == size arr2 && sz1 == size arr3 && sz1 == size arr4
--  , PrefIndexLinear gi1 <- unsafePrefIndex arr1
--  , PrefIndexLinear gi2 <- unsafePrefIndex arr2
--  , PrefIndexLinear gi3 <- unsafePrefIndex arr3
--  , PrefIndexLinear gi4 <- unsafePrefIndex arr4 =
--    makeArrayLinear comp sz1 (\ !i -> f (gi1 i) (gi2 i) (gi3 i) (gi4 i))
--  | otherwise = izipWith4 (\ _ e1 e2 e3 e4 -> f e1 e2 e3 e4) arr1 arr2 arr3 arr4
--  where
--    comp = getComp arr1 <> getComp arr2 <> getComp arr3 <> getComp arr4
--    sz1 = size arr1
{-# INLINE zipWith4 #-}

-- | Just like `zipWith4`, except with an index aware function.
--
-- @since 0.5.4
izipWith4
  :: (Index ix, Source r1 e1, Source r2 e2, Source r3 e3, Source r4 e4)
  => (ix -> e1 -> e2 -> e3 -> e4 -> e)
  -> Array r1 ix e1
  -> Array r2 ix e2
  -> Array r3 ix e3
  -> Array r4 ix e4
  -> Array D ix e
izipWith4 f arr1 arr2 arr3 arr4 =
  makeArray
    (getComp arr1 <> getComp arr2 <> getComp arr3 <> getComp arr4)
    ( SafeSz
        ( liftIndex2
            min
            ( liftIndex2
                min
                (liftIndex2 min (coerce (size arr1)) (coerce (size arr2)))
                (coerce (size arr3))
            )
            (coerce (size arr4))
        )
    )
    ( \ !ix ->
        f ix (unsafeIndex arr1 ix) (unsafeIndex arr2 ix) (unsafeIndex arr3 ix) (unsafeIndex arr4 ix)
    )
{-# INLINE izipWith4 #-}

-- | Similar to `zipWith`, except does it sequentially and using the `Applicative`. Note that
-- resulting array has Manifest representation.
--
-- @since 0.3.0
zipWithA
  :: (Source r1 e1, Source r2 e2, Applicative f, Manifest r e, Index ix)
  => (e1 -> e2 -> f e)
  -> Array r1 ix e1
  -> Array r2 ix e2
  -> f (Array r ix e)
zipWithA f arr1 arr2
  | sz1 == size arr2
  , PrefIndexLinear gi1 <- unsafePrefIndex arr1
  , PrefIndexLinear gi2 <- unsafePrefIndex arr2 =
      setComp (getComp arr1 <> getComp arr2) <$> makeArrayLinearA sz1 (\ !i -> f (gi1 i) (gi2 i))
  | otherwise = izipWithA (const f) arr1 arr2
  where
    !sz1 = size arr1
{-# INLINE zipWithA #-}

-- | Similar to `zipWith`, except does it sequentially and using the `Applicative`. Note that
-- resulting array has Manifest representation.
--
-- @since 0.3.0
izipWithA
  :: (Source r1 e1, Source r2 e2, Applicative f, Manifest r e, Index ix)
  => (ix -> e1 -> e2 -> f e)
  -> Array r1 ix e1
  -> Array r2 ix e2
  -> f (Array r ix e)
izipWithA f arr1 arr2 =
  setComp (getComp arr1 <> getComp arr2)
    <$> makeArrayA
      (SafeSz (liftIndex2 min (coerce (size arr1)) (coerce (size arr2))))
      (\ !ix -> f ix (unsafeIndex arr1 ix) (unsafeIndex arr2 ix))
{-# INLINE izipWithA #-}

-- | Same as `zipWithA`, but for three arrays.
--
-- @since 0.3.0
zipWith3A
  :: (Source r1 e1, Source r2 e2, Source r3 e3, Applicative f, Manifest r e, Index ix)
  => (e1 -> e2 -> e3 -> f e)
  -> Array r1 ix e1
  -> Array r2 ix e2
  -> Array r3 ix e3
  -> f (Array r ix e)
zipWith3A f = izipWith3A (const f)
{-# INLINE zipWith3A #-}

-- | Same as `izipWithA`, but for three arrays.
--
-- @since 0.3.0
izipWith3A
  :: (Source r1 e1, Source r2 e2, Source r3 e3, Applicative f, Manifest r e, Index ix)
  => (ix -> e1 -> e2 -> e3 -> f e)
  -> Array r1 ix e1
  -> Array r2 ix e2
  -> Array r3 ix e3
  -> f (Array r ix e)
izipWith3A f arr1 arr2 arr3 =
  setComp (getComp arr1 <> getComp arr2 <> getComp arr3)
    <$> makeArrayA sz (\ !ix -> f ix (unsafeIndex arr1 ix) (unsafeIndex arr2 ix) (unsafeIndex arr3 ix))
  where
    sz =
      SafeSz $
        liftIndex2 min (liftIndex2 min (coerce (size arr1)) (coerce (size arr2))) (coerce (size arr3))
{-# INLINE izipWith3A #-}

--------------------------------------------------------------------------------
-- traverse --------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Traverse with an `Applicative` action over an array sequentially.
--
-- /Note/ - using `traversePrim` instead will always be significantly faster, roughly
-- about 30 times faster in practice.
--
-- @since 0.2.6
traverseA
  :: forall r ix e r' a f
   . (Source r' a, Manifest r e, Index ix, Applicative f)
  => (a -> f e)
  -> Array r' ix a
  -> f (Array r ix e)
traverseA f arr =
  unsafeResize (size arr) . fromList (getComp arr) <$> traverse f (toList arr)
{-# INLINE traverseA #-}

-- | Traverse sequentially over a source array, while discarding the result.
--
-- @since 0.3.0
traverseA_
  :: forall r ix e a f
   . (Index ix, Source r e, Applicative f)
  => (e -> f a)
  -> Array r ix e
  -> f ()
traverseA_ f arr =
  case unsafePrefIndex arr of
    PrefIndex gix -> iterA_ zeroIndex (unSz sz) oneIndex (<) (f . gix)
    PrefIndexLinear gi -> loopA_ 0 (< totalElem sz) (+ 1) (f . gi)
  where
    sz = size arr
{-# INLINE traverseA_ #-}

-- | Sequence actions in a source array.
--
-- @since 0.3.0
sequenceA
  :: forall r ix e r' f
   . (Source r' (f e), Manifest r e, Index ix, Applicative f)
  => Array r' ix (f e)
  -> f (Array r ix e)
sequenceA = traverseA id
{-# INLINE sequenceA #-}

-- | Sequence actions in a source array, while discarding the result.
--
-- @since 0.3.0
sequenceA_
  :: forall r ix e f
   . (Index ix, Source r (f e), Applicative f)
  => Array r ix (f e)
  -> f ()
sequenceA_ = traverseA_ id
{-# INLINE sequenceA_ #-}

-- | Traverse with an `Applicative` index aware action over an array sequentially.
--
-- @since 0.2.6
itraverseA
  :: forall r ix e r' a f
   . (Source r' a, Manifest r e, Index ix, Applicative f)
  => (ix -> a -> f e)
  -> Array r' ix a
  -> f (Array r ix e)
itraverseA f arr =
  setComp (getComp arr) <$> makeArrayA (size arr) (\ !ix -> f ix (unsafeIndex arr ix))
{-# INLINE itraverseA #-}

-- | Traverse with an `Applicative` index aware action over an array sequentially.
--
-- @since 0.2.6
itraverseA_
  :: forall r ix e a f
   . (Source r a, Index ix, Applicative f)
  => (ix -> a -> f e)
  -> Array r ix a
  -> f ()
itraverseA_ f arr =
  case unsafePrefIndex arr of
    PrefIndex gix ->
      iterA_ zeroIndex (unSz sz) oneIndex (<) (\ !ix -> f ix (gix ix))
    PrefIndexLinear gi ->
      iterTargetA_ defRowMajor 0 sz zeroIndex oneStride $ \i ix -> f ix (gi i)
  where
    sz = size arr
{-# INLINE itraverseA_ #-}

-- | Traverse sequentially within `PrimMonad` over an array with an action.
--
-- @since 0.3.0
traversePrim
  :: forall r ix b r' a m
   . (Source r' a, Manifest r b, Index ix, PrimMonad m)
  => (a -> m b)
  -> Array r' ix a
  -> m (Array r ix b)
traversePrim f arr = do
  let sz = size arr
  marr <- unsafeNew sz
  case unsafePrefIndex arr of
    PrefIndex gix ->
      iterTargetA_ defRowMajor 0 sz zeroIndex oneStride $ \i ix ->
        f (gix ix) >>= unsafeLinearWrite marr i
    PrefIndexLinear gi ->
      loopA_ 0 (< totalElem sz) (+ 1) $ \i ->
        f (gi i) >>= unsafeLinearWrite marr i
  unsafeFreeze (getComp arr) marr
{-# INLINE traversePrim #-}

-- | Same as `traversePrim`, but traverse with index aware action.
--
-- @since 0.3.0
itraversePrim
  :: forall r ix b r' a m
   . (Source r' a, Manifest r b, Index ix, PrimMonad m)
  => (ix -> a -> m b)
  -> Array r' ix a
  -> m (Array r ix b)
itraversePrim f arr = do
  let sz = size arr
  marr <- unsafeNew sz
  case unsafePrefIndex arr of
    PrefIndex gix ->
      iterTargetA_ defRowMajor 0 sz zeroIndex oneStride $ \i ix ->
        f ix (gix ix) >>= unsafeLinearWrite marr i
    PrefIndexLinear gi ->
      iterTargetA_ defRowMajor 0 sz zeroIndex oneStride $ \i ix ->
        f ix (gi i) >>= unsafeLinearWrite marr i
  unsafeFreeze (getComp arr) marr
{-# INLINE itraversePrim #-}

--------------------------------------------------------------------------------
-- mapM ------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Map a monadic action over an array sequentially.
--
-- @since 0.2.6
mapM
  :: forall r ix b r' a m
   . (Source r' a, Manifest r b, Index ix, Monad m)
  => (a -> m b)
  -- ^ Mapping action
  -> Array r' ix a
  -- ^ Source array
  -> m (Array r ix b)
mapM = traverseA
{-# INLINE mapM #-}

-- | Same as `mapM` except with arguments flipped.
--
-- @since 0.2.6
forM
  :: forall r ix b r' a m
   . (Source r' a, Manifest r b, Index ix, Monad m)
  => Array r' ix a
  -> (a -> m b)
  -> m (Array r ix b)
forM = flip traverseA
{-# INLINE forM #-}

-- | Map an index aware monadic action over an array sequentially.
--
-- @since 0.2.6
imapM
  :: forall r ix b r' a m
   . (Source r' a, Manifest r b, Index ix, Monad m)
  => (ix -> a -> m b)
  -> Array r' ix a
  -> m (Array r ix b)
imapM = itraverseA
{-# INLINE imapM #-}

-- | Same as `forM`, except with an index aware action.
--
-- @since 0.5.1
iforM
  :: forall r ix b r' a m
   . (Source r' a, Manifest r b, Index ix, Monad m)
  => Array r' ix a
  -> (ix -> a -> m b)
  -> m (Array r ix b)
iforM = flip itraverseA
{-# INLINE iforM #-}

-- | Map a monadic function over an array sequentially, while discarding the result.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array as A
-- >>> rangeStepM Par (Ix1 10) 12 60 >>= A.mapM_ print
-- 10
-- 22
-- 34
-- 46
-- 58
--
-- @since 0.1.0
mapM_ :: (Source r a, Index ix, Monad m) => (a -> m b) -> Array r ix a -> m ()
mapM_ = traverseA_
{-# INLINE mapM_ #-}

-- | Just like `mapM_`, except with flipped arguments.
--
-- ==== __Examples__
--
-- Here is a common way of iterating N times using a for loop in an imperative
-- language with mutation being an obvious side effect:
--
-- >>> import Data.Massiv.Array as A
-- >>> import Data.IORef
-- >>> ref <- newIORef 0 :: IO (IORef Int)
-- >>> A.forM_ (range Seq (Ix1 0) 1000) $ \ i -> modifyIORef' ref (+i)
-- >>> readIORef ref
-- 499500
forM_ :: (Source r a, Index ix, Monad m) => Array r ix a -> (a -> m b) -> m ()
forM_ = flip traverseA_
{-# INLINE forM_ #-}

-- | Map a monadic index aware function over an array sequentially, while discarding the result.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> imapM_ (curry print) $ range Seq (Ix1 10) 15
-- (0,10)
-- (1,11)
-- (2,12)
-- (3,13)
-- (4,14)
--
-- @since 0.1.0
imapM_ :: (Index ix, Source r a, Monad m) => (ix -> a -> m b) -> Array r ix a -> m ()
imapM_ = itraverseA_
{-# INLINE imapM_ #-}

-- | Just like `imapM_`, except with flipped arguments.
iforM_ :: (Source r a, Index ix, Monad m) => Array r ix a -> (ix -> a -> m b) -> m ()
iforM_ = flip itraverseA_
{-# INLINE iforM_ #-}

-- | Map an `IO` action over an `Array`. Underlying computation strategy is respected and will be
-- parallelized when requested. Unfortunately no fusion is possible and new array will be create
-- upon each call.
--
-- @since 0.2.6
mapIO
  :: forall r ix b r' a m
   . (Size r', Load r' ix a, Manifest r b, MonadUnliftIO m)
  => (a -> m b)
  -> Array r' ix a
  -> m (Array r ix b)
mapIO action = imapIO (const action)
{-# INLINE mapIO #-}

-- | Similar to `mapIO`, but ignores the result of mapping action and does not
-- create a resulting array, therefore it is faster. Use this instead of `mapIO`
-- when result is irrelevant. Most importantly it will follow the iteration
-- logic outlined by the supplied array.
--
-- @since 0.2.6
mapIO_
  :: forall r ix e a m
   . (Load r ix e, MonadUnliftIO m)
  => (e -> m a)
  -> Array r ix e
  -> m ()
mapIO_ action arr =
  withRunInIO $ \run ->
    withMassivScheduler_ (getComp arr) $ \scheduler ->
      iterArrayLinearM_ scheduler arr (\_ -> void . run . action)
{-# INLINE mapIO_ #-}

-- | Same as `mapIO_`, but map an index aware action instead.
--
-- @since 0.2.6
imapIO_
  :: forall r ix e a m
   . (Load r ix e, MonadUnliftIO m)
  => (ix -> e -> m a)
  -> Array r ix e
  -> m ()
imapIO_ action arr =
  withRunInIO $ \run ->
    withMassivScheduler_ (getComp arr) $ \scheduler ->
      let sz = outerSize arr
       in -- It is ok to use outerSize in context of DS and L. Former is 1-dim,
          -- so sz is never evaluated and for the latter outerSize has to be
          -- called regardless how this function is implemented.
          iterArrayLinearM_ scheduler arr (\i -> void . run . action (fromLinearIndex sz i))
{-# INLINE imapIO_ #-}

-- | Same as `mapIO` but map an index aware action instead. Respects computation strategy.
--
-- @since 0.2.6
imapIO
  :: forall r ix b r' a m
   . (Size r', Load r' ix a, Manifest r b, MonadUnliftIO m)
  => (ix -> a -> m b)
  -> Array r' ix a
  -> m (Array r ix b)
imapIO action arr = do
  let sz = size arr
  withRunInIO $ \run -> do
    marr <- unsafeNew sz
    withMassivScheduler_ (getComp arr) $ \scheduler ->
      iterArrayLinearM_ scheduler arr $ \ !i e ->
        run (action (fromLinearIndex sz i) e) >>= unsafeLinearWrite marr i
    unsafeFreeze (getComp arr) marr
{-# INLINE imapIO #-}

-- | Same as `mapIO` but with arguments flipped.
--
-- @since 0.2.6
forIO
  :: forall r ix b r' a m
   . (Size r', Load r' ix a, Manifest r b, MonadUnliftIO m)
  => Array r' ix a
  -> (a -> m b)
  -> m (Array r ix b)
forIO = flip mapIO
{-# INLINE forIO #-}

-- | Same as `imapIO`, but ignores the inner computation strategy and uses
-- stateful workers during computation instead. Use
-- `Control.Scheduler.initWorkerStates` for the `WorkerStates` initialization.
--
-- @since 0.3.4
imapWS
  :: forall r ix b r' a s m
   . (Source r' a, Manifest r b, Index ix, MonadUnliftIO m, PrimMonad m)
  => WorkerStates s
  -> (ix -> a -> s -> m b)
  -> Array r' ix a
  -> m (Array r ix b)
imapWS states f arr = generateArrayWS states (size arr) (\ix s -> f ix (unsafeIndex arr ix) s)
{-# INLINE imapWS #-}

-- | Same as `imapWS`, but without the index.
--
-- @since 0.3.4
mapWS
  :: forall r ix b r' a s m
   . (Source r' a, Manifest r b, Index ix, MonadUnliftIO m, PrimMonad m)
  => WorkerStates s
  -> (a -> s -> m b)
  -> Array r' ix a
  -> m (Array r ix b)
mapWS states f = imapWS states (\_ -> f)
{-# INLINE mapWS #-}

-- | Same as `imapWS`, but with source array and mapping action arguments flipped.
--
-- @since 0.3.4
iforWS
  :: forall r ix b r' a s m
   . (Source r' a, Manifest r b, Index ix, MonadUnliftIO m, PrimMonad m)
  => WorkerStates s
  -> Array r' ix a
  -> (ix -> a -> s -> m b)
  -> m (Array r ix b)
iforWS states f arr = imapWS states arr f
{-# INLINE iforWS #-}

-- | Same as `iforWS`, but without the index.
--
-- @since 0.3.4
forWS
  :: forall r ix b r' a s m
   . (Source r' a, Manifest r b, Index ix, MonadUnliftIO m, PrimMonad m)
  => WorkerStates s
  -> Array r' ix a
  -> (a -> s -> m b)
  -> m (Array r ix b)
forWS states arr f = imapWS states (\_ -> f) arr
{-# INLINE forWS #-}

-- | Same as `mapIO_` but with arguments flipped.
--
-- ==== __Example__
--
-- This is the same example as in `forM_`, with important difference that accumulator `ref` will be
-- modified concurrently by as many threads as there are capabilities.
--
-- >>> import Data.Massiv.Array
-- >>> import Data.IORef
-- >>> ref <- newIORef 0 :: IO (IORef Int)
-- >>> forIO_ (range Par (Ix1 0) 1000) $ \ i -> atomicModifyIORef' ref (\v -> (v+i, ()))
-- >>> readIORef ref
-- 499500
--
-- @since 0.2.6
forIO_ :: (Load r ix e, MonadUnliftIO m) => Array r ix e -> (e -> m a) -> m ()
forIO_ = flip mapIO_
{-# INLINE forIO_ #-}

-- | Same as `imapIO` but with arguments flipped.
--
-- @since 0.2.6
iforIO
  :: forall r ix b r' a m
   . (Size r', Load r' ix a, Manifest r b, MonadUnliftIO m)
  => Array r' ix a
  -> (ix -> a -> m b)
  -> m (Array r ix b)
iforIO = flip imapIO
{-# INLINE iforIO #-}

-- | Same as `imapIO_` but with arguments flipped.
--
-- @since 0.2.6
iforIO_
  :: forall r ix e a m
   . (Load r ix e, MonadUnliftIO m)
  => Array r ix e
  -> (ix -> e -> m a)
  -> m ()
iforIO_ = flip imapIO_
{-# INLINE iforIO_ #-}

iterArrayLinearM_
  :: forall r ix e m s
   . (Load r ix e, MonadPrimBase s m)
  => Scheduler s ()
  -> Array r ix e
  -- ^ Array that is being loaded
  -> (Int -> e -> m ())
  -- ^ Function that writes an element into target array
  -> m ()
iterArrayLinearM_ scheduler arr f =
  stToPrim $ iterArrayLinearST_ scheduler arr (\i -> primToPrim . f i)
{-# INLINE iterArrayLinearM_ #-}

iterArrayLinearWithSetM_
  :: forall r ix e m s
   . (Load r ix e, MonadPrimBase s m)
  => Scheduler s ()
  -> Array r ix e
  -- ^ Array that is being loaded
  -> (Int -> e -> m ())
  -- ^ Function that writes an element into target array
  -> (Ix1 -> Sz1 -> e -> m ())
  -- ^ Function that efficiently sets a region of an array
  -- to the supplied value target array
  -> m ()
iterArrayLinearWithSetM_ scheduler arr f set =
  stToPrim $
    iterArrayLinearWithSetST_ scheduler arr (\i -> primToPrim . f i) (\i n -> primToPrim . set i n)
{-# INLINE iterArrayLinearWithSetM_ #-}

iterArrayLinearWithStrideM_
  :: forall r ix e m s
   . (StrideLoad r ix e, MonadPrimBase s m)
  => Scheduler s ()
  -> Stride ix
  -- ^ Stride to use
  -> Sz ix
  -- ^ Size of the target array affected by the stride.
  -> Array r ix e
  -- ^ Array that is being loaded
  -> (Int -> e -> m ())
  -- ^ Function that writes an element into target array
  -> m ()
iterArrayLinearWithStrideM_ scheduler stride sz arr f =
  stToPrim $ iterArrayLinearWithStrideST_ scheduler stride sz arr (\i -> primToPrim . f i)
{-# INLINE iterArrayLinearWithStrideM_ #-}

-- iterArrayM_ ::
--      Scheduler s ()
--   -> Array r ix e -- ^ Array that is being loaded
--   -> (Int -> e -> ST s ()) -- ^ Function that writes an element into target array
--   -> ST s ()
-- iterArrayM_ scheduler arr uWrite

-- Deprecated

-- | Same as `imapM_`, but will use the supplied scheduler.
--
-- @since 0.3.1
imapSchedulerM_
  :: (Index ix, Source r e, MonadPrimBase s m)
  => Scheduler s ()
  -> (ix -> e -> m a)
  -> Array r ix e
  -> m ()
imapSchedulerM_ scheduler action arr = do
  let sz = size arr
  splitLinearlyWith_
    scheduler
    (totalElem sz)
    (unsafeLinearIndex arr)
    (\i -> void . action (fromLinearIndex sz i))
{-# INLINE imapSchedulerM_ #-}

-- | Same as `imapM_`, but will use the supplied scheduler.
--
-- @since 0.3.1
iforSchedulerM_
  :: (Index ix, Source r e, MonadPrimBase s m)
  => Scheduler s ()
  -> Array r ix e
  -> (ix -> e -> m a)
  -> m ()
iforSchedulerM_ scheduler arr action = imapSchedulerM_ scheduler action arr
{-# INLINE iforSchedulerM_ #-}

-- -- | Load an array into memory.
-- --
-- -- @since 0.3.0
-- loadArrayM
--   :: Scheduler s ()
--   -> Array r ix e -- ^ Array that is being loaded
--   -> (Int -> e -> ST s ()) -- ^ Function that writes an element into target array
--   -> ST s ()
-- loadArrayM scheduler arr uWrite =
--   loadArrayWithSetM scheduler arr uWrite $ \offset sz e ->
--     loopM_ offset (< (offset + unSz sz)) (+1) (`uWrite` e)
-- {-# INLINE loadArrayM #-}

-- -- | Load an array into memory, just like `loadArrayM`. Except it also accepts a
-- -- function that is potentially optimized for setting many cells in a region to the same
-- -- value
-- --
-- -- @since 0.5.8
-- loadArrayWithSetM
--   :: Scheduler s ()
--   -> Array r ix e -- ^ Array that is being loaded
--   -> (Ix1 -> e -> ST s ()) -- ^ Function that writes an element into target array
--   -> (Ix1 -> Sz1 -> e -> ST s ()) -- ^ Function that efficiently sets a region of an array
--                                   -- to the supplied value target array
--   -> ST s ()
-- loadArrayWithSetM scheduler arr uWrite _ = loadArrayM scheduler arr uWrite
-- {-# INLINE loadArrayWithSetM #-}

-- iterArrayLinearWithStrideST
--   :: Scheduler s ()
--   -> Stride ix -- ^ Stride to use
--   -> Sz ix -- ^ Size of the target array affected by the stride.
--   -> Array r ix e -- ^ Array that is being loaded
--   -> (Int -> e -> ST s ()) -- ^ Function that writes an element into target array
--   -> ST s ()
