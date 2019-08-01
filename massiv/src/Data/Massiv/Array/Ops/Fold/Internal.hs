{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Massiv.Array.Ops.Fold.Internal
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Ops.Fold.Internal
  (
    foldlS
  , foldrS
  , ifoldlS
  , ifoldrS
  --Monadic
  , foldlM
  , foldrM
  , foldlM_
  , foldrM_
  , ifoldlM
  , ifoldrM
  , ifoldlM_
  , ifoldrM_
  --Special folds
  , fold
  , foldMono
  , foldlInternal
  , ifoldlInternal
  , foldrFB
  , lazyFoldlS
  , lazyFoldrS
  -- Parallel folds
  , foldlP
  , foldrP
  , ifoldlP
  , ifoldrP
  , ifoldlIO
  , ifoldrIO
  , splitReduce
  , splitReduceInternal
  , splitReduce2
  , splitReduce2Internal
  ) where

import Control.Monad (void, when)
import Control.Scheduler
import qualified Data.Foldable as F
import Data.Functor.Identity (runIdentity)
import Data.Massiv.Core.Common
import Prelude hiding (foldl, foldr)
import System.IO.Unsafe (unsafePerformIO)


-- | /O(n)/ - Unstructured fold of an array.
--
-- @since 0.3.0
fold ::
     (Monoid e, Source r ix e)
  => Array r ix e -- ^ Source array
  -> e
fold = foldlInternal mappend mempty mappend mempty
{-# INLINE fold #-}


-- | /O(n)/ - This is exactly like `Data.Foldable.foldMap`, but for arrays. Fold over an array,
-- while converting each element into a `Monoid`. Also known as map-reduce. If elements of the array
-- are already a `Monoid` you can use `fold` instead.
--
-- @since 0.1.4
foldMono ::
     (Source r ix e, Monoid m)
  => (e -> m) -- ^ Convert each element of an array to an appropriate `Monoid`.
  -> Array r ix e -- ^ Source array
  -> m
foldMono f = foldlInternal (\a e -> a `mappend` f e) mempty mappend mempty
{-# INLINE foldMono #-}


-- | /O(n)/ - Monadic left fold.
--
-- @since 0.1.0
foldlM :: (Source r ix e, Monad m) => (a -> e -> m a) -> a -> Array r ix e -> m a
foldlM f = ifoldlM (\ a _ b -> f a b)
{-# INLINE foldlM #-}


-- | /O(n)/ - Monadic left fold, that discards the result.
--
-- @since 0.1.0
foldlM_ :: (Source r ix e, Monad m) => (a -> e -> m a) -> a -> Array r ix e -> m ()
foldlM_ f = ifoldlM_ (\ a _ b -> f a b)
{-# INLINE foldlM_ #-}


-- | /O(n)/ - Monadic left fold with an index aware function.
--
-- @since 0.1.0
ifoldlM :: (Source r ix e, Monad m) => (a -> ix -> e -> m a) -> a -> Array r ix e -> m a
ifoldlM f !acc !arr =
  iterM zeroIndex (unSz (size arr)) (pureIndex 1) (<) acc $ \ !ix !a -> f a ix (unsafeIndex arr ix)
{-# INLINE ifoldlM #-}


-- | /O(n)/ - Monadic left fold with an index aware function, that discards the result.
--
-- @since 0.1.0
ifoldlM_ :: (Source r ix e, Monad m) => (a -> ix -> e -> m a) -> a -> Array r ix e -> m ()
ifoldlM_ f acc = void . ifoldlM f acc
{-# INLINE ifoldlM_ #-}


-- | /O(n)/ - Monadic right fold.
--
-- @since 0.1.0
foldrM :: (Source r ix e, Monad m) => (e -> a -> m a) -> a -> Array r ix e -> m a
foldrM f = ifoldrM (\_ e a -> f e a)
{-# INLINE foldrM #-}


-- | /O(n)/ - Monadic right fold, that discards the result.
--
-- @since 0.1.0
foldrM_ :: (Source r ix e, Monad m) => (e -> a -> m a) -> a -> Array r ix e -> m ()
foldrM_ f = ifoldrM_ (\_ e a -> f e a)
{-# INLINE foldrM_ #-}


-- | /O(n)/ - Monadic right fold with an index aware function.
--
-- @since 0.1.0
ifoldrM :: (Source r ix e, Monad m) => (ix -> e -> a -> m a) -> a -> Array r ix e -> m a
ifoldrM f !acc !arr =
  iterM (liftIndex (subtract 1) (unSz (size arr))) zeroIndex (pureIndex (-1)) (>=) acc $ \ !ix !acc0 ->
    f ix (unsafeIndex arr ix) acc0
{-# INLINE ifoldrM #-}


-- | /O(n)/ - Monadic right fold with an index aware function, that discards the result.
--
-- @since 0.1.0
ifoldrM_ :: (Source r ix e, Monad m) => (ix -> e -> a -> m a) -> a -> Array r ix e -> m ()
ifoldrM_ f !acc !arr = void $ ifoldrM f acc arr
{-# INLINE ifoldrM_ #-}



-- | /O(n)/ - Left fold, computed sequentially with lazy accumulator.
--
-- @since 0.1.0
lazyFoldlS :: Source r ix e => (a -> e -> a) -> a -> Array r ix e -> a
lazyFoldlS f initAcc arr = go initAcc 0 where
    len = totalElem (size arr)
    go acc k | k < len = go (f acc (unsafeLinearIndex arr k)) (k + 1)
             | otherwise = acc
{-# INLINE lazyFoldlS #-}


-- | /O(n)/ - Right fold, computed sequentially with lazy accumulator.
--
-- @since 0.1.0
lazyFoldrS :: Source r ix e => (e -> a -> a) -> a -> Array r ix e -> a
lazyFoldrS = foldrFB
{-# INLINE lazyFoldrS #-}


-- | /O(n)/ - Left fold, computed sequentially.
--
-- @since 0.1.0
foldlS :: Source r ix e => (a -> e -> a) -> a -> Array r ix e -> a
foldlS f = ifoldlS (\ a _ e -> f a e)
{-# INLINE foldlS #-}


-- | /O(n)/ - Left fold with an index aware function, computed sequentially.
--
-- @since 0.1.0
ifoldlS :: Source r ix e
        => (a -> ix -> e -> a) -> a -> Array r ix e -> a
ifoldlS f acc = runIdentity . ifoldlM (\ a ix e -> return $ f a ix e) acc
{-# INLINE ifoldlS #-}


-- | /O(n)/ - Right fold, computed sequentially.
--
-- @since 0.1.0
foldrS :: Source r ix e => (e -> a -> a) -> a -> Array r ix e -> a
foldrS f = ifoldrS (\_ e a -> f e a)
{-# INLINE foldrS #-}


-- | /O(n)/ - Right fold with an index aware function, computed sequentially.
--
-- @since 0.1.0
ifoldrS :: Source r ix e => (ix -> e -> a -> a) -> a -> Array r ix e -> a
ifoldrS f acc = runIdentity . ifoldrM (\ ix e a -> return $ f ix e a) acc
{-# INLINE ifoldrS #-}


-- | Version of foldr that supports @foldr/build@ list fusion implemented by GHC.
--
-- @since 0.1.0
foldrFB :: Source r ix e => (e -> b -> b) -> b -> Array r ix e -> b
foldrFB c n arr = go 0
  where
    !k = totalElem (size arr)
    go !i
      | i == k = n
      | otherwise = let !v = unsafeLinearIndex arr i in v `c` go (i + 1)
{-# INLINE [0] foldrFB #-}



-- | /O(n)/ - Left fold, computed with respect of array's computation strategy. Because we do
-- potentially split the folding among many threads, we also need a combining function and an
-- accumulator for the results. Depending on the number of threads being used, results can be
-- different, hence is the `MonadIO` constraint.
--
-- ===__Examples__
--
-- >>> import Data.Massiv.Array
-- >>> foldlP (flip (:)) [] (flip (:)) [] $ makeArrayR D Seq (Sz1 6) id
-- [[5,4,3,2,1,0]]
-- >>> foldlP (flip (:)) [] (++) [] $ makeArrayR D Seq (Sz1 6) id
-- [5,4,3,2,1,0]
-- >>> foldlP (flip (:)) [] (flip (:)) [] $ makeArrayR D (ParN 3) (Sz1 6) id
-- [[5,4],[3,2],[1,0]]
-- >>> foldlP (flip (:)) [] (++) [] $ makeArrayR D (ParN 3) (Sz1 6) id
-- [1,0,3,2,5,4]
--
-- @since 0.1.0
foldlP :: (MonadIO m, Source r ix e) =>
          (a -> e -> a) -- ^ Folding function @g@.
       -> a -- ^ Accumulator. Will be applied to @g@ multiple times, thus must be neutral.
       -> (b -> a -> b) -- ^ Chunk results folding function @f@.
       -> b -- ^ Accumulator for results of chunks folding.
       -> Array r ix e -> m b
foldlP f fAcc g gAcc = liftIO . ifoldlP (\ x _ -> f x) fAcc g gAcc
{-# INLINE foldlP #-}

-- | /O(n)/ - Left fold with an index aware function, computed in parallel. Just
-- like `foldlP`, except that folding function will receive an index of an
-- element it is being applied to.
--
-- @since 0.1.0
ifoldlP :: (MonadIO m, Source r ix e) =>
           (a -> ix -> e -> a) -> a -> (b -> a -> b) -> b -> Array r ix e -> m b
ifoldlP f fAcc g gAcc =
  liftIO . ifoldlIO (\acc ix -> return . f acc ix) fAcc (\acc -> return . g acc) gAcc
{-# INLINE ifoldlP #-}


-- | /O(n)/ - Right fold, computed with respect to computation strategy. Same as `foldlP`, except
-- directed from the last element in the array towards beginning.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> foldrP (:) [] (++) [] $ makeArrayR D (ParN 2) (Sz2 2 3) fromIx2
-- [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
-- >>> foldrP (:) [] (:) [] $ makeArrayR D Seq (Sz1 6) id
-- [[0,1,2,3,4,5]]
-- >>> foldrP (:) [] (:) [] $ makeArrayR D (ParN 3) (Sz1 6) id
-- [[0,1],[2,3],[4,5]]
--
-- @since 0.1.0
foldrP :: (MonadIO m, Source r ix e) =>
          (e -> a -> a) -> a -> (a -> b -> b) -> b -> Array r ix e -> m b
foldrP f fAcc g gAcc = liftIO . ifoldrP (const f) fAcc g gAcc
{-# INLINE foldrP #-}


-- | /O(n)/ - Right fold with an index aware function, while respecting the computation strategy.
-- Same as `ifoldlP`, except directed from the last element in the array towards
-- beginning, but also row-major.
--
-- @since 0.1.0
ifoldrP ::
     (MonadIO m, Source r ix e)
  => (ix -> e -> a -> a)
  -> a
  -> (a -> b -> b)
  -> b
  -> Array r ix e
  -> m b
ifoldrP f fAcc g gAcc = liftIO . ifoldrIO (\ix e -> pure . f ix e) fAcc (\e -> pure . g e) gAcc
{-# INLINE ifoldrP #-}


-- | This folding function breaks referential transparency on some functions
-- @f@, therefore it is kept here for internal use only.
foldlInternal :: Source r ix e => (a -> e -> a) -> a -> (b -> a -> b) -> b -> Array r ix e -> b
foldlInternal g initAcc f resAcc = unsafePerformIO . foldlP g initAcc f resAcc
{-# INLINE foldlInternal #-}


ifoldlInternal :: Source r ix e => (a -> ix -> e -> a) -> a -> (b -> a -> b) -> b -> Array r ix e -> b
ifoldlInternal g initAcc f resAcc = unsafePerformIO . ifoldlP g initAcc f resAcc
{-# INLINE ifoldlInternal #-}


-- | Similar to `ifoldlP`, except that folding functions themselves do live in IO
--
-- @since 0.1.0
ifoldlIO ::
     (MonadUnliftIO m, Source r ix e)
  => (a -> ix -> e -> m a) -- ^ Index aware folding IO action
  -> a -- ^ Accumulator
  -> (b -> a -> m b) -- ^ Folding action that is applied to the results of a parallel fold
  -> b -- ^ Accumulator for chunks folding
  -> Array r ix e
  -> m b
ifoldlIO f !initAcc g !tAcc !arr = do
  let !sz = size arr
      !totalLength = totalElem sz
  results <-
    withScheduler (getComp arr) $ \scheduler ->
      splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
        loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
          scheduleWork scheduler $
          iterLinearM sz start (start + chunkLength) 1 (<) initAcc $ \ !i ix !acc ->
            f acc ix (unsafeLinearIndex arr i)
        when (slackStart < totalLength) $
          scheduleWork scheduler $
          iterLinearM sz slackStart totalLength 1 (<) initAcc $ \ !i ix !acc ->
            f acc ix (unsafeLinearIndex arr i)
  F.foldlM g tAcc results
{-# INLINE ifoldlIO #-}

-- | Split an array into linear row-major vector chunks and apply an action to each of
-- them. Number of chunks will depend on the computation strategy. Results of each action
-- will be combined with a folding action.
--
-- @since 0.4.1
splitReduce ::
     (MonadUnliftIO m, Source r ix e)
  => (Scheduler m a -> Array r Ix1 e -> m a)
  -> (b -> a -> m b) -- ^ Folding action that is applied to the results of a parallel fold
  -> b -- ^ Accumulator for chunks folding
  -> Array r ix e
  -> m b
splitReduce f g !tAcc !arr = do
  let !totalLength = totalElem (size arr)
  results <-
    withScheduler (getComp arr) $ \scheduler ->
      splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
        loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
          scheduleWork scheduler $ f scheduler $ unsafeLinearSlice start (SafeSz chunkLength) arr
        when (slackStart < totalLength) $
          scheduleWork scheduler $
          f scheduler $ unsafeLinearSlice slackStart (SafeSz (totalLength - slackStart)) arr
  F.foldlM g tAcc results
{-# INLINE splitReduce #-}


splitReduceInternal ::
     (Source r ix e) => (Array r Ix1 e -> a) -> (b -> a -> b) -> b -> Array r ix e -> b
splitReduceInternal f g tAcc =
  unsafePerformIO . splitReduce (\_ -> pure . f) (\acc -> pure . g acc) tAcc
{-# INLINE splitReduceInternal #-}


-- | Split two arrays into linear row-major vector chunks and apply an action to each of
-- them pointwise. Similar to zipWith followed by a foldl on each individual chunk. Number
-- of chunks will depend on the computation strategy. Results of each action will be
-- combined with a folding action. If number of elements in both arrays do not match up
-- the smallest one will be used.
--
-- @since 0.4.1
splitReduce2 ::
     (MonadUnliftIO m, Source r1 ix e1, Source r2 ix e2)
  => (Scheduler m a -> Array r1 Ix1 e1 -> Array r2 Ix1 e2 -> m a)
  -> (b -> a -> m b) -- ^ Folding action that is applied to the results of a parallel fold
  -> b -- ^ Accumulator for chunks folding
  -> Array r1 ix e1
  -> Array r2 ix e2
  -> m b
splitReduce2 f g !tAcc !arr1 !arr2 = do
  let !totalLength = min (totalElem (size arr1)) (totalElem (size arr2))
  results <-
    withScheduler (getComp arr1 <> getComp arr2) $ \scheduler ->
      splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
        loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
          let slice1 = unsafeLinearSlice start (SafeSz chunkLength) arr1
              slice2 = unsafeLinearSlice start (SafeSz chunkLength) arr2
           in scheduleWork scheduler $ f scheduler slice1 slice2
        when (slackStart < totalLength) $
          scheduleWork scheduler $
          let slackLength = SafeSz (totalLength - slackStart)
              slice1 = unsafeLinearSlice slackStart slackLength arr1
              slice2 = unsafeLinearSlice slackStart slackLength arr2
           in f scheduler slice1 slice2
  F.foldlM g tAcc results
{-# INLINE splitReduce2 #-}


splitReduce2Internal ::
     (Source r1 ix e1, Source r2 ix e2)
  => (Array r1 Ix1 e1 -> Array r2 Ix1 e2 -> a)
  -> (b -> a -> b)
  -> b
  -> Array r1 ix e1
  -> Array r2 ix e2
  -> b
splitReduce2Internal f g tAcc arr1 =
  unsafePerformIO . splitReduce2 (\_ a1 -> pure . f a1) (\acc -> pure . g acc) tAcc arr1
{-# INLINE splitReduce2Internal #-}



-- | Similar to `ifoldrP`, except that folding functions themselves do live in IO
--
-- @since 0.1.0
ifoldrIO :: (MonadUnliftIO m, Source r ix e) =>
           (ix -> e -> a -> m a) -> a -> (a -> b -> m b) -> b -> Array r ix e -> m b
ifoldrIO f !initAcc g !tAcc !arr = do
  let !sz = size arr
      !totalLength = totalElem sz
  results <-
    withScheduler (getComp arr) $ \ scheduler ->
      splitLinearly (numWorkers scheduler) totalLength $ \ chunkLength slackStart -> do
        when (slackStart < totalLength) $
          scheduleWork scheduler $
          iterLinearM sz (totalLength - 1) slackStart (-1) (>=) initAcc $ \ !i ix !acc ->
            f ix (unsafeLinearIndex arr i) acc
        loopM_ slackStart (> 0) (subtract chunkLength) $ \ !start ->
          scheduleWork scheduler $
            iterLinearM sz (start - 1) (start - chunkLength) (-1) (>=) initAcc $ \ !i ix !acc ->
              f ix (unsafeLinearIndex arr i) acc
  F.foldlM (flip g) tAcc results
{-# INLINE ifoldrIO #-}
