{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
-- |
-- Module      : Data.Massiv.Array.Ops.Sort
-- Copyright   : (c) Alexey Kuleshevich 2018-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Ops.Sort
  ( tally
  , quicksort
  , quicksortBy
  , quicksortByM
  , quicksortM_
  , quicksortByM_
  , unsafeUnstablePartitionRegionM
  ) where

import Control.Monad.IO.Unlift
import Control.Monad (when)
import Control.Monad.Primitive
import Control.Scheduler
import Data.Massiv.Array.Delayed.Stream
import Data.Massiv.Array.Mutable
import Data.Massiv.Array.Ops.Transform
import Data.Massiv.Core.Common
import Data.Massiv.Vector (scatMaybes, sunfoldrN)
import System.IO.Unsafe

-- | Count number of occurrences of each element in the array. Results will be
-- sorted in ascending order of the element.
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Array as A
-- >>> xs = fromList Seq [2, 4, 3, 2, 4, 5, 2, 1] :: Array P Ix1 Int
-- >>> xs
-- Array P Seq (Sz1 8)
--   [ 2, 4, 3, 2, 4, 5, 2, 1 ]
-- >>> tally xs
-- Array DS Seq (Sz1 5)
--   [ (1,1), (2,3), (3,1), (4,2), (5,1) ]
--
-- @since 0.4.4
tally :: (Manifest r e, Load r ix e, Ord e) => Array r ix e -> Vector DS (e, Int)
tally arr
  | isEmpty arr = setComp (getComp arr) empty
  | otherwise = scatMaybes $ sunfoldrN (liftSz2 (+) sz oneSz) count (0, 0, sorted ! 0)
  where
    sz@(Sz k) = size sorted
    count (!i, !n, !prev)
      | i < k =
        let !e' = unsafeLinearIndex sorted i
         in if prev == e'
              then Just (Nothing, (i + 1, n + 1, prev))
              else Just (Just (prev, n), (i + 1, 1, e'))
      | otherwise = Just (Just (prev, n), (i + 1, n, prev))
    {-# INLINE count #-}
    sorted = quicksort $ flatten arr
{-# INLINE tally #-}



-- | Partition a segment of a vector. Starting and ending indices are unchecked.
--
-- @since 1.0.0
unsafeUnstablePartitionRegionM ::
     forall r e m. (Manifest r e, PrimMonad m)
  => MVector (PrimState m) r e
  -> (e -> m Bool)
  -> Ix1 -- ^ Start index of the region
  -> Ix1 -- ^ End index of the region
  -> m Ix1
unsafeUnstablePartitionRegionM marr f start end = fromLeft start (end + 1)
  where
    fromLeft i j
      | i == j = pure i
      | otherwise = do
        e <- f =<< unsafeLinearRead marr i
        if e
          then fromLeft (i + 1) j
          else fromRight i (j - 1)
    fromRight i j
      | i == j = pure i
      | otherwise = do
        x <- unsafeLinearRead marr j
        e <- f x
        if e
          then do
            unsafeLinearWrite marr j =<< unsafeLinearRead marr i
            unsafeLinearWrite marr i x
            fromLeft (i + 1) j
          else fromRight i (j - 1)
{-# INLINE unsafeUnstablePartitionRegionM #-}


-- | This is an implementation of [Quicksort](https://en.wikipedia.org/wiki/Quicksort), which is an
-- efficient, but unstable sort that uses Median-of-three for pivot choosing, as such it performs
-- very well not only for random values, but also for common edge cases like already sorted,
-- reversed sorted and arrays with many duplicate elements. It will also respect the computation
-- strategy and will result in a nice speed up for systems with multiple CPUs.
--
-- @since 0.3.2
quicksort ::
     (Manifest r e, Ord e) => Vector r e -> Vector r e
quicksort arr = unsafePerformIO $ withMArray_ arr quicksortM_
{-# INLINE quicksort #-}


-- | Same as `quicksortBy`, but instead of `Ord` constraint expects a custom `Ordering`.
--
-- @since 0.6.1
quicksortByM ::
     (Manifest r e, MonadUnliftIO m) => (e -> e -> m Ordering) -> Vector r e -> m (Vector r e)
quicksortByM f arr = withRunInIO $ \run -> withMArray_ arr (quicksortByM_ (\x y -> run (f x y)))
{-# INLINE quicksortByM #-}

-- | Same as `quicksortBy`, but instead of `Ord` constraint expects a custom `Ordering`.
--
-- @since 0.6.1
quicksortBy :: Manifest r e => (e -> e -> Ordering) -> Vector r e -> Vector r e
quicksortBy f arr =
  unsafePerformIO $ withMArray_ arr (quicksortByM_ (\x y -> pure $ f x y))
{-# INLINE quicksortBy #-}

-- | Manifest version of `quicksort`
--
-- @since 0.3.2
quicksortM_ ::
     (Ord e, Manifest r e, MonadPrimBase s m)
  => Scheduler s ()
  -> MVector s r e
  -> m ()
quicksortM_ = quicksortInternalM_ (\e1 e2 -> pure $ e1 < e2) (\e1 e2 -> pure $ e1 == e2)
{-# INLINE quicksortM_ #-}


-- | Same as `quicksortM_`, but instead of `Ord` constraint expects a custom `Ordering`.
--
-- @since 0.6.1
quicksortByM_ ::
     (Manifest r e, MonadPrimBase s m)
  => (e -> e -> m Ordering)
  -> Scheduler s ()
  -> MVector s r e
  -> m ()
quicksortByM_ compareM =
  quicksortInternalM_ (\x y -> (LT ==) <$> compareM x y) (\x y -> (EQ ==) <$> compareM x y)
{-# INLINE quicksortByM_ #-}


quicksortInternalM_ ::
     (Manifest r e, MonadPrimBase s m)
  => (e -> e -> m Bool)
  -> (e -> e -> m Bool)
  -> Scheduler s ()
  -> MVector s r e
  -> m ()
quicksortInternalM_ fLT fEQ scheduler marr =
  scheduleWork scheduler $ qsort (numWorkers scheduler) 0 (unSz (sizeOfMArray marr) - 1)
  where
    ltSwap i j = do
      ei <- unsafeLinearRead marr i
      ej <- unsafeLinearRead marr j
      lt <- fLT ei ej
      if lt
        then do
          unsafeLinearWrite marr i ej
          unsafeLinearWrite marr j ei
          pure ei
        else pure ej
    {-# INLINE ltSwap #-}
    getPivot lo hi = do
      let !mid = (hi + lo) `div` 2
      _ <- ltSwap mid lo
      _ <- ltSwap hi lo
      ltSwap mid hi
    {-# INLINE getPivot #-}
    qsort !n !lo !hi =
      when (lo < hi) $ do
        p <- getPivot lo hi
        l <- unsafeUnstablePartitionRegionM marr (`fLT` p) lo (hi - 1)
        h <- unsafeUnstablePartitionRegionM marr (`fEQ` p) l hi
        if n > 0
          then do
            let !n' = n - 1
            scheduleWork scheduler $ qsort n' lo (l - 1)
            scheduleWork scheduler $ qsort n' h hi
          else do
            qsort n lo (l - 1)
            qsort n h hi
{-# INLINE quicksortInternalM_ #-}
