{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : Data.Array.Massiv.Ops.Fold
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Ops.Fold
  ( -- * Monadic folds
    foldlM
  , foldrM
  , foldlM_
  , foldrM_
  , ifoldlM
  , ifoldrM
  , ifoldlM_
  , ifoldrM_
  -- * Sequential folds
  , foldlS
  , lazyFoldlS
  , foldrS
  , lazyFoldrS
  , ifoldlS
  , ifoldrS
  , sumS
  , productS
  -- * Parallel folds
  , foldlP
  , foldrP
  , ifoldlP
  , ifoldrP
  , ifoldlOnP
  , ifoldrOnP
  , foldP
  , sumP
  , productP
  ) where

import Data.Functor.Identity
import           Control.DeepSeq             (NFData, deepseq)
import           Control.Monad               (void, when)
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Scheduler
import qualified Data.Foldable               as F (foldl', foldr')
import           Data.List                   (sortOn)
import           System.IO.Unsafe            (unsafePerformIO)
-- TODO: Use CPP to account for sortOn is only available since base-4.8
--import Data.List (sortBy)
--import Data.Function (on)



-- | /O(n)/ - Monadic left fold.
foldlM :: (Source r ix e, Monad m) => (a -> e -> m a) -> a -> Array r ix e -> m a
foldlM f = ifoldlM (\ a _ b -> f a b)
{-# INLINE foldlM #-}


-- | /O(n)/ - Monadic left fold, that discards the result.
foldlM_ :: (Source r ix e, Monad m) => (a -> e -> m a) -> a -> Array r ix e -> m ()
foldlM_ f = ifoldlM_ (\ a _ b -> f a b)
{-# INLINE foldlM_ #-}


-- | /O(n)/ - Monadic left fold with an index aware function.
ifoldlM :: (Source r ix e, Monad m) => (a -> ix -> e -> m a) -> a -> Array r ix e -> m a
ifoldlM f !acc !arr =
  iterM zeroIndex (size arr) 1 (<) acc $ \ !ix !a -> f a ix (unsafeIndex arr ix)
{-# INLINE ifoldlM #-}


-- | /O(n)/ - Monadic left fold with an index aware function, that discards the result.
ifoldlM_ :: (Source r ix e, Monad m) => (a -> ix -> e -> m a) -> a -> Array r ix e -> m ()
ifoldlM_ f acc = void . ifoldlM f acc
{-# INLINE ifoldlM_ #-}


-- | /O(n)/ - Monadic right fold.
foldrM :: (Source r ix e, Monad m) => (e -> a -> m a) -> a -> Array r ix e -> m a
foldrM f = ifoldrM (\_ e a -> f e a)
{-# INLINE foldrM #-}


-- | /O(n)/ - Monadic right fold, that discards the result.
foldrM_ :: (Source r ix e, Monad m) => (e -> a -> m a) -> a -> Array r ix e -> m ()
foldrM_ f = ifoldrM_ (\_ e a -> f e a)
{-# INLINE foldrM_ #-}


-- | /O(n)/ - Monadic right fold with an index aware function.
ifoldrM :: (Source r ix e, Monad m) => (ix -> e -> a -> m a) -> a -> Array r ix e -> m a
ifoldrM f !acc !arr =
  iterM (liftIndex (subtract 1) (size arr)) zeroIndex (-1) (>=) acc $ \ !ix !acc0 ->
    f ix (unsafeIndex arr ix) acc0
{-# INLINE ifoldrM #-}


-- | /O(n)/ - Monadic right fold with an index aware function, that discards the result.
ifoldrM_ :: (Source r ix e, Monad m) => (ix -> e -> a -> m a) -> a -> Array r ix e -> m ()
ifoldrM_ f !acc !arr = void $ ifoldrM f acc arr
{-# INLINE ifoldrM_ #-}



-- | /O(n)/ - Left fold, computed sequentially with lazy accumulator.
lazyFoldlS :: Source r ix e => (a -> e -> a) -> a -> Array r ix e -> a
lazyFoldlS f initAcc arr = go initAcc 0 where
    len = totalElem (size arr)
    go acc k | k < len = go (f acc (unsafeLinearIndex arr k)) (k + 1)
             | otherwise = acc
{-# INLINE lazyFoldlS #-}


-- | /O(n)/ - Right fold, computed sequentially with lazy accumulator.
lazyFoldrS :: Source r ix e => (e -> a -> a) -> a -> Array r ix e -> a
lazyFoldrS f initAcc arr = go initAcc (totalElem (size arr) - 1) where
  go acc k | k >= 0 = go (f (unsafeLinearIndex arr k) acc) (k + 1)
           | otherwise = acc
{-# INLINE lazyFoldrS #-}


-- | /O(n)/ - Left fold, computed sequentially.
foldlS :: Source r ix e => (a -> e -> a) -> a -> Array r ix e -> a
foldlS f = ifoldlS (\ a _ e -> f a e)
{-# INLINE foldlS #-}


-- | /O(n)/ - Right fold, computed sequentially.
foldrS :: Source r ix e => (e -> a -> a) -> a -> Array r ix e -> a
foldrS f = ifoldrS (\_ e a -> f e a)
{-# INLINE foldrS #-}


-- | /O(n)/ - Left fold with an index aware function, computed sequentially.
ifoldlS :: Source r ix e
        => (a -> ix -> e -> a) -> a -> Array r ix e -> a
ifoldlS f acc = runIdentity . ifoldlM (\ a ix e -> return $ f a ix e) acc
{-# INLINE ifoldlS #-}


-- | /O(n)/ - Right fold with an index aware function, computed sequentially.
ifoldrS :: Source r ix e => (ix -> e -> a -> a) -> a -> Array r ix e -> a
ifoldrS f acc = runIdentity . ifoldrM (\ ix e a -> return $ f ix e a) acc
{-# INLINE ifoldrS #-}


-- | /O(n)/ - Compute sum sequentially.
sumS :: (Source r ix e, Num e) =>
        Array r ix e -> e
sumS = foldrS (+) 0
{-# INLINE sumS #-}


-- | /O(n)/ - Compute product sequentially.
productS :: (Source r ix e, Num e) =>
            Array r ix e -> e
productS = foldrS (*) 1
{-# INLINE productS #-}


-- | /O(n)/ - Left fold, computed in parallel. Parallelization of folding
-- is implemented in such a way that an array is split into a number of chunks
-- of equal length, plus an extra one for the remainder. Number of chunks is the
-- same as number of available cores (capabilities) plus one, and each chunk is
-- individually folded by a separate core with a function @g@. Results from
-- folding each chunk are further folded with another function @f@, thus
-- allowing us to use information about the strucutre of an array during
-- folding.
--
-- ==== __Examples__
--
-- Emulate different number of cores. (/Note/: @setNumCapabilities@ does not
-- actually affect number of parallel workers)
--
-- >>> :m Control.Concurrent
-- >>> foldlP (flip (:)) [] (flip (:)) [] $ makeArray1D 11 id
-- [[10,9,8,7,6,5,4,3,2,1,0]]
-- >>> setNumCapabilities 3
-- >>> foldlP (flip (:)) [] (flip (:)) [] $ makeArray1D 11 id
-- [[10,9],[8,7,6],[5,4,3],[2,1,0]]
--
-- The order in which chunks folding results will be supplied to function @f@ is
-- guaranteed to be consecutive, i.e. aligned with folding direction.
--
-- >>> setNumCapabilities 4
-- >>> foldlP (flip (:) . reverse) [] (flip (:)) [] $ makeArray1D 11 id
-- [[10,9,8],[5,4],[1,0],[3,2],[7,6]]
--
foldlP :: (NFData a, Source r ix e) =>
          (b -> a -> b) -- ^ Chunk results folding function @f@.
       -> b -- ^ Accumulator for results of chunks folding.
       -> (a -> e -> a) -- ^ Chunks folding function @g@.
       -> a -- ^ Accumulator for each chunk.
       -> Array r ix e -> IO b
foldlP g !tAcc f = ifoldlP g tAcc (\ x _ -> f x)
{-# INLINE foldlP #-}

-- | Just like `ifoldlP`, but allows you to specify which cores to run
-- computation on.
ifoldlOnP :: (NFData a, Source r ix e) =>
           [Int] -> (b -> a -> b) -> b -> (a -> ix -> e -> a) -> a -> Array r ix e -> IO b
ifoldlOnP wIds g !tAcc f !initAcc !arr = do
  let !sz = size arr
  results <-
    splitWork wIds sz $ \ !scheduler !chunkLength !totalLength !slackStart -> do
      jId <-
        loopM 0 (< slackStart) (+ chunkLength) 0 $ \ !start !jId -> do
          submitRequest scheduler $
            JobRequest jId $
            iterLinearM sz start (start + chunkLength) 1 (<) initAcc $ \ !i ix !acc ->
              let res = f acc ix (unsafeLinearIndex arr i)
              in res `deepseq` return res
          return (jId + 1)
      when (slackStart < totalLength) $
        submitRequest scheduler $
        JobRequest jId $
        iterLinearM sz slackStart totalLength 1 (<) initAcc $ \ !i ix !acc ->
          let res = f acc ix (unsafeLinearIndex arr i)
          in res `deepseq` return res
  return $ F.foldl' g tAcc $ map jobResult $ sortOn jobResultId results
{-# INLINE ifoldlOnP #-}

-- | /O(n)/ - Left fold with an index aware function, computed in parallel. Just
-- like `foldlP`, except that folding function will receive an index of an
-- element it is being applied to.
ifoldlP :: (NFData a, Source r ix e) =>
           (b -> a -> b) -> b -> (a -> ix -> e -> a) -> a -> Array r ix e -> IO b
ifoldlP = ifoldlOnP []
{-# INLINE ifoldlP #-}


-- | /O(n)/ - Left fold, computed in parallel. Same as `foldlP`, except directed
-- from the last element in the array towards beginning.
foldrP :: (NFData a, Source r ix e) =>
          (a -> b -> b) -> b -> (e -> a -> a) -> a -> Array r ix e -> IO b
foldrP g !tAcc f = ifoldrP g tAcc (const f)
{-# INLINE foldrP #-}


-- | Just like `ifoldrP`, but allows you to specify which cores to run
-- computation on.
ifoldrOnP :: (NFData a, Source r ix e) =>
           [Int] -> (a -> b -> b) -> b -> (ix -> e -> a -> a) -> a -> Array r ix e -> IO b
ifoldrOnP wIds g !tAcc f !initAcc !arr = do
  let !sz = size arr
  results <-
    splitWork wIds sz $ \ !scheduler !chunkLength !totalLength !slackStart -> do
      when (slackStart < totalLength) $
        submitRequest scheduler $
        JobRequest 0 $
        iterLinearM sz (totalLength - 1) slackStart (-1) (>=) initAcc $ \ !i ix !acc ->
          return $ f ix (unsafeLinearIndex arr i) acc
      loopM slackStart (> 0) (subtract chunkLength) 1 $ \ !start !jId -> do
        submitRequest scheduler $
          JobRequest jId $
          iterLinearM sz (start - 1) (start - chunkLength) (-1) (>=) initAcc $ \ !i ix !acc ->
            let res = f ix (unsafeLinearIndex arr i) acc
            in res `deepseq` return res
        return (jId + 1)
  return $
    F.foldr' g tAcc $ reverse $ map jobResult $ sortOn jobResultId results
{-# INLINE ifoldrOnP #-}


-- | /O(n)/ - Right fold with an index aware function, computed in parallel.
-- Same as `ifoldlP`, except directed from the last element in the array towards
-- beginning.
ifoldrP :: (NFData a, Source r ix e) =>
           (a -> b -> b) -> b -> (ix -> e -> a -> a) -> a -> Array r ix e -> IO b
ifoldrP = ifoldrOnP []
{-# INLINE ifoldrP #-}




-- | /O(n)/ - Unstructured fold, computed in parallel.
foldP :: (NFData e, Source r ix e) =>
         (e -> e -> e) -> e -> Array r ix e -> e
foldP f !initAcc = unsafePerformIO . foldlP f initAcc f initAcc
{-# INLINE foldP #-}



-- | /O(n)/ - Compute sum in parallel.
sumP :: (Source r ix e, NFData e, Num e) =>
        Array r ix e -> e
sumP = foldP (+) 0
{-# INLINE sumP #-}


-- | /O(n)/ - Compute product in parallel.
productP :: (Source r ix e, NFData e, Num e) =>
            Array r ix e -> e
productP = foldP (*) 1
{-# INLINE productP #-}


