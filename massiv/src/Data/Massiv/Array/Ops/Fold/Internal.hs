{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Massiv.Array.Ops.Fold.Internal
-- Copyright   : (c) Alexey Kuleshevich 2018
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
  , foldlInternal
  , foldrFB
  , lazyFoldlS
  , lazyFoldrS
  -- Parallel folds
  , foldlP
  , foldrP
  , ifoldlP
  , ifoldrP
  , foldlOnP
  , ifoldlIO
  , foldrOnP
  , ifoldlOnP
  , ifoldrOnP
  , ifoldrIO
  ) where

import           Control.Monad              (void, when)
import qualified Data.Foldable              as F
import           Data.Functor.Identity      (runIdentity)
import           Data.Massiv.Core
import           Data.Massiv.Core.Common
import           Data.Massiv.Core.Scheduler
import           Prelude                    hiding (all, and, any, foldl, foldr,
                                             maximum, minimum, or, product, sum)
import           System.IO.Unsafe           (unsafePerformIO)




-- | /O(n)/ - Unstructured fold of an array.
fold :: Source r ix e =>
        (e -> e -> e) -- ^ Folding function (like with left fold, first argument
                      -- is an accumulator)
     -> e -- ^ Initial element. Has to be neutral with respect to the folding
          -- function.
     -> Array r ix e -- ^ Source array
     -> e
fold f initAcc = foldlInternal f initAcc f initAcc
{-# INLINE fold #-}



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
  iterM zeroIndex (size arr) (pureIndex 1) (<) acc $ \ !ix !a -> f a ix (unsafeIndex arr ix)
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
  iterM (liftIndex (subtract 1) (size arr)) zeroIndex (pureIndex (-1)) (>=) acc $ \ !ix !acc0 ->
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
lazyFoldrS = foldrFB
{-# INLINE lazyFoldrS #-}


-- | /O(n)/ - Left fold, computed sequentially.
foldlS :: Source r ix e => (a -> e -> a) -> a -> Array r ix e -> a
foldlS f = ifoldlS (\ a _ e -> f a e)
{-# INLINE foldlS #-}


-- | /O(n)/ - Left fold with an index aware function, computed sequentially.
ifoldlS :: Source r ix e
        => (a -> ix -> e -> a) -> a -> Array r ix e -> a
ifoldlS f acc = runIdentity . ifoldlM (\ a ix e -> return $ f a ix e) acc
{-# INLINE ifoldlS #-}


-- | /O(n)/ - Right fold, computed sequentially.
foldrS :: Source r ix e => (e -> a -> a) -> a -> Array r ix e -> a
foldrS f = ifoldrS (\_ e a -> f e a)
{-# INLINE foldrS #-}


-- | Version of foldr that supports @foldr/build@ list fusion implemented by GHC.
foldrFB :: Source r ix e => (e -> b -> b) -> b -> Array r ix e -> b
foldrFB c n arr = go 0
  where
    !k = totalElem (size arr)
    go !i
      | i == k = n
      | otherwise = let !v = unsafeLinearIndex arr i in v `c` go (i + 1)
{-# INLINE [0] foldrFB #-}



-- | /O(n)/ - Right fold with an index aware function, computed sequentially.
ifoldrS :: Source r ix e => (ix -> e -> a -> a) -> a -> Array r ix e -> a
ifoldrS f acc = runIdentity . ifoldrM (\ ix e a -> return $ f ix e a) acc
{-# INLINE ifoldrS #-}



-- | /O(n)/ - Left fold, computed in parallel. Parallelization of folding is implemented in such a
-- way that an array is split into a number of chunks of equal length, plus an extra one for the
-- left over. Number of chunks is the same as number of available cores (capabilities) plus one, and
-- each chunk is individually folded by a separate core with a function @g@. Results from folding
-- each chunk are further folded with another function @f@, thus allowing us to use information
-- about the structure of an array during folding.
--
-- ===__Examples__
--
-- >>> foldlP (flip (:)) [] (flip (:)) [] $ makeArrayR U Seq (Ix1 11) id
-- [[10,9,8,7,6,5,4,3,2,1,0]]
--
-- And this is how the result would look like if the above computation would be performed in a
-- program executed with @+RTS -N3@, i.e. with 3 capabilities:
--
-- >>> foldlOnP [1,2,3] (flip (:)) [] (flip (:)) [] $ makeArrayR U Seq (Ix1 11) id
-- [[10,9],[8,7,6],[5,4,3],[2,1,0]]
--
foldlP :: Source r ix e =>
          (a -> e -> a) -- ^ Folding function @g@.
       -> a -- ^ Accumulator. Will be applied to @g@ multiple times, thus must be neutral.
       -> (b -> a -> b) -- ^ Chunk results folding function @f@.
       -> b -- ^ Accumulator for results of chunks folding.
       -> Array r ix e -> IO b
foldlP f = ifoldlP (\ x _ -> f x)
{-# INLINE foldlP #-}


-- | Just like `foldlP`, but allows you to specify which cores (capabilities) to run computation
-- on. The order in which chunked results will be supplied to function @f@ is guaranteed to be
-- consecutive and aligned with the folding direction.
foldlOnP
  :: Source r ix e
  => [Int] -> (a -> e -> a) -> a -> (b -> a -> b) -> b -> Array r ix e -> IO b
foldlOnP wIds f = ifoldlOnP wIds (\ x _ -> f x)
{-# INLINE foldlOnP #-}



-- | Parallel left fold.
ifoldlIO :: Source r ix e =>
            [Int] -- ^ List of capabilities
         -> (a -> ix -> e -> IO a) -- ^ Index aware folding IO action
         -> a -- ^ Accumulator
         -> (b -> a -> IO b) -- ^ Folding action that is applied to results of parallel fold
         -> b -- ^ Accumulator for chunks folding
         -> Array r ix e -> IO b
ifoldlIO wIds f !initAcc g !tAcc !arr = do
  let !sz = size arr
  results <-
    divideWork wIds sz $ \ !scheduler !chunkLength !totalLength !slackStart -> do
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


-- | Just like `ifoldlP`, but allows you to specify which cores to run
-- computation on.
ifoldlOnP :: Source r ix e =>
           [Int] -> (a -> ix -> e -> a) -> a -> (b -> a -> b) -> b -> Array r ix e -> IO b
ifoldlOnP wIds f initAcc g =
  ifoldlIO wIds (\acc ix -> return . f acc ix) initAcc (\acc -> return . g acc)
{-# INLINE ifoldlOnP #-}



-- | /O(n)/ - Left fold with an index aware function, computed in parallel. Just
-- like `foldlP`, except that folding function will receive an index of an
-- element it is being applied to.
ifoldlP :: Source r ix e =>
           (a -> ix -> e -> a) -> a -> (b -> a -> b) -> b -> Array r ix e -> IO b
ifoldlP = ifoldlOnP []
{-# INLINE ifoldlP #-}


-- | /O(n)/ - Right fold, computed in parallel. Same as `foldlP`, except directed
-- from the last element in the array towards beginning.
--
-- ==== __Examples__
--
-- >>> foldrP (++) [] (:) [] $ makeArray2D (3,4) id
-- [(0,0),(0,1),(0,2),(0,3),(1,0),(1,1),(1,2),(1,3),(2,0),(2,1),(2,2),(2,3)]
--
foldrP :: Source r ix e =>
          (e -> a -> a) -> a -> (a -> b -> b) -> b -> Array r ix e -> IO b
foldrP f = ifoldrP (const f)
{-# INLINE foldrP #-}


-- | Just like `foldrP`, but allows you to specify which cores to run
-- computation on.
--
-- ==== __Examples__
--
-- Number of wokers dictate the result structure:
--
-- >>> foldrOnP [1,2,3] (:) [] (:) [] $ makeArray1D 9 id
-- [[0,1,2],[3,4,5],[6,7,8]]
-- >>> foldrOnP [1,2,3] (:) [] (:) [] $ makeArray1D 10 id
-- [[0,1,2],[3,4,5],[6,7,8],[9]]
-- >>> foldrOnP [1,2,3] (:) [] (:) [] $ makeArray1D 12 id
-- [[0,1,2,3],[4,5,6,7],[8,9,10,11]]
--
-- But most of the time that structure is of no importance:
--
-- >>> foldrOnP [1,2,3] (++) [] (:) [] $ makeArray1D 10 id
-- [0,1,2,3,4,5,6,7,8,9]
--
-- Same as `foldlOnP`, order is guaranteed to be consecutive and in proper direction:
--
-- >>> fmap snd $ foldrOnP [1,2,3] (\x (i, acc) -> (i + 1, (i, x):acc)) (1, []) (:) [] $ makeArray1D 11 id
-- [(4,[0,1,2]),(3,[3,4,5]),(2,[6,7,8]),(1,[9,10])]
-- >>> fmap (P.zip [4,3..]) <$> foldrOnP [1,2,3] (:) [] (:) [] $ makeArray1D 11 id
-- [(4,[0,1,2]),(3,[3,4,5]),(2,[6,7,8]),(1,[9,10])]
--
foldrOnP :: Source r ix e =>
            [Int] -> (e -> a -> a) -> a -> (a -> b -> b) -> b -> Array r ix e -> IO b
foldrOnP wIds f = ifoldrOnP wIds (const f)
{-# INLINE foldrOnP #-}


-- | Parallel right fold. Differs from `ifoldrP` in that it accepts `IO` actions instead of the
-- usual pure functions as arguments.
ifoldrIO :: Source r ix e =>
           [Int] -> (ix -> e -> a -> IO a) -> a -> (a -> b -> IO b) -> b -> Array r ix e -> IO b
ifoldrIO wIds f !initAcc g !tAcc !arr = do
  let !sz = size arr
  results <-
    divideWork wIds sz $ \ !scheduler !chunkLength !totalLength !slackStart -> do
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


-- | /O(n)/ - Right fold with an index aware function, computed in parallel.
-- Same as `ifoldlP`, except directed from the last element in the array towards
-- beginning.
ifoldrOnP :: Source r ix e =>
           [Int] -> (ix -> e -> a -> a) -> a -> (a -> b -> b) -> b -> Array r ix e -> IO b
ifoldrOnP wIds f !initAcc g =
  ifoldrIO wIds (\ix e -> return . f ix e) initAcc (\e -> return . g e)
{-# INLINE ifoldrOnP #-}


-- | Just like `ifoldrOnP`, but allows you to specify which cores to run computation on.
ifoldrP :: Source r ix e =>
           (ix -> e -> a -> a) -> a -> (a -> b -> b) -> b -> Array r ix e -> IO b
ifoldrP = ifoldrOnP []
{-# INLINE ifoldrP #-}


-- | This folding function breaks referencial transparency on some functions
-- @f@, therefore it is kept here for internal use only.
foldlInternal :: Source r ix e =>
         (a -> e -> a) -> a -> (b -> a -> b) -> b -> Array r ix e -> b
foldlInternal g initAcc f resAcc = \ arr ->
  case getComp arr of
    Seq        -> f resAcc (foldlS g initAcc arr)
    ParOn wIds -> unsafePerformIO $ foldlOnP wIds g initAcc f resAcc arr
{-# INLINE foldlInternal #-}
