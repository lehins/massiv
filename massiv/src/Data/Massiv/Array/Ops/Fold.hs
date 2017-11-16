{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Massiv.Array.Ops.Fold
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Ops.Fold
  (
  -- ** Unstructured folds
    fold
  , sum
  , product
  , and
  , or
  , all
  , any
  -- ** Directed folds
  , foldl
  , ifoldl
  , foldr
  , ifoldr
  -- ** Sequential folds
  , foldlS
  , foldrS
  , ifoldlS
  , ifoldrS
  -- ** Special folds
  , foldrFB
  , lazyFoldlS
  , lazyFoldrS
  -- ** Parallel folds
  , foldlP
  , foldrP
  , ifoldlP
  , ifoldrP
  , foldlOnP
  , foldrOnP
  , ifoldlOnP
  , ifoldrOnP
  -- ** Monadic folds
  , foldlM
  , foldrM
  , foldlM_
  , foldrM_
  , ifoldlM
  , ifoldrM
  , ifoldlM_
  , ifoldrM_
  ) where

import           Control.Monad              (void, when)
import qualified Data.Foldable              as F
import           Data.Functor.Identity      (runIdentity)
import           Data.Massiv.Core.Common
import           Data.Massiv.Core.Scheduler
import           Prelude                    hiding (all, and, any, foldl, foldr,
                                             or, product, sum)
import           System.IO.Unsafe           (unsafePerformIO)


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


-- | Version of foldr that supports foldr/build list fusion implemented by GHC.
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



-- | /O(n)/ - Left fold, computed in parallel. Parallelization of folding
-- is implemented in such a way that an array is split into a number of chunks
-- of equal length, plus an extra one for the remainder. Number of chunks is the
-- same as number of available cores (capabilities) plus one, and each chunk is
-- individually folded by a separate core with a function @g@. Results from
-- folding each chunk are further folded with another function @f@, thus
-- allowing us to use information about the structure of an array during
-- folding.
--
-- >>> foldlP (flip (:)) [] (flip (:)) [] $ makeArray1D 11 id
-- [[10,9,8,7,6,5,4,3,2,1,0]]
--
-- This is how the result would look like if the above computation would be
-- performed in a program compiled with @+RTS -N3@, i.e. with 3 capabilities:
--
-- >>> foldlOnP [1,2,3] (flip (:)) [] (flip (:)) [] $ makeArray1D 11 id
-- [[10,9],[8,7,6],[5,4,3],[2,1,0]]
--
foldlP :: Source r ix e =>
          (b -> a -> b) -- ^ Chunk results folding function @f@.
       -> b -- ^ Accumulator for results of chunks folding.
       -> (a -> e -> a) -- ^ Folding function @g@.
       -> a -- ^ Accumulator. Will be applied to @g@ multiple times, thus must be neutral.
       -> Array r ix e -> IO b
foldlP g !tAcc f = ifoldlP g tAcc (\ x _ -> f x)
{-# INLINE foldlP #-}


-- | Just like `foldlP`, but allows you to specify which cores (capabilities) to run
-- computation on.
--
-- ==== __Examples__
--
-- The order in which chunked results will be supplied to function @f@ is
-- guaranteed to be consecutive, i.e. aligned with folding direction.
--
-- >>> fmap snd $ foldlOnP [1,2,3] (\(i, acc) x -> (i + 1, (i, x):acc)) (1, []) (flip (:)) [] $ makeArray1D 11 id
-- [(4,[10,9]),(3,[8,7,6]),(2,[5,4,3]),(1,[2,1,0])]
-- >>> fmap (P.zip [4,3..]) <$> foldlOnP [1,2,3] (flip (:)) [] (flip (:)) [] $ makeArray1D 11 id
-- [(4,[10,9]),(3,[8,7,6]),(2,[5,4,3]),(1,[2,1,0])]
--
foldlOnP
  :: Source r ix e
  => [Int] -> (b -> a -> b) -> b -> (a -> e -> a) -> a -> Array r ix e -> IO b
foldlOnP wIds g !tAcc f = ifoldlOnP wIds g tAcc (\ x _ -> f x)
{-# INLINE foldlOnP #-}


-- | Just like `ifoldlP`, but allows you to specify which cores to run
-- computation on.
ifoldlOnP :: Source r ix e =>
           [Int] -> (b -> a -> b) -> b -> (a -> ix -> e -> a) -> a -> Array r ix e -> IO b
ifoldlOnP wIds g !tAcc f !initAcc !arr = do
  let !sz = size arr
  results <-
    divideWork wIds sz $ \ !scheduler !chunkLength !totalLength !slackStart -> do
      loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start -> do
          scheduleWork scheduler $
            iterLinearM sz start (start + chunkLength) 1 (<) initAcc $ \ !i ix !acc ->
              let res = f acc ix (unsafeLinearIndex arr i)
              in res `seq` return res
      when (slackStart < totalLength) $
        scheduleWork scheduler $
        iterLinearM sz slackStart totalLength 1 (<) initAcc $ \ !i ix !acc ->
          let res = f acc ix (unsafeLinearIndex arr i)
          in res `seq` return res
  return $ F.foldl' g tAcc results
{-# INLINE ifoldlOnP #-}



-- | /O(n)/ - Left fold with an index aware function, computed in parallel. Just
-- like `foldlP`, except that folding function will receive an index of an
-- element it is being applied to.
ifoldlP :: Source r ix e =>
           (b -> a -> b) -> b -> (a -> ix -> e -> a) -> a -> Array r ix e -> IO b
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
          (a -> b -> b) -> b -> (e -> a -> a) -> a -> Array r ix e -> IO b
foldrP g !tAcc f = ifoldrP g tAcc (const f)
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
            [Int] -> (a -> b -> b) -> b -> (e -> a -> a) -> a -> Array r ix e -> IO b
foldrOnP wIds g !tAcc f = ifoldrOnP wIds g tAcc (const f)
{-# INLINE foldrOnP #-}


-- | Just like `ifoldrP`, but allows you to specify which cores to run
-- computation on.
ifoldrOnP :: Source r ix e =>
           [Int] -> (a -> b -> b) -> b -> (ix -> e -> a -> a) -> a -> Array r ix e -> IO b
ifoldrOnP wIds g !tAcc f !initAcc !arr = do
  let !sz = size arr
  results <-
    divideWork wIds sz $ \ !scheduler !chunkLength !totalLength !slackStart -> do
      when (slackStart < totalLength) $
        scheduleWork scheduler $
        iterLinearM sz (totalLength - 1) slackStart (-1) (>=) initAcc $ \ !i ix !acc ->
          return $ f ix (unsafeLinearIndex arr i) acc
      loopM_ slackStart (> 0) (subtract chunkLength) $ \ !start ->
        scheduleWork scheduler $
          iterLinearM sz (start - 1) (start - chunkLength) (-1) (>=) initAcc $ \ !i ix !acc ->
            let res = f ix (unsafeLinearIndex arr i) acc
            in res `seq` return res
  return $ F.foldr' g tAcc results
{-# INLINE ifoldrOnP #-}


-- | /O(n)/ - Right fold with an index aware function, computed in parallel.
-- Same as `ifoldlP`, except directed from the last element in the array towards
-- beginning.
ifoldrP :: Source r ix e =>
           (a -> b -> b) -> b -> (ix -> e -> a -> a) -> a -> Array r ix e -> IO b
ifoldrP = ifoldrOnP []
{-# INLINE ifoldrP #-}



-- | /O(n)/ - Unstructured fold of an array.
fold :: Source r ix e =>
        (e -> e -> e) -- ^ Folding function (like with left fold, first argument
                      -- is an accumulator)
     -> e -- ^ Initial element, that is neutral with respect to the folding
          -- function.
     -> Array r ix e -- ^ Source array
     -> e
fold f initAcc = \ arr ->
  case getComp arr of
    Seq        -> foldlS f initAcc arr
    ParOn wIds -> unsafePerformIO $ foldlOnP wIds f initAcc f initAcc arr
{-# INLINE fold #-}


-- | /O(n)/ - Compute sum of all elements.
sum :: (Source r ix e, Num e) =>
        Array r ix e -> e
sum = fold (+) 0
{-# INLINE sum #-}


-- | /O(n)/ - Compute product of all elements.
product :: (Source r ix e, Num e) =>
            Array r ix e -> e
product = fold (*) 1
{-# INLINE product #-}


-- | /O(n)/ - Compute conjunction of all elements.
and :: (Source r ix Bool) =>
       Array r ix Bool -> Bool
and = fold (&&) True
{-# INLINE and #-}


-- | /O(n)/ - Compute disjunction of all elements.
or :: Source r ix Bool =>
      Array r ix Bool -> Bool
or = fold (||) False
{-# INLINE or #-}


-- | Determines whether all element of the array satisfy the predicate.
all :: Source r ix e =>
       (e -> Bool) -> Array r ix e -> Bool
all f = foldl (&&) True (\acc el -> acc && f el) True
{-# INLINE all #-}

-- | Determines whether any element of the array satisfies the predicate.
any :: Source r ix e =>
       (e -> Bool) -> Array r ix e -> Bool
any f = foldl (||) False (\acc el -> acc || f el) False
{-# INLINE any #-}


-- | /O(n)/ - Left fold of an array.
foldl :: Source r ix e =>
     (b -> a -> b) -> b -> (a -> e -> a) -> a -> Array r ix e -> b
foldl f resAcc g initAcc = \ arr ->
  case getComp arr of
    Seq        -> f resAcc (foldlS g initAcc arr)
    ParOn wIds -> unsafePerformIO $ foldlOnP wIds f resAcc g initAcc arr
{-# INLINE foldl #-}


-- | /O(n)/ - Right fold of an array.
foldr :: Source r ix e =>
         (a -> b -> b) -> b -> (e -> a -> a) -> a -> Array r ix e -> b
foldr f resAcc g initAcc = \ arr ->
  case getComp arr of
    Seq        -> f (foldrS g initAcc arr) resAcc
    ParOn wIds -> unsafePerformIO $ foldrOnP wIds f resAcc g initAcc arr
{-# INLINE foldr #-}


-- | /O(n)/ - Index aware left fold of an array.
ifoldl :: Source r ix e =>
     (b -> a -> b) -> b -> (a -> ix -> e -> a) -> a -> Array r ix e -> b
ifoldl f resAcc g initAcc = \ arr ->
  case getComp arr of
    Seq        -> f resAcc (ifoldlS g initAcc arr)
    ParOn wIds -> unsafePerformIO $ ifoldlOnP wIds f resAcc g initAcc arr
{-# INLINE ifoldl #-}


-- | /O(n)/ - Index aware right fold of an array.
ifoldr :: Source r ix e =>
          (a -> b -> b) -> b -> (ix -> e -> a -> a) -> a -> Array r ix e -> b
ifoldr f resAcc g initAcc = \ arr ->
  case getComp arr of
    Seq        -> f (ifoldrS g initAcc arr) resAcc
    ParOn wIds -> unsafePerformIO $ ifoldrOnP wIds f resAcc g initAcc arr
{-# INLINE ifoldr #-}



-- -- | Just like `ifoldrP`, but allows you to specify which cores to run
-- -- computation on.
-- foldOnP :: Source r ix a =>
--            [Int] -> (a -> a -> a) -> a -> Array r ix a -> IO a
-- foldOnP wIds f !initAcc !arr = do
--   let !sz = size arr
--   results <-
--     splitWork wIds sz $ \ !scheduler !chunkLength !totalLength !slackStart -> do
--       when (slackStart < totalLength) $
--         scheduleWork scheduler $
--         JobRequest $
--         iterLinearM sz (totalLength - 1) slackStart (-1) (>=) initAcc $ \ !i _ !acc ->
--           return $ f (unsafeLinearIndex arr i) acc
--       loopM_ slackStart (> 0) (subtract chunkLength) $ \ !start ->
--         scheduleWork scheduler $
--           JobRequest $
--           iterLinearM sz (start - 1) (start - chunkLength) (-1) (>=) initAcc $ \ !i _ !acc ->
--             return $! f (unsafeLinearIndex arr i) acc
--   return $ F.foldl' f initAcc $ map jobResult results
-- {-# INLINE foldOnP #-}
