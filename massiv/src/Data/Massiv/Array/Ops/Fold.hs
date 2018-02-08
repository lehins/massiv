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
  , minimum
  , maximum
  , sum
  , product
  , and
  , or
  , all
  , any
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
  , ifoldlIO
  , foldrOnP
  , ifoldlOnP
  , ifoldrOnP
  , ifoldrIO
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
import           Data.Massiv.Core
import           Data.Massiv.Core.Common
import           Data.Massiv.Core.Scheduler
import           Prelude                    hiding (all, and, any, foldl, foldr,
                                             maximum, minimum, or, product, sum)
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
          (a -> e -> a) -- ^ Folding function @g@.
       -> a -- ^ Accumulator. Will be applied to @g@ multiple times, thus must be neutral.
       -> (b -> a -> b) -- ^ Chunk results folding function @f@.
       -> b -- ^ Accumulator for results of chunks folding.
       -> Array r ix e -> IO b
foldlP f = ifoldlP (\ x _ -> f x)
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
  => [Int] -> (a -> e -> a) -> a -> (b -> a -> b) -> b -> Array r ix e -> IO b
foldlOnP wIds f = ifoldlOnP wIds (\ x _ -> f x)
{-# INLINE foldlOnP #-}



-- | Parallel left fold.
ifoldlIO :: Source r ix e =>
            [Int] -- ^ List of capabilities
         -> (a -> ix -> e -> IO a) -- ^ Index aware folding function
         -> a -- ^ Accumulator
         -> (b -> a -> IO b) -- ^ Folding funding that is applied to results of parallel fold
         -> b -- ^ Accumulator for chunks folding
         -> Array r ix e -> IO b
ifoldlIO wIds f !initAcc g !tAcc !arr = do
  let !sz = size arr
  results <-
    divideWork wIds sz $ \ !scheduler !chunkLength !totalLength !slackStart -> do
      loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start -> do
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


-- | Parallel right fold
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
  F.foldrM g tAcc results
{-# INLINE ifoldrIO #-}

-- | Just like `ifoldrP`, but allows you to specify which cores to run
-- computation on.
ifoldrOnP :: Source r ix e =>
           [Int] -> (ix -> e -> a -> a) -> a -> (a -> b -> b) -> b -> Array r ix e -> IO b
ifoldrOnP wIds f !initAcc g =
  ifoldrIO wIds (\ix e -> return . f ix e) initAcc (\e -> return . g e)
{-# INLINE ifoldrOnP #-}


-- | /O(n)/ - Right fold with an index aware function, computed in parallel.
-- Same as `ifoldlP`, except directed from the last element in the array towards
-- beginning.
ifoldrP :: Source r ix e =>
           (ix -> e -> a -> a) -> a -> (a -> b -> b) -> b -> Array r ix e -> IO b
ifoldrP = ifoldrOnP []
{-# INLINE ifoldrP #-}



-- | /O(n)/ - Unstructured fold of an array.
fold :: Source r ix e =>
        (e -> e -> e) -- ^ Folding function (like with left fold, first argument
                      -- is an accumulator)
     -> e -- ^ Initial element. Has to be neutral with respect to the folding
          -- function.
     -> Array r ix e -- ^ Source array
     -> e
fold f initAcc = foldl f initAcc f initAcc
{-# INLINE fold #-}

-- | /O(n)/ - Compute maximum of all elements.
maximum :: (Source r ix e, Ord e) =>
           Array r ix e -> e
maximum = \arr ->
  if isEmpty arr
    then error "Data.Massiv.Array.maximum - empty"
    else fold max (evaluateAt arr zeroIndex) arr
{-# INLINE maximum #-}


-- | /O(n)/ - Compute minimum of all elements.
minimum :: (Source r ix e, Ord e) =>
           Array r ix e -> e
minimum = \arr ->
  if isEmpty arr
    then error "Data.Massiv.Array.minimum - empty"
    else fold max (evaluateAt arr zeroIndex) arr
{-# INLINE minimum #-}


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
all f = foldl (\acc el -> acc && f el) True (&&) True
{-# INLINE all #-}

-- | Determines whether any element of the array satisfies the predicate.
any :: Source r ix e =>
       (e -> Bool) -> Array r ix e -> Bool
any f = foldl (\acc el -> acc || f el) False (||) False
{-# INLINE any #-}


-- | This folding function breaks referencial transparency on some functions
-- @f@, therefore it is kept here for internal use only.
foldl :: Source r ix e =>
         (a -> e -> a) -> a -> (b -> a -> b) -> b -> Array r ix e -> b
foldl g initAcc f resAcc = \ arr ->
  case getComp arr of
    Seq        -> f resAcc (foldlS g initAcc arr)
    ParOn wIds -> unsafePerformIO $ foldlOnP wIds g initAcc f resAcc arr
{-# INLINE foldl #-}


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
