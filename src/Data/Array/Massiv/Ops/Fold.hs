{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
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
  --, toListP'
  , lazyFoldlS
  , foldrS
  , foldrFB
  , lazyFoldrS
  , ifoldlS
  , ifoldrS
  , sumS
  , productS
  -- * Parallel folds
  , foldlP
  --, foldlP'
  , foldrP
  , ifoldlP
  , ifoldrP
  , foldlOnP
  , foldrOnP
  , ifoldlOnP
  , ifoldrOnP
  , foldP
  , sumP
  , productP
  ) where

--import Prelude hiding (map)
import           Control.DeepSeq                  (NFData, deepseq)
import           Control.Monad                    (when)
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Common.Ops
import           Data.Array.Massiv.Scheduler

import qualified Data.Foldable                    as F (foldl', foldr')
import           Data.List                        (sortOn)
import           System.IO.Unsafe                 (unsafePerformIO)

-- import           Data.Array.Massiv.Common.Shape   (Slice, (!>))
-- import           Data.Array.Massiv.Manifest.Boxed
-- import           Data.Array.Massiv.Mutable
-- import GHC.Base (build)

-- TODO: Use CPP to account for sortOn is only available since base-4.8
--import Data.List (sortBy)
--import Data.Function (on)




-- | /O(n)/ - Compute sum sequentially.
sumS :: (Source r ix e, Num e) => Array r ix e -> e
sumS = foldrS (+) 0
{-# INLINE sumS #-}


-- | /O(n)/ - Compute product sequentially.
productS :: (Source r ix e, Num e) => Array r ix e -> e
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
-- >>> foldlP (flip (:)) [] (flip (:)) [] $ makeArray1D 11 id
-- [[10,9,8,7,6,5,4,3,2,1,0]]
--
-- This is how the result would look like if the above computation would be
-- performed in a program compiled with @+RTS -N3@, i.e. with 3 capabilities:
--
-- >>> foldlOnP [1,2,3] (flip (:)) [] (flip (:)) [] $ makeArray1D 11 id
-- [[10,9],[8,7,6],[5,4,3],[2,1,0]]
--
foldlP :: (NFData a, Source r ix e) =>
          (b -> a -> b) -- ^ Chunk results folding function @f@.
       -> b -- ^ Accumulator for results of chunks folding.
       -> (a -> e -> a) -- ^ Chunks folding function @g@.
       -> a -- ^ Accumulator for each chunk.
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
  :: (NFData a, Source r ix e)
  => [Int] -> (b -> a -> b) -> b -> (a -> e -> a) -> a -> Array r ix e -> IO b
foldlOnP wIds g !tAcc f = ifoldlOnP wIds g tAcc (\ x _ -> f x)
{-# INLINE foldlOnP #-}


-- | Just like `ifoldlP`, but allows you to specify which cores to run
-- computation on.
ifoldlOnP :: (NFData a, Source r ix e) =>
           [Int] -> (b -> a -> b) -> b -> (a -> ix -> e -> a) -> a -> Array r ix e -> IO b
ifoldlOnP wIds g !tAcc f !initAcc !arr = do
  let !sz = size arr
  results <-
    splitWork wIds sz $ \ !scheduler !chunkLength !totalLength !slackStart -> do
      loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start -> do
          submitRequest scheduler $
            JobRequest $
            iterLinearM sz start (start + chunkLength) 1 (<) initAcc $ \ !i ix !acc ->
              let res = f acc ix (unsafeLinearIndex arr i)
              in res `deepseq` return res
      when (slackStart < totalLength) $
        submitRequest scheduler $
        JobRequest $
        iterLinearM sz slackStart totalLength 1 (<) initAcc $ \ !i ix !acc ->
          let res = f acc ix (unsafeLinearIndex arr i)
          in res `deepseq` return res
  return $ F.foldl' g tAcc $ map jobResult $ sortOn jobResultId results
{-# INLINE ifoldlOnP #-}


-- foldlP'
--   :: (NFData a, Source r ix e)
--   => (b -> a -> b) -> b -> (a -> e -> a) -> a -> Array r ix e -> IO b
-- foldlP' g !tAcc f = ifoldlOnP' [] g tAcc (\ x _ -> f x)
-- {-# INLINE foldlP' #-}


-- -- | Just like `ifoldlP`, but allows you to specify which cores to run
-- -- computation on.
-- ifoldlOnP' :: forall r ix a b e . (NFData a, Source r ix e) =>
--            [Int] -> (b -> a -> b) -> b -> (a -> ix -> e -> a) -> a -> Array r ix e -> IO b
-- ifoldlOnP' wIds g !tAcc f !initAcc !arr = do
--   let !sz = size arr
--   mResArrM <- splitWork' wIds sz $ \ !scheduler !chunkLength !totalLength !slackStart -> do
--       let !hasSlack = slackStart < totalLength
--       let !resSize = numWorkers scheduler + if hasSlack then 1 else 0
--       resArrM <- unsafeNew resSize
--       loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start -> do
--           submitRequest scheduler $
--             JobRequest $ do
--               iterRes <- iterLinearM sz start (start + chunkLength) 1 (<) initAcc $ \ !i ix !acc ->
--                 let res = f acc ix (unsafeLinearIndex arr i)
--                 in res `deepseq` return res
--               unsafeLinearWrite resArrM jId iterRes
--       when hasSlack $
--         submitRequest scheduler $
--         JobRequest $ do
--           iterRes <- iterLinearM sz slackStart totalLength 1 (<) initAcc $ \ !i ix !acc ->
--             let res = f acc ix (unsafeLinearIndex arr i)
--             in res `deepseq` return res
--           unsafeLinearWrite resArrM jIdSlack iterRes
--       return resArrM
--   case mResArrM of
--     Just resArrM -> do
--       resArr <- unsafeFreeze resArrM
--       return $ foldlS g tAcc (resArr :: Array B DIM1 a)
--     Nothing -> return tAcc
-- {-# INLINE ifoldlOnP' #-}


------------------


-- foldlP' :: (NFData a, Slice r ix e) =>
--           (b -> a -> b) -- ^ Chunk results folding function @f@.
--        -> b -- ^ Accumulator for results of chunks folding.
--        -> (a -> e -> a) -- ^ Chunks folding function @g@.
--        -> a -- ^ Accumulator for each chunk.
--        -> Array r ix e -> IO b
-- foldlP' g !tAcc f = ifoldlOnP' [] g tAcc (\ x _ -> f x)
-- {-# INLINE foldlP' #-}


-- -- | Just like `ifoldlP`, but allows you to specify which cores to run
-- -- computation on.
-- ifoldlOnP' :: forall r ix a b e . (NFData a, Slice r ix e) =>
--            [Int] -> (b -> a -> b) -> b -> (a -> ix -> e -> a) -> a -> Array r ix e -> IO b
-- ifoldlOnP' wIds g !tAcc f !initAcc !arr = do
--   let !k = fst $ unconsDim (size arr)
--   scheduler <- makeScheduler wIds
--   resArrM <- unsafeNew k
--   iterM_ 0 k 1 (<) $ \ !i -> do
--     submitRequest scheduler $
--       JobRequest i $ do
--       let res = ifoldlS (\ accI ix -> f accI (consDim i ix)) initAcc (arr !> i)
--       res `deepseq` unsafeLinearWrite resArrM i res
--   waitTillDone scheduler
--   resArr <- unsafeFreeze resArrM
--   return $ foldlS g tAcc (resArr :: Array B DIM1 a)
-- {-# INLINE ifoldlOnP' #-}


-- foldrP' :: (NFData a, Slice r ix e) =>
--            (e -> a -> a) -> a -> Array r ix e -> IO [a]
-- foldrP' f = ifoldrOnP' [] (const f)
-- {-# INLINE foldrP' #-}


-- -- | Just like `ifoldlP`, but allows you to specify which cores to run
-- -- computation on.
-- ifoldrOnP' :: forall r ix a e . (NFData a, Slice r ix e) =>
--             [Int] -> (ix -> e -> a -> a) -> a -> Array r ix e -> IO [a]
-- ifoldrOnP' wIds f !initAcc !arr = do
--   let !k = fst $ unconsDim (size arr)
--   scheduler <- makeScheduler wIds
--   resArrM <- unsafeNew k
--   iterM_ 0 k 1 (<) $ \ !i -> do
--     submitRequest scheduler $
--       JobRequest i $ do
--       let res = ifoldrS (\ ix -> f (consDim i ix)) initAcc (arr !> i)
--       res `deepseq` unsafeLinearWrite resArrM i res
--   waitTillDone scheduler
--   resArr <- unsafeFreeze resArrM
--   return $ build $ \ c n -> toListFB c n (resArr :: Array B DIM1 a)
-- {-# INLINE ifoldrOnP' #-}

-- -- | Just like `ifoldlP`, but allows you to specify which cores to run
-- -- computation on.
-- toListP' :: forall r ix e . (NFData e, Slice r ix e) =>
--             Array r ix e -> [[e]]
-- toListP' !arr = resArr `deepseqP` build (\c n -> toListFB c n (resArr :: Array B DIM1 [e]))
--   where
--     !k = fst $ unconsDim (size arr)
--     resArr = makeArray k $ \i -> build (\c n -> toListFB c n (arr !> i))
-- {-# INLINE toListP' #-}

-- -- | Just like `ifoldlP`, but allows you to specify which cores to run
-- -- computation on.
-- toListP'' :: forall r ix e . (NFData e, Slice r ix e) =>
--             Array r ix e -> IO [[e]]
-- toListP'' !arr = do
--   let !k = fst $ unconsDim (size arr)
--   scheduler <- makeScheduler []
--   resArrM <- unsafeNew k
--   iterM_ 0 k 1 (<) $ \ !i -> do
--     submitRequest scheduler $
--       JobRequest i $ do
--       let res = build (\ c n -> toListFB c n (arr !> i))
--       res `seq` unsafeLinearWrite resArrM i res
--   waitTillDone scheduler
--   resArr <- unsafeFreeze resArrM
--   return $ build $ \ c n -> toListFB c n (resArr :: Array B DIM1 [e])
-- {-# INLINE toListP'' #-}


-- foldrP' :: (NFData a, Slice r ix e) =>
--            (a -> b -> b) -> b -> (e -> a -> a) -> a -> Array r ix e -> IO b
-- foldrP' g !tAcc f = ifoldrOnP' [] g tAcc (const f)
-- {-# INLINE foldrP' #-}


-- -- | Just like `ifoldlP`, but allows you to specify which cores to run
-- -- computation on.
-- ifoldrOnP' :: forall r ix a b e . (NFData a, Slice r ix e) =>
--             [Int] -> (a -> b -> b) -> b -> (ix -> e -> a -> a) -> a -> Array r ix e -> IO b
-- ifoldrOnP' wIds g !tAcc f !initAcc !arr = do
--   let !k = fst $ unconsDim (size arr)
--   scheduler <- makeScheduler wIds
--   resArrM <- unsafeNew k
--   iterM_ 0 k 1 (<) $ \ !i -> do
--     submitRequest scheduler $
--       JobRequest i $ do
--       let res = ifoldrS (\ ix -> f (consDim i ix)) initAcc (arr !> i)
--       res `deepseq` unsafeLinearWrite resArrM i res
--   waitTillDone scheduler
--   resArr <- unsafeFreeze resArrM
--   return $ foldrS g tAcc (resArr :: Array B DIM1 a)
-- {-# INLINE ifoldrOnP' #-}


-----------

-- | /O(n)/ - Left fold with an index aware function, computed in parallel. Just
-- like `foldlP`, except that folding function will receive an index of an
-- element it is being applied to.
ifoldlP :: (NFData a, Source r ix e) =>
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
foldrP :: (NFData a, Source r ix e) =>
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
foldrOnP :: (NFData a, Source r ix e) =>
            [Int] -> (a -> b -> b) -> b -> (e -> a -> a) -> a -> Array r ix e -> IO b
foldrOnP wIds g !tAcc f = ifoldrOnP wIds g tAcc (const f)
{-# INLINE foldrOnP #-}


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
        JobRequest $
        iterLinearM sz (totalLength - 1) slackStart (-1) (>=) initAcc $ \ !i ix !acc ->
          return $ f ix (unsafeLinearIndex arr i) acc
      loopM_ slackStart (> 0) (subtract chunkLength) $ \ !start ->
        submitRequest scheduler $
          JobRequest $
          iterLinearM sz (start - 1) (start - chunkLength) (-1) (>=) initAcc $ \ !i ix !acc ->
            let res = f ix (unsafeLinearIndex arr i) acc
            in res `deepseq` return res
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


