{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Array.Massiv.Delayed
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Delayed where

import           Control.Monad                  (void)
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Common.Shape
import           Data.Array.Massiv.Scheduler
import           Data.Foldable


-- | Delayed representation.
data D


data instance Array D ix e = DArray { dSize :: !ix
                                    , dUnsafeIndex :: ix -> e }



-- | /O(1)/ Conversion from a source array to `D` representation.
delay :: Source r ix e => Array r ix e -> Array D ix e
delay arr = DArray (size arr) (unsafeIndex arr)
{-# INLINE delay #-}


instance Index ix => Massiv D ix e where
  size = dSize
  {-# INLINE size #-}

  makeArray = DArray . liftIndex (max 0)
  {-# INLINE makeArray #-}


instance Index ix => Source D ix e where
  unsafeIndex = dUnsafeIndex
  {-# INLINE unsafeIndex #-}

instance Index ix => Shape D ix e where
  unsafeReshape !sz !arr =
    DArray sz $ \ !ix ->
      unsafeIndex arr (fromLinearIndex (size arr) (toLinearIndex sz ix))
  {-# INLINE unsafeReshape #-}

  unsafeExtract !sIx !newSz !arr =
    DArray newSz $ \ !ix ->
      unsafeIndex arr (liftIndex2 (+) ix sIx)
  {-# INLINE unsafeExtract #-}


instance (Index ix, Index (Lower ix)) => Slice D ix e where

  (!?>) !arr !i
    | isSafeIndex m i = Just (DArray szL (\ !ix -> unsafeIndex arr (consDim i ix)))
    | otherwise = Nothing
    where
      !sz = size arr
      !(m, szL) = unconsDim sz
  {-# INLINE (!?>) #-}

  (<!?) !arr !i
    | isSafeIndex m i = Just (DArray szL (\ !ix -> unsafeIndex arr (snocDim ix i)))
    | otherwise = Nothing
    where
      !sz = size arr
      !(szL, m) = unsnocDim sz
  {-# INLINE (<!?) #-}


instance (Eq e, Index ix, Foldable (Array D ix)) => Eq (Array D ix e) where
  (==) (DArray sz1 uIndex1) (DArray sz2 uIndex2) =
    sz1 == sz2 && foldl' (&&) True (DArray sz1 (\ !ix -> uIndex1 ix == uIndex2 ix))


instance Functor (Array D ix) where
  fmap f (DArray sz g) = DArray sz (f . g)
  {-# INLINE fmap #-}


instance Index ix => Applicative (Array D ix) where
  pure a = DArray (liftIndex (+ 1) zeroIndex) (const a)
  (<*>) (DArray sz1 uIndex1) (DArray sz2 uIndex2) =
    DArray (liftIndex2 (*) sz1 sz2) $ \ !ix ->
      (uIndex1 (liftIndex2 mod ix sz1)) (uIndex2 (liftIndex2 mod ix sz2))



-- | Row-major folding over a delayed array.
instance Index ix => Foldable (Array D ix) where
  foldl f acc (DArray sz g) =
    iter zeroIndex sz 1 (<) acc $ \ix acc0 -> f acc0 (g ix)
  {-# INLINE foldl #-}
  foldl' f !acc (DArray sz g) =
    iter zeroIndex sz 1 (<) acc $ \ !ix !acc0 -> f acc0 (g ix)
  {-# INLINE foldl' #-}
  foldr f acc (DArray sz g) =
    iter (liftIndex (subtract 1) sz) zeroIndex (-1) (>=) acc $ \i acc0 -> f (g i) acc0
  {-# INLINE foldr #-}
  foldr' f !acc (DArray sz g) =
    iter (liftIndex (subtract 1) sz) zeroIndex (-1) (>=) acc $ \ !i !acc0 -> f (g i) acc0
  {-# INLINE foldr' #-}
  null (DArray sz _) = totalElem sz == 0
  {-# INLINE null #-}
  sum = foldl' (+) 0
  {-# INLINE sum #-}
  product = foldl' (*) 1
  {-# INLINE product #-}
  length = totalElem . size
  {-# INLINE length #-}



instance Index ix => Load D ix where
  loadS (DArray sz f) _ unsafeWrite =
    iterM_ zeroIndex sz 1 (<) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (f ix)
  {-# INLINE loadS #-}
  loadP (DArray sz f) _ unsafeWrite = do
    void $ splitWork sz $ \ !scheduler !chunkLength !totalLength !slackStart -> do
      loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
        submitRequest scheduler $
        JobRequest 0 $
        iterLinearM_ sz start (start + chunkLength) 1 (<) $ \ !k !ix -> do
          unsafeWrite k $ f ix
      submitRequest scheduler $
        JobRequest 0 $
        iterLinearM_ sz slackStart totalLength 1 (<) $ \ !k !ix -> do
          unsafeWrite k (f ix)
  {-# INLINE loadP #-}

-- foldP :: Source r ix e =>
--          (b -> b -> b) -> (b -> e -> b) -> b -> Array r ix e -> IO b
-- foldP g f !initAcc !arr = do
--   numWorkers <- getNumWorkers
--   scheduler <- makeScheduler
--   let !sz = size arr
--       !totalLength = totalElem sz
--       !chunkLength = totalLength `quot` numWorkers
--       !slackStart = chunkLength * numWorkers
--   loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
--     submitRequest scheduler $
--     JobRequest 0 $
--     iterM start (start + chunkLength) 1 (<) initAcc $ \ !ix !acc ->
--       return $ f acc (unsafeLinearIndex arr ix)
--   submitRequest scheduler $
--     JobRequest 0 $
--     iterM slackStart totalLength 1 (<) initAcc $ \ !ix !acc ->
--       return $ f acc (unsafeLinearIndex arr ix)
--   let g' !jRes acc = g (jobResult jRes) acc
--   collectResults scheduler g' initAcc
-- {-# INLINE foldP #-}

-- foldlP' :: Source r ix e =>
--            (b -> a -> b) -> b -> (a -> e -> a) -> a -> Array r ix e -> IO b
-- foldlP' g !tAcc f !initAcc !arr = do
--   numWorkers <- getNumWorkers
--   scheduler <- makeScheduler
--   let !sz = size arr
--       !totalLength = totalElem sz
--       !chunkLength = totalLength `quot` numWorkers
--       !slackStart = chunkLength * numWorkers
--   jId <- loopM 0 (< slackStart) (+ chunkLength) 0 $ \ !start !jId -> do
--     submitRequest scheduler $
--       JobRequest jId $
--       iterM start (start + chunkLength) 1 (<) initAcc $ \ !ix !acc ->
--         return $ f acc (unsafeLinearIndex arr ix)
--     return (jId + 1)
--   when (slackStart < totalLength) $
--     submitRequest scheduler $
--     JobRequest jId $
--     iterM slackStart totalLength 1 (<) initAcc $ \ !ix !acc ->
--       return $ f acc (unsafeLinearIndex arr ix)
--   results <- collectResults scheduler (:) []
--   return $ foldl' g tAcc $ map jobResult $ sortOn jobResultId results
-- {-# INLINE foldlP' #-}



-- ifoldlP' :: Source r ix e =>
--            (b -> a -> b) -> b -> (a -> ix -> e -> a) -> a -> Array r ix e -> IO b
-- ifoldlP' g !tAcc f !initAcc !arr = do
--   numWorkers <- getNumWorkers
--   scheduler <- makeScheduler
--   let !sz = size arr
--       !totalLength = totalElem sz
--       !chunkLength = totalLength `quot` numWorkers
--       !slackStart = chunkLength * numWorkers
--   jId <-
--     loopM 0 (< slackStart) (+ chunkLength) 0 $ \ !start !jId -> do
--       submitRequest scheduler $
--         JobRequest jId $
--         iterM start (start + chunkLength) 1 (<) initAcc $ \ !i !acc ->
--           let !ix = fromLinearIndex sz i
--           in return $ f acc ix (unsafeIndex arr ix)
--       return (jId + 1)
--   when (slackStart < totalLength) $
--     submitRequest scheduler $
--     JobRequest jId $
--     iterM slackStart totalLength 1 (<) initAcc $ \ !i !acc ->
--       let !ix = fromLinearIndex sz i
--       in return $ f acc ix (unsafeIndex arr ix)
--   results <- collectResults scheduler (:) []
--   return $ foldl' g tAcc $ map jobResult $ sortOn jobResultId results
-- {-# INLINE ifoldlP' #-}


-- foldrP' :: Source r ix e =>
--            (a  -> b -> b) -> b -> (e -> a -> a) -> a -> Array r ix e -> IO b
-- foldrP' g !tAcc f !initAcc !arr = do
--   numWorkers <- getNumWorkers
--   scheduler <- makeScheduler
--   let !sz = size arr
--       !totalLength = totalElem sz
--       !chunkLength = totalLength `quot` numWorkers
--       !slackStart = chunkLength * numWorkers
--   when (slackStart < totalLength) $
--     submitRequest scheduler $
--     JobRequest 0 $
--     iterM (totalLength - 1) slackStart (-1) (>=) initAcc $ \ !ix !acc ->
--       return $ f (unsafeLinearIndex arr ix) acc
--   void $ loopM slackStart (> 0) (subtract chunkLength) 1 $ \ !start !jId -> do
--     submitRequest scheduler $
--       JobRequest jId $
--       iterM (start - 1) (start - chunkLength) (-1) (>=) initAcc $ \ !ix !acc ->
--         return $ f (unsafeLinearIndex arr ix) acc
--     return (jId + 1)
--   results <- collectResults scheduler (:) []
--   return $ foldr' g tAcc $ reverse $ map jobResult $ sortOn jobResultId results
-- {-# INLINE foldrP' #-}
