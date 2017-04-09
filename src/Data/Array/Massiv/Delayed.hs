{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Data.Array.Massiv.Delayed
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Delayed where

-- import Control.Monad
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Common.Shape
import           Data.Array.Massiv.Compute.Gang
import           Data.Array.Massiv.Compute.Scheduler

import           Data.Foldable


-- | Delayed representation.
data D


data instance Array D ix e = DArray { dSize :: !ix
                                    , dUnsafeIndex :: ix -> e }


makeArray :: Index ix => ix -> (ix -> e) -> Array D ix e
makeArray !ix = DArray (liftIndex (max 0) ix)
{-# INLINE makeArray #-}

-- | _O(1)_ Conversion from a source array to `D` representation.
delay :: Source r ix e => Array r ix e -> Array D ix e
delay arr = DArray (size arr) (unsafeIndex arr)
{-# INLINE delay #-}


instance Index ix => Massiv D ix where
  size = dSize
  {-# INLINE size #-}

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
  loadS (DArray sz f) unsafeWrite =
    iterM_ zeroIndex sz 1 (<) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (f ix)
  {-# INLINE loadS #-}
  loadP arr@(DArray sz f) unsafeWrite = do
    let !gSize = gangSize theGang
        !totalLength = totalElem sz
        !(chunkLength, slackLength) = totalLength `quotRem` gSize
    gangIO theGang $ \ !tid ->
      let !start = tid * chunkLength
          !end = start + chunkLength
      in do
        iterLinearM_ sz start end 1 (<) $ \ !k !ix ->
          unsafeWrite k $ f ix
    -- loopM_ (totalLength - slackLength) (< totalLength) (+ 1) $ \ !i ->
    --   unsafeWrite i (unsafeLinearIndex arr i)
    iterLinearM_ sz (totalLength - slackLength) totalLength 1 (<) $ \ !k !ix ->
      unsafeWrite k (unsafeIndex arr ix)
  {-# INLINE loadP #-}


data ID

data instance Array ID ix e = IDArray !(Array D ix e)

instance Index ix => Massiv ID ix where
  size (IDArray arr) = size arr


-- instance Index ix => Load ID ix where
--   loadS (IDArray arr) unsafeWrite = loadS arr unsafeWrite
--   {-# INLINE loadS #-}
--   loadP (IDArray arr@(DArray sz f)) unsafeWrite = do
--     let !gSize = gangSize theGang
--         !totalLength = totalElem sz
--         !slackLength = totalLength `rem` gSize
--         !end = totalLength - slackLength
--     gangIO theGang $ \ !tid ->
--       iterLinearM_ sz tid end gSize (<) $ \ !k !ix -> do
--         unsafeWrite k $ f ix
--     iterLinearM_ sz end totalLength 1 (<) $ \ !k !ix ->
--       unsafeWrite k (unsafeIndex arr ix)
--   {-# INLINE loadP #-}


toInterleaved :: Array D ix e -> Array ID ix e
toInterleaved = IDArray
{-# INLINE toInterleaved #-}

-- toInterleaved :: Source r ix e => Array r ix e -> Array ID ix e
-- toInterleaved = IDArray . delay
-- {-# INLINE toInterleaved #-}


instance Index ix =>
         Load ID ix where
  loadS (IDArray arr) unsafeWrite = loadS arr unsafeWrite
  {-# INLINE loadS #-}
  loadP (IDArray (DArray sz f)) unsafeWrite = do
    numWorkers <- getNumWorkers
    scheduler <- makeScheduler
    let !totalLength = totalElem sz
        !chunkLength = totalLength `quot` numWorkers
        !slackStart  = chunkLength * numWorkers
    loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
      submitRequest scheduler $
      JobRequest $
      iterLinearM_ sz start (start + chunkLength) 1 (<) $ \ !k !ix -> do
        unsafeWrite k $ f ix
    submitRequest scheduler $
      JobRequest $
      iterLinearM_ sz slackStart totalLength 1 (<) $ \ !k !ix -> do
        unsafeWrite k (f ix)
    waitTillDone scheduler
  {-# INLINE loadP #-}


foldP :: Source r ix e =>
         (b -> b -> b) -> (b -> e -> b) -> b -> Array r ix e -> IO b
foldP g f !initAcc !arr = do
  numWorkers <- getNumWorkers
  scheduler <- makeScheduler
  let !sz = size arr
      !totalLength = totalElem sz
      !chunkLength = totalLength `quot` numWorkers
      !slackStart = chunkLength * numWorkers
  loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
    submitRequest scheduler $
    JobRequest $
    iterM start (start + chunkLength) 1 (<) initAcc $ \ !ix !acc ->
      return $ f acc (unsafeLinearIndex arr ix)
  submitRequest scheduler $
    JobRequest $
    iterM slackStart totalLength 1 (<) initAcc $ \ !ix !acc ->
      return $ f acc (unsafeLinearIndex arr ix)
  collectResults scheduler g initAcc
{-# INLINE foldP #-}


sumP :: (Source r ix e, Num e) =>
        Array r ix e -> IO e
sumP = foldP (+) (+) 0
{-# INLINE sumP #-}
