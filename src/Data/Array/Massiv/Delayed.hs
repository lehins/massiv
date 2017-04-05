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

import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Common.Shape
import           Data.Array.Massiv.Compute.Gang

import           Data.Foldable


-- | Delayed representation.
data D


data instance Array D ix e = DArray { dSize :: !ix
                                    , dUnsafeIndex :: ix -> e }


makeArray :: Index ix => ix -> (ix -> e) -> Array D ix e
makeArray !ix = DArray (liftIndex (max 0) ix)
{-# INLINE makeArray #-}


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

  -- unsafeExtract !sIx !eIx !arr =
  --   DArray (liftIndex2 (-) eIx sIx) $ \ !ix ->
  --     unsafeIndex arr (liftIndex2 (+) ix sIx)
  -- {-# INLINE unsafeExtract #-}
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
  loadS (DArray sz f) unsafeWrite = do
    iterM_ zeroIndex sz 1 (<) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (f ix)
  {-# INLINE loadS #-}
  loadP arr@(DArray arrSize f) unsafeWrite = do
    let !gSize = gangSize theGang
        !totalLength = totalElem arrSize
        !(chunkLength, slackLength) = totalLength `quotRem` gSize
    gangIO theGang $ \ !tid ->
      let !start = tid * chunkLength
          !end = start + chunkLength
      in do
        iterLinearM_ arrSize start end 1 (<) $ \ !k !ix -> do
          unsafeWrite k $ f ix
    loopM_ (totalLength - slackLength) (< totalLength) (+ 1) $ \ !i ->
      unsafeWrite i (unsafeLinearIndex arr i)
  {-# INLINE loadP #-}
