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
import           Data.Array.Massiv.Compute.Gang

import           Data.Foldable
import           GHC.Base                 (quotRemInt)


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
instance Foldable (Array D DIM1) where
  foldl f acc (DArray k g) = loop 0 (< k) (+ 1) acc $ \i acc0 -> f acc0 (g i)
  {-# INLINE foldl #-}
  foldl' f !acc (DArray k g) =
    loop 0 (< k) (+ 1) acc $ \ !i !acc0 -> f acc0 (g i)
  {-# INLINE foldl' #-}
  foldr f acc (DArray k g) =
    loop (k - 1) (>= 0) (subtract 1) acc $ \i acc0 -> f (g i) acc0
  {-# INLINE foldr #-}
  foldr' f !acc (DArray k g) =
    loop (k - 1) (>= 0) (subtract 1) acc $ \ !i !acc0 -> f (g i) acc0
  {-# INLINE foldr' #-}
  null (DArray k _) = k == 0
  {-# INLINE null #-}
  sum = foldr' (+) 0
  {-# INLINE sum #-}
  product = foldr' (*) 1
  {-# INLINE product #-}
  length = totalElem . size
  {-# INLINE length #-}


-- | Row-major folding over a delayed array.
instance Foldable (Array D DIM2) where
  foldl f acc (DArray (m, n) g) =
    loop 0 (< m) (+ 1) acc $ \i acc0 ->
      loop 0 (< n) (+ 1) acc0 $ \j acc1 -> f acc1 (g (i, j))
  {-# INLINE foldl #-}
  foldl' f !acc (DArray (m, n) g) =
    loop 0 (< m) (+ 1) acc $ \ !i !acc0 ->
      loop 0 (< n) (+ 1) acc0 $ \ !j !acc1 -> f acc1 (g (i, j))
  {-# INLINE foldl' #-}
  foldr f acc (DArray (m, n) g) =
    loop (m - 1) (>= 0) (subtract 1) acc $ \i acc0 ->
      loop (n - 1) (>= 0) (subtract 1) acc0 $ \j acc1 -> f (g (i, j)) acc1
  {-# INLINE foldr #-}
  foldr' f !acc (DArray (m, n) g) =
    loop (m - 1) (>= 0) (subtract 1) acc $ \ !i !acc0 ->
      loop (n - 1) (>= 0) (subtract 1) acc0 $ \ !j !acc1 -> f (g (i, j)) acc1
  {-# INLINE foldr' #-}
  null (DArray (m, n) _) = m == 0 || n == 0
  {-# INLINE null #-}
  sum = foldr' (+) 0
  {-# INLINE sum #-}
  product = foldr' (*) 1
  {-# INLINE product #-}
  length = totalElem . size
  {-# INLINE length #-}



-- | Row-major folding over a delayed array.
instance Foldable (Array D DIM3) where
  foldr f acc (DArray (m, n, o) g) =
    loop 0 (< m) (+ 1) acc $ \i acc0 ->
      loop 0 (< n) (+ 1) acc0 $ \j acc1 ->
        loop 0 (< o) (+ 1) acc1 $ \k acc2 -> f (g (i, j, k)) acc2
  {-# INLINE foldr #-}
  foldr' f !acc (DArray (m, n, o) g) =
    loop 0 (< m) (+ 1) acc $ \ !i !acc0 ->
      loop 0 (< n) (+ 1) acc0 $ \ !j !acc1 ->
        loop 0 (< o) (+ 1) acc1 $ \ !k !acc2 -> f (g (i, j, k)) acc2
  {-# INLINE foldr' #-}
  foldl f acc (DArray (m, n, o) g) =
    loop (m - 1) (>= 0) (subtract 1) acc $ \i acc0 ->
      loop (n - 1) (>= 0) (subtract 1) acc0 $ \j acc1 ->
        loop (o - 1) (>= 0) (subtract 1) acc1 $ \k acc2 -> f acc2 (g (i, j, k))
  {-# INLINE foldl #-}
  foldl' f !acc (DArray (m, n, o) g) =
    loop (m - 1) (>= 0) (subtract 1) acc $ \ !i !acc0 ->
      loop (n - 1) (>= 0) (subtract 1) acc0 $ \ !j !acc1 ->
        loop (o - 1) (>= 0) (subtract 1) acc1 $ \ !k !acc2 -> f acc2 (g (i, j, k))
  {-# INLINE foldl' #-}
  null (DArray (m, n, o) _) = m == 0 || n == 0 || o == 0
  {-# INLINE null #-}
  sum = foldr' (+) 0
  {-# INLINE sum #-}
  product = foldr' (*) 1
  {-# INLINE product #-}
  length = totalElem . size
  {-# INLINE length #-}



instance Load D DIM1 where
  loadS (DArray sz f) unsafeWrite = do
    iterateWithLinearM_ RowMajor sz 0 sz $ \ !k !ix ->
      unsafeWrite k (f ix)
  {-# INLINE loadS #-}
  loadP arr@(DArray arrSize f) unsafeWrite = do
    let !gSize = gangSize theGang
        !totalLength = totalElem arrSize
        !(chunkLength, slackLength) = totalLength `quotRemInt` gSize
    gangIO theGang $ \ !tid ->
      let !start = tid * chunkLength
          !end = start + chunkLength
      in do
        iterateLinearM_ RowMajor arrSize start end $ \ !k !ix -> do
          unsafeWrite k $ f ix
    loopM_ (totalLength - slackLength) (< totalLength) (+ 1) $ \ !i ->
      unsafeWrite i (unsafeLinearIndex arr i)
  {-# INLINE loadP #-}

instance Load D DIM2 where
  loadS (DArray sz f) unsafeWrite =
    iterateWithLinearM_ RowMajor sz (0, 0) sz $ \ !k !ix ->
      unsafeWrite k (f ix)
  {-# INLINE loadS #-}
  loadP arr@(DArray arrSize f) unsafeWrite = do
    let !gSize = gangSize theGang
        !totalLength = totalElem arrSize
        !(chunkLength, slackLength) = totalLength `quotRemInt` gSize
    gangIO theGang $ \ !tid ->
      let !start = tid * chunkLength
          !end = start + chunkLength
      in do
        iterateLinearM_ RowMajor arrSize start end $ \ !k !ix -> do
          unsafeWrite k $ f ix
    loopM_ (totalLength - slackLength) (< totalLength) (+ 1) $ \ !i ->
      unsafeWrite i (unsafeLinearIndex arr i)
  {-# INLINE loadP #-}
