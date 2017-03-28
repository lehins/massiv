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

import           Data.Foldable
import           GHC.Base                 (quotRemInt)


-- | Delayed representation.
data D


data instance Array D ix e = DArray { dSize :: !ix
                                    , dUnsafeIndex :: ix -> e }


errorDims :: String -> String -> a
errorDims fName errMsg =
  error $ "Data.Array.Massiv.Delayed." ++ fName ++ " - Incorrect dimensions: " ++ errMsg



makeArray :: ix -> (ix -> e) -> Array D ix e
makeArray = DArray
{-# INLINE makeArray #-}


instance Index ix => Massiv D ix where
  size = dSize
  {-# INLINE size #-}


instance Source D DIM1 where
  type Elt D DIM1 e = e
  unsafeIndex = dUnsafeIndex
  {-# INLINE unsafeIndex #-}
  unsafeLinearIndex = dUnsafeIndex
  {-# INLINE unsafeLinearIndex #-}
  (!?) (DArray k f) !i
    | isSafeIndex k i = Just (f i)
    | otherwise = Nothing
  {-# INLINE (!?) #-}

instance Source D DIM2 where
  unsafeIndex = dUnsafeIndex
  {-# INLINE unsafeIndex #-}
  unsafeLinearIndex (DArray (_, n) f) !l = f (quotRemInt l n)
  {-# INLINE unsafeLinearIndex #-}
  (!?) !arr !i
    | isSafeIndex m i = Just (DArray n (\ !j -> unsafeIndex arr (i, j)))
    | otherwise = Nothing
    where
      !(m, n) = size arr
  {-# INLINE (!?) #-}


instance Source D DIM3 where
  unsafeIndex (DArray _ g) = g
  {-# INLINE unsafeIndex #-}
  unsafeLinearIndex (DArray (_, n, o) f) !l = f (i, j, k)
    where !(h, k) = quotRemInt l o
          !(i, j) = quotRemInt h n
  {-# INLINE unsafeLinearIndex #-}
  (!?) !arr !i
    | isSafeIndex m i = Just (DArray (n, o) (\ !(j, k) -> unsafeIndex arr (i, j, k)))
    | otherwise = Nothing
    where
      !(m, n, o) = size arr
  {-# INLINE (!?) #-}


-- instance (Source D ix, Massiv ix) => Functor (Array D ix) where
--   fmap = mapA
--   {-# INLINE fmap #-}


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
