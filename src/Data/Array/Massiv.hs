{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Data.Array.Massiv
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv
  ( module Data.Array.Massiv.Index
  , module Data.Array.Massiv.Common
  , module Data.Array.Massiv.Delayed
  , module Data.Array.Massiv.Manifest
  , makeArray1D
  , makeArray2D
  , makeArray3D
  , maybeIndex
  , (!)
  , (?)
  , index
  , mapA
  ) where

import Data.Array.Massiv.Index
import Data.Array.Massiv.Common
import Data.Array.Massiv.Delayed
import Data.Array.Massiv.Manifest




makeArray1D :: Int -> (Int -> e) -> Array D DIM1 e
makeArray1D = DArray
  -- | k >= 0 = DArray1D k
  -- | otherwise = errorDims "makeArray1D" (show k)
{-# INLINE makeArray1D #-}


makeArray2D :: (Int, Int) -> ((Int, Int) -> e) -> Array D DIM2 e
makeArray2D = DArray
  -- | (m == 0 || n == 0) || (m > 0 && n > 0) = DArray2D m n
  -- | otherwise = errorDims "makeArray2D" $ show (m, n)
{-# INLINE makeArray2D #-}

makeArray3D :: (Int, Int, Int) -> ((Int, Int, Int) -> e) -> Array D DIM3 e
makeArray3D = DArray
  -- | (m == 0 || n == 0 || o == 0) || (m > 0 && n > 0 && o > 0) =
  -- | otherwise = errorDims "makeArray3D" $ show (m, n, o)
{-# INLINE makeArray3D #-}


errorIx :: (Show a2, Show a1) => a2 -> a1 -> a
errorIx ix sz =
  error $ "Index out of bounds: " ++ show ix ++ " for array size: " ++ show sz


maybeIndex :: Source r ix => Array r ix e -> ix -> Maybe e
maybeIndex !arr !ix
  | isSafeIndex (size arr) ix = Just (unsafeIndex arr ix)
  | otherwise = Nothing
{-# INLINE maybeIndex #-}


index :: Source r ix => Array r ix e -> ix -> e
index !arr !ix
  | isSafeIndex (size arr) ix = unsafeIndex arr ix
  | otherwise = errorIx ix (size arr)
{-# INLINE index #-}


(!) :: Source r ix => Array r ix e -> Int -> Elt r ix e
(!) arr ix =
  case arr !? ix of
    Just res -> res
    Nothing -> errorIx ix (size arr)
{-# INLINE (!) #-}


(?) :: Source r ix => Maybe (Array r ix e) -> Int -> Maybe (Elt r ix e)
(?) Nothing _      = Nothing
(?) (Just arr) !ix = arr !? ix
{-# INLINE (?) #-}

mapA :: Source r ix => (b -> e) -> Array r ix b -> Array D ix e
mapA f !arr = makeArray (size arr) (f . unsafeIndex arr)
{-# INLINE mapA #-}
