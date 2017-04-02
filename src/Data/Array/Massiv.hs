{-# LANGUAGE FlexibleContexts #-}
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
  ( module Data.Array.Massiv.Common
  , module Data.Array.Massiv.Delayed
  , module Data.Array.Massiv.Manifest
  -- * Construction
  --, makeArray
  , makeArray1D
  , makeArray2D
  , makeArray3D
  -- * Mapping
  , map
  , imap
  -- * Zipping
  , zipWith
  , zipWith3
  , izipWith
  , izipWith3
  -- * Geometric Operators
  --, transpose
  , backpermute
  , unsafeBackpermute
  ) where
import Prelude hiding (map, zipWith, zipWith3)
import Data.Array.Massiv.Common
import Data.Array.Massiv.Delayed
import Data.Array.Massiv.Manifest



makeArray1D :: Int -> (Int -> e) -> Array D DIM1 e
makeArray1D = makeArray
{-# INLINE makeArray1D #-}


makeArray2D :: (Int, Int) -> ((Int, Int) -> e) -> Array D DIM2 e
makeArray2D = makeArray
{-# INLINE makeArray2D #-}

makeArray3D :: (Int, Int, Int) -> ((Int, Int, Int) -> e) -> Array D DIM3 e
makeArray3D = makeArray
{-# INLINE makeArray3D #-}


map :: Source r ix b => (b -> e) -> Array r ix b -> Array D ix e
map f !arr = DArray (size arr) (f . unsafeIndex arr)
{-# INLINE map #-}

imap :: Source r ix b => (ix -> b -> e) -> Array r ix b -> Array D ix e
imap f !arr = DArray (size arr) (\ !ix -> f ix (unsafeIndex arr ix))
{-# INLINE imap #-}


zipWith
  :: (Source r1 ix a1, Source r2 ix a2)
  => (a1 -> a2 -> e) -> Array r1 ix a1 -> Array r2 ix a2 -> Array D ix e
zipWith f !arr1 arr2 =
  DArray (liftIndex2 min (size arr1) (size arr1)) $ \ !ix ->
    f (unsafeIndex arr1 ix) (unsafeIndex arr2 ix)
{-# INLINE zipWith #-}


zipWith3
  :: (Source r1 ix a1, Source r2 ix a2, Source r3 ix a3)
  => (a1 -> a2 -> a3 -> e) -> Array r1 ix a1 -> Array r2 ix a2 -> Array r3 ix a3 -> Array D ix e
zipWith3 f !arr1 arr2 arr3 =
  DArray (liftIndex2 min (liftIndex2 min (size arr1) (size arr1)) (size arr3)) $ \ !ix ->
    f (unsafeIndex arr1 ix) (unsafeIndex arr2 ix) (unsafeIndex arr3 ix)
{-# INLINE zipWith3 #-}


izipWith
  :: (Source r1 ix a1, Source r2 ix a2)
  => (ix -> a1 -> a2 -> e) -> Array r1 ix a1 -> Array r2 ix a2 -> Array D ix e
izipWith f !arr1 arr2 =
  DArray (liftIndex2 min (size arr1) (size arr1)) $ \ !ix ->
    f ix (unsafeIndex arr1 ix) (unsafeIndex arr2 ix)
{-# INLINE izipWith #-}


izipWith3
  :: (Source r1 ix a1, Source r2 ix a2, Source r3 ix a3)
  => (ix -> a1 -> a2 -> a3 -> e)
  -> Array r1 ix a1
  -> Array r2 ix a2
  -> Array r3 ix a3
  -> Array D ix e
izipWith3 f !arr1 arr2 arr3 =
  DArray (liftIndex2 min (liftIndex2 min (size arr1) (size arr1)) (size arr3)) $ \ !ix ->
    f ix (unsafeIndex arr1 ix) (unsafeIndex arr2 ix) (unsafeIndex arr3 ix)
{-# INLINE izipWith3 #-}


-- transpose
--   :: (Index (Lower ix), Index (Lower (Lower ix)), Source r ix e)
--   => Array r ix e -> Array D ix e
-- transpose !arr = DArray (transInner (size arr)) newVal where
--   transInner !ix = snocDim (snocDim ixU2 n) m where
--     !(ixU1, n) = unsnocDim ix
--     !(ixU2, m) = unsnocDim ixU1
--   {-# INLINE transInner #-}
--   newVal !ix = unsafeIndex arr (transInner ix)
--   {-# INLINE newVal #-}
-- {-# INLINE transpose #-}


backpermute :: Source r ix1 e => ix -> (ix -> ix1) -> Array r ix1 e -> Array D ix e
backpermute !sz ixF !arr = DArray sz $ \ !ix -> safeIndex arr (ixF ix)
{-# INLINE backpermute #-}


unsafeBackpermute :: Source r ix1 e => ix -> (ix -> ix1) -> Array r ix1 e -> Array D ix e
unsafeBackpermute sz ixF arr = DArray sz $ \ !ix -> unsafeIndex arr (ixF ix)
{-# INLINE unsafeBackpermute #-}


