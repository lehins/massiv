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
  , module Data.Array.Massiv.Manifest.Unboxed -- TODO: Remove
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
  , transpose
  , backpermute
  , unsafeBackpermute
  , reshape
  , reshape'
  , unsafeReshape
  -- * Enumeration
  , range
  , rangeStep
  , enumFromN
  , enumFromStepN
  ) where
import Prelude hiding (map, zipWith, zipWith3)
import Data.Array.Massiv.Common
import Data.Array.Massiv.Delayed
import Data.Array.Massiv.Manifest
import Data.Array.Massiv.Manifest.Unboxed

--import qualified  Data.Vector.Unboxed as VU
--import Data.Foldable

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


transpose
  :: (Index (Lower ix), Index (Lower (Lower ix)), Source r ix e)
  => Array r ix e -> Array D ix e
transpose !arr = DArray (transInner (size arr)) newVal where
  transInner !ix = snocDim (snocDim ixU2 n) m where
    !(ixU1, n) = unsnocDim ix
    !(ixU2, m) = unsnocDim ixU1
  {-# INLINE transInner #-}
  newVal !ix = unsafeIndex arr (transInner ix)
  {-# INLINE newVal #-}
{-# INLINE transpose #-}

transposeOuter
  :: (Index (Lower ix), Index (Lower (Lower ix)), Source r ix e)
  => Array r ix e -> Array D ix e
transposeOuter !arr = DArray (transOuter (size arr)) newVal where
  transOuter !ix = consDim m (consDim n ixU2) where
    !(n, ixU1) = unconsDim ix
    !(m, ixU2) = unconsDim ixU1
  {-# INLINE transOuter #-}
  newVal !ix = unsafeIndex arr (transOuter ix)
  {-# INLINE newVal #-}
{-# INLINE transposeOuter #-}


backpermute :: Source r ix1 e => ix -> (ix -> ix1) -> Array r ix1 e -> Array D ix e
backpermute !sz ixF !arr = DArray sz $ \ !ix -> safeIndex arr (ixF ix)
{-# INLINE backpermute #-}


unsafeBackpermute :: Source r ix1 e => ix -> (ix -> ix1) -> Array r ix1 e -> Array D ix e
unsafeBackpermute sz ixF arr = DArray sz $ \ !ix -> unsafeIndex arr (ixF ix)
{-# INLINE unsafeBackpermute #-}


unsafeReshape :: Source r ix e => ix -> Array r ix e -> Array D ix e
unsafeReshape sz arr =
  DArray
    sz
    (\ !ix -> unsafeIndex arr (fromLinearIndex sz (toLinearIndex (size arr) ix)))
{-# INLINE unsafeReshape #-}

reshape :: (Index ix, Source r ix1 e) => ix -> Array r ix1 e -> Either String (Array D ix e)
reshape sz arr
  | totalElem sz == totalElem (size arr) =
    Right $
    DArray
      sz
      (\ !ix ->
         unsafeIndex arr (fromLinearIndex (size arr) (toLinearIndex sz ix)))
  | otherwise =
    Left $
    "Total number of elements do not match: " ++ show sz ++ " vs " ++ show (size arr)
{-# INLINE reshape #-}

-- | Same as `reshape`, but raise an error if supplied dimensions are incorrect.
reshape' :: (Index ix, Source r ix1 e) => ix -> Array r ix1 e -> Array D ix e
reshape' sz = either error id . reshape sz
{-# INLINE reshape' #-}


unsafeSlice :: Source r ix e => ix -> ix -> Array r ix e -> Array D ix e
unsafeSlice sIx eIx arr =
  DArray (liftIndex2 (-) eIx sIx) $ \ !ix ->
    unsafeIndex arr (liftIndex2 (+) ix sIx)


-- | Create a Vector with a range of @Int@s incremented by 1.
-- @range k0 k1 == rangeStep k0 k1 1@
--
-- >>> toList $ range 1 6
-- [1,2,3,4,5]
-- >>> toList $ range (-2) 3
-- [-2,-1,0,1,2]
range :: Int -> Int -> Array D DIM1 Int
range k0 k1 = makeArray1D (max 0 (k1 - k0)) (+ k0)
{-# INLINE range #-}


rangeStep :: Int -> Int -> Int -> Array D DIM1 Int
rangeStep k0 k1 step = makeArray1D ((k1 - k0) `div` step) (\ !i -> k0 + i*step)
{-# INLINE rangeStep #-}


-- |
--
-- >>> toList $ enumFromN 5 3
-- [5,6,7]
enumFromN :: Num e => e -> Int -> Array D DIM1 e
enumFromN s k = DArray k $ \ !i -> fromIntegral i + s
{-# INLINE enumFromN #-}

-- |
--
-- >>> toList $ enumFromStepN 1 0.1 5
-- [1.0,1.1,1.2,1.3,1.4]
enumFromStepN :: Num e => e -> e -> Int -> Array D DIM1 e
enumFromStepN s step k = DArray k $ \ !i -> fromIntegral i * step + s
{-# INLINE enumFromStepN #-}


