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
  , module Data.Array.Massiv.Common.Shape
  , module Data.Array.Massiv.Delayed
  , module Data.Array.Massiv.Manifest
  , module Data.Array.Massiv.Manifest.Unboxed -- TODO: Remove
  -- * Accessors
  -- ** Size information
  , size
  , null
  -- * Construction
  --, makeArray
  , makeArray1D
  , makeArray2D
  , makeArray3D
  -- * Mapping
  , map
  , imap
  , imapM_
  -- * Zipping
  , zipWith
  , zipWith3
  , izipWith
  , izipWith3
  -- * Geometric Operators
  , transpose
  , transposeInner
  , transposeOuter
  , backpermute
  , unsafeBackpermute
  , reshape
  , reshape'
  , extract
  , extractFromTo
  , append
  , append'
  -- * Monadic folds
  , foldlM
  , foldlM_
  , ifoldlM
  , ifoldlM_
  -- * Enumeration
  , range
  , rangeStep
  , enumFromN
  , enumFromStepN
  ) where

import Prelude hiding (null, length, map, zipWith, zipWith3)
import Control.Monad (void, guard)
import Data.Maybe
import Data.Array.Massiv.Common
import Data.Array.Massiv.Common.Shape
import Data.Array.Massiv.Delayed
import Data.Array.Massiv.Manifest
import Data.Array.Massiv.Manifest.Unboxed

--import qualified  Data.Vector.Unboxed as VU


length :: Massiv r ix => Array r ix e -> Int
length = totalElem . size
{-# INLINE length #-}

null :: Massiv r ix => Array r ix e -> Bool
null !arr = 0 == length arr
{-# INLINE null #-}

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
zipWith f !arr1 !arr2 =
  DArray (liftIndex2 min (size arr1) (size arr1)) $ \ !ix ->
    f (unsafeIndex arr1 ix) (unsafeIndex arr2 ix)
{-# INLINE zipWith #-}


zipWith3
  :: (Source r1 ix a1, Source r2 ix a2, Source r3 ix a3)
  => (a1 -> a2 -> a3 -> e) -> Array r1 ix a1 -> Array r2 ix a2 -> Array r3 ix a3 -> Array D ix e
zipWith3 f !arr1 !arr2 !arr3 =
  DArray (liftIndex2 min (liftIndex2 min (size arr1) (size arr1)) (size arr3)) $ \ !ix ->
    f (unsafeIndex arr1 ix) (unsafeIndex arr2 ix) (unsafeIndex arr3 ix)
{-# INLINE zipWith3 #-}


izipWith
  :: (Source r1 ix a1, Source r2 ix a2)
  => (ix -> a1 -> a2 -> e) -> Array r1 ix a1 -> Array r2 ix a2 -> Array D ix e
izipWith f !arr1 !arr2 =
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
izipWith3 f !arr1 !arr2 !arr3 =
  DArray (liftIndex2 min (liftIndex2 min (size arr1) (size arr1)) (size arr3)) $ \ !ix ->
    f ix (unsafeIndex arr1 ix) (unsafeIndex arr2 ix) (unsafeIndex arr3 ix)
{-# INLINE izipWith3 #-}

transpose :: Source r DIM2 e => Array r DIM2 e -> Array D DIM2 e
transpose = transposeInner
{-# INLINE transpose #-}

transposeInner
  :: (Index (Lower ix), Source r ix e)
  => Array r ix e -> Array D ix e
transposeInner !arr = DArray (transInner (size arr)) newVal
  where
    transInner !ix =
      fromMaybe (error "transposeInner: Impossible happened") $ do
        n <- getIndex ix (rank ix)
        m <- getIndex ix (rank ix - 1)
        ix' <- setIndex ix (rank ix) m
        setIndex ix' (rank ix - 1) n
    {-# INLINE transInner #-}
    newVal !ix = unsafeIndex arr (transInner ix)
    {-# INLINE newVal #-}
{-# INLINE transposeInner #-}

transposeOuter
  :: (Index (Lower ix), Source r ix e)
  => Array r ix e -> Array D ix e
transposeOuter !arr = DArray (transOuter (size arr)) newVal
  where
    transOuter !ix =
      fromMaybe (error "transposeOuter: Impossible happened") $ do
        n <- getIndex ix 1
        m <- getIndex ix 2
        ix' <- setIndex ix 1 m
        setIndex ix' 2 n
    {-# INLINE transOuter #-}
    newVal !ix = unsafeIndex arr (transOuter ix)
    {-# INLINE newVal #-}
{-# INLINE transposeOuter #-}


backpermute :: Source r ix1 e => ix -> (ix -> ix1) -> Array r ix1 e -> Array D ix e
backpermute !sz ixF !arr = DArray sz $ \ !ix -> safeIndex arr (ixF ix)
{-# INLINE backpermute #-}


unsafeBackpermute :: Source r ix1 e => ix -> (ix -> ix1) -> Array r ix1 e -> Array D ix e
unsafeBackpermute !sz ixF !arr = DArray sz $ \ !ix -> unsafeIndex arr (ixF ix)
{-# INLINE unsafeBackpermute #-}




foldlM :: (Source r ix b, Monad m) => (a -> b -> m a) -> a -> Array r ix b -> m a
foldlM f !acc !arr =
  iterM zeroIndex (size arr) 1 (<) acc $ \ !ix !a -> f a (unsafeIndex arr ix)
{-# INLINE foldlM #-}

foldlM_ :: (Source r ix b, Monad m) => (a -> ix -> b -> m a) -> a -> Array r ix b -> m ()
foldlM_ f !acc !arr =
  void $ iterM zeroIndex (size arr) 1 (<) acc $ \ !ix !a -> f a ix (unsafeIndex arr ix)
{-# INLINE foldlM_ #-}


ifoldlM :: (Source r ix b, Monad m) => (a -> ix -> b -> m a) -> a -> Array r ix b -> m a
ifoldlM f !acc !arr =
  iterM zeroIndex (size arr) 1 (<) acc $ \ !ix !a -> f a ix (unsafeIndex arr ix)
{-# INLINE ifoldlM #-}

ifoldlM_ :: (Source r ix b, Monad m) => (a -> ix -> b -> m a) -> a -> Array r ix b -> m ()
ifoldlM_ f !acc !arr =
  void $ iterM zeroIndex (size arr) 1 (<) acc $ \ !ix !a -> f a ix (unsafeIndex arr ix)
{-# INLINE ifoldlM_ #-}


imapM_ :: (Source r ix a, Monad m) => (ix -> a -> m b) -> Array r ix a -> m ()
imapM_ f arr =
  iterM_ zeroIndex (size arr) 1 (<) $ \ !ix -> f ix (unsafeIndex arr ix)

-- | Create a Vector with a range of @Int@s incremented by 1.
-- @range k0 k1 == rangeStep k0 k1 1@
--
-- >>> toList $ range 1 6
-- [1,2,3,4,5]
-- >>> toList $ range (-2) 3
-- [-2,-1,0,1,2]
range :: Int -> Int -> Array D DIM1 Int
range !k0 !k1 = makeArray1D (max 0 (k1 - k0)) (+ k0)
{-# INLINE range #-}


rangeStep :: Int -> Int -> Int -> Array D DIM1 Int
rangeStep !k0 !k1 !step = makeArray1D ((k1 - k0) `div` step) (\ !i -> k0 + i*step)
{-# INLINE rangeStep #-}


-- |
--
-- >>> toList $ enumFromN 5 3
-- [5,6,7]
enumFromN :: Num e => e -> Int -> Array D DIM1 e
enumFromN !s !k = DArray k $ \ !i -> fromIntegral i + s
{-# INLINE enumFromN #-}

-- |
--
-- >>> toList $ enumFromStepN 1 0.1 5
-- [1.0,1.1,1.2,1.3,1.4]
enumFromStepN :: Num e => e -> e -> Int -> Array D DIM1 e
enumFromStepN !s !step !k = DArray k $ \ !i -> fromIntegral i * step + s
{-# INLINE enumFromStepN #-}

append
  :: (Source r1 ix e, Source r ix e) =>
     Int -> Array r1 ix e -> Array r ix e -> Maybe (Array D ix e)
append !n !arr1 !arr2 = do
  let !sz1 = size arr1
      !sz2 = size arr2
  k1 <- getIndex sz1 n
  k2 <- getIndex sz2 n
  sz1' <- setIndex sz2 n k1
  guard $ sz1 == sz1'
  newSz <- setIndex sz1 n (k1 + k2)
  return $
    DArray newSz $ \ !ix ->
      fromMaybe (error "append: Impossible happened") $ do
        k' <- getIndex ix n
        if k' < k1
          then Just (unsafeIndex arr1 ix)
          else do
            i <- getIndex ix n
            ix' <- setIndex ix n (i - k1)
            return $ unsafeIndex arr2 ix'
{-# INLINE append #-}

append'
  :: (Source r1 ix e, Source r2 ix e) =>
     Int -> Array r1 ix e -> Array r2 ix e -> Array D ix e
append' !n !arr1 !arr2 =
  case append n arr1 arr2 of
    Just arr -> arr
    Nothing ->
      error $
      if 0 < n && n <= rank (size arr1)
        then "Dimension mismatch: " ++ show arr1 ++ " and " ++ show arr2
        else "Invalid dimension index: " ++ show n
{-# INLINE append' #-}
