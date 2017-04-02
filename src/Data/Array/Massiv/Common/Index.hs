{-# LANGUAGE BangPatterns            #-}
{-# LANGUAGE CPP                     #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE TypeFamilies            #-}
-- |
-- Module      : Data.Array.Massiv.Common.Index
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Common.Index where

import           GHC.Base (quotRemInt)

type DIM0 = Z

type DIM1 = Int

type DIM2 = (Int, Int)

type DIM3 = (Int, Int, Int)

type family Lower ix :: *
type family Higher ix :: *

class (Eq ix, Show ix, Lower (Higher ix) ~ ix, Higher (Lower ix) ~ ix) => Index ix where

  zeroIndex :: ix

  -- | Check whether index is within the size.
  isSafeIndex :: ix -- ^ Size
              -> ix -- ^ Index
              -> Bool

  -- | Total number of elements in an array of this size.
  totalElem :: ix -> Int

  -- | Produce linear index from size and index
  toLinearIndex :: ix -- ^ Size
                -> ix -- ^ Index
                -> Int

  -- | Produce N Dim index from size and linear index
  fromLinearIndex :: ix -> Int -> ix

  liftIndex :: (Int -> Int) -> ix -> ix

  liftIndex2 :: (Int -> Int -> Int) -> ix -> ix -> ix

  repairIndex :: ix -> ix -> (Int -> Int -> Int) -> (Int -> Int -> Int) -> ix

  consDim :: Index (Lower ix) => Int -> Lower ix -> ix

  unconsDim :: Index (Lower ix) => ix -> (Int, Lower ix)

  snocDim :: Index (Lower ix) => Lower ix -> Int -> ix

  unsnocDim :: Index (Lower ix) => ix -> (Lower ix, Int)


data UnsupportedDIM

data Z = Z deriving (Eq, Show)

errorBelowZero :: a
errorBelowZero = error "There is no dimension that is lower than DIM0"

type instance Higher UnsupportedDIM = Z
type instance Lower UnsupportedDIM = DIM3

type instance Lower Z = UnsupportedDIM
type instance Higher Z = Int


instance Index Z where
  zeroIndex = Z
  totalElem _ = 0
  isSafeIndex _   _    = False
  toLinearIndex _ _ = 0
  fromLinearIndex _ _ = Z
  repairIndex _ _ _ _ = Z
  consDim _ _ = Z
  unconsDim _ = errorBelowZero
  snocDim _ _ = Z
  unsnocDim _ = errorBelowZero
  liftIndex _ = id
  liftIndex2 _ _ _ = Z


type instance Lower DIM1 = Z
type instance Higher DIM1 = DIM2

instance Index DIM1 where
  zeroIndex = 0
  {-# INLINE zeroIndex #-}
  totalElem = id
  {-# INLINE totalElem #-}
  isSafeIndex !k !i = 0 <= i && i < k
  {-# INLINE isSafeIndex #-}
  toLinearIndex _ = id
  {-# INLINE toLinearIndex #-}
  fromLinearIndex _ = id
  {-# INLINE fromLinearIndex #-}
  repairIndex !k !i rBelow rOver
    | i < 0 = rBelow k i
    | i >= k = rOver k i
    | otherwise = i
  {-# INLINE repairIndex #-}
  consDim i Z = i
  {-# INLINE consDim #-}
  unconsDim i = (i, Z)
  {-# INLINE unconsDim #-}
  snocDim Z i = i
  {-# INLINE snocDim #-}
  unsnocDim i = (Z, i)
  {-# INLINE unsnocDim #-}
  liftIndex f = f
  {-# INLINE liftIndex #-}
  liftIndex2 f = f
  {-# INLINE liftIndex2 #-}

type instance Lower DIM2 = DIM1
type instance Higher DIM2 = DIM3


instance Index DIM2 where
  zeroIndex = (0, 0)
  {-# INLINE zeroIndex #-}
  totalElem !(m, n) = m * n
  {-# INLINE totalElem #-}
  isSafeIndex !(m, n) !(i, j) = 0 <= i && 0 <= j && i < m && j < n
  {-# INLINE isSafeIndex #-}
  toLinearIndex !(_, n) !(i, j) = n * i + j
  {-# INLINE[3] toLinearIndex #-}
  fromLinearIndex !(_, n) !k = k `quotRemInt` n
  {-# INLINE fromLinearIndex #-}
  consDim = (,)
  {-# INLINE consDim #-}
  unconsDim = id
  {-# INLINE unconsDim #-}
  snocDim = (,)
  {-# INLINE snocDim #-}
  unsnocDim = id
  {-# INLINE unsnocDim #-}
  repairIndex = repairIndexRec
  {-# INLINE repairIndex #-}
  liftIndex f (i, j) = (f i, f j)
  {-# INLINE liftIndex #-}
  liftIndex2 f (i0, j0) (i1, j1) = (f i0 i1, f j0 j1)
  {-# INLINE liftIndex2 #-}

type instance Lower DIM3 = DIM2
type instance Higher DIM3 = UnsupportedDIM

instance Index DIM3 where
  zeroIndex = (0, 0, 0)
  {-# INLINE zeroIndex #-}
  totalElem !(m, n, o) = m * n * o
  {-# INLINE totalElem #-}
  isSafeIndex !(m, n, o) !(i, j, k) =
    0 <= i && 0 <= j && 0 < k && i < m && j < n && k < o
  {-# INLINE isSafeIndex #-}
  toLinearIndex !(_, n, o) !(i, j, k) = n * i + j * o + k
  {-# INLINE toLinearIndex #-}
  fromLinearIndex !(_, n, o) !l = (i, j, k)
    where !(h, k) = quotRemInt l o
          !(i, j) = quotRemInt h n
  {-# INLINE fromLinearIndex #-}
  consDim i (j, k) = (i, j, k)
  {-# INLINE consDim #-}
  unconsDim (i, j, k) = (i, (j, k))
  {-# INLINE unconsDim #-}
  snocDim (i, j) k = (i, j, k)
  {-# INLINE snocDim #-}
  unsnocDim (i, j, k) = ((i, j), k)
  {-# INLINE unsnocDim #-}
  repairIndex = repairIndexRec
  {-# INLINE repairIndex #-}
  liftIndex f (i, j, k) = (f i, f j, f k)
  {-# INLINE liftIndex #-}
  liftIndex2 f (i0, j0, k0) (i1, j1, k1) = (f i0 i1, f j0 j1, f k0 k1)
  {-# INLINE liftIndex2 #-}



data Border e = Fill e | Wrap | Edge | Reflect | Continue



handleBorderIndex :: Index ix => Border e -> ix -> (ix -> e) -> ix -> e
handleBorderIndex border !sz getVal !ix =
  case border of
    Fill val -> if isSafeIndex sz ix then getVal ix else val
    Wrap     -> getVal (repairIndex sz ix (flip mod) (flip mod))
    Edge     -> getVal (repairIndex sz ix (const (const 0)) (\ !k _ -> k - 1))
    Reflect  -> getVal (repairIndex sz ix (\ !k !i -> (abs i - 1) `mod` k)
                        (\ !k !i -> (-i - 1) `mod` k))
    Continue -> getVal (repairIndex sz ix (\ !k !i -> abs i `mod` k)
                        (\ !k !i -> (-i - 2) `mod` k))
{-# INLINE handleBorderIndex #-}



repairIndexRec :: (Index (Lower ix), Index ix) =>
                  ix -> ix -> (Int -> Int -> Int) -> (Int -> Int -> Int) -> ix
repairIndexRec !sz !ix rBelow rOver =
    snocDim (repairIndex szL ixL rBelow rOver) (repairIndex sz0 ix0 rBelow rOver)
    where !(szL, sz0) = unsnocDim sz
          !(ixL, ix0) = unsnocDim ix
{-# INLINE repairIndexRec #-}


liftIndexRec :: (Index (Lower ix), Index ix) =>
                (Int -> Int) -> ix -> ix
liftIndexRec f !ix = snocDim (liftIndex f ixL) (liftIndex f ix0)
  where
    !(ixL, ix0) = unsnocDim ix
{-# INLINE liftIndexRec #-}


liftIndex2Rec :: (Index (Lower ix), Index ix) =>
                (Int -> Int -> Int) -> ix -> ix -> ix
liftIndex2Rec f !ix !ixD = snocDim (liftIndex2 f ixL ixDL) (liftIndex2 f ix0 ixD0)
  where
    !(ixL, ix0) = unsnocDim ix
    !(ixDL, ixD0) = unsnocDim ixD
{-# INLINE liftIndex2Rec #-}


fromLinearIndexRec :: (Index (Lower ix), Index ix) =>
                      ix -> Int -> ix
fromLinearIndexRec !sz !k = snocDim (fromLinearIndex szL kL) j
  where !(kL, j) = quotRemInt k n
        !(szL, n) = unsnocDim sz
{-# INLINE fromLinearIndexRec #-}
