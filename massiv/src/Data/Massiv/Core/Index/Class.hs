{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
-- |
-- Module      : Data.Massiv.Core.Index.Class
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Core.Index.Class where

import           Control.DeepSeq           (NFData (..))
import           Data.Functor.Identity     (runIdentity)
import           Data.Massiv.Core.Iterator
import           GHC.TypeLits

newtype Dim = Dim Int deriving (Show, Eq, Ord, Num, Real, Integral, Enum)

data Ix0 = Ix0 deriving (Eq, Ord, Show)

type Ix1T = Int

type Ix2T = (Int, Int)

type Ix3T = (Int, Int, Int)

type Ix4T = (Int, Int, Int, Int)

type Ix5T = (Int, Int, Int, Int, Int)

type family Lower ix :: *

type instance Lower Ix1T = Ix0
type instance Lower Ix2T = Ix1T
type instance Lower Ix3T = Ix2T
type instance Lower Ix4T = Ix3T
type instance Lower Ix5T = Ix4T


class (Eq ix, Ord ix, Show ix, NFData ix) => Index ix where
  type Rank ix :: Nat

  rank :: ix -> Dim

  zeroIndex :: ix

  -- | Total number of elements in an array of this size.
  totalElem :: ix -> Int

  consDim :: Int -> Lower ix -> ix

  unconsDim :: ix -> (Int, Lower ix)

  snocDim :: Lower ix -> Int -> ix

  unsnocDim :: ix -> (Lower ix, Int)

  dropDim :: ix -> Dim -> Maybe (Lower ix)

  getIndex :: ix -> Dim -> Maybe Int

  setIndex :: ix -> Dim -> Int -> Maybe ix

  -- | Zip together two indices with a function
  liftIndex2 :: (Int -> Int -> Int) -> ix -> ix -> ix

  -- | Map a function over an index
  liftIndex :: (Int -> Int) -> ix -> ix
  liftIndex f = liftIndex2 (\_ i -> f i) zeroIndex
  {-# INLINE [1] liftIndex #-}

  -- | Check whether index is within the size.
  isSafeIndex :: ix -- ^ Size
              -> ix -- ^ Index
              -> Bool
  default isSafeIndex :: Index (Lower ix) => ix -> ix -> Bool
  isSafeIndex !sz !ix = isSafeIndex n0 i0 && isSafeIndex szL ixL
    where
      !(n0, szL) = unconsDim sz
      !(i0, ixL) = unconsDim ix
  {-# INLINE [1] isSafeIndex #-}

  -- | Produce linear index from size and index
  toLinearIndex :: ix -- ^ Size
                -> ix -- ^ Index
                -> Int

  default toLinearIndex :: Index (Lower ix) => ix -> ix -> Int
  toLinearIndex !sz !ix = toLinearIndex szL ixL * n + i
    where !(szL, n) = unsnocDim sz
          !(ixL, i) = unsnocDim ix
  {-# INLINE [1] toLinearIndex #-}

  toLinearIndexAcc :: Int -> ix -> ix -> Int
  default toLinearIndexAcc :: Index (Lower ix) => Int -> ix -> ix -> Int
  toLinearIndexAcc !acc !sz !ix = toLinearIndexAcc (acc * n + i) szL ixL
    where !(n, szL) = unconsDim sz
          !(i, ixL) = unconsDim ix
  {-# INLINE [1] toLinearIndexAcc #-}

  -- | Produce N Dim index from size and linear index
  fromLinearIndex :: ix -> Int -> ix
  default fromLinearIndex :: Index (Lower ix) => ix -> Int -> ix
  fromLinearIndex sz k = consDim q ixL
    where !(q, ixL) = fromLinearIndexAcc (snd (unconsDim sz)) k
  {-# INLINE [1] fromLinearIndex #-}

  fromLinearIndexAcc :: ix -> Int -> (Int, ix)
  default fromLinearIndexAcc :: Index (Lower ix) => ix -> Int -> (Int, ix)
  fromLinearIndexAcc ix' !k = (q, consDim r ixL)
    where !(m, ix) = unconsDim ix'
          !(kL, ixL) = fromLinearIndexAcc ix k
          !(q, r) = quotRem kL m
  {-# INLINE [1] fromLinearIndexAcc #-}

  repairIndex :: ix -> ix -> (Int -> Int -> Int) -> (Int -> Int -> Int) -> ix
  default repairIndex :: Index (Lower ix)
    => ix -> ix -> (Int -> Int -> Int) -> (Int -> Int -> Int) -> ix
  repairIndex !sz !ix rBelow rOver =
    consDim (repairIndex n i rBelow rOver) (repairIndex szL ixL rBelow rOver)
    where !(n, szL) = unconsDim sz
          !(i, ixL) = unconsDim ix
  {-# INLINE [1] repairIndex #-}

  iter :: ix -> ix -> Int -> (Int -> Int -> Bool) -> a -> (ix -> a -> a) -> a
  iter sIx eIx inc cond acc f =
    runIdentity $ iterM sIx eIx inc cond acc (\ix -> return . f ix)
  {-# INLINE iter #-}

  iterM :: Monad m =>
           ix -- ^ Start index
        -> ix -- ^ End index
        -> Int -- ^ Increment
        -> (Int -> Int -> Bool) -- ^ Continue iteration while predicate is True (eg. until end of row)
        -> a -- ^ Initial value for an accumulator
        -> (ix -> a -> m a) -- ^ Accumulator function
        -> m a
  default iterM :: (Index (Lower ix), Monad m)
    => ix -> ix -> Int -> (Int -> Int -> Bool) -> a -> (ix -> a -> m a) -> m a
  iterM !sIx !eIx !inc cond !acc f =
    loopM k0 (`cond` k1) (+ inc) acc $ \ !i !acc0 ->
      iterM sIxL eIxL inc cond acc0 $ \ !ix ->
        f (consDim i ix)
    where
      !(k0, sIxL) = unconsDim sIx
      !(k1, eIxL) = unconsDim eIx
  {-# INLINE iterM #-}

  iterM_ :: Monad m => ix -> ix -> Int -> (Int -> Int -> Bool) -> (ix -> m a) -> m ()
  default iterM_ :: (Index (Lower ix), Monad m)
    => ix -> ix -> Int -> (Int -> Int -> Bool) -> (ix -> m a) -> m ()
  iterM_ !sIx !eIx !inc cond f =
    loopM_ k0 (`cond` k1) (+ inc) $ \ !i ->
      iterM_ sIxL eIxL inc cond $ \ !ix ->
        f (consDim i ix)
    where
      !(k0, sIxL) = unconsDim sIx
      !(k1, eIxL) = unconsDim eIx
  {-# INLINE iterM_ #-}


instance Index Ix1T where
  type Rank Ix1T = 1
  rank _ = 1
  {-# INLINE [1] rank #-}
  zeroIndex = 0
  {-# INLINE [1] zeroIndex #-}
  totalElem = id
  {-# INLINE [1] totalElem #-}
  isSafeIndex !k !i = 0 <= i && i < k
  {-# INLINE [1] isSafeIndex #-}
  toLinearIndex _ = id
  {-# INLINE [1] toLinearIndex #-}
  toLinearIndexAcc !acc m i  = acc * m + i
  {-# INLINE [1] toLinearIndexAcc #-}
  fromLinearIndex _ = id
  {-# INLINE [1] fromLinearIndex #-}
  fromLinearIndexAcc n k = k `quotRem` n
  {-# INLINE [1] fromLinearIndexAcc #-}
  repairIndex !k !i rBelow rOver
    | i < 0 = rBelow k i
    | i >= k = rOver k i
    | otherwise = i
  {-# INLINE [1] repairIndex #-}
  consDim i _ = i
  {-# INLINE [1] consDim #-}
  unconsDim i = (i, Ix0)
  {-# INLINE [1] unconsDim #-}
  snocDim _ i = i
  {-# INLINE [1] snocDim #-}
  unsnocDim i = (Ix0, i)
  {-# INLINE [1] unsnocDim #-}
  getIndex i 1 = Just i
  getIndex _ _ = Nothing
  {-# INLINE [1] getIndex #-}
  setIndex _ 1 i = Just i
  setIndex _ _ _ = Nothing
  {-# INLINE [1] setIndex #-}
  dropDim _ 1 = Just Ix0
  dropDim _ _ = Nothing
  {-# INLINE [1] dropDim #-}
  liftIndex f = f
  {-# INLINE [1] liftIndex #-}
  liftIndex2 f = f
  {-# INLINE [1] liftIndex2 #-}
  iter k0 k1 inc cond = loop k0 (`cond` k1) (+inc)
  {-# INLINE iter #-}
  iterM k0 k1 inc cond = loopM k0 (`cond` k1) (+inc)
  {-# INLINE iterM #-}
  iterM_ k0 k1 inc cond = loopM_ k0 (`cond` k1) (+inc)
  {-# INLINE iterM_ #-}


instance Index Ix2T where
  type Rank Ix2T = 2
  rank _ = 2
  {-# INLINE [1] rank #-}
  zeroIndex = (0, 0)
  {-# INLINE [1] zeroIndex #-}
  totalElem !(m, n) = m * n
  {-# INLINE [1] totalElem #-}
  toLinearIndex !(_, n) !(i, j) = n * i + j
  {-# INLINE [1] toLinearIndex #-}
  fromLinearIndex (_, n) !k = k `quotRem` n
  {-# INLINE [1] fromLinearIndex #-}
  consDim = (,)
  {-# INLINE [1] consDim #-}
  unconsDim = id
  {-# INLINE [1] unconsDim #-}
  snocDim = (,)
  {-# INLINE [1] snocDim #-}
  unsnocDim = id
  {-# INLINE [1] unsnocDim #-}
  getIndex (i, _) 2 = Just i
  getIndex (_, j) 1 = Just j
  getIndex _      _ = Nothing
  {-# INLINE [1] getIndex #-}
  setIndex (_, j) 2 i = Just (i, j)
  setIndex (i, _) 1 j = Just (i, j)
  setIndex _      _ _ = Nothing
  {-# INLINE [1] setIndex #-}
  dropDim (_, j) 2 = Just j
  dropDim (i, _) 1 = Just i
  dropDim _      _ = Nothing
  {-# INLINE [1] dropDim #-}
  liftIndex2 f (i0, j0) (i1, j1) = (f i0 i1, f j0 j1)
  {-# INLINE [1] liftIndex2 #-}


instance Index Ix3T where
  type Rank Ix3T = 3
  rank _ = 3
  {-# INLINE [1] rank #-}
  zeroIndex = (0, 0, 0)
  {-# INLINE [1] zeroIndex #-}
  totalElem !(m, n, o) = m * n * o
  {-# INLINE [1] totalElem #-}
  consDim i (j, k) = (i, j, k)
  {-# INLINE [1] consDim #-}
  unconsDim (i, j, k) = (i, (j, k))
  {-# INLINE [1] unconsDim #-}
  snocDim (i, j) k = (i, j, k)
  {-# INLINE [1] snocDim #-}
  unsnocDim (i, j, k) = ((i, j), k)
  {-# INLINE [1] unsnocDim #-}
  getIndex (i, _, _) 3 = Just i
  getIndex (_, j, _) 2 = Just j
  getIndex (_, _, k) 1 = Just k
  getIndex _         _ = Nothing
  {-# INLINE [1] getIndex #-}
  setIndex (_, j, k) 3 i = Just (i, j, k)
  setIndex (i, _, k) 2 j = Just (i, j, k)
  setIndex (i, j, _) 1 k = Just (i, j, k)
  setIndex _      _ _    = Nothing
  {-# INLINE [1] setIndex #-}
  dropDim (_, j, k) 3 = Just (j, k)
  dropDim (i, _, k) 2 = Just (i, k)
  dropDim (i, j, _) 1 = Just (i, j)
  dropDim _      _    = Nothing
  {-# INLINE [1] dropDim #-}
  liftIndex2 f (i0, j0, k0) (i1, j1, k1) = (f i0 i1, f j0 j1, f k0 k1)
  {-# INLINE [1] liftIndex2 #-}


instance Index Ix4T where
  type Rank Ix4T = 4
  rank _ = 4
  {-# INLINE [1] rank #-}
  zeroIndex = (0, 0, 0, 0)
  {-# INLINE [1] zeroIndex #-}
  totalElem !(n1, n2, n3, n4) = n1 * n2 * n3 * n4
  {-# INLINE [1] totalElem #-}
  consDim i1 (i2, i3, i4) = (i1, i2, i3, i4)
  {-# INLINE [1] consDim #-}
  unconsDim (i1, i2, i3, i4) = (i1, (i2, i3, i4))
  {-# INLINE [1] unconsDim #-}
  snocDim (i1, i2, i3) i4 = (i1, i2, i3, i4)
  {-# INLINE [1] snocDim #-}
  unsnocDim (i1, i2, i3, i4) = ((i1, i2, i3), i4)
  {-# INLINE [1] unsnocDim #-}
  getIndex (i1,  _,  _,  _) 4 = Just i1
  getIndex ( _, i2,  _,  _) 3 = Just i2
  getIndex ( _,  _, i3,  _) 2 = Just i3
  getIndex ( _,  _,  _, i4) 1 = Just i4
  getIndex _                _ = Nothing
  {-# INLINE [1] getIndex #-}
  setIndex ( _, i2, i3, i4) 4 i1 = Just (i1, i2, i3, i4)
  setIndex (i1,  _, i3, i4) 3 i2 = Just (i1, i2, i3, i4)
  setIndex (i1, i2,  _, i4) 2 i3 = Just (i1, i2, i3, i4)
  setIndex (i1, i2, i3,  _) 1 i4 = Just (i1, i2, i3, i4)
  setIndex _                _  _ = Nothing
  {-# INLINE [1] setIndex #-}
  dropDim ( _, i2, i3, i4) 4 = Just (i2, i3, i4)
  dropDim (i1,  _, i3, i4) 3 = Just (i1, i3, i4)
  dropDim (i1, i2,  _, i4) 2 = Just (i1, i2, i4)
  dropDim (i1, i2, i3,  _) 1 = Just (i1, i2, i3)
  dropDim _      _           = Nothing
  {-# INLINE [1] dropDim #-}
  liftIndex2 f (i0, i1, i2, i3) (j0, j1, j2, j3) = (f i0 j0, f i1 j1, f i2 j2, f i3 j3)
  {-# INLINE [1] liftIndex2 #-}


instance Index Ix5T where
  type Rank Ix5T = 5
  rank _ = 5
  {-# INLINE [1] rank #-}
  zeroIndex = (0, 0, 0, 0, 0)
  {-# INLINE [1] zeroIndex #-}
  totalElem !(n1, n2, n3, n4, n5) = n1 * n2 * n3 * n4 * n5
  {-# INLINE [1] totalElem #-}
  consDim i1 (i2, i3, i4, i5) = (i1, i2, i3, i4, i5)
  {-# INLINE [1] consDim #-}
  unconsDim (i1, i2, i3, i4, i5) = (i1, (i2, i3, i4, i5))
  {-# INLINE [1] unconsDim #-}
  snocDim (i1, i2, i3, i4) i5 = (i1, i2, i3, i4, i5)
  {-# INLINE [1] snocDim #-}
  unsnocDim (i1, i2, i3, i4, i5) = ((i1, i2, i3, i4), i5)
  {-# INLINE [1] unsnocDim #-}
  getIndex (i1,  _,  _,  _,  _) 5 = Just i1
  getIndex ( _, i2,  _,  _,  _) 4 = Just i2
  getIndex ( _,  _, i3,  _,  _) 3 = Just i3
  getIndex ( _,  _,  _, i4,  _) 2 = Just i4
  getIndex ( _,  _,  _,  _, i5) 1 = Just i5
  getIndex _                _     = Nothing
  {-# INLINE [1] getIndex #-}
  setIndex ( _, i2, i3, i4, i5) 5 i1 = Just (i1, i2, i3, i4, i5)
  setIndex (i1,  _, i3, i4, i5) 4 i2 = Just (i1, i2, i3, i4, i5)
  setIndex (i1, i2,  _, i4, i5) 3 i3 = Just (i1, i2, i3, i4, i5)
  setIndex (i1, i2, i3,  _, i5) 2 i4 = Just (i1, i2, i3, i4, i5)
  setIndex (i1, i2, i3, i4,  _) 1 i5 = Just (i1, i2, i3, i4, i5)
  setIndex _                    _  _ = Nothing
  {-# INLINE [1] setIndex #-}
  dropDim ( _, i2, i3, i4, i5) 5 = Just (i2, i3, i4, i5)
  dropDim (i1,  _, i3, i4, i5) 4 = Just (i1, i3, i4, i5)
  dropDim (i1, i2,  _, i4, i5) 3 = Just (i1, i2, i4, i5)
  dropDim (i1, i2, i3,  _, i5) 2 = Just (i1, i2, i3, i5)
  dropDim (i1, i2, i3, i4,  _) 1 = Just (i1, i2, i3, i4)
  dropDim _                    _ = Nothing
  {-# INLINE [1] dropDim #-}
  liftIndex2 f (i0, i1, i2, i3, i4) (j0, j1, j2, j3, j4) =
    (f i0 j0, f i1 j1, f i2 j2, f i3 j3, f i4 j4)
  {-# INLINE [1] liftIndex2 #-}



errorIx :: (Show ix, Show ix') => String -> ix -> ix' -> a
errorIx fName sz ix =
  error $
  fName ++
  ": Index out of bounds: " ++ show ix ++ " for Array of size: " ++ show sz
{-# NOINLINE errorIx #-}
