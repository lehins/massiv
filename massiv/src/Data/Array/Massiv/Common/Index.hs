{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
-- |
-- Module      : Data.Array.Massiv.Common.Index
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Common.Index where

import           Control.DeepSeq (NFData)

newtype Dim = Dim Int deriving (Show, Eq, Ord, Num, Real, Integral, Enum)

type Ix1T = Int

type Ix2T = (Int, Int)

type Ix3T = (Int, Int, Int)

type Ix4T = (Int, Int, Int, Int)

type Ix5T = (Int, Int, Int, Int, Int)

type family Lower ix :: *

type instance Lower Ix1T = ZeroDim
type instance Lower Ix2T = Ix1T
type instance Lower Ix3T = Ix2T
type instance Lower Ix4T = Ix3T
type instance Lower Ix5T = Ix4T


-- | Approach to be used near the borders during various transformations.
-- Whenever a function needs information not only about an element of interest, but
-- also about it's neighbours, it will go out of bounds around the image edges,
-- hence is this set of approaches that can be used in such situtation.
data Border e =
  Fill e    -- ^ Fill in a constant element.
              --
              -- @
              --            outside |  Image  | outside
              -- ('Fill' 0) : 0 0 0 0 | 1 2 3 4 | 0 0 0 0
              -- @
              --
  | Wrap      -- ^ Wrap around from the opposite border of the array.
              --
              -- @
              --            outside |  Image  | outside
              -- 'Wrap' :     1 2 3 4 | 1 2 3 4 | 1 2 3 4
              -- @
              --
  | Edge      -- ^ Replicate the element at the edge.
              --
              -- @
              --            outside |  Image  | outside
              -- 'Edge' :     1 1 1 1 | 1 2 3 4 | 4 4 4 4
              -- @
              --
  | Reflect   -- ^ Mirror like reflection.
              --
              -- @
              --            outside |  Image  | outside
              -- 'Reflect' :  4 3 2 1 | 1 2 3 4 | 4 3 2 1
              -- @
              --
  | Continue  -- ^ Also mirror like reflection, but without repeating the edge element.
              --
              -- @
              --            outside |  Image  | outside
              -- 'Continue' : 1 4 3 2 | 1 2 3 4 | 3 2 1 4
              -- @
              --
  deriving (Eq, Show)



class (Eq ix, Ord ix, Show ix, NFData ix) => Index ix where

  rank :: ix -> Dim

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

  fromLinearIndexAcc :: ix -> Int -> (Int, ix)

  liftIndex :: (Int -> Int) -> ix -> ix

  liftIndex2 :: (Int -> Int -> Int) -> ix -> ix -> ix

  repairIndex :: ix -> ix -> (Int -> Int -> Int) -> (Int -> Int -> Int) -> ix

  consDim :: Index (Lower ix) => Int -> Lower ix -> ix

  unconsDim :: Index (Lower ix) => ix -> (Int, Lower ix)

  snocDim :: Index (Lower ix) => Lower ix -> Int -> ix

  unsnocDim :: Index (Lower ix) => ix -> (Lower ix, Int)

  getIndex :: ix -> Dim -> Maybe Int

  setIndex :: ix -> Dim -> Int -> Maybe ix

  dropIndex :: ix -> Dim -> Maybe (Lower ix)

  iter :: ix -> ix -> Int -> (Int -> Int -> Bool) -> a -> (ix -> a -> a) -> a

  iterM :: Monad m =>
           ix -- ^ Start index
        -> ix -- ^ End index
        -> Int -- ^ Increment
        -> (Int -> Int -> Bool) -- ^ Continue iteration while predicate is True (eg. until end of row)
        -> a -- ^ Initial value for an accumulator
        -> (ix -> a -> m a) -- ^ Accumulator function
        -> m a

  iterM_ :: Monad m => ix -> ix -> Int -> (Int -> Int -> Bool) -> (ix -> m a) -> m ()

oneIndex :: Index ix => ix
oneIndex = liftIndex (+ 1) zeroIndex

data ZeroDim = ZeroDim deriving (Eq, Ord, Show)


instance Index Ix1T where
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
  unconsDim i = (i, ZeroDim)
  {-# INLINE [1] unconsDim #-}
  snocDim _ i = i
  {-# INLINE [1] snocDim #-}
  unsnocDim i = (ZeroDim, i)
  {-# INLINE [1] unsnocDim #-}
  getIndex i 1 = Just i
  getIndex _ _ = Nothing
  {-# INLINE [1] getIndex #-}
  setIndex _ 1 i = Just i
  setIndex _ _ _ = Nothing
  {-# INLINE [1] setIndex #-}
  dropIndex _ 1 = Just ZeroDim
  dropIndex _ _ = Nothing
  {-# INLINE [1] dropIndex #-}
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
  rank _ = 2
  {-# INLINE [1] rank #-}
  zeroIndex = (0, 0)
  {-# INLINE [1] zeroIndex #-}
  totalElem !(m, n) = m * n
  {-# INLINE [1] totalElem #-}
  isSafeIndex !(m, n) !(i, j) = 0 <= i && 0 <= j && i < m && j < n
  {-# INLINE [1] isSafeIndex #-}
  toLinearIndex !(_, n) !(i, j) = n * i + j
  {-# INLINE [1] toLinearIndex #-}
  fromLinearIndex (_, n) !k = k `quotRem` n
  {-# INLINE [1] fromLinearIndex #-}
  fromLinearIndexAcc (m, n) !k =
    case k `quotRem` n of
      (q, r) -> case q `quotRem` m of
                  (q', r') -> (q', (r', r))
  {-# INLINE [1] fromLinearIndexAcc #-}
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
  dropIndex (_, j) 2 = Just j
  dropIndex (i, _) 1 = Just i
  dropIndex _      _ = Nothing
  {-# INLINE [1] dropIndex #-}
  repairIndex = repairIndexRec
  {-# INLINE [1] repairIndex #-}
  liftIndex f (i, j) = (f i, f j)
  {-# INLINE [1] liftIndex #-}
  liftIndex2 f (i0, j0) (i1, j1) = (f i0 i1, f j0 j1)
  {-# INLINE [1] liftIndex2 #-}
  iter !(i, j) !(m, n) !inc cond !accInit f =
    loop i (`cond` m) (+ inc) accInit $ \ !i' !acc0 ->
      loop j (`cond` n) (+ inc) acc0 $ \ !j' -> f (i', j')
  {-# INLINE iter #-}
  iterM !(i, j) !(m, n) !inc cond !accInit f =
    loopM i (`cond` m) (+ inc) accInit $ \ !i' !acc0 ->
      loopM j (`cond` n) (+ inc) acc0 $ \ !j' -> f (i', j')
  {-# INLINE iterM #-}
  iterM_ !(i, j) !(m, n) !inc cond f =
    loopM_ i (`cond` m) (+ inc) $ \ !i' ->
      loopM_ j (`cond` n) (+ inc) $ \ !j' -> f (i', j')
  {-# INLINE iterM_ #-}


instance Index Ix3T where
  rank _ = 3
  {-# INLINE [1] rank #-}
  zeroIndex = (0, 0, 0)
  {-# INLINE [1] zeroIndex #-}
  totalElem !(m, n, o) = m * n * o
  {-# INLINE [1] totalElem #-}
  isSafeIndex !(m, n, o) !(i, j, k) =
    0 <= i && 0 <= j && 0 <= k && i < m && j < n && k < o
  {-# INLINE [1] isSafeIndex #-}
  toLinearIndex !(_, n, o) !(i, j, k) = (n * i + j) * o + k
  {-# INLINE [1] toLinearIndex #-}
  fromLinearIndex !(_, n, o) !l = (i, j, k)
    where !(h, k) = quotRem l o
          !(i, j) = quotRem h n
  {-# INLINE [1] fromLinearIndex #-}
  fromLinearIndexAcc !ix' !k = (q, consDim r ixL)
    where !(m, ix) = unconsDim ix'
          !(kL, ixL) = fromLinearIndexAcc ix k
          !(q, r) = quotRem kL m
  {-# INLINE [1] fromLinearIndexAcc #-}
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
  dropIndex (_, j, k) 3 = Just (j, k)
  dropIndex (i, _, k) 2 = Just (i, k)
  dropIndex (i, j, _) 1 = Just (i, j)
  dropIndex _      _    = Nothing
  {-# INLINE [1] dropIndex #-}
  repairIndex = repairIndexRec
  {-# INLINE [1] repairIndex #-}
  liftIndex f (i, j, k) = (f i, f j, f k)
  {-# INLINE [1] liftIndex #-}
  liftIndex2 f (i0, j0, k0) (i1, j1, k1) = (f i0 i1, f j0 j1, f k0 k1)
  {-# INLINE [1] liftIndex2 #-}
  -- ## GHC 8.2 is unable to optimize to this from iterRec?!
  iter !sIx !eIx !inc cond !acc f =
    loop k0 (`cond` k1) (+ inc) acc $ \ i acc0 ->
      iter sIxL eIxL inc cond acc0 $ \ ix -> f (consDim i ix)
    where
      !(k0, sIxL) = unconsDim sIx
      !(k1, eIxL) = unconsDim eIx
  {-# INLINE iter #-}
  iterM !sIx !eIx !inc cond !acc f =
    loopM k0 (`cond` k1) (+ inc) acc $ \ i acc0 ->
      iterM sIxL eIxL inc cond acc0 $ \ ix -> f (consDim i ix)
    where
      !(k0, sIxL) = unconsDim sIx
      !(k1, eIxL) = unconsDim eIx
  {-# INLINE iterM #-}
  -- iter = iterRec
  -- {-# INLINE iter #-}
  -- iterM = iterMRec
  -- {-# INLINE iterM #-}
  iterM_ = iterMRec_
  {-# INLINE iterM_ #-}


instance Index Ix4T where
  rank _ = 4
  {-# INLINE [1] rank #-}
  zeroIndex = (0, 0, 0, 0)
  {-# INLINE [1] zeroIndex #-}
  totalElem !(n1, n2, n3, n4) = n1 * n2 * n3 * n4
  {-# INLINE [1] totalElem #-}
  isSafeIndex = isSafeIndexRec
  {-# INLINE [1] isSafeIndex #-}
  -- toLinearIndex = toLinearIndexRec
  -- {-# INLINE [1] toLinearIndex #-}
  toLinearIndex !sz !ix = toLinearIndex szL ixL * n + i
    where !(szL, n) = unsnocDim sz
          !(ixL, i) = unsnocDim ix
  {-# INLINE [1] toLinearIndex #-}
  fromLinearIndex !sz !k = snocDim (fromLinearIndex szL kL) j
    where !(kL, j) = quotRem k n
          !(szL, n) = unsnocDim sz
  {-# INLINE [1] fromLinearIndex #-}
  -- fromLinearIndex = fromLinearIndexRec
  -- {-# INLINE [1] fromLinearIndex #-}
  --fromLinearIndexAcc = fromLinearIndexAccRec
  fromLinearIndexAcc !ix' !k = (q, consDim r ixL)
    where !(m, ix) = unconsDim ix'
          !(kL, ixL) = fromLinearIndexAcc ix k
          !(q, r) = quotRem kL m
  {-# INLINE [1] fromLinearIndexAcc #-}
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
  dropIndex ( _, i2, i3, i4) 4 = Just (i2, i3, i4)
  dropIndex (i1,  _, i3, i4) 3 = Just (i1, i3, i4)
  dropIndex (i1, i2,  _, i4) 2 = Just (i1, i2, i4)
  dropIndex (i1, i2, i3,  _) 1 = Just (i1, i2, i3)
  dropIndex _      _           = Nothing
  {-# INLINE [1] dropIndex #-}
  repairIndex = repairIndexRec
  {-# INLINE [1] repairIndex #-}
  liftIndex f (i0, i1, i2, i3) = (f i0, f i1, f i2, f i3)
  {-# INLINE [1] liftIndex #-}
  liftIndex2 f (i0, i1, i2, i3) (j0, j1, j2, j3) = (f i0 j0, f i1 j1, f i2 j2, f i3 j3)
  {-# INLINE [1] liftIndex2 #-}
  iter !sIx !eIx !inc cond !acc f =
    loop k0 (`cond` k1) (+ inc) acc $ \ i acc0 ->
      iter sIxL eIxL inc cond acc0 $ \ ix -> f (consDim i ix)
    where
      !(k0, sIxL) = unconsDim sIx
      !(k1, eIxL) = unconsDim eIx
  {-# INLINE iter #-}
  iterM !sIx !eIx !inc cond !acc f =
    loopM k0 (`cond` k1) (+ inc) acc $ \ i acc0 ->
      iterM sIxL eIxL inc cond acc0 $ \ ix -> f (consDim i ix)
    where
      !(k0, sIxL) = unconsDim sIx
      !(k1, eIxL) = unconsDim eIx
  {-# INLINE iterM #-}
  -- iter = iterRec
  -- {-# INLINE iter #-}
  -- iterM = iterMRec
  -- {-# INLINE iterM #-}
  iterM_ = iterMRec_
  {-# INLINE iterM_ #-}


instance Index Ix5T where
  rank _ = 5
  {-# INLINE [1] rank #-}
  zeroIndex = (0, 0, 0, 0, 0)
  {-# INLINE [1] zeroIndex #-}
  totalElem !(n1, n2, n3, n4, n5) = n1 * n2 * n3 * n4 * n5
  {-# INLINE [1] totalElem #-}
  isSafeIndex = isSafeIndexRec
  {-# INLINE [1] isSafeIndex #-}
  toLinearIndex !sz !ix = toLinearIndex szL ixL * n + i
    where !(szL, n) = unsnocDim sz
          !(ixL, i) = unsnocDim ix
  {-# INLINE [1] toLinearIndex #-}
  fromLinearIndex !sz !k = snocDim (fromLinearIndex szL kL) j
    where !(kL, j) = quotRem k n
          !(szL, n) = unsnocDim sz
  fromLinearIndexAcc = fromLinearIndexAccRec
  {-# INLINE [1] fromLinearIndexAcc #-}
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
  dropIndex ( _, i2, i3, i4, i5) 5 = Just (i2, i3, i4, i5)
  dropIndex (i1,  _, i3, i4, i5) 4 = Just (i1, i3, i4, i5)
  dropIndex (i1, i2,  _, i4, i5) 3 = Just (i1, i2, i4, i5)
  dropIndex (i1, i2, i3,  _, i5) 2 = Just (i1, i2, i3, i5)
  dropIndex (i1, i2, i3, i4,  _) 1 = Just (i1, i2, i3, i4)
  dropIndex _                    _ = Nothing
  {-# INLINE [1] dropIndex #-}
  repairIndex = repairIndexRec
  {-# INLINE [1] repairIndex #-}
  liftIndex f (i0, i1, i2, i3, i4) = (f i0, f i1, f i2, f i3, f i4)
  {-# INLINE [1] liftIndex #-}
  liftIndex2 f (i0, i1, i2, i3, i4) (j0, j1, j2, j3, j4) =
    (f i0 j0, f i1 j1, f i2 j2, f i3 j3, f i4 j4)
  {-# INLINE [1] liftIndex2 #-}
  iter !sIx !eIx !inc cond !acc f =
    loop k0 (`cond` k1) (+ inc) acc $ \ i acc0 ->
      iter sIxL eIxL inc cond acc0 $ \ ix -> f (consDim i ix)
    where
      !(k0, sIxL) = unconsDim sIx
      !(k1, eIxL) = unconsDim eIx
  {-# INLINE iter #-}
  iterM !sIx !eIx !inc cond !acc f =
    loopM k0 (`cond` k1) (+ inc) acc $ \ i acc0 ->
      iterM sIxL eIxL inc cond acc0 $ \ ix -> f (consDim i ix)
    where
      !(k0, sIxL) = unconsDim sIx
      !(k1, eIxL) = unconsDim eIx
  {-# INLINE iterM #-}
  -- iter = iterRec
  -- {-# INLINE iter #-}
  -- iterM = iterMRec
  -- {-# INLINE iterM #-}
  iterM_ = iterMRec_
  {-# INLINE iterM_ #-}


-- | Checks whether array with this size can hold at least one element.
isSafeSize :: Index ix => ix -> Bool
isSafeSize = (zeroIndex >=)
{-# INLINE [1] isSafeSize #-}


-- | Checks whether array with this size can hold at least one element.
isNonEmpty :: Index ix => ix -> Bool
isNonEmpty !sz = isSafeIndex sz zeroIndex
{-# INLINE [1] isNonEmpty #-}


headDim :: (Index ix, Index (Lower ix)) => ix -> Int
headDim = fst . unconsDim
{-# INLINE [1] headDim #-}

lastDim :: (Index ix, Index (Lower ix)) => ix -> Int
lastDim = snd . unsnocDim
{-# INLINE [1] lastDim #-}



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
{-# INLINE [1] handleBorderIndex #-}


isSafeIndexRec :: (Index (Lower ix), Index ix) => ix -> ix -> Bool
isSafeIndexRec !sz !ix = isSafeIndex n0 i0 && isSafeIndex szL ixL
    where
      !(n0, szL) = unconsDim sz
      !(i0, ixL) = unconsDim ix
{-# INLINE [1] isSafeIndexRec #-}


repairIndexRec :: (Index (Lower ix), Index ix) =>
                  ix -> ix -> (Int -> Int -> Int) -> (Int -> Int -> Int) -> ix
repairIndexRec !sz !ix rBelow rOver =
    snocDim (repairIndex szL ixL rBelow rOver) (repairIndex sz0 ix0 rBelow rOver)
    where !(szL, sz0) = unsnocDim sz
          !(ixL, ix0) = unsnocDim ix
{-# INLINE [1] repairIndexRec #-}


toLinearIndexRec :: (Index (Lower ix), Index ix) =>
                    ix -> ix -> Int
toLinearIndexRec !sz !ix = toLinearIndex szL ixL * n + i
  where !(szL, n) = unsnocDim sz
        !(ixL, i) = unsnocDim ix
{-# INLINE [1] toLinearIndexRec #-}



fromLinearIndexAccRec :: (Index (Lower ix), Index ix) => ix -> Int -> (Int, ix)
fromLinearIndexAccRec ix' !k = (q, consDim r ixL)
  where !(m, ix) = unconsDim ix'
        !(kL, ixL) = fromLinearIndexAcc ix k
        !(q, r) = quotRem kL m
{-# INLINE [1] fromLinearIndexAccRec #-}


iterRec
  :: (Index (Lower ix), Index ix)
  => ix -> ix -> Int -> (Int -> Int -> Bool) -> a -> (ix -> a -> a) -> a
iterRec !sIx !eIx !inc cond !acc f =
    loop k0 (`cond` k1) (+ inc) acc $ \ !i !acc0 ->
      iter sIxL eIxL inc cond acc0 $ \ !ix -> f (consDim i ix)
    where
      !(k0, sIxL) = unconsDim sIx
      !(k1, eIxL) = unconsDim eIx
{-# INLINE iterRec #-}


iterMRec
  :: (Index (Lower ix), Index ix, Monad m)
  => ix -> ix -> Int -> (Int -> Int -> Bool) -> a -> (ix -> a -> m a) -> m a
iterMRec !sIx !eIx !inc cond !acc f = do
    let !(k0, sIxL) = unconsDim sIx
        !(k1, eIxL) = unconsDim eIx
    loopM k0 (`cond` k1) (+ inc) acc $ \ !i !acc0 ->
      iterM sIxL eIxL inc cond acc0 $ \ !ix ->
        f (consDim i ix)
{-# INLINE iterMRec #-}

iterMRec_
  :: (Index (Lower ix), Index ix, Monad m)
  => ix -> ix -> Int -> (Int -> Int -> Bool) -> (ix -> m a) -> m ()
iterMRec_ !sIx !eIx !inc cond f = do
    let !(k0, sIxL) = unconsDim sIx
        !(k1, eIxL) = unconsDim eIx
    loopM_ k0 (`cond` k1) (+ inc) $ \ !i ->
      iterM_ sIxL eIxL inc cond $ \ !ix ->
        f (consDim i ix)
{-# INLINE iterMRec_ #-}


iterLinearM_ :: (Index ix, Monad m) =>
                ix -- ^ Size
             -> Int -- ^ Start
             -> Int -- ^ End
             -> Int -- ^ Increment
             -> (Int -> Int -> Bool) -- ^ Continuation condition
             -> (Int -> ix -> m ()) -- ^ Monadic action that takes index in both forms
             -> m ()
iterLinearM_ !sz !k0 !k1 !inc cond f =
  loopM_ k0 (`cond` k1) (+ inc) $ \ !i -> f i (fromLinearIndex sz i)
{-# INLINE iterLinearM_ #-}

-- | Iterate over N-dimensional space from start to end with accumulator
iterLinearM :: (Index ix, Monad m)
            => ix -- ^ Size
            -> Int -- ^ Linear start
            -> Int -- ^ Linear end
            -> Int -- ^ Increment
            -> (Int -> Int -> Bool) -- ^ Continuation condition (continue if True)
            -> a -- ^ Accumulator
            -> (Int -> ix -> a -> m a)
            -> m a
iterLinearM !sz !k0 !k1 !inc cond !acc f =
  loopM k0 (`cond` k1) (+ inc) acc $ \ !i !acc0 -> f i (fromLinearIndex sz i) acc0
{-# INLINE iterLinearM #-}


-- | Very efficient loop with an accumulator
loop :: Int -> (Int -> Bool) -> (Int -> Int) -> a -> (Int -> a -> a) -> a
loop !init' condition increment !initAcc f = go init' initAcc where
  go !step !acc =
    case condition step of
      False -> acc
      True  -> go (increment step) (f step acc)
{-# INLINE loop #-}


-- | Very efficient monadic loop
loopM_ :: Monad m => Int -> (Int -> Bool) -> (Int -> Int) -> (Int -> m a) -> m ()
loopM_ !init' condition increment f = go init' where
  go !step =
    case condition step of
      False -> return ()
      True  -> f step >> go (increment step)
{-# INLINE loopM_ #-}


-- | Very efficient monadic loop with an accumulator
loopM :: Monad m => Int -> (Int -> Bool) -> (Int -> Int) -> a -> (Int -> a -> m a) -> m a
loopM !init' condition increment !initAcc f = go init' initAcc where
  go !step !acc =
    case condition step of
      False -> return acc
      True  -> f step acc >>= go (increment step)
{-# INLINE loopM #-}
