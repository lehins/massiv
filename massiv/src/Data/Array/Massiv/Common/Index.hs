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

import           Control.DeepSeq (NFData)


type DIM1 = Int

type DIM2 = (Int, Int)

type DIM3 = (Int, Int, Int)

type DIM4 = (Int, Int, Int, Int)

type DIM5 = (Int, Int, Int, Int, Int)

type family Lower ix :: *

type instance Lower DIM1 = ZeroDim
type instance Lower DIM2 = DIM1
type instance Lower DIM3 = DIM2
type instance Lower DIM4 = DIM3
type instance Lower DIM5 = DIM4

class (Eq ix, Ord ix, Show ix, NFData ix) => Index ix where

  rank :: ix -> Int

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

  toLinearIndexAcc :: Int -> ix -> ix -> Int

  -- | Produce N Dim index from size and linear index
  fromLinearIndex :: ix -> Int -> ix

  liftIndex :: (Int -> Int) -> ix -> ix

  liftIndex2 :: (Int -> Int -> Int) -> ix -> ix -> ix

  repairIndex :: ix -> ix -> (Int -> Int -> Int) -> (Int -> Int -> Int) -> ix

  consDim :: Index (Lower ix) => Int -> Lower ix -> ix

  unconsDim :: Index (Lower ix) => ix -> (Int, Lower ix)

  snocDim :: Index (Lower ix) => Lower ix -> Int -> ix

  unsnocDim :: Index (Lower ix) => ix -> (Lower ix, Int)

  getIndex :: ix -> Int -> Maybe Int

  setIndex :: ix -> Int -> Int -> Maybe ix

  dropIndex :: ix -> Int -> Maybe (Lower ix)

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


instance Index DIM1 where
  rank _ = 1
  {-# INLINE rank #-}
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
  consDim i _ = i
  {-# INLINE consDim #-}
  unconsDim i = (i, ZeroDim)
  {-# INLINE unconsDim #-}
  snocDim _ i = i
  {-# INLINE snocDim #-}
  unsnocDim i = (ZeroDim, i)
  {-# INLINE unsnocDim #-}
  getIndex i 1 = Just i
  getIndex _ _ = Nothing
  {-# INLINE getIndex #-}
  setIndex _ 1 i = Just i
  setIndex _ _ _ = Nothing
  {-# INLINE setIndex #-}
  dropIndex _ 1 = Just ZeroDim
  dropIndex _ _ = Nothing
  {-# INLINE dropIndex #-}
  liftIndex f = f
  {-# INLINE liftIndex #-}
  liftIndex2 f = f
  {-# INLINE liftIndex2 #-}
  iter k0 k1 inc cond = loop k0 (`cond` k1) (+inc)
  {-# INLINE iter #-}
  iterM k0 k1 inc cond = loopM k0 (`cond` k1) (+inc)
  {-# INLINE iterM #-}
  iterM_ k0 k1 inc cond = loopM_ k0 (`cond` k1) (+inc)
  {-# INLINE iterM_ #-}


instance Index DIM2 where
  rank _ = 2
  {-# INLINE rank #-}
  zeroIndex = (0, 0)
  {-# INLINE zeroIndex #-}
  totalElem !(m, n) = m * n
  {-# INLINE totalElem #-}
  isSafeIndex !(m, n) !(i, j) = 0 <= i && 0 <= j && i < m && j < n
  {-# INLINE isSafeIndex #-}
  toLinearIndex !(_, n) !(i, j) = n * i + j
  {-# INLINE toLinearIndex #-}
  fromLinearIndex !(_, n) !k = k `quotRem` n
  {-# INLINE fromLinearIndex #-}
  consDim = (,)
  {-# INLINE consDim #-}
  unconsDim = id
  {-# INLINE unconsDim #-}
  snocDim = (,)
  {-# INLINE snocDim #-}
  unsnocDim = id
  {-# INLINE unsnocDim #-}
  getIndex (i, _) 1 = Just i
  getIndex (_, j) 2 = Just j
  getIndex _      _ = Nothing
  {-# INLINE getIndex #-}
  setIndex (_, j) 1 i = Just (i, j)
  setIndex (i, _) 2 j = Just (i, j)
  setIndex _      _ _ = Nothing
  {-# INLINE setIndex #-}
  dropIndex (_, j) 1 = Just j
  dropIndex (i, _) 2 = Just i
  dropIndex _      _ = Nothing
  {-# INLINE dropIndex #-}
  repairIndex = repairIndexRec
  {-# INLINE repairIndex #-}
  liftIndex f (i, j) = (f i, f j)
  {-# INLINE liftIndex #-}
  liftIndex2 f (i0, j0) (i1, j1) = (f i0 i1, f j0 j1)
  {-# INLINE liftIndex2 #-}
  iter = iterRec
  {-# INLINE iter #-}
  iterM = iterMRec
  {-# INLINE iterM #-}
  iterM_ = iterMRec_
  {-# INLINE iterM_ #-}


instance Index DIM3 where
  rank _ = 3
  {-# INLINE rank #-}
  zeroIndex = (0, 0, 0)
  {-# INLINE zeroIndex #-}
  totalElem !(m, n, o) = m * n * o
  {-# INLINE totalElem #-}
  isSafeIndex !(m, n, o) !(i, j, k) =
    0 <= i && 0 <= j && 0 <= k && i < m && j < n && k < o
  {-# INLINE isSafeIndex #-}
  toLinearIndex !(_, n, o) !(i, j, k) = (n * i + j) * o + k
  {-# INLINE toLinearIndex #-}
  fromLinearIndex !(_, n, o) !l = (i, j, k)
    where !(h, k) = quotRem l o
          !(i, j) = quotRem h n
  {-# INLINE fromLinearIndex #-}
  consDim i (j, k) = (i, j, k)
  {-# INLINE consDim #-}
  unconsDim (i, j, k) = (i, (j, k))
  {-# INLINE unconsDim #-}
  snocDim (i, j) k = (i, j, k)
  {-# INLINE snocDim #-}
  unsnocDim (i, j, k) = ((i, j), k)
  {-# INLINE unsnocDim #-}
  getIndex (i, _, _) 1 = Just i
  getIndex (_, j, _) 2 = Just j
  getIndex (_, _, k) 3 = Just k
  getIndex _         _ = Nothing
  {-# INLINE getIndex #-}
  setIndex (_, j, k) 1 i = Just (i, j, k)
  setIndex (i, _, k) 2 j = Just (i, j, k)
  setIndex (i, j, _) 3 k = Just (i, j, k)
  setIndex _      _ _ = Nothing
  {-# INLINE setIndex #-}
  dropIndex (_, j, k) 1 = Just (j, k)
  dropIndex (i, _, k) 2 = Just (i, k)
  dropIndex (i, j, _) 3 = Just (i, j)
  dropIndex _      _ = Nothing
  {-# INLINE dropIndex #-}
  repairIndex = repairIndexRec
  {-# INLINE repairIndex #-}
  liftIndex f (i, j, k) = (f i, f j, f k)
  {-# INLINE liftIndex #-}
  liftIndex2 f (i0, j0, k0) (i1, j1, k1) = (f i0 i1, f j0 j1, f k0 k1)
  {-# INLINE liftIndex2 #-}
  iter = iterRec
  {-# INLINE iter #-}
  iterM = iterMRec
  {-# INLINE iterM #-}
  iterM_ = iterMRec_
  {-# INLINE iterM_ #-}


instance Index DIM4 where
  rank _ = 4
  {-# INLINE rank #-}
  zeroIndex = (0, 0, 0, 0)
  {-# INLINE zeroIndex #-}
  totalElem !(n1, n2, n3, n4) = n1 * n2 * n3 * n4
  {-# INLINE totalElem #-}
  isSafeIndex = isSafeIndexRec
  {-# INLINE isSafeIndex #-}
  toLinearIndex = toLinearIndexRec
  {-# INLINE toLinearIndex #-}
  fromLinearIndex = fromLinearIndexRec
  {-# INLINE fromLinearIndex #-}
  consDim i1 (i2, i3, i4) = (i1, i2, i3, i4)
  {-# INLINE consDim #-}
  unconsDim (i1, i2, i3, i4) = (i1, (i2, i3, i4))
  {-# INLINE unconsDim #-}
  snocDim (i1, i2, i3) i4 = (i1, i2, i3, i4)
  {-# INLINE snocDim #-}
  unsnocDim (i1, i2, i3, i4) = ((i1, i2, i3), i4)
  {-# INLINE unsnocDim #-}
  getIndex (i1,  _,  _,  _) 1 = Just i1
  getIndex ( _, i2,  _,  _) 2 = Just i2
  getIndex ( _,  _, i3,  _) 3 = Just i3
  getIndex ( _,  _,  _, i4) 4 = Just i4
  getIndex _                _ = Nothing
  {-# INLINE getIndex #-}
  setIndex ( _, i2, i3, i4) 1 i1 = Just (i1, i2, i3, i4)
  setIndex (i1,  _, i3, i4) 2 i2 = Just (i1, i2, i3, i4)
  setIndex (i1, i2,  _, i4) 3 i3 = Just (i1, i2, i3, i4)
  setIndex (i1, i2, i3,  _) 4 i4 = Just (i1, i2, i3, i4)
  setIndex _                _  _ = Nothing
  {-# INLINE setIndex #-}
  dropIndex ( _, i2, i3, i4) 1 = Just (i2, i3, i4)
  dropIndex (i1,  _, i3, i4) 2 = Just (i1, i3, i4)
  dropIndex (i1, i2,  _, i4) 3 = Just (i1, i2, i4)
  dropIndex (i1, i2, i3,  _) 4 = Just (i1, i2, i3)
  dropIndex _      _ = Nothing
  {-# INLINE dropIndex #-}
  repairIndex = repairIndexRec
  {-# INLINE repairIndex #-}
  liftIndex f (i0, i1, i2, i3) = (f i0, f i1, f i2, f i3)
  {-# INLINE liftIndex #-}
  liftIndex2 f (i0, i1, i2, i3) (j0, j1, j2, j3) = (f i0 j0, f i1 j1, f i2 j2, f i3 j3)
  {-# INLINE liftIndex2 #-}
  iter = iterRec
  {-# INLINE iter #-}
  iterM = iterMRec
  {-# INLINE iterM #-}
  iterM_ = iterMRec_
  {-# INLINE iterM_ #-}


instance Index DIM5 where
  rank _ = 5
  {-# INLINE rank #-}
  zeroIndex = (0, 0, 0, 0, 0)
  {-# INLINE zeroIndex #-}
  totalElem !(n1, n2, n3, n4, n5) = n1 * n2 * n3 * n4 * n5
  {-# INLINE totalElem #-}
  isSafeIndex = isSafeIndexRec
  {-# INLINE isSafeIndex #-}
  toLinearIndex = toLinearIndexRec
  {-# INLINE toLinearIndex #-}
  fromLinearIndex = fromLinearIndexRec
  {-# INLINE fromLinearIndex #-}
  consDim i1 (i2, i3, i4, i5) = (i1, i2, i3, i4, i5)
  {-# INLINE consDim #-}
  unconsDim (i1, i2, i3, i4, i5) = (i1, (i2, i3, i4, i5))
  {-# INLINE unconsDim #-}
  snocDim (i1, i2, i3, i4) i5 = (i1, i2, i3, i4, i5)
  {-# INLINE snocDim #-}
  unsnocDim (i1, i2, i3, i4, i5) = ((i1, i2, i3, i4), i5)
  {-# INLINE unsnocDim #-}
  getIndex (i1,  _,  _,  _,  _) 1 = Just i1
  getIndex ( _, i2,  _,  _,  _) 2 = Just i2
  getIndex ( _,  _, i3,  _,  _) 3 = Just i3
  getIndex ( _,  _,  _, i4,  _) 4 = Just i4
  getIndex ( _,  _,  _,  _, i5) 5 = Just i5
  getIndex _                _ = Nothing
  {-# INLINE getIndex #-}
  setIndex ( _, i2, i3, i4, i5) 1 i1 = Just (i1, i2, i3, i4, i5)
  setIndex (i1,  _, i3, i4, i5) 2 i2 = Just (i1, i2, i3, i4, i5)
  setIndex (i1, i2,  _, i4, i5) 3 i3 = Just (i1, i2, i3, i4, i5)
  setIndex (i1, i2, i3,  _, i5) 4 i4 = Just (i1, i2, i3, i4, i5)
  setIndex (i1, i2, i3, i4,  _) 5 i5 = Just (i1, i2, i3, i4, i5)
  setIndex _                    _  _ = Nothing
  {-# INLINE setIndex #-}
  dropIndex ( _, i2, i3, i4, i5) 1 = Just (i2, i3, i4, i5)
  dropIndex (i1,  _, i3, i4, i5) 2 = Just (i1, i3, i4, i5)
  dropIndex (i1, i2,  _, i4, i5) 3 = Just (i1, i2, i4, i5)
  dropIndex (i1, i2, i3,  _, i5) 4 = Just (i1, i2, i3, i5)
  dropIndex (i1, i2, i3, i4,  _) 5 = Just (i1, i2, i3, i4)
  dropIndex _      _ = Nothing
  {-# INLINE dropIndex #-}
  repairIndex = repairIndexRec
  {-# INLINE repairIndex #-}
  liftIndex f (i0, i1, i2, i3, i4) = (f i0, f i1, f i2, f i3, f i4)
  {-# INLINE liftIndex #-}
  liftIndex2 f (i0, i1, i2, i3, i4) (j0, j1, j2, j3, j4) =
    (f i0 j0, f i1 j1, f i2 j2, f i3 j3, f i4 j4)
  {-# INLINE liftIndex2 #-}
  iter = iterRec
  {-# INLINE iter #-}
  iterM = iterMRec
  {-# INLINE iterM #-}
  iterM_ = iterMRec_
  {-# INLINE iterM_ #-}


-- | Checks whether array with this size can hold at least one element.
isSafeSize :: Index ix => ix -> Bool
isSafeSize = (zeroIndex >=)
{-# INLINE isSafeSize #-}


-- | Checks whether array with this size can hold at least one element.
isNonEmpty :: Index ix => ix -> Bool
isNonEmpty !sz = isSafeIndex sz zeroIndex
{-# INLINE isNonEmpty #-}


headDim :: (Index ix, Index (Lower ix)) => ix -> Int
headDim = fst . unconsDim
{-# INLINE headDim #-}

lastDim :: (Index ix, Index (Lower ix)) => ix -> Int
lastDim = snd . unsnocDim
{-# INLINE lastDim #-}

-- | Approach to be used with respect to the border of an array
-- when index goes out of bounds.
data Border e = Fill e | Wrap | Edge | Reflect | Continue deriving (Eq, Show)



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


isSafeIndexRec :: (Index (Lower ix), Index ix) => ix -> ix -> Bool
isSafeIndexRec !sz !ix = isSafeIndex n0 i0 && isSafeIndex szL ixL
    where
      !(n0, szL) = unconsDim sz
      !(i0, ixL) = unconsDim ix
{-# INLINE isSafeIndexRec #-}


repairIndexRec :: (Index (Lower ix), Index ix) =>
                  ix -> ix -> (Int -> Int -> Int) -> (Int -> Int -> Int) -> ix
repairIndexRec !sz !ix rBelow rOver =
    snocDim (repairIndex szL ixL rBelow rOver) (repairIndex sz0 ix0 rBelow rOver)
    where !(szL, sz0) = unsnocDim sz
          !(ixL, ix0) = unsnocDim ix
{-# INLINE repairIndexRec #-}


-- liftIndexRec :: (Index (Lower ix), Index ix) =>
--                 (Int -> Int) -> ix -> ix
-- liftIndexRec f !ix = snocDim (liftIndex f ixL) (liftIndex f ix0)
--   where
--     !(ixL, ix0) = unsnocDim ix
-- {-# INLINE liftIndexRec #-}


-- liftIndex2Rec :: (Index (Lower ix), Index ix) =>
--                 (Int -> Int -> Int) -> ix -> ix -> ix
-- liftIndex2Rec f !ix !ixD = snocDim (liftIndex2 f ixL ixDL) (liftIndex2 f ix0 ixD0)
--   where
--     !(ixL, ix0) = unsnocDim ix
--     !(ixDL, ixD0) = unsnocDim ixD
-- {-# INLINE liftIndex2Rec #-}


toLinearIndexRec :: (Index (Lower ix), Index ix) =>
                    ix -> ix -> Int
toLinearIndexRec !sz !ix = toLinearIndex szL ixL * n + i
  where !(szL, n) = unsnocDim sz
        !(ixL, i) = unsnocDim ix
{-# INLINE toLinearIndexRec #-}


fromLinearIndexRec :: (Index (Lower ix), Index ix) =>
                      ix -> Int -> ix
fromLinearIndexRec !sz !k = snocDim (fromLinearIndex szL kL) j
  where !(kL, j) = quotRem k n
        !(szL, n) = unsnocDim sz
{-# INLINE fromLinearIndexRec #-}


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
             -> (Int -> Int -> Bool) -- ^ Terminating condition
             -> (Int -> ix -> m ()) -- ^ Monadic action that takes index in both forms
             -> m ()
iterLinearM_ !sz !k0 !k1 !inc cond f =
  loopM_ k0 (`cond` k1) (+ inc) $ \ !i -> f i (fromLinearIndex sz i)
{-# INLINE iterLinearM_ #-}

-- | Iterate over N-dimensional space from start to end with accumulator
iterLinearM :: (Index ix, Monad m)
            => ix
            -> Int
            -> Int
            -> Int
            -> (Int -> Int -> Bool)
            -> a
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
