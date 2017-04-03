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

type DIM1 = Int

type DIM2 = (Int, Int)

type DIM3 = (Int, Int, Int)

type DIM4 = (Int, Int, Int, Int)

type DIM5 = (Int, Int, Int, Int, Int)

type family Lower ix :: *
type family Higher ix :: *

type instance Lower () = DIM5
type instance Lower Z = ()
type instance Lower DIM1 = Z
type instance Lower DIM2 = DIM1
type instance Lower DIM3 = DIM2
type instance Lower DIM4 = DIM3
type instance Lower DIM5 = DIM4

type instance Higher () = Z
type instance Higher Z = DIM1
type instance Higher DIM1 = DIM2
type instance Higher DIM2 = DIM3
type instance Higher DIM3 = DIM4
type instance Higher DIM4 = DIM5
type instance Higher DIM5 = ()



class (Eq ix, Show ix) => Index ix where

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

  iter :: ix -> ix -> a -> (ix -> a -> a) -> a

  iterM :: Monad m => ix -> ix -> a -> (ix -> a -> m a) -> m a

  iterM_ :: Monad m => ix -> ix -> (ix -> m ()) -> m ()



data Z = Z deriving (Eq, Show)

errorBelowZero :: a
errorBelowZero = error "There is no dimension that is lower than DIM0"

instance Index Z where
  zeroIndex = Z
  {-# INLINE zeroIndex #-}
  totalElem _ = 0
  {-# INLINE totalElem #-}
  isSafeIndex _   _    = False
  {-# INLINE isSafeIndex #-}
  toLinearIndex _ _ = 0
  {-# INLINE toLinearIndex #-}
  fromLinearIndex _ _ = Z
  {-# INLINE fromLinearIndex #-}
  repairIndex _ _ _ _ = Z
  {-# INLINE repairIndex #-}
  consDim _ _ = Z
  {-# INLINE consDim #-}
  unconsDim _ = errorBelowZero
  {-# INLINE unconsDim #-}
  snocDim _ _ = Z
  {-# INLINE snocDim #-}
  unsnocDim _ = errorBelowZero
  {-# INLINE unsnocDim #-}
  liftIndex _ _ = Z
  {-# INLINE liftIndex #-}
  liftIndex2 _ _ _ = Z
  {-# INLINE liftIndex2 #-}
  iter _ _ acc f = f Z acc
  {-# INLINE iter #-}
  iterM _ _ acc f = f Z acc
  {-# INLINE iterM #-}
  iterM_ _ _ f = f Z
  {-# INLINE iterM_ #-}


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
  consDim i _ = i
  {-# INLINE consDim #-}
  unconsDim i = (i, Z)
  {-# INLINE unconsDim #-}
  snocDim _ i = i
  {-# INLINE snocDim #-}
  unsnocDim i = (Z, i)
  {-# INLINE unsnocDim #-}
  liftIndex f = f
  {-# INLINE liftIndex #-}
  liftIndex2 f = f
  {-# INLINE liftIndex2 #-}
  iter k0 k1 = loop k0 (<k1) (+1)
  {-# INLINE iter #-}
  iterM k0 k1 = loopM k0 (<k1) (+1)
  {-# INLINE iterM #-}
  iterM_ k0 k1 = loopM_ k0 (<k1) (+1)
  {-# INLINE iterM_ #-}


instance Index DIM2 where
  zeroIndex = (0, 0)
  {-# INLINE zeroIndex #-}
  totalElem !(m, n) = m * n
  {-# INLINE totalElem #-}
  isSafeIndex !(m, n) !(i, j) = 0 <= i && 0 <= j && i < m && j < n
  {-# INLINE isSafeIndex #-}
  toLinearIndex !(_, n) !(i, j) = n * i + j
  {-# INLINE toLinearIndex #-}
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
  iter = iterRec
  {-# INLINE iter #-}
  iterM = iterMRec
  {-# INLINE iterM #-}
  iterM_ = iterMRec_
  {-# INLINE iterM_ #-}


instance Index DIM3 where
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
  iter = iterRec
  {-# INLINE iter #-}
  iterM = iterMRec
  {-# INLINE iterM #-}
  iterM_ = iterMRec_
  {-# INLINE iterM_ #-}


instance Index DIM4 where
  zeroIndex = (0, 0, 0, 0)
  {-# INLINE zeroIndex #-}
  totalElem !(n0, n1, n2, n3) = n0 * n1 * n2 * n3
  {-# INLINE totalElem #-}
  isSafeIndex = isSafeIndexRec
  {-# INLINE isSafeIndex #-}
  toLinearIndex = toLinearIndexRec
  {-# INLINE toLinearIndex #-}
  fromLinearIndex = fromLinearIndexRec
  {-# INLINE fromLinearIndex #-}
  consDim i0 (i1, i2, i3) = (i0, i1, i2, i3)
  {-# INLINE consDim #-}
  unconsDim (i0, i1, i2, i3) = (i0, (i1, i2, i3))
  {-# INLINE unconsDim #-}
  snocDim (i0, i1, i2) i3 = (i0, i1, i2, i3)
  {-# INLINE snocDim #-}
  unsnocDim (i0, i1, i2, i3) = ((i0, i1, i2), i3)
  {-# INLINE unsnocDim #-}
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
  zeroIndex = (0, 0, 0, 0, 0)
  {-# INLINE zeroIndex #-}
  totalElem !(n0, n1, n2, n3, n4) = n0 * n1 * n2 * n3 * n4
  {-# INLINE totalElem #-}
  isSafeIndex = isSafeIndexRec
  {-# INLINE isSafeIndex #-}
  toLinearIndex = toLinearIndexRec
  {-# INLINE toLinearIndex #-}
  fromLinearIndex = fromLinearIndexRec
  {-# INLINE fromLinearIndex #-}
  consDim i0 (i1, i2, i3, i4) = (i0, i1, i2, i3, i4)
  {-# INLINE consDim #-}
  unconsDim (i0, i1, i2, i3, i4) = (i0, (i1, i2, i3, i4))
  {-# INLINE unconsDim #-}
  snocDim (i0, i1, i2, i3) i4 = (i0, i1, i2, i3, i4)
  {-# INLINE snocDim #-}
  unsnocDim (i0, i1, i2, i3, i4) = ((i0, i1, i2, i3), i4)
  {-# INLINE unsnocDim #-}
  repairIndex = repairIndexRec
  {-# INLINE repairIndex #-}
  liftIndex f (i0, i1, i2, i3, i4) = (f i0, f i1, f i2, f i3, f i4)
  {-# INLINE liftIndex #-}
  liftIndex2 f (i0, i1, i2, i3, i4) (j0, j1, j2, j3, j4) =
    (f i0 j0, f i1 j1, f i2 j2, f i3 j3, f i4 j4)
  {-# INLINE liftIndex2 #-}
  iter = iterRec
  -- iter !(i0, i1, i2, i3, i4) !(j0, j1, j2, j3, j4) !acc f =
  --   loop i0 (< j0) (+ 1) acc $ \ !k0 !acc0 ->
  --     loop i1 (< j1) (+ 1) acc0 $ \ !k1 !acc1 ->
  --       loop i2 (< j2) (+ 1) acc1 $ \ !k2 !acc2 ->
  --         loop i3 (< j3) (+ 1) acc2 $ \ !k3 !acc3 ->
  --           loop i4 (< j4) (+ 1) acc3 $ \ !k4 !acc4 -> f (k0, k1, k2, k3, k4) acc4
  {-# INLINE iter #-}
  iterM = iterMRec
  {-# INLINE iterM #-}
  iterM_ = iterMRec_
  {-# INLINE iterM_ #-}



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


toLinearIndexRec :: (Index (Lower ix), Index ix) =>
                      ix -> ix -> Int
toLinearIndexRec !sz !ix = (toLinearIndex szL ixL) * n + i
  where !(szL, n) = unsnocDim sz
        !(ixL, i) = unsnocDim ix
{-# INLINE toLinearIndexRec #-}


fromLinearIndexRec :: (Index (Lower ix), Index ix) =>
                      ix -> Int -> ix
fromLinearIndexRec !sz !k = snocDim (fromLinearIndex szL kL) j
  where !(kL, j) = quotRemInt k n
        !(szL, n) = unsnocDim sz
{-# INLINE fromLinearIndexRec #-}


iterRec :: (Index (Lower ix), Index ix) => ix -> ix -> a -> (ix -> a -> a) -> a
iterRec sIx eIx acc f =
    loop k0 (< k1) (+ 1) acc $ \ !i !acc0 ->
      iter sIxL eIxL acc0 $ \ !ix -> f (consDim i ix)
    where
      !(k0, sIxL) = unconsDim sIx
      !(k1, eIxL) = unconsDim eIx
{-# INLINE iterRec #-}


iterMRec :: (Index (Lower ix), Index ix, Monad m) => ix -> ix -> a -> (ix -> a -> m a) -> m a
iterMRec !sIx !eIx !acc f = do
    let !(k0, sIxL) = unconsDim sIx
        !(k1, eIxL) = unconsDim eIx
    loopM k0 (< k1) (+ 1) acc $ \ !i !acc0 ->
      iterM sIxL eIxL acc0 $ \ !ix ->
        f (consDim i ix)
{-# INLINE iterMRec #-}

iterMRec_ :: (Index (Lower ix), Index ix, Monad m) => ix -> ix -> (ix -> m ()) -> m ()
iterMRec_ !sIx !eIx f = do
    let !(k0, sIxL) = unconsDim sIx
        !(k1, eIxL) = unconsDim eIx
    loopM_ k0 (< k1) (+ 1) $ \ !i ->
      iterM_ sIxL eIxL $ \ !ix ->
        f (consDim i ix)
{-# INLINE iterMRec_ #-}


iterLinearM_ :: (Index ix, Monad m) => ix -> Int -> Int -> (Int -> ix -> m ()) -> m ()
iterLinearM_ sz k0 k1 f = loopM_ k0 (<k1) (+1) $ \ !i -> f i (fromLinearIndex sz i)
{-# INLINE iterLinearM_ #-}

-- | Iterate over N-dimensional space from start to end with accumulator
iterLinearM :: (Index ix, Monad m) => ix -> Int -> Int -> a -> (Int -> ix -> a -> m a) -> m a
iterLinearM sz k0 k1 acc f =
  loopM k0 (< k1) (+ 1) acc $ \ !i acc0 -> f i (fromLinearIndex sz i) acc0
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
  go !step acc =
    case condition step of
      False -> return acc
      True  -> f step acc >>= go (increment step)
{-# INLINE loopM #-}
