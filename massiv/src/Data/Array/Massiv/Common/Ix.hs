{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

#if __GLASGOW_HASKELL__ >= 800

  {-# LANGUAGE TypeFamilyDependencies #-}

#endif
-- |
-- Module      : Data.Array.Massiv.Common.Ix
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Common.Ix where

import           Control.DeepSeq
import           Data.Array.Massiv.Common.Index
import           Data.Monoid                    ((<>))
import           Data.Proxy
import           GHC.TypeLits


infixr 5 :>, :.

type Ix1 = Int

data Ix2 = (:.) {-# UNPACK #-} !Int {-# UNPACK #-} !Int

type Ix3 = IxN 3
type Ix4 = IxN 4
type Ix5 = IxN 5


#if __GLASGOW_HASKELL__ >= 800

data IxN (n :: Nat) where
  (:>) :: {-# UNPACK #-} !Int -> !(Ix (n - 1)) -> IxN n

type family Ix (n :: Nat) = r | r -> n where
  Ix 0 = ZeroDim
  Ix 1 = Int
  Ix 2 = Ix2
  Ix n = IxN n

#else

data IxN (n :: Nat) where
  (:>) :: Rank (Ix (n - 1)) ~ (n - 1) => {-# UNPACK #-} !Int -> !(Ix (n - 1)) -> IxN n

type family Ix (n :: Nat) where
  Ix 0 = ZeroDim
  Ix 1 = Int
  Ix 2 = Ix2
  Ix n = IxN n

type family Rank ix where
  Rank ZeroDim = 0
  Rank Ix1 = 1
  Rank Ix2 = 2
  Rank (IxN n) = n

#endif


type instance Lower Ix2 = Ix1
type instance Lower (IxN n) = Ix (n-1)


instance Show Ix2 where
  show (i :. j)  = show i ++ " x " ++ show j

instance Show (Ix (n-1)) => Show (IxN n) where
  show (i :> ix) = show i ++ " x " ++ show ix


instance NFData Ix2 where
  rnf ix = ix `seq` ()

instance NFData (IxN n) where
  rnf ix = ix `seq` ()


instance Eq Ix2 where
  (i1 :. j1)  == (i2 :. j2) = i1 == i2 && j1 == j2

instance Eq (Ix (n-1)) => Eq (IxN n) where
  (i1 :> ix1) == (i2 :> ix2) = i1 == i2 && ix1 == ix2


instance Ord Ix2 where
  compare (i1 :. j1) (i2 :. j2)  = compare i1 i2 <> compare j1 j2

instance Ord (Ix (n-1)) => Ord (IxN n) where
  compare (i1 :> ix1) (i2 :> ix2) = compare i1 i2 <> compare ix1 ix2


toIx2 :: Ix2T -> Ix2
toIx2 (i, j) = i :. j
{-# INLINE [1] toIx2 #-}

fromIx2 :: Ix2 -> Ix2T
fromIx2 (i :. j) = (i, j)
{-# INLINE [1] fromIx2 #-}

-- | Convert a function on 2D indices
toIxF2 :: (Ix2T -> a) -> Ix2 -> a
toIxF2 g (i :. j) = g (i, j)
{-# INLINE [1] toIxF2 #-}

toIx3 :: Ix3T -> Ix3
toIx3 (i, j, k) = i :> j :. k
{-# INLINE [1] toIx3 #-}

fromIx3 :: Ix3 -> Ix3T
fromIx3 (i :> j :. k) = (i, j, k)
{-# INLINE [1] fromIx3 #-}

toIxF3 :: (Ix3T -> a) -> Ix3 -> a
toIxF3 g (i :> j :. k) = g (i, j, k)
{-# INLINE [1] toIxF3 #-}


instance Index Ix2 where
  rank _ = 2
  {-# INLINE [1] rank #-}
  zeroIndex = (0 :. 0)
  {-# INLINE [1] zeroIndex #-}
  totalElem (m :. n) = m * n
  {-# INLINE [1] totalElem #-}
  isSafeIndex (m :. n) (i :. j) = 0 <= i && 0 <= j && i < m && j < n
  {-# INLINE [1] isSafeIndex #-}
  toLinearIndex (_ :. n) (i :. j) = n * i + j
  {-# INLINE [1] toLinearIndex #-}
  fromLinearIndex (_ :. n) k = case k `quotRem` n of
                                 (i, j) -> i :. j
  {-# INLINE [1] fromLinearIndex #-}
  fromLinearIndexAcc (m :. n) !k =
    case k `quotRem` n of
      (q, r) -> case q `quotRem` m of
                  (q', r') -> (q', r' :. r)
  {-# INLINE [1] fromLinearIndexAcc #-}
  consDim = (:.)
  {-# INLINE [1] consDim #-}
  unconsDim (i :. ix) = (i, ix)
  {-# INLINE [1] unconsDim #-}
  snocDim i j = i :. j
  {-# INLINE [1] snocDim #-}
  unsnocDim (i :. j) = (i, j)
  {-# INLINE [1] unsnocDim #-}
  getIndex (i :. _) 2 = Just i
  getIndex (_ :. j) 1 = Just j
  getIndex _        _ = Nothing
  {-# INLINE [1] getIndex #-}
  setIndex (_ :. j) 2 i = Just (i :. j)
  setIndex (i :. _) 1 j = Just (i :. j)
  setIndex _        _ _ = Nothing
  {-# INLINE [1] setIndex #-}
  dropIndex (_ :. j) 2 = Just j
  dropIndex (i :. _) 1 = Just i
  dropIndex _      _   = Nothing
  {-# INLINE [1] dropIndex #-}
  repairIndex (m :. n) (i :. j) rBelow rOver =
    repairIndex m i rBelow rOver :. repairIndex n j rBelow rOver
  {-# INLINE [1] repairIndex #-}
  liftIndex f (i :. j) = (f i :. f j)
  {-# INLINE [1] liftIndex #-}
  liftIndex2 f (i0 :. j0) (i1 :. j1) = (f i0 i1 :. f j0 j1)
  {-# INLINE [1] liftIndex2 #-}
  iter (i :. j) (m :. n) !inc cond !accInit f =
    loop i (`cond` m) (+ inc) accInit $ \ !i' !acc0 ->
      loop j (`cond` n) (+ inc) acc0 $ \ !j' -> f (i' :. j')
  {-# INLINE iter #-}
  iterM (i :. j) (m :. n) !inc cond !accInit f =
    loopM i (`cond` m) (+ inc) accInit $ \ !i' !acc0 ->
      loopM j (`cond` n) (+ inc) acc0 $ \ !j' -> f (i' :. j')
  {-# INLINE iterM #-}
  iterM_ (i :. j) (m :. n) !inc cond f =
    loopM_ i (`cond` m) (+ inc) $ \ !i' ->
      loopM_ j (`cond` n) (+ inc) $ \ !j' -> f (i' :. j')
  {-# INLINE iterM_ #-}


instance {-# OVERLAPPING #-} Index (IxN 3) where
  rank _ = 3
  {-# INLINE [1] rank #-}
  zeroIndex = (0 :> 0 :. 0)
  {-# INLINE [1] zeroIndex #-}
  totalElem (m :> n :. o) = m * n * o
  {-# INLINE [1] totalElem #-}
  isSafeIndex (m :> n :. o) (i :> j :. k) =
    0 <= i && 0 <= j && 0 <= k && i < m && j < n && k < o
  {-# INLINE [1] isSafeIndex #-}
  toLinearIndex (_ :> n :. o) (i :> j :. k) = (n * i + j) * o + k
  {-# INLINE [1] toLinearIndex #-}
  fromLinearIndex (_ :> ix) k = let !(q, ixL) = fromLinearIndexAccRec ix k in q :> ixL
  {-# INLINE [1] fromLinearIndex #-}
  fromLinearIndexAcc = fromLinearIndexAccRec
  {-# INLINE [1] fromLinearIndexAcc #-}
  consDim = (:>)
  {-# INLINE [1] consDim #-}
  unconsDim (i :> ix) = (i, ix)
  {-# INLINE [1] unconsDim #-}
  snocDim (i :. j) k = i :> j :. k
  {-# INLINE [1] snocDim #-}
  unsnocDim (i :> j :. k) = (i :. j, k)
  {-# INLINE [1] unsnocDim #-}
  getIndex (i :> _ :. _) 3 = Just i
  getIndex (_ :> j :. _) 2 = Just j
  getIndex (_ :> _ :. k) 1 = Just k
  getIndex _             _ = Nothing
  {-# INLINE [1] getIndex #-}
  setIndex (_ :> j :. k) 3 i = Just (i :> j :. k)
  setIndex (i :> _ :. k) 2 j = Just (i :> j :. k)
  setIndex (i :> j :. _) 1 k = Just (i :> j :. k)
  setIndex _             _ _ = Nothing
  {-# INLINE [1] setIndex #-}
  dropIndex (_ :> j :. k) 3 = Just (j :. k)
  dropIndex (i :> _ :. k) 2 = Just (i :. k)
  dropIndex (i :> j :. _) 1 = Just (i :. j)
  dropIndex _             _ = Nothing
  {-# INLINE [1] dropIndex #-}
  repairIndex (m :> n :. l) (i :> j :. k) rBelow rOver =
    repairIndex m i rBelow rOver :>
    repairIndex n j rBelow rOver :.
    repairIndex l k rBelow rOver
  {-# INLINE [1] repairIndex #-}
  liftIndex f (i :> j :. k) = f i :> f j :. f k
  {-# INLINE [1] liftIndex #-}
  liftIndex2 f (i0 :> j0 :. k0) (i1 :> j1 :. k1) = f i0 i1 :> f j0 j1 :. f k0 k1
  {-# INLINE [1] liftIndex2 #-}
  iter (i :> j :. k) (m :> n :. o) !inc cond !accInit f =
    loop i (`cond` m) (+ inc) accInit $ \ !i' !acc0 ->
      loop j (`cond` n) (+ inc) acc0 $ \ !j' !acc1 ->
        loop k (`cond` o) (+ inc) acc1 $ \ !k' -> f (i' :> j' :. k')
  {-# INLINE iter #-}
  iterM (i :> j :. k) (m :> n :. o) !inc cond !accInit f =
    loopM i (`cond` m) (+ inc) accInit $ \ !i' !acc0 ->
      loopM j (`cond` n) (+ inc) acc0 $ \ !j' !acc1 ->
        loopM k (`cond` o) (+ inc) acc1 $ \ !k' -> f (i' :> j' :. k')
  {-# INLINE iterM #-}
  iterM_ = iterMRec_
  {-# INLINE iterM_ #-}


instance (4 <= n,
          KnownNat n,
          Index (Ix (n - 1)),
          Index (Ix ((n - 1) - 1)),
#if __GLASGOW_HASKELL__ < 800
          Rank (Ix ((n - 1) - 1)) ~ ((n - 1) - 1),
#endif
          IxN (n - 1) ~ Ix (n - 1)
          ) => Index (IxN n) where
  rank _ = fromInteger $ natVal (Proxy :: Proxy n)
  {-# INLINE [1] rank #-}
  zeroIndex = 0 :> (zeroIndex :: Ix (n - 1))
  {-# INLINE [1] zeroIndex #-}
  totalElem (i :> ix) = i * totalElem ix
  {-# INLINE [1] totalElem #-}
  isSafeIndex (n :> sz) (i :> ix) = 0 <= i && i < n && isSafeIndex sz ix
  {-# INLINE [1] isSafeIndex #-}
  toLinearIndex (n :> sz) (i :> ix) = toLinearIndex sz ix * n + i
  {-# INLINE [1] toLinearIndex #-}
  fromLinearIndex (_ :> ix) k = let !(q, ixL) = fromLinearIndexAccRec ix k in q :> ixL
  {-# INLINE [1] fromLinearIndex #-}
  fromLinearIndexAcc = fromLinearIndexAccRec
  {-# INLINE [1] fromLinearIndexAcc #-}
  consDim = (:>)
  {-# INLINE [1] consDim #-}
  unconsDim (i :> ix) = (i, ix)
  {-# INLINE [1] unconsDim #-}
  snocDim (i :> ix) k = i :> snocDim ix k
  {-# INLINE [1] snocDim #-}
  unsnocDim (i :> ix) = case unsnocDim ix of
                          (jx, j) -> (i :> jx, j)
  {-# INLINE [1] unsnocDim #-}
  getIndex ix@(j :> jx) k | k == rank ix = Just j
                          | otherwise = getIndex jx k
  {-# INLINE [1] getIndex #-}
  setIndex ix@(j :> jx) k o | k == rank ix = Just (o :> jx)
                            | otherwise = (j :>) <$> setIndex jx k o
  {-# INLINE [1] setIndex #-}
  dropIndex ix@(j :> jx) k | k == rank ix = Just jx
                           | otherwise = (j :>) <$> dropIndex jx k
  {-# INLINE [1] dropIndex #-}
  repairIndex (s :> sz) (i :> ix) rBelow rOver =
    repairIndex s i rBelow rOver :> repairIndex sz ix rBelow rOver
  {-# INLINE [1] repairIndex #-}
  liftIndex f (i :> ix) = f i :> liftIndex f ix
  {-# INLINE [1] liftIndex #-}
  liftIndex2 f (i1 :> ix1) (i2 :> ix2) = f i1 i2 :> liftIndex2 f ix1 ix2
  {-# INLINE [1] liftIndex2 #-}
  iter (k0 :> sIxL) (k1 :> eIxL) !inc cond !accInit f =
    loop k0 (`cond` k1) (+ inc) accInit $ \ !i !accI ->
      iter sIxL eIxL inc cond accI $ \ !ix -> f (i :> ix)
  {-# INLINE iter #-}
  iterM (k0 :> sIxL) (k1 :> eIxL) !inc cond !accInit f =
    loopM k0 (`cond` k1) (+ inc) accInit $ \ i accI ->
      iterM sIxL eIxL inc cond accI $ \ ix -> f (i :> ix)
  {-# INLINE iterM #-}
  iterM_ (k0 :> sIxL) (k1 :> eIxL) !inc cond f =
    loopM_ k0 (`cond` k1) (+ inc) $ \ !i ->
      iterM_ sIxL eIxL inc cond $ \ !ix -> f (i :> ix)
  {-# INLINE iterM_ #-}







-- A low level optimizations, that marginally (by 1%) imroves performance.
--{-# LANGUAGE UnboxedTuples    #-}
--{-# LANGUAGE MagicHash        #-}
-- import GHC.Prim
-- import GHC.Int

-- fromLinearIx :: Ix n -> Int -> Ix n
-- fromLinearIx (_ :. I# n#) (I# k#) =
--   case k# `quotRemInt#` n# of
--     (# i#, j# #) -> I# i# :. I# j#
-- fromLinearIx (_ :> ix) (I# k#) =
--   case fromLinearIxAcc# ix k# of
--     (# q#, ix' #) -> I# q# :> ix'
-- {-# INLINE [1] fromLinearIx #-}


-- fromLinearIxAcc :: Ix n -> Int -> (Int, Ix n)
-- fromLinearIxAcc ix (I# k#) = case fromLinearIxAcc# ix k# of
--                                (# q#, ix' #) -> (I# q#, ix')
-- {-# INLINE [1] fromLinearIxAcc #-}

-- fromLinearIxAcc# :: Ix n -> Int# -> (# Int#, Ix n #)
-- fromLinearIxAcc# (_ :. I# n#) k# =
--   case k# `quotRemInt#` n# of
--     (# q#, r# #) -> (# q#, I# q# :. I# r# #)
-- fromLinearIxAcc# (I# m# :> ix) k# =
--   case fromLinearIxAcc# ix k# of
--     (# kL#, ixL #) ->
--       case kL# `quotRemInt#` m# of
--         (# q#, r# #) -> (# q#, I# r# :> ixL #)
-- {-# INLINE [1] fromLinearIxAcc# #-}

