{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

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
import           Data.Proxy
import           GHC.TypeLits


infixr 5 :>, :.

data Ix (n :: Nat) where
  (:.) :: {-# UNPACK #-} !Int -> {-# UNPACK #-} !Int -> Ix 2
  (:>) :: {-# UNPACK #-} !Int -> !(Ix (n - 1)) -> Ix n

type family IxLower (n :: Nat) where
  IxLower 2 = Int
  IxLower n = Ix (n - 1)

type instance Lower (Ix n) = IxLower n

type Ix1 = Int
type Ix2 = Ix 2
type Ix3 = Ix 3
type Ix4 = Ix 4
type Ix5 = Ix 5

instance NFData (Ix n) where
  rnf ix = ix `seq` ()

instance Show (Ix n) where

  show (i :. j)  = show i ++ " x " ++ show j
  show (i :> ix) = show i ++ " x " ++ show ix

instance Eq (Ix n) where

  (i1 :. j1)  == (i2 :. j2) = i1 == i2 && j1 == j2
  (i1 :> ix1) == (i2 :> ix2) = i1 == i2 && ix1 == ix2
  _  == _ = False

instance Ord (Ix n) where

  (i1 :. j1)  <= (i2 :. j2) = i1 <= i2 && j1 <= j2
  (i1 :> ix1) <= (i2 :> ix2) = i1 <= i2 && ix1 <= ix2
  _ <= _ = False



instance Index (Ix 2) where
  rank _ = 2
  {-# INLINE [1] rank #-}
  zeroIndex = (0 :. 0)
  {-# INLINE [1] zeroIndex #-}
  totalElem (m :. n) = m * n
  totalElem _        = errorPattern "Ix 2.totalElem"
  {-# INLINE [1] totalElem #-}
  isSafeIndex (m :. n) (i :. j) = 0 <= i && 0 <= j && i < m && j < n
  isSafeIndex _        _        = errorPattern "Ix 2.isSafeIndex"
  {-# INLINE [1] isSafeIndex #-}
  toLinearIndex (_ :. n) (i :. j) = n * i + j
  toLinearIndex _        _        = errorPattern "Ix 2.toLinearIndex"
  {-# INLINE [1] toLinearIndex #-}
  fromLinearIndex (_ :. n) k = case k `quotRem` n of
                                 (i, j) -> i :. j
  fromLinearIndex _        _  = errorPattern "Ix 2.fromLinearIndex"
  {-# INLINE [1] fromLinearIndex #-}
  consDim = (:.)
  {-# INLINE [1] consDim #-}
  unconsDim (i :. ix) = (i, ix)
  unconsDim _         = errorPattern "Ix 2.unconsDim"
  {-# INLINE [1] unconsDim #-}
  snocDim i j = i :. j
  {-# INLINE [1] snocDim #-}
  unsnocDim (i :. j) = (i, j)
  unsnocDim _        = errorPattern "Ix 2.unsnocDim"
  {-# INLINE [1] unsnocDim #-}
  getIndex (i :. _) 1 = Just i
  getIndex (_ :. j) 2 = Just j
  getIndex _        _ = Nothing
  {-# INLINE [1] getIndex #-}
  setIndex (_ :. j) 1 i = Just (i :. j)
  setIndex (i :. _) 2 j = Just (i :. j)
  setIndex _        _ _ = Nothing
  {-# INLINE [1] setIndex #-}
  dropIndex (_ :. j) 1 = Just j
  dropIndex (i :. _) 2 = Just i
  dropIndex _      _   = Nothing
  {-# INLINE [1] dropIndex #-}
  repairIndex = repairIx
  {-# INLINE [1] repairIndex #-}
  liftIndex f (i :. j) = (f i :. f j)
  liftIndex _ _        = errorPattern "Ix 2.liftIndex"
  {-# INLINE [1] liftIndex #-}
  liftIndex2 f (i0 :. j0) (i1 :. j1) = (f i0 i1 :. f j0 j1)
  liftIndex2 _ _          _          = errorPattern "Ix 2.liftIndex2"
  {-# INLINE [1] liftIndex2 #-}
  iter (i :. j) (m :. n) !inc cond !accInit f =
    loop i (`cond` m) (+ inc) accInit $ \ !i' !acc0 ->
      loop j (`cond` n) (+ inc) acc0 $ \ !j' -> f (i' :. j')
  iter _        _        _   _    _       _ = errorPattern "Ix 2.iter"
  {-# INLINE iter #-}
  iterM (i :. j) (m :. n) !inc cond !accInit f =
    loopM i (`cond` m) (+ inc) accInit $ \ !i' !acc0 ->
      loopM j (`cond` n) (+ inc) acc0 $ \ !j' -> f (i' :. j')
  iterM _        _        _   _    _       _ = errorPattern "Ix 2.iterM"
  {-# INLINE iterM #-}
  iterM_ = iterMRec_
  {-# INLINE iterM_ #-}



instance Index (Ix 3) where
  rank _ = 3
  {-# INLINE [1] rank #-}
  zeroIndex = (0 :> 0 :. 0)
  {-# INLINE [1] zeroIndex #-}
  totalElem (m :> n :. o) = m * n * o
  totalElem _             = errorPattern "Ix 3.totalElem"
  {-# INLINE [1] totalElem #-}
  isSafeIndex (m :> n :. o) (i :> j :. k) =
    0 <= i && 0 <= j && 0 <= k && i < m && j < n && k < o
  isSafeIndex _        _        = errorPattern "Ix 3.isSafeIndex"
  {-# INLINE [1] isSafeIndex #-}
  toLinearIndex = toLinearIx
  {-# INLINE [1] toLinearIndex #-}
  fromLinearIndex = fromLinearIx
  {-# INLINE [1] fromLinearIndex #-}
  consDim = (:>)
  {-# INLINE [1] consDim #-}
  unconsDim (i :> ix) = (i, ix)
  {-# INLINE [1] unconsDim #-}
  snocDim (i :. j) k = i :> j :. k
  snocDim _        _ = errorPattern "Ix 3.snocDim"
  {-# INLINE [1] snocDim #-}
  unsnocDim (i :> j :. k) = (i :. j, k)
  unsnocDim _             = errorPattern "Ix 3.unsnocDim"
  {-# INLINE [1] unsnocDim #-}
  getIndex (i :> _ :. _) 1 = Just i
  getIndex (_ :> j :. _) 2 = Just j
  getIndex (_ :> _ :. k) 3 = Just k
  getIndex _             _ = Nothing
  {-# INLINE [1] getIndex #-}
  setIndex (_ :> j :. k) 1 i = Just (i :> j :. k)
  setIndex (i :> _ :. k) 2 j = Just (i :> j :. k)
  setIndex (i :> j :. _) 3 k = Just (i :> j :. k)
  setIndex _             _ _ = Nothing
  {-# INLINE [1] setIndex #-}
  dropIndex (_ :> j :. k) 1 = Just (j :. k)
  dropIndex (i :> _ :. k) 2 = Just (i :. k)
  dropIndex (i :> j :. _) 3 = Just (i :. j)
  dropIndex _             _ = Nothing
  {-# INLINE [1] dropIndex #-}
  repairIndex = repairIx
  {-# INLINE [1] repairIndex #-}
  liftIndex f (i :> j :. k) = f i :> f j :. f k
  liftIndex _ _             = errorPattern "Ix 3.liftIndex"
  {-# INLINE [1] liftIndex #-}
  liftIndex2 f (i0 :> j0 :. k0) (i1 :> j1 :. k1) = f i0 i1 :> f j0 j1 :. f k0 k1
  liftIndex2 _ _                _                = errorPattern "Ix 3.liftIndex2"
  {-# INLINE [1] liftIndex2 #-}
  iter (k0 :> sIxL) (k1 :> eIxL) !inc cond !accInit f =
    loop k0 (`cond` k1) (+ inc) accInit $ \ !i !accI ->
      iter sIxL eIxL inc cond accI $ \ !ix -> f (i :> ix)
  {-# INLINE iter #-}
  iterM (k0 :> sIxL) (k1 :> eIxL) !inc cond !accInit f =
    loopM k0 (`cond` k1) (+ inc) accInit $ \ !i !accI ->
      iterM sIxL eIxL inc cond accI $ \ !ix -> f (i :> ix)
  {-# INLINE iterM #-}
  iterM_ (k0 :> sIxL) (k1 :> eIxL) !inc cond f =
    loopM_ k0 (`cond` k1) (+ inc) $ \ !i ->
      iterM_ sIxL eIxL inc cond $ \ !ix -> f (i :> ix)
  {-# INLINE iterM_ #-}



instance (4 <= n,
          KnownNat n,
          Index (Ix (n - 1)),
          IxLower n ~ Ix (n - 1),
          IxLower (n - 1) ~ Ix ((n - 1) - 1)
          ) => Index (Ix n) where
  rank _ = fromInteger $ natVal (Proxy :: Proxy n)
  {-# INLINE [1] rank #-}
  zeroIndex = 0 :> (zeroIndex :: Ix (n - 1))
  {-# INLINE [1] zeroIndex #-}
  totalElem (i :> ix) = i * totalElem ix
  {-# INLINE [1] totalElem #-}
  isSafeIndex (n :> sz) (i :> ix) = 0 <= i && i < n && isSafeIndex sz ix
  {-# INLINE [1] isSafeIndex #-}
  toLinearIndex = toLinearIx
  {-# INLINE [1] toLinearIndex #-}
  fromLinearIndex = fromLinearIx
  {-# INLINE [1] fromLinearIndex #-}
  consDim = (:>)
  {-# INLINE [1] consDim #-}
  unconsDim (i :> ix) = (i, ix)
  {-# INLINE [1] unconsDim #-}
  snocDim = snocIx
  {-# INLINE [1] snocDim #-}
  unsnocDim = unsnocIx
  {-# INLINE [1] unsnocDim #-}
  getIndex (j :> jx) k | k == 1 = Just j
                       | otherwise = getIndex jx (k - 1)
  {-# INLINE [1] getIndex #-}
  setIndex (i :> ix) k j | k == 1 = Just (j :> ix)
                         | otherwise = (i :>) <$> setIndex ix (k - 1) j
  {-# INLINE [1] setIndex #-}
  dropIndex (j :> jx) k | k == 1 = Just jx
                        | otherwise = (j :>) <$> dropIndex jx (k - 1)
  {-# INLINE [1] dropIndex #-}
  repairIndex = repairIx
  {-# INLINE [1] repairIndex #-}
  liftIndex f (i :> ix) = f i :> liftIndex f ix
  {-# INLINE [1] liftIndex #-}
  liftIndex2 f (i1 :> ix1) (i2 :> ix2) = f i1 i2 :> liftIndex2 f ix1 ix2
  {-# INLINE [1] liftIndex2 #-}
  iter (k0 :> sIxL) (k1 :> eIxL) !inc cond !accInit f =
    loop k0 (`cond` k1) (+ inc) accInit $ \ i accI ->
      iter sIxL eIxL inc cond accI $ \ ix -> f (i :> ix)
  {-# INLINE iter #-}
  iterM (k0 :> sIxL) (k1 :> eIxL) !inc cond !accInit f =
    loopM k0 (`cond` k1) (+ inc) accInit $ \ i accI ->
      iterM sIxL eIxL inc cond accI $ \ ix -> f (i :> ix)
  {-# INLINE iterM #-}
  iterM_ (k0 :> sIxL) (k1 :> eIxL) !inc cond f =
    loopM_ k0 (`cond` k1) (+ inc) $ \ !i ->
      iterM_ sIxL eIxL inc cond $ \ !ix -> f (i :> ix)
  {-# INLINE iterM_ #-}



snocIx :: Ix (n - 1) -> Int -> Ix n
snocIx (i :. j) k  = i :> j :. k
snocIx (i :> ix) k = i :> snocIx ix k
{-# INLINE [1] snocIx #-}

unsnocIx :: Ix n -> (Ix (n - 1), Int)
unsnocIx (i :> j :. k) = (i :. j, k)
unsnocIx (i :> ix)     = case unsnocIx ix of
                           (jx, j) -> (i :> jx, j)
unsnocIx _             = errorPattern "unsnocIx"
{-# INLINE [1] unsnocIx #-}


toLinearIx :: Ix n -> Ix n -> Int
toLinearIx (_ :. n) (i :. j)             = n * i + j
toLinearIx (_ :> n :. o)  (i :> j :. k)  = (n * i + j) * o + k
toLinearIx (_ :> n :> sz) (i :> j :> ix) = toLinearIxAcc (i * n + j) sz ix
toLinearIx _               _             = errorPattern "toLinearIx"
{-# INLINE [1] toLinearIx #-}

toLinearIxAcc :: Int -> Ix n -> Ix n -> Int
toLinearIxAcc !acc (m :. n)  (i :. j)  = n * (acc * m + i) + j
toLinearIxAcc !acc (n :> sz) (i :> ix) = toLinearIxAcc (acc * n + i) sz ix
toLinearIxAcc _ _ _                    = errorPattern "toLinearIxAcc"
{-# INLINE [1] toLinearIxAcc #-}



fromLinearIx :: Ix n -> Int -> Ix n
fromLinearIx (_ :. n)  k = case k `quotRem` n of
                             (i, j) -> i :. j
fromLinearIx (_ :> ix) k = let !(q, ixL) = fromLinearIxAcc ix k in q :> ixL
{-# INLINE [1] fromLinearIx #-}


fromLinearIxAcc :: Ix n -> Int -> (Int, Ix n)
fromLinearIxAcc (_ :. n)  !k = case k `quotRem` n of
                                 (q, r) -> (q, q :. r)
fromLinearIxAcc (m :> ix) !k = (q, r :> ixL)
  where !(kL, ixL) = fromLinearIxAcc ix k
        !(q, r) = quotRem kL m
{-# INLINE [1] fromLinearIxAcc #-}


repairIx :: Ix n -> Ix n -> (Int -> Int -> Int) -> (Int -> Int -> Int) -> Ix n
repairIx (m :. n)  (i :. j)  rBelow rOver =
    repairIndex m i rBelow rOver :. repairIndex n j rBelow rOver
repairIx (s :> sz) (i :> ix) rBelow rOver =
    repairIndex s i rBelow rOver :> repairIx sz ix rBelow rOver
repairIx _         _         _      _     = errorPattern "toLinearIxAcc"
{-# INLINE [1] repairIx #-}


errorPattern :: String -> e
errorPattern fName = error $ fName ++ ": Impossible happened."
{-# NOINLINE errorPattern #-}



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


-- foldlIx :: (b -> Int -> b) -> b -> Ix n -> b
-- foldlIx f !acc (i :. j)  = f (f acc i) j
-- foldlIx f !acc (i :> ix) = foldlIx f (f acc i) ix
-- {-# INLINE foldlIx #-}


-- foldrIx :: (Int -> b -> b) -> b -> Ix n -> b
-- foldrIx f !acc (i :. j)  = f i (f j acc)
-- foldrIx f !acc (i :> ix) = f i (foldrIx f acc ix)
-- {-# INLINE foldrIx #-}
