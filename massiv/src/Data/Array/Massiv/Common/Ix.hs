{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
-- |
-- Module      : Data.Array.Massiv.Common.Ix
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Common.Ix where

import           Data.Proxy
import           GHC.TypeLits
-- import Data.Typeable
import           Control.DeepSeq

import           Data.Array.Massiv.Common.Index

infixr 5 :>, :.

data Ix (n :: Nat) where
        Z :: Ix 0
        (:>) :: !Int -> !(Ix (n - 1)) -> Ix n

pattern (:!) :: Int -> Int -> Ix 2
pattern (:!) i j = i :> j :> Z


pattern (:.) :: Int -> Ix (n - 1) -> Ix n
pattern (:.) i ix = (:>) i ix

type instance Lower (Ix 1) = ZeroDim
type instance Lower (Ix 2) = Int
type instance Lower (Ix 3) = Ix 2
type instance Lower (Ix 4) = Ix 3
type instance Lower (Ix 5) = Ix 4

type Ix2 = Ix 2

instance NFData (Ix n) where
  rnf ix = ix `seq` ()

instance Show (Ix n) where

  show Z         = "Z"
  show (i :> ix) = show i ++ " x " ++ show ix

instance Eq (Ix n) where

  Z == Z = True
  (i1 :> ix1) == (i2 :> ix2) = i1 == i2 && ix1 == ix2
  _  == _ = False

instance Ord (Ix n) where

  Z <= Z = True
  (i1 :> ix1) <= (i2 :> ix2) = i1 <= i2 && ix1 <= ix2
  _ <= _ = False


instance Index (Ix 1) where
  rank _ = 1
  {-# INLINE rank #-}
  zeroIndex = (0 :> Z)
  {-# INLINE zeroIndex #-}
  totalElem (k :> Z) = k
  {-# INLINE totalElem #-}
  isSafeIndex (k :> Z) (i :> Z) = 0 <= i && i < k
  {-# INLINE isSafeIndex #-}
  toLinearIndex _ (i :> Z) = i
  {-# INLINE toLinearIndex #-}
  fromLinearIndex _ i = i :> Z
  {-# INLINE fromLinearIndex #-}
  consDim i _ = (i :> Z)
  {-# INLINE consDim #-}
  unconsDim (i :> Z) = (i, ZeroDim)
  {-# INLINE unconsDim #-}
  snocDim _ i = i :> Z
  {-# INLINE snocDim #-}
  unsnocDim (i :> Z) = (ZeroDim, i)
  {-# INLINE unsnocDim #-}
  repairIndex (k :> Z) (i :> Z) rBelow rOver
    | i < 0 = rBelow k i :> Z
    | i >= k = rOver k i :> Z
    | otherwise = i :> Z
  {-# INLINE repairIndex #-}
  getIndex (i :> Z) 1 = Just i
  getIndex _ _        = Nothing
  {-# INLINE getIndex #-}
  setIndex _ 1 i = Just (i :> Z)
  setIndex _ _ _ = Nothing
  {-# INLINE setIndex #-}
  dropIndex _ 1 = Just ZeroDim
  dropIndex _ _ = Nothing
  {-# INLINE dropIndex #-}
  liftIndex f (i :> Z) = f i :> Z
  {-# INLINE liftIndex #-}
  liftIndex2 f (i1 :> Z) (i2 :> Z) = f i1 i2 :> Z
  {-# INLINE liftIndex2 #-}
  iter (k0 :> Z) (k1 :> Z) inc cond acc f =
    loop k0 (`cond` k1) (+inc) acc $ \ i a -> f (i :> Z) a
  {-# INLINE iter #-}
  iterM (k0 :> Z) (k1 :> Z) inc cond acc f =
    loopM k0 (`cond` k1) (+inc) acc $ \ i a -> f (i :> Z) a
  {-# INLINE iterM #-}
  iterM_ (k0 :> Z) (k1 :> Z) inc cond f =
    loopM_ k0 (`cond` k1) (+inc) $ \ i -> f (i :> Z)
  {-# INLINE iterM_ #-}


instance Index (Ix 2) where
  rank _ = 2
  {-# INLINE rank #-}
  zeroIndex = (0 :! 0)
  {-# INLINE zeroIndex #-}
  totalElem (m :> n :> Z) = m * n
  {-# INLINE totalElem #-}
  isSafeIndex (m :! n) (i :! j) = 0 <= i && 0 <= j && i < m && j < n
  {-# INLINE isSafeIndex #-}
  toLinearIndex (_ :! n) (i :! j) = n * i + j
  {-# INLINE toLinearIndex #-}
  fromLinearIndex (_ :! n) !k = let (i, j) = k `quotRem` n in (i :! j)
  {-# INLINE fromLinearIndex #-}
  consDim = (:!)
  {-# INLINE consDim #-}
  unconsDim (i :! j) = (i, j)
  {-# INLINE unconsDim #-}
  snocDim i j = i :! j
  {-# INLINE snocDim #-}
  unsnocDim (i :! j) = (i, j)
  {-# INLINE unsnocDim #-}
  getIndex (i :! _) 1 = Just i
  getIndex (_ :! j) 2 = Just j
  getIndex _      _   = Nothing
  {-# INLINE getIndex #-}
  setIndex (_ :! j) 1 i = Just (i :! j)
  setIndex (i :! _) 2 j = Just (i :! j)
  setIndex _      _ _   = Nothing
  {-# INLINE setIndex #-}
  dropIndex (_ :! j) 1 = Just j
  dropIndex (i :! _) 2 = Just i
  dropIndex _      _   = Nothing
  {-# INLINE dropIndex #-}
  repairIndex = repairIndexRec
  {-# INLINE repairIndex #-}
  liftIndex f (i :! j) = (f i :! f j)
  {-# INLINE liftIndex #-}
  liftIndex2 f (i0 :! j0) (i1 :! j1) = (f i0 i1 :! f j0 j1)
  {-# INLINE liftIndex2 #-}
  iter = iterRec
  {-# INLINE iter #-}
  iterM = iterMRec
  {-# INLINE iterM #-}
  iterM_ = iterMRec_
  {-# INLINE iterM_ #-}


foldlIx :: forall b n. (b -> Int -> b) -> b -> Ix n -> b
foldlIx f = go where
  go :: b -> Ix m -> b
  go acc Z         = acc
  go acc (i :> ix) = go (f acc i) ix


foldrIx :: forall b n. (Int -> b -> b) -> b -> Ix n -> b
foldrIx f = go where
  go :: b -> Ix m -> b
  go acc Z         = acc
  go acc (i :> ix) = f i (go acc ix)


unsnocIx :: Ix n -> (Ix (n-1), Int)
unsnocIx (i :> Z)  = (Z, i)
unsnocIx (i :> ix) = let !(ix', j) = unsnocIx ix in (i :> ix', j)

snocIx :: Ix (n - 1) -> Int -> Ix n
snocIx Z j         = j :> Z
snocIx (i :> ix) j = i :> snocIx ix j

dropLast :: Ix n -> Ix (n-1)
dropLast (_ :> Z)  = Z
dropLast (i :> ix) = i :> dropLast ix


-- generateIx :: KnownNat n => Proxy n -> (Integer -> Int) -> Ix n
-- generateIx proxyRank f = go 0 where
--   last = fromInteger $ natVal proxyRank
--   go n | n == last = Z
--        | otherwise = f n :> go (n+1)


-- rev :: forall n. Ix n -> Ix n
-- rev ix = go Z ix where
--   go :: Ix l -> Ix (m-l) -> Ix m
--   go acc Z = acc
--   go acc (i :> ix) = go (i :> acc) ix

instance (Index (Ix (n - 1)), Lower (Ix n) ~ Ix (n-1), 3 <= n, KnownNat n) => Index (Ix n) where
  rank _ = fromInteger $ natVal (Proxy :: Proxy n)
  {-# INLINE rank #-}
  zeroIndex = 0 :> (zeroIndex :: Ix (n - 1))
  {-# INLINE zeroIndex #-}
  totalElem = foldlIx (*) 1
  {-# INLINE totalElem #-}
  isSafeIndex = isSafeIndexRec
  {-# INLINE isSafeIndex #-}
  toLinearIndex = toLinearIndexRec
  {-# INLINE toLinearIndex #-}
  fromLinearIndex = fromLinearIndexRec
  {-# INLINE fromLinearIndex #-}
  consDim = (:>)
  {-# INLINE consDim #-}
  unconsDim (i :> ix) = (i, ix)
  {-# INLINE unconsDim #-}
  snocDim = snocIx
  {-# INLINE snocDim #-}
  unsnocDim = unsnocIx
  {-# INLINE unsnocDim #-}
  getIndex = getIxN
  {-# INLINE getIndex #-}
  setIndex = setIxN
  {-# INLINE setIndex #-}
  dropIndex = dropIxN
  {-# INLINE dropIndex #-}
  repairIndex = repairIndexRec
  {-# INLINE repairIndex #-}
  liftIndex f (i :> ix) = f i :> liftIndex f ix
  {-# INLINE liftIndex #-}
  liftIndex2 f (i1 :> ix1) (i2 :> ix2) = f i1 i2 :> liftIndex2 f ix1 ix2
  {-# INLINE liftIndex2 #-}
  iter = iterRec
  {-# INLINE iter #-}
  iterM = iterMRec
  {-# INLINE iterM #-}
  iterM_ = iterMRec_
  {-# INLINE iterM_ #-}


-- dropIx :: forall i n. (KnownNat n, KnownNat i, 1 <= i, i <= n) => Ix n -> Proxy i -> Ix (n - 1)
-- dropIx ix = fromJust . dropIxN ix . fromInteger . natVal


getIxN :: Ix n -> Int -> Maybe Int
getIxN Z _ = Nothing
getIxN (j :> jx) k | k == 1 = Just j
                    | otherwise = getIxN jx (k - 1)


setIxN :: Ix n -> Int -> Int -> Maybe (Ix n)
setIxN Z _ _ = Nothing
setIxN (i :> ix) k j | k == 1 = Just (j :> ix)
                     | otherwise = (i :>) <$> setIxN ix (k - 1) j

dropIxN :: Ix n -> Int -> Maybe (Ix (n - 1))
dropIxN Z _ = Nothing
dropIxN (j :> jx) k | k == 1 = Just jx
                    | otherwise = (j :>) <$> dropIxN jx (k - 1)
