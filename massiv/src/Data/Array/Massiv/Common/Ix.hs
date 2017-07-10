-- {-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
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

infixr 5 :> --, :.

data Ix (n :: Nat) where
  Z :: Ix 0
  (:>) :: {-# UNPACK #-} !Int -> !(Ix (n - 1)) -> Ix n

pattern (:.) :: Int -> Int -> Ix 2
pattern (:.) i j = i :> j :> Z

-- pattern (:.) :: Int -> Ix (n - 1) -> Ix n
-- pattern (:.) i ix = (:>) i ix

type instance Lower (Ix n) = Ix (n - 1)

type Ix1 = Ix 1
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
  unconsDim (i :> Z) = (i, Z)
  {-# INLINE unconsDim #-}
  snocDim _ i = i :> Z
  {-# INLINE snocDim #-}
  unsnocDim (i :> Z) = (Z, i)
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
  dropIndex _ 1 = Just Z
  dropIndex _ _ = Nothing
  {-# INLINE dropIndex #-}
  liftIndex f (i :> Z) = f i :> Z
  {-# INLINE liftIndex #-}
  liftIndex2 f (i1 :> Z) (i2 :> Z) = f i1 i2 :> Z
  {-# INLINE liftIndex2 #-}
  iter (k0 :> Z) (k1 :> Z) inc cond acc f =
    loop k0 (`cond` k1) (+inc) acc $ \ i a -> f (i :> Z) a
  {-# INLINE iter #-}
  iterM (k0 :> Z) (k1 :> Z) !inc cond !acc f =
    loopM k0 (`cond` k1) (+inc) acc $ \ !i !a -> f (i :> Z) a
  {-# INLINE iterM #-}
  iterM_ (k0 :> Z) (k1 :> Z) inc cond f =
    loopM_ k0 (`cond` k1) (+inc) $ \ i -> f (i :> Z)
  {-# INLINE iterM_ #-}


toLinearIx :: Ix n -> Ix n -> Int
toLinearIx (_ :> Z) (i :> Z)               = i
toLinearIx (_ :> k :> sz') (p :> q :> ix') = toLinearIxAcc (p * k + q) sz' ix'
toLinearIx _ _                             = 0
{-# INLINE toLinearIx #-}

toLinearIxAcc :: Int -> Ix n -> Ix n -> Int
toLinearIxAcc !acc Z Z                 = acc
toLinearIxAcc !acc (n :> sz) (i :> ix) = toLinearIxAcc (acc * n + i) sz ix
toLinearIxAcc _ _ _                    = 0
{-# INLINE toLinearIxAcc #-}

-- fromLinearIndexRec :: (Index (Lower ix), Index ix) =>
--                       ix -> Int -> ix
-- fromLinearIndexRec !sz !k = snocDim (fromLinearIndex szL kL) j
--   where !(kL, j) = quotRem k n
--         !(szL, n) = unsnocDim sz
-- {-# INLINE fromLinearIndexRec #-}


-- fromLinearIxAcc :: Ix n -> Int -> ((Ix (n - k) -> Ix n), Int)
-- fromLinearIxAcc (_ :> n :> Z) k = let !(i, j) = quotRem k n in ((i :>), j)
-- fromLinearIxAcc (n :> ix) k = (ixF . (i :>), kL)
--   where !(ixF, kL) = fromLinearIxAcc ix k
--         !(i, j) = quotRem kL n


-- fromLinearIx Z _ = Z
-- fromLinearIx (_ :> Z) k = k :> Z
fromLinearIxAcc :: Ix n -> Int -> (Int, Ix n)
fromLinearIxAcc (n :> Z) !k = (q, r :> Z)
  where !(q, r) = quotRem k n
fromLinearIxAcc (m :> ix) !k = (q, r :> ixL)
  where !(kL, ixL) = fromLinearIxAcc ix k
        !(q, r) = quotRem kL m
{-# INLINE fromLinearIxAcc #-}

fromLinearIx :: Ix n -> Int -> Ix n
fromLinearIx (_ :> ix) k = let !(q, ixL) = fromLinearIxAcc ix k in q :> ixL
{-# INLINE fromLinearIx #-}


-- instance Index (Ix 2) where
--   rank _ = 2
--   {-# INLINE rank #-}
--   zeroIndex = (0 :. 0)
--   {-# INLINE zeroIndex #-}
--   totalElem (m :> n :> Z) = m * n
--   {-# INLINE totalElem #-}
--   isSafeIndex (m :. n) (i :. j) = 0 <= i && 0 <= j && i < m && j < n
--   {-# INLINE isSafeIndex #-}
--   toLinearIndex (_ :. n) (i :. j) = n * i + j
--   {-# INLINE toLinearIndex #-}
--   fromLinearIndex (_ :. n) !k = let (i, j) = k `quotRem` n in (i :. j)
--   {-# INLINE fromLinearIndex #-}
--   consDim = (:.)
--   {-# INLINE consDim #-}
--   unconsDim (i :. j) = (i, j)
--   {-# INLINE unconsDim #-}
--   snocDim i j = i :. j
--   {-# INLINE snocDim #-}
--   unsnocDim (i :. j) = (i, j)
--   {-# INLINE unsnocDim #-}
--   getIndex (i :. _) 1 = Just i
--   getIndex (_ :. j) 2 = Just j
--   getIndex _      _   = Nothing
--   {-# INLINE getIndex #-}
--   setIndex (_ :. j) 1 i = Just (i :. j)
--   setIndex (i :. _) 2 j = Just (i :. j)
--   setIndex _      _ _   = Nothing
--   {-# INLINE setIndex #-}
--   dropIndex (_ :. j) 1 = Just j
--   dropIndex (i :. _) 2 = Just i
--   dropIndex _      _   = Nothing
--   {-# INLINE dropIndex #-}
--   repairIndex = repairIndexRec
--   {-# INLINE repairIndex #-}
--   liftIndex f (i :. j) = (f i :. f j)
--   {-# INLINE liftIndex #-}
--   liftIndex2 f (i0 :. j0) (i1 :. j1) = (f i0 i1 :. f j0 j1)
--   {-# INLINE liftIndex2 #-}
--   iter = iterRec
--   {-# INLINE iter #-}
--   iterM = iterMRec
--   {-# INLINE iterM #-}
--   iterM_ = iterMRec_
--   {-# INLINE iterM_ #-}


instance Index (Ix 2) where
  rank _ = 2
  {-# INLINE rank #-}
  zeroIndex = (0 :. 0)
  {-# INLINE zeroIndex #-}
  totalElem (m :. n) = m * n
  {-# INLINE totalElem #-}
  isSafeIndex (m :. n) (i :. j) = 0 <= i && 0 <= j && i < m && j < n
  {-# INLINE isSafeIndex #-}
  toLinearIndex (_ :. n) (i :. j) = n * i + j
  {-# INLINE [1] toLinearIndex #-}
  fromLinearIndex (_ :. n) !k = let (i, j) = k `quotRem` n in (i :. j)
  {-# INLINE fromLinearIndex #-}
  consDim = (:>)
  {-# INLINE consDim #-}
  unconsDim (i :> ix) = (i, ix)
  {-# INLINE unconsDim #-}
  snocDim (i :> Z) j = i :> j :> Z
  {-# INLINE snocDim #-}
  unsnocDim (i :. j) = (i :> Z, j)
  {-# INLINE unsnocDim #-}
  getIndex (i :. _) 1 = Just i
  getIndex (_ :. j) 2 = Just j
  getIndex _        _ = Nothing
  {-# INLINE getIndex #-}
  setIndex (_ :. j) 1 i = Just (i :. j)
  setIndex (i :. _) 2 j = Just (i :. j)
  setIndex _        _ _ = Nothing
  {-# INLINE setIndex #-}
  dropIndex (_ :. j) 1 = Just (j :> Z)
  dropIndex (i :. _) 2 = Just (i :> Z)
  dropIndex _      _   = Nothing
  {-# INLINE dropIndex #-}
  repairIndex = repairIndexRec
  {-# INLINE repairIndex #-}
  liftIndex f (i :. j) = (f i :. f j)
  {-# INLINE liftIndex #-}
  liftIndex2 f (i0 :. j0) (i1 :. j1) = (f i0 i1 :. f j0 j1)
  {-# INLINE liftIndex2 #-}
  iter = iterRec
  {-# INLINE iter #-}
  -- iterM (k0 :> sIxL) (k1 :> eIxL) !inc cond !accInit f = do
  --   loopM k0 (`cond` k1) (+ inc) accInit $ \ !i !accI ->
  --     iterM sIxL eIxL inc cond accI $ \ !ix ->
  --       f (i :> ix)
  iterM (i :. j) (m :. n) inc cond accInit f = do
    loopM i (`cond` m) (+ inc) accInit $ \ i' acc0 ->
      loopM j (`cond` n) (+ inc) acc0 $ \ j' -> f (i' :. j')
  {-# INLINE iterM #-}
  -- iterM = iterMRec
  -- {-# INLINE iterM #-}
  iterM_ = iterMRec_
  {-# INLINE iterM_ #-}



instance Index (Ix 3) where
  rank _ = 3
  {-# INLINE rank #-}
  zeroIndex = (0 :> 0 :. 0)
  {-# INLINE zeroIndex #-}
  totalElem (l :> m :. n) = l * m * n
  {-# INLINE totalElem #-}
  isSafeIndex (l :> m :. n) (h :> i :. j) = 0 <= h && 0 <= i && 0 <= j && h < l && i < m && j < n
  {-# INLINE isSafeIndex #-}
  toLinearIndex (_ :> n :. o) (i :> j :. k) = (n * i + j) * o + k
  {-# INLINE toLinearIndex #-}
  fromLinearIndex (_ :> n :. o) !l = (i :> j :. k)
    where !(h, k) = quotRem l o
          !(i, j) = quotRem h n
  {-# INLINE fromLinearIndex #-}
  consDim = (:>)
  {-# INLINE consDim #-}
  unconsDim (i :> ix) = (i, ix)
  {-# INLINE unconsDim #-}
  snocDim (i :. j) k = i :> j :. k
  {-# INLINE snocDim #-}
  unsnocDim (i :> j :. k) = (i :. j, k)
  {-# INLINE unsnocDim #-}
  getIndex (i :> _) 1           = Just i
  getIndex (_ :> j :> _) 2      = Just j
  getIndex (_ :> _ :> k :> Z) 3 = Just k
  getIndex _                  _ = Nothing
  {-# INLINE getIndex #-}
  setIndex (_ :> ix)      1 i = Just (i :> ix)
  setIndex (i :> _ :> ix) 2 j = Just (i :> j :> ix)
  setIndex (i :> j :> _)  3 k = Just (i :> j :. k)
  setIndex _      _ _         = Nothing
  {-# INLINE setIndex #-}
  dropIndex (_ :> ix) 1      = Just ix
  dropIndex (i :> _ :> ix) 2 = Just (i :> ix)
  dropIndex (i :> j :> _) 3  = Just (i :. j)
  dropIndex _      _         = Nothing
  {-# INLINE dropIndex #-}
  repairIndex = repairIndexRec
  {-# INLINE repairIndex #-}
  liftIndex f (i :> j :. k) = f i :> f j :. f k
  liftIndex _ _             = errorPattern "Ix 3.liftIndex"
  {-# INLINE liftIndex #-}
  liftIndex2 f (i0 :> j0 :. k0) (i1 :> j1 :. k1) = (f i0 i1 :> f j0 j1 :. f k0 k1)
  liftIndex2 _ _ _ = errorPattern "Ix 3.liftIndex2"
  {-# INLINE liftIndex2 #-}
  iter = iterRec
  {-# INLINE iter #-}
  iterM (i :> j :. k) (l :> m :. n) inc cond accInit f = do
    loopM i (`cond` l) (+ inc) accInit $ \ i' acc0 ->
      loopM j (`cond` m) (+ inc) acc0 $ \ j' acc1 ->
        loopM k (`cond` n) (+ inc) acc1 $ \ k' -> f (i' :> j' :. k')
  {-# INLINE iterM #-}
  -- iterM (k0 :> sIxL) (k1 :> eIxL) !inc cond !accInit f = do
  --   loopM k0 (`cond` k1) (+ inc) accInit $ \ !i !accI ->
  --     iterM sIxL eIxL inc cond accI $ \ !ix ->
  --       f (i :> ix)
  -- {-# INLINE iterM #-}
  -- iterM = iterMRec
  -- {-# INLINE iterM #-}
  iterM_ = iterMRec_
  {-# INLINE iterM_ #-}

errorPattern :: String -> e
errorPattern fName = error $ fName ++ ": Impossible happened."
{-# NOINLINE errorPattern #-}


foldlIx :: (b -> Int -> b) -> b -> Ix n -> b
foldlIx _ !acc Z         = acc
foldlIx f !acc (i :> ix) = foldlIx f (f acc i) ix
{-# INLINE foldlIx #-}


-- foldrIx :: forall b n. (Int -> b -> b) -> b -> Ix n -> b
-- foldrIx f = go where
--   go :: b -> Ix m -> b
--   go !acc Z         = acc
--   go !acc (i :> ix) = f i (go acc ix)
-- {-# INLINE foldrIx #-}

unsnocIx :: Ix n -> (Ix (n-1), Int)
unsnocIx Z = error "Impossible happened: Zero dimension index, cannot unsnoc"
unsnocIx (i :> Z)  = (Z, i)
unsnocIx (i :> ix) = let !(jx, j) = unsnocIx ix in (i :> jx, j)
{-# INLINE unsnocIx #-}


snocIx :: Ix (n - 1) -> Int -> Ix n
snocIx Z j         = j :> Z
snocIx (i :> ix) j = i :> snocIx ix j
{-# INLINE snocIx #-}


-- instance (Index (Ix (n - 1)), 2 <= n, KnownNat n) => Index (Ix n) where
--   rank _ = fromInteger $ natVal (Proxy :: Proxy n)
--   {-# INLINE rank #-}
--   zeroIndex = 0 :> (zeroIndex :: Ix (n - 1))
--   {-# INLINE zeroIndex #-}
--   totalElem = foldlIx (*) 1
--   {-# INLINE totalElem #-}
--   isSafeIndex = isSafeIndexRec
--   {-# INLINE isSafeIndex #-}
--   toLinearIndex = toLinearIndexRec
--   {-# INLINE toLinearIndex #-}
--   fromLinearIndex = fromLinearIndexRec
--   {-# INLINE fromLinearIndex #-}
--   consDim = (:>)
--   {-# INLINE consDim #-}
--   unconsDim (i :> ix) = (i, ix)
--   {-# INLINE unconsDim #-}
--   snocDim = snocIx
--   {-# INLINE snocDim #-}
--   unsnocDim = unsnocIx
--   {-# INLINE unsnocDim #-}
--   getIndex = getIxN
--   {-# INLINE getIndex #-}
--   setIndex = setIxN
--   {-# INLINE setIndex #-}
--   dropIndex = dropIxN
--   {-# INLINE dropIndex #-}
--   repairIndex = repairIndexRec
--   {-# INLINE repairIndex #-}
--   liftIndex f (i :> ix) = f i :> liftIndex f ix
--   {-# INLINE liftIndex #-}
--   liftIndex2 f (i1 :> ix1) (i2 :> ix2) = f i1 i2 :> liftIndex2 f ix1 ix2
--   {-# INLINE liftIndex2 #-}
--   iter = iterRec
--   {-# INLINE iter #-}
--   iterM = iterMRec
--   {-# INLINE iterM #-}
--   iterM_ = iterMRec_
--   {-# INLINE iterM_ #-}


-- dropIx :: forall i n. (KnownNat n, KnownNat i, 1 <= i, i <= n) => Ix n -> Proxy i -> Ix (n - 1)
-- dropIx ix = fromJust . dropIxN ix . fromInteger . natVal


instance (Index (Ix (n - 1)), 4 <= n, KnownNat n) => Index (Ix n) where
  rank _ = fromInteger $ natVal (Proxy :: Proxy n)
  {-# INLINE rank #-}
  zeroIndex = 0 :> (zeroIndex :: Ix (n - 1))
  {-# INLINE zeroIndex #-}
  totalElem (i :> ix) = i * totalElem ix
  {-# INLINE totalElem #-}
  isSafeIndex (n :> sz) (i :> ix) = 0 <= i && i < n && isSafeIndex sz ix
  -- isSafeIndex sz ix = isSafeIndexRec sz ix
  {-# INLINE isSafeIndex #-}
  --toLinearIndex (_ :> k :> sz') (p :> q :> ix') = toLinearIxAcc (p * k + q) sz' ix'
  --toLinearIndex (_ :> n :> Z) (i :> j :> Z) = n * i + j
  --toLinearIndex sz ix = toLinearIndexRec sz ix
  --toLinearIndex (_ :> n :> Z) (i :> j :> Z) = n * i + j
  toLinearIndex sz ix = toLinearIx sz ix
  {-# INLINE toLinearIndex #-}
  --fromLinearIndex sz ix = fromLinearIndexRec sz ix
  --fromLinearIndex (_ :> n :> Z) !k = let (i, j) = k `quotRem` n in (i :> j :> Z)
  fromLinearIndex sz ix = fromLinearIx sz ix
  {-# INLINE fromLinearIndex #-}
  consDim = (:>)
  {-# INLINE consDim #-}
  unconsDim (i :> ix) = (i, ix)
  {-# INLINE unconsDim #-}
  snocDim = snocIx
  {-# INLINE snocDim #-}
  unsnocDim = unsnocIx
  {-# INLINE unsnocDim #-}
  -- getIndex (i :> _ :> Z) 1 = Just i
  -- getIndex (_ :> j :> Z) 2 = Just j
  getIndex (j :> jx) k | k == 1 = Just j
                       | otherwise = getIndex jx (k - 1)
  {-# INLINE getIndex #-}
  -- setIndex (_ :> j :> Z) 1 i = Just (i :> j :> Z)
  -- setIndex (i :> _ :> Z) 2 j = Just (i :> j :> Z)
  setIndex (i :> ix) k j | k == 1 = Just (j :> ix)
                         | otherwise = (i :>) <$> setIndex ix (k - 1) j
  {-# INLINE setIndex #-}
  -- dropIndex (_ :> j :> Z) 1 = Just (j :> Z)
  -- dropIndex (i :> _ :> Z) 2 = Just (i :> Z)
  dropIndex (j :> jx) k | k == 1 = Just jx
                        | otherwise = (j :>) <$> dropIndex jx (k - 1)
  {-# INLINE dropIndex #-}
  -- repairIndex = repairIndexRec
  -- {-# INLINE repairIndex #-}
  liftIndex f (i :> ix) = f i :> liftIndex f ix
  {-# INLINE liftIndex #-}
  liftIndex2 f (i1 :> ix1) (i2 :> ix2) = f i1 i2 :> liftIndex2 f ix1 ix2
  {-# INLINE liftIndex2 #-}
  -- iter = iterRec
  -- {-# INLINE iter #-}
  -- iterM = iterMRec
  -- {-# INLINE iterM #-}
  --iterM_ = iterIxM_ -- iterMRec_
  iterM (k0 :> sIxL) (k1 :> eIxL) !inc cond !accInit f = do
    loopM k0 (`cond` k1) (+ inc) accInit $ \ !i !accI ->
      iterM sIxL eIxL inc cond accI $ \ !ix ->
        f (i :> ix)
  {-# INLINE iterM #-}
  iterM_ (k0 :> sIxL) (k1 :> eIxL) !inc cond f = do
    loopM_ k0 (`cond` k1) (+ inc) $ \ !i ->
      iterM_ sIxL eIxL inc cond $ \ !ix ->
        f (i :> ix)
  {-# INLINE iterM_ #-}



getIxN :: Ix n -> Int -> Maybe Int
getIxN Z _ = Nothing
getIxN (j :> jx) k | k == 1 = Just j
                   | otherwise = getIxN jx (k - 1)
{-# INLINE getIxN #-}


setIxN :: Ix n -> Int -> Int -> Maybe (Ix n)
setIxN Z _ _ = Nothing
setIxN (i :> ix) k j | k == 1 = Just (j :> ix)
                     | otherwise = (i :>) <$> setIxN ix (k - 1) j
{-# INLINE setIxN #-}

dropIxN :: Ix n -> Int -> Maybe (Ix (n - 1))
dropIxN Z _ = Nothing
dropIxN (j :> jx) k | k == 1 = Just jx
                    | otherwise = (j :>) <$> dropIxN jx (k - 1)
{-# INLINE dropIxN #-}
