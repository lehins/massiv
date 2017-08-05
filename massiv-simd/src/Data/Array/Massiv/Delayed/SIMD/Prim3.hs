{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UnboxedTuples             #-}
-- |
-- Module      : Data.Array.Massiv.Delayed.SIMD.Prim
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Delayed.SIMD.Prim3 where

import           Control.Monad.Primitive
import           Control.Monad.ST                     (runST)
import           Data.Array.Massiv.Common
-- import           Data.Array.Massiv.Manifest           (toVector')
import           Data.Array.Massiv.Manifest.Primitive
-- import           Data.Array.Massiv.Mutable            (Target)
import           Data.Monoid                          ((<>))
import           Data.Primitive                       (sizeOf)
import           Data.Primitive.ByteArray
import qualified Data.Vector.Primitive                as VP
import           GHC.Exts
import           GHC.Int
-- import           GHC.Prim
import           Data.Proxy

data V = V

data Step e = YieldV Int DoubleX2#
            | YieldE Int Double#
            | Done

data Vector e = Vector (Int -> Step e)


data instance Array V ix Double = VArray { vComp        :: !Comp
                                         , vSize        :: !ix
                                         , vStream      :: !(Vector Double)
                                         }





fromByteArray :: Index ix
              => Comp -> ix -> Int -> ByteArray -> Array V ix Double
fromByteArray c sz (I# o#) (ByteArray ba#) = VArray c sz (Vector vec)
  where
    !n = totalElem sz
    w = 2
    q = n - (n `rem` w)
    vec i@(I# i#) =
      case i < q of
        False ->
          case i < n of
            False -> Done
            _  -> YieldE (i + 1) (indexDoubleArray# ba# i#)
        _ -> YieldV (i + w) (indexDoubleArrayAsDoubleX2# ba# i#)
    {-# INLINE vec #-}
{-# INLINE fromByteArray #-}


delaySIMD :: (Index ix) => Array P ix Double -> Array V ix Double
delaySIMD (PArray c sz (VP.Vector offset _ ba)) = fromByteArray c sz offset ba
{-# INLINE delaySIMD #-}


mapVector :: (Double# -> Double#)
          -> (DoubleX2# -> DoubleX2#)
          -> Array V ix Double -> Array V ix Double
mapVector fE# fV# (VArray c sz (Vector next)) = VArray c sz $ Vector vec
  where
    vec i =
      case next i of
        YieldV i v# -> YieldV i (fV# v#)
        YieldE i e# -> YieldE i (fE# e#)
        Done          -> Done
    {-# INLINE vec #-}
{-# INLINE mapVector #-}


zipWithVector :: (Double# -> Double# -> Double#)
          -> (DoubleX2# -> DoubleX2# -> DoubleX2#)
          -> Array V ix Double -> Array V ix Double -> Array V ix Double
zipWithVector fE# fV# (VArray c1 sz1 (Vector next1)) (VArray c2 _ (Vector next2)) =
  VArray (c1 <> c2) sz1 $ Vector vec
  where vec i =
          case (# next1 i, next2 i #) of
            (# YieldV i' v1#, YieldV _ v2# #) -> YieldV i' (fV# v1# v2#)
            (# YieldE i' e1#, YieldE _ e2# #) -> YieldE i' (fE# e1# e2#)
            (# Done, Done #)                  -> Done
            _                                 -> error "Impossible"
        {-# INLINE vec #-}
{-# INLINE zipWithVector #-}

instance Index ix => Num (Array V ix Double) where
  (+) = zipWithVector (+##) plusDoubleX2#
  {-# INLINE (+) #-}
  (-) = zipWithVector (-##) minusDoubleX2#
  {-# INLINE (-) #-}
  (*) = zipWithVector (*##) timesDoubleX2#
  {-# INLINE (*) #-}
  -- negate = mapVector negate negateDoubleX2#
  -- {-# INLINE negate #-}
  -- abs = mapElem' abs
  -- {-# INLINE abs #-}
  -- signum = mapElem' signum
  -- {-# INLINE signum #-}
  -- fromInteger = singletonSIMD' . fromInteger
  -- {-# INLINE fromInteger #-}




computeSIMD :: Index ix =>
  Array V ix Double -> Array P ix Double
computeSIMD arr@(VArray c sz (Vector vec)) =
  runST $ do
    let !n = totalElem sz
    mba@(MutableByteArray mba#) <-
      newByteArray (n * sizeOf (undefined :: Double))
    let load i@(I# i#) s# =
          case vec i of
            YieldV i' v# ->
              case writeDoubleArrayAsDoubleX2# mba# i# v# s# of
                s'# -> load i' s'#
            YieldE i' e# ->
              case writeDoubleArray# mba# i# e# s# of
                s'# -> load i' s'#
            Done -> s#
    primitive_ (load 0)
    ba <- unsafeFreezeByteArray mba
    return $ PArray c sz $ VP.Vector 0 n ba
{-# INLINE computeSIMD #-}



sumSIMD :: Index ix =>
  Array V ix Double -> Double
sumSIMD arr@(VArray c sz (Vector vec)) =
  case loop 0 (broadcastDoubleX2# 0.0##) 0.0## of
    (# (# accV1#, accV2# #), accE# #) -> D# (accV1# +## accV2# +## accE#)
  where loop i@(I# i#) accV# accE# =
          case vec i of
            YieldV i' v# ->
              loop i' (plusDoubleX2# v# accV#) accE#
            YieldE i' e# ->
              loop i' accV# (e# +## accE#)
            Done -> (# unpackDoubleX2# accV#, accE# #)
{-# INLINE sumSIMD #-}
