{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnboxedTuples         #-}
-- |
-- Module      : Data.Array.Massiv.Delayed.SIMD.Prim
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Delayed.SIMD.Prim where

import           Control.Monad.Primitive
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Mutable (Target)
import           Data.Array.Massiv.Manifest (P, fromVector', toVector')
import           Data.Monoid                ((<>))
import           Data.Primitive.ByteArray
import           Data.Primitive ( sizeOf )
import           Data.Vector.Primitive      as VP
import           GHC.Int
import           GHC.Prim


data VP = VP

type family IxElem r e :: *
type family MapElem r e :: *
type family ZipWithElem r e :: *

type family IxVector r e :: *
type family MapVector r e :: *
type family ZipWithVector r e :: *


data instance Array VP ix e = VPArray { vpComp        :: !Comp
                                      , vpSize        :: !ix
                                      , vpIndexElem   :: IxElem VP e
                                      , vpIndexVector :: IxVector VP e
                                      }


type instance IxElem VP Int32 = Int# -> Int#
type instance MapElem VP Int32 = Int# -> Int#
type instance ZipWithElem VP Int32 = Int# -> Int# -> Int#

type instance IxVector VP Int32 = Int# -> Int32X4#
type instance MapVector VP Int32 = Int32X4# -> Int32X4#
type instance ZipWithVector VP Int32 = Int32X4# -> Int32X4# -> Int32X4#



class SIMD r ix e where

  singletonSIMD :: e -> Array r ix e

  mapElem :: MapElem r e -> Array r ix e -> Array r ix e

  mapVector :: MapElem r e -> MapVector r e -> Array r ix e -> Array r ix e

  zipWithVector :: ZipWithElem r e -> ZipWithVector r e -> Array r ix e -> Array r ix e -> Array r ix e



instance Index ix => SIMD VP ix Int32 where
  singletonSIMD (I32# i) =
    VPArray Seq oneIndex (\_ -> i) (\_ -> broadcastInt32X4# i)
  {-# INLINE singletonSIMD #-}

  mapElem fE (VPArray c sz ixE ixV) =
    VPArray c sz (\i -> fE (ixE i)) (\i -> applyEach (unpackInt32X4# (ixV i)))
    where applyEach (# x0#, x1#, x2#, x3# #) =
            packInt32X4# (# fE x0#, fE x1#, fE x2#, fE x3# #)
          {-# INLINE applyEach #-}
  {-# INLINE mapElem #-}

  mapVector fE fV (VPArray c sz ixE ixV) =
    VPArray c sz (\i -> fE (ixE i)) (\i -> fV (ixV i))
  {-# INLINE mapVector #-}

  zipWithVector fE fV (VPArray c1 sz1 ixE1 ixV1) (VPArray c2 _ ixE2 ixV2) =
    VPArray (c1 <> c2) sz1 (\i -> fE (ixE1 i) (ixE2 i)) (\i -> fV (ixV1 i) (ixV2 i))
  {-# INLINE zipWithVector #-}





instance Index ix =>  Num (Array VP ix Int32) where
  (+) = zipWithVector (\x# y# -> narrow32Int# (x# +# y#)) plusInt32X4#
  {-# INLINE (+) #-}
  (-) = zipWithVector (\x# y# -> narrow32Int# (x# -# y#)) minusInt32X4#
  {-# INLINE (-) #-}
  (*) = zipWithVector (\x# y# -> narrow32Int# (x# *# y#)) timesInt32X4#
  {-# INLINE (*) #-}
  negate = mapVector (\x# -> negateInt# (narrow32Int# x#)) negateInt32X4#
  {-# INLINE negate #-}
  abs =
    mapElem
      (\x# ->
         case x# <# 0# of
           0# -> negateInt# (narrow32Int# x#)
           _  -> x#)
  {-# INLINE abs #-}
  signum =
    mapElem (\x# -> case x# of
                0# -> 0#
                _ -> case x# <# 0# of
                         0# -> negateInt# (narrow32Int# x#)
                         _  -> x#)
  {-# INLINE signum #-}
  fromInteger = singletonSIMD . fromInteger
  {-# INLINE fromInteger #-}

delaySIMD32 :: Index ix => Array P ix Int32  -> Array VP ix Int32
delaySIMD32 arr =
  VPArray
    (getComp arr)
    (size arr)
    (\i# -> indexInt32Array# ba# i#)
    (\i# -> indexInt32ArrayAsInt32X4# ba# i#)
  -- TODO: handle sliced
  where
    !(VP.Vector _ _ (ByteArray ba#)) = toVector' arr
{-# INLINE delaySIMD32 #-}



computeInt32 :: (PrimMonad m, Index ix) => Array VP ix Int32 -> m (Array P ix Int32)
computeInt32 (VPArray _ sz ixElem ixVec) = do
  let !n@(I# n#) = totalElem sz
      q# = n# -# (n# `remInt#` 4#)
  mba@(MutableByteArray mba#) <- newByteArray (n * sizeOf (undefined :: Int32))
  let loadVector# i# s# =
        case i# <# q# of
          0# -> s#
          _  -> loadVector# (i# +# 4#) (writeInt32ArrayAsInt32X4# mba# i# (ixVec i#) s#)
  let loadSlack# i# s# =
        case i# <# n# of
          0# -> s#
          _  -> loadSlack# (i# +# 1#) (writeInt32Array# mba# i# (ixElem i#) s#)
  primitive_ $ \ s# -> loadSlack# q# (loadVector# 0# s#)
  ba <- unsafeFreezeByteArray mba
  return $ fromVector' sz $ VP.Vector 0 n ba
{-# INLINE computeInt32 #-}


-- computeInt32 :: (PrimMonad m, Index ix) => Array VP ix Int32 -> m (Array P ix Int32)
-- computeInt32 (VPArray _ sz ixElem ixVec) = do
--   let !n@(I# n#) = totalElem sz
--       q# = n# -# (n# `remInt#` 4#)
--   let loadVector# arr# i# s# =
--         case i# <# q# of
--           0# -> (# s#, arr# #)
--           _  -> loadVector# arr# (i# +# 4#) (writeInt32ArrayAsInt32X4# arr# i# (ixVec i#) s#)
--   let loadSlack# arr# i# s# =
--         case i# <# n# of
--           0# -> (# s#, arr# #)
--           _  -> loadSlack# arr# (i# +# 1#) (writeInt32Array# arr# i# (ixElem i#) s#)
--   ba <- primitive $ \ s# -> let (# s1#, mba1# #) = newByteArray# n# s#
--                                 (# s2#, mba2# #) = loadVector# mba1# 0# s1#
--                                 (# s3#, mba3# #) = loadSlack# mba2# q# s2#
--                             in case unsafeFreezeByteArray# mba3# s3# of
--                                    (# s'#, arr'# #) -> (# s'#, ByteArray arr'# #)
--                              --in unsafeFreezeByteArray# mba3# s3#
--   return $ fromVector' sz $ VP.Vector 0 n ba
-- {-# INLINE computeInt32 #-}




-- sumByteArrayInt32X4 :: ByteArray -> Int -> Int32
-- sumByteArrayInt32X4 (ByteArray ba#) (I# n#) = I32# (addRest# (x0# +# x1# +# x2# +# x3#) q#)
--   where
--     q# = n# -# (n# `remInt#` 4#)
--     (# x0#, x1#, x2#, x3# #) = unpackInt32X4# (goInt32X4# (broadcastInt32X4# 0#) 0#)
--     goInt32X4# acc# i# =
--       case i# <# q# of
--         0# -> acc#
--         _  -> goInt32X4# (plusInt32X4# acc# (indexInt32ArrayAsInt32X4# ba# i#)) (i# +# 4#)
--     addRest# acc# i# =
--       case i# <# n# of
--         0# -> acc#
--         _  -> addRest# (acc# +# indexInt32Array# ba# i#) (i# +# 1#)
