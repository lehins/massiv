{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
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
import           Control.Monad.ST                    (runST)
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Manifest           (toVector')
import           Data.Array.Massiv.Manifest.Primitive
import           Data.Array.Massiv.Mutable            (Target)
import           Data.Monoid                          ((<>))
import           Data.Primitive                       (sizeOf)
import           Data.Primitive.ByteArray
import           Data.Vector.Primitive                as VP
import           GHC.Exts
import           GHC.Int
import           GHC.Prim
import Data.Proxy

data VP = VP

-- type family IxElem r e :: *
-- type family MapElem r e :: *
-- type family ZipWithElem r e :: *

type family IxVector r e :: *
type family MapVector r e :: *
type family ZipWithVector r e :: *


data instance Array VP ix e = VPArray { vpComp        :: !Comp
                                      , vpSize        :: !ix
                                      , vpIndexElem   :: Int -> e
                                      , vpIndexVector :: IxVector VP e
                                      }


-- type instance IxElem VP Int32 = Int# -> Int#
-- type instance MapElem VP Int32 = Int# -> Int#
-- type instance ZipWithElem VP Int32 = Int# -> Int# -> Int#

type instance IxVector VP Int32 = Int# -> Int32X4#
type instance MapVector VP Int32 = Int32X4# -> Int32X4#
type instance ZipWithVector VP Int32 = Int32X4# -> Int32X4# -> Int32X4#

type instance IxVector VP Double = Int# -> DoubleX2#
type instance MapVector VP Double = DoubleX2# -> DoubleX2#
type instance ZipWithVector VP Double = DoubleX2# -> DoubleX2# -> DoubleX2#


type family IxV e :: *
type family Func1 e :: *
type family Func2 e :: *
type family Broadcast e :: *
type family Fold e :: *

--data Int32X4 = Int32X4 Int32X4#

type instance IxV Int32 = Int# -> Int32X4#
type instance Func1 Int32 = Int32X4# -> Int32X4#
type instance Func2 Int32 = Int32X4# -> Int32X4# -> Int32X4#
type instance Broadcast Int32 = Int32 -> Int32X4#


type instance IxV Double = Int# -> DoubleX2#
type instance Func1 Double = DoubleX2# -> DoubleX2#
type instance Func2 Double = DoubleX2# -> DoubleX2# -> DoubleX2#
type instance Broadcast Double = Double -> DoubleX2#


class SIMD' e where

  vectorWidth' :: proxy e -> Int#

  broadcast :: e -> IxV e

  liftIxV :: (e -> e) -> IxV e -> IxV e

  plus :: proxy e -> Func2 e

  compose :: proxy e -> IxV e -> Func1 e -> IxV e

  compose2 :: proxy e -> IxV e -> IxV e -> Func2 e -> IxV e

  --foldlV :: proxy e -> Int# -> IxV e -> Func2 e -> Func1 e
  --foldlV :: proxy e -> Fold e

  foldlV :: (e -> e -> e) -> e -> Int# -> IxV e -> Func2 e -> e

  sumV :: Int# -> IxV e -> e

  --compose2 :: proxy e -> IxV e -> IxV e -> Func2 e -> IxV e

  indexByteArray' :: proxy e -> ByteArray# -> Int# -> IxV e

  writeByteArray' :: proxy e -> IxV e -> MutableByteArray# s -> Int# -> State# s -> State# s

unInt32# :: Int32 -> Int#
unInt32# (I32# x#) = x#
{-# INLINE unInt32# #-}


instance SIMD' Int32 where
  vectorWidth' _ = 4#
  broadcast (I32# x#) = \ _ -> broadcastInt32X4# x#
  plus _ = plusInt32X4#
  liftIxV f ixF = (\i# -> packInt32X4# (applyEach (unpackInt32X4# (ixF i#))))
    where applyEach (# x0#, x1#, x2#, x3# #) = (# f' x0#, f' x1#, f' x2#, f' x3# #)
          --{-# INLINE applyEach #-}
          f' x# = unInt32# (f (I32# x#))
          --{-# INLINE f' #-}
  compose _ ixF vF = \ i# -> vF (ixF i#)
  compose2 _ ixV1 ixV2 fV = \ i# -> fV (ixV1 i#) (ixV2 i#)
  foldlV f (I32# a#) q# ixF# fV# =
    case unpackInt32X4# (foldlV# 0# (broadcastInt32X4# a#)) of
      (# x0#, x1#, x2#, x3# #) -> I32# (f# (f# (f# x0# x1#) x2#) x3#)
    where f# acc# x# = unInt32# (f (I32# acc#) (I32# x#))
          {-# INLINE f# #-}
          w# = vectorWidth' (Proxy :: Proxy Int32)
          foldlV# i# acc# =
            case i# <# q# of
              0# -> acc#
              _  -> foldlV# (i# +# w#) (fV# acc# (ixF# i#))
  {-# INLINE foldlV #-}
  indexByteArray' _ ba# o# = \ i# -> indexInt32ArrayAsInt32X4# ba# (i# +# o#)
  writeByteArray' _ fV# mba# i# = writeInt32ArrayAsInt32X4# mba# i# (fV# i#)
  {-# INLINE writeByteArray' #-}

unDouble# :: Double -> Double#
unDouble# (D# x#) = x#
{-# INLINE unDouble# #-}

instance SIMD' Double where
  vectorWidth' _ = 2#
  --{-# INLINE vectorWidth' #-}
  broadcast (D# x#) = \ _ -> broadcastDoubleX2# x#
  --{-# INLINE broadcast #-}
  plus _ = plusDoubleX2#
  --{-# INLINE plus #-}
  liftIxV f ixF = (\i# -> packDoubleX2# (applyEach (unpackDoubleX2# (ixF i#))))
    where applyEach (# x0#, x1# #) = (# f# x0#, f# x1# #)
          --{-# INLINE applyEach #-}
          f# x# = unDouble# (f (D# x#))
          --{-# INLINE f# #-}
  --{-# INLINE liftIxV #-}
  compose _ ixF vF = \ i# -> vF (ixF i#)
  --{-# INLINE compose #-}
  compose2 _ ixV1 ixV2 fV = \ i# -> fV (ixV1 i#) (ixV2 i#)
  --{-# INLINE compose2 #-}
  foldlV = foldlDoubleX2
  --{-# INLINE foldlV #-}
  sumV = sumDoubleX2
  --{-# INLINE sumV #-}
  indexByteArray' _ ba# o# = \ i# -> indexDoubleArrayAsDoubleX2# ba# (i# +# o#)
  --{-# INLINE indexByteArray' #-}
  writeByteArray' _ fV# mba# i# = writeDoubleArrayAsDoubleX2# mba# i# (fV# i#)
  --{-# INLINE writeByteArray' #-}



sumDoubleX2 :: Int#
            -> (Int# -> DoubleX2#)
            -> Double
sumDoubleX2 q# ixF# =
    case unpackDoubleX2# (sumV# 0# (broadcastDoubleX2# 0.0##)) of
      (# x0#, x1# #) -> D# (x0# +## x1#)
    where w# = vectorWidth' (Proxy :: Proxy Double)
          sumV# i# acc# =
            case i# <# q# of
              0# -> acc#
              _  -> sumV# (i# +# w#) (plusDoubleX2# acc# (ixF# i#))
--{-# INLINE sumDoubleX2 #-}





foldlDoubleX2 :: (Double -> Double -> Double)
              -> Double
              -> Int#
              -> (Int# -> DoubleX2#)
              -> (DoubleX2# -> DoubleX2# -> DoubleX2#)
              -> Double
foldlDoubleX2 f (D# a#) q# ixF# fV# =
    case unpackDoubleX2# (foldlV# 0# (broadcastDoubleX2# a#)) of
      (# x0#, x1# #) -> D# (f# x0# x1#)
    where f# acc# x# = unDouble# (f (D# acc#) (D# x#))
          {-# INLINE f# #-}
          w# = vectorWidth' (Proxy :: Proxy Double)
          foldlV# i# acc# =
            case i# <# q# of
              0# -> acc#
              _  -> foldlV# (i# +# w#) (fV# acc# (ixF# i#))
          {-# INLINE foldlV# #-}
{-# INLINE foldlDoubleX2 #-}


data VS = VS

data instance Array VS ix e = VSArray { vsComp  :: !Comp
                                      , vsSize  :: ix
                                      , vsElem  :: Int -> e
                                      , vsVect  :: IxV e }


singletonSIMD' :: forall ix e. (SIMD' e, Index ix) => e -> Array VS ix e
singletonSIMD' x =
  VSArray Seq oneIndex (\_ -> x) (broadcast x)
{-# INLINE singletonSIMD' #-}

fromByteArray' :: forall ix e. (SIMD' e, Prim e, Index ix)
               => Comp -> ix -> Int -> ByteArray -> Array VS ix e
fromByteArray' c sz o@(I# o#) ba@(ByteArray ba#) =
  VSArray c sz (\i -> indexByteArray ba (i + o)) (indexByteArray' (Proxy :: Proxy e) ba# o#)
{-# INLINE fromByteArray' #-}


mapVector' :: forall ix e. SIMD' e => (e -> e) -> Func1 e -> Array VS ix e -> Array VS ix e
mapVector' fE fV (VSArray c sz ixE ixV) =
  VSArray c sz (fE . ixE) (compose (Proxy :: Proxy e) ixV fV)
{-# INLINE mapVector' #-}



mapElem' :: forall ix e. SIMD' e => (e -> e) -> Array VS ix e -> Array VS ix e
mapElem' fE (VSArray c sz ixE ixV) =
  VSArray c sz (fE . ixE) (liftIxV fE ixV)
{-# INLINE mapElem' #-}


zipWithVector' :: forall ix e. SIMD' e =>
  (e -> e -> e) -> Func2 e -> Array VS ix e -> Array VS ix e -> Array VS ix e
zipWithVector' fE fV (VSArray c1 sz1 ixE1 ixV1) (VSArray c2 _ ixE2 ixV2) =
    VSArray (c1 <> c2) sz1 (\i -> fE (ixE1 i) (ixE2 i)) (compose2 (Proxy :: Proxy e) ixV1 ixV2 fV)
{-# INLINE zipWithVector' #-}


instance Index ix => Num (Array VS ix Int32) where
  (+) = zipWithVector' (+) plusInt32X4#
  {-# INLINE (+) #-}
  (-) = zipWithVector' (-) minusInt32X4#
  {-# INLINE (-) #-}
  (*) = zipWithVector' (*) timesInt32X4#
  {-# INLINE (*) #-}
  negate = mapVector' negate negateInt32X4#
  {-# INLINE negate #-}
  abs = mapElem' abs
  {-# INLINE abs #-}
  signum = mapElem' signum
  {-# INLINE signum #-}
  fromInteger = singletonSIMD' . fromInteger
  {-# INLINE fromInteger #-}


instance Index ix => Num (Array VS ix Double) where
  (+) = zipWithVector' (+) plusDoubleX2#
  {-# INLINE (+) #-}
  (-) = zipWithVector' (-) minusDoubleX2#
  {-# INLINE (-) #-}
  (*) = zipWithVector' (*) timesDoubleX2#
  {-# INLINE (*) #-}
  negate = mapVector' negate negateDoubleX2#
  {-# INLINE negate #-}
  abs = mapElem' abs
  {-# INLINE abs #-}
  signum = mapElem' signum
  {-# INLINE signum #-}
  fromInteger = singletonSIMD' . fromInteger
  {-# INLINE fromInteger #-}



class SIMD r e where

  vectorWidth :: Array r ix e -> Int#

  singletonSIMD :: Index ix => e -> Array r ix e

  fromByteArray :: Index ix => Comp -> ix -> Int -> ByteArray -> Array r ix e

  mapElem :: (e -> e) -> Array r ix e -> Array r ix e

  mapVector :: (e -> e) -> MapVector r e -> Array r ix e -> Array r ix e

  zipWithVector :: (e -> e -> e) -> ZipWithVector r e -> Array r ix e -> Array r ix e -> Array r ix e

  writeVector :: Array r ix e -> IxVector r e -> MutableByteArray# s -> Int# -> State# s -> State# s


instance SIMD VP Int32 where

  vectorWidth _ = 4#

  fromByteArray c sz o@(I# o#) ba@(ByteArray ba#) =
    VPArray
    c
    sz
    (\i -> indexByteArray ba (i + o))
    (\i# -> indexInt32ArrayAsInt32X4# ba# (i# +# o#))
  {-# INLINE fromByteArray #-}

  singletonSIMD x@(I32# x#) =
    VPArray Seq oneIndex (\_ -> x) (\_ -> broadcastInt32X4# x#)
  {-# INLINE singletonSIMD #-}

  mapElem fE (VPArray c sz ixE ixV) =
    VPArray c sz (fE . ixE) (\i# -> packInt32X4# (applyEach (unpackInt32X4# (ixV i#))))
    where applyEach (# x0#, x1#, x2#, x3# #) =
            (# fE' x0#, fE' x1#, fE' x2#, fE' x3# #)
          {-# INLINE applyEach #-}
          fE' x# = unI32# (fE (I32# x#))
          {-# INLINE fE' #-}
          unI32# (I32# x#) = x#
          {-# INLINE unI32# #-}
  {-# INLINE mapElem #-}

  mapVector fE fV (VPArray c sz ixE ixV) =
    VPArray c sz (\i -> fE (ixE i)) (\i -> fV (ixV i))
  {-# INLINE mapVector #-}

  zipWithVector fE fV (VPArray c1 sz1 ixE1 ixV1) (VPArray c2 _ ixE2 ixV2) =
    VPArray (c1 <> c2) sz1 (\i -> fE (ixE1 i) (ixE2 i)) (\i -> fV (ixV1 i) (ixV2 i))
  {-# INLINE zipWithVector #-}

  writeVector _ fV mba# i# = writeInt32ArrayAsInt32X4# mba# i# (fV i#)
  {-# INLINE writeVector #-}



instance SIMD VP Double where

  vectorWidth _ = 2#

  fromByteArray c sz o@(I# o#) ba@(ByteArray ba#) =
    VPArray
    c
    sz
    (\i -> indexByteArray ba (i + o))
    (\i# -> indexDoubleArrayAsDoubleX2# ba# (i# +# o#))
  {-# INLINE fromByteArray #-}

  singletonSIMD x@(D# x#) =
    VPArray Seq oneIndex (\_ -> x) (\_ -> broadcastDoubleX2# x#)
  {-# INLINE singletonSIMD #-}

  mapElem fE (VPArray c sz ixE ixV) =
    VPArray c sz (fE . ixE) (\i# -> packDoubleX2# (applyEach (unpackDoubleX2# (ixV i#))))
    where applyEach (# x0#, x1# #) = (# fE' x0#, fE' x1# #)
          {-# INLINE applyEach #-}
          fE' x# = unD# (fE (D# x#))
          {-# INLINE fE' #-}
          unD# (D# x#) = x#
          {-# INLINE unD# #-}
  {-# INLINE mapElem #-}

  mapVector fE fV (VPArray c sz ixE ixV) =
    VPArray c sz (\i -> fE (ixE i)) (\i -> fV (ixV i))
  {-# INLINE mapVector #-}

  zipWithVector fE fV (VPArray c1 sz1 ixE1 ixV1) (VPArray c2 _ ixE2 ixV2) =
    VPArray (c1 <> c2) sz1 (\i -> fE (ixE1 i) (ixE2 i)) (\i -> fV (ixV1 i) (ixV2 i))
  {-# INLINE zipWithVector #-}

  writeVector _ fV mba# i# = writeDoubleArrayAsDoubleX2# mba# i# (fV i#)
  {-# INLINE writeVector #-}




instance Index ix =>  Num (Array VP ix Int32) where
  (+) = zipWithVector (+) plusInt32X4#
  {-# INLINE (+) #-}
  (-) = zipWithVector (-) minusInt32X4#
  {-# INLINE (-) #-}
  (*) = zipWithVector (*) timesInt32X4#
  {-# INLINE (*) #-}
  negate = mapVector negate negateInt32X4#
  {-# INLINE negate #-}
  abs = mapElem abs
  {-# INLINE abs #-}
  signum = mapElem signum
  {-# INLINE signum #-}
  fromInteger = singletonSIMD . fromInteger
  {-# INLINE fromInteger #-}



instance Index ix =>  Num (Array VP ix Double) where
  (+) = zipWithVector (+) plusDoubleX2#
  {-# INLINE (+) #-}
  (-) = zipWithVector (-) minusDoubleX2#
  {-# INLINE (-) #-}
  (*) = zipWithVector (*) timesDoubleX2#
  {-# INLINE (*) #-}
  negate = mapVector negate negateDoubleX2#
  {-# INLINE negate #-}
  abs = mapElem abs
  {-# INLINE abs #-}
  signum = mapElem signum
  {-# INLINE signum #-}
  fromInteger = singletonSIMD . fromInteger
  {-# INLINE fromInteger #-}


delaySIMD :: (Index ix, SIMD VP e, Prim e) => Array P ix e  -> Array VP ix e
delaySIMD (PArray c sz (VP.Vector offset _ ba)) = fromByteArray c sz offset ba
{-# INLINE delaySIMD #-}


computeSIMD :: forall e ix . (Prim e, Index ix, SIMD VP e) =>
  Array VP ix e -> Array P ix e
computeSIMD arr@(VPArray c sz ixElem ixVec) = runST $ do
  let !n@(I# n#) = totalElem sz
      w# = vectorWidth arr
      q# = n# -# (n# `remInt#` w#)
  mba@(MutableByteArray mba#) <- newByteArray (n * sizeOf (undefined :: e))
  let loadVector# i# s# =
        case i# <# q# of
          0# -> s#
          _  -> loadVector# (i# +# w#) (writeVector arr ixVec mba# i# s#)
  let loadSlack i =
        if i < n
          then writeByteArray mba i (ixElem i) >> loadSlack (i + 1)
          else return ()
  primitive_ (loadVector# 0#)
  loadSlack (I# q#)
  ba <- unsafeFreezeByteArray mba
  return $ PArray c sz $ VP.Vector 0 n ba
{-# INLINE computeSIMD #-}




delaySIMD' :: (Index ix, SIMD' e, Prim e) => Array P ix e  -> Array VS ix e
delaySIMD' (PArray c sz (VP.Vector offset _ ba)) = fromByteArray' c sz offset ba
{-# INLINE delaySIMD' #-}


computeSIMD' :: forall e ix . (Prim e, Index ix, SIMD' e) =>
  Array VS ix e -> Array P ix e
computeSIMD' arr@(VSArray c sz ixElem ixVec) = runST $ do
  let !n@(I# n#) = totalElem sz
      vP = Proxy :: Proxy e
      w# = vectorWidth' vP
      q# = n# -# (n# `remInt#` w#)
  mba@(MutableByteArray mba#) <- newByteArray (n * sizeOf (undefined :: e))
  let loadVector# i# s# =
        case i# <# q# of
          0# -> s#
          _  -> loadVector# (i# +# w#) (writeByteArray' vP ixVec mba# i# s#)
  let loadSlack i =
        if i < n
          then writeByteArray mba i (ixElem i) >> loadSlack (i + 1)
          else return ()
  primitive_ (loadVector# 0#)
  loadSlack (I# q#)
  ba <- unsafeFreezeByteArray mba
  return $ PArray c sz $ VP.Vector 0 n ba
{-# INLINE computeSIMD' #-}


foldSIMD' :: forall e ix . (Prim e, Index ix, SIMD' e) =>
  (e -> e -> e) -> Func2 e -> e -> Array VS ix e -> e
foldSIMD' f vF# a (VSArray c sz ixElem ixVec#) =
  --foldlSlack (I# q#) (foldlV f a q# ixVec# vF#)
  foldlSlack (I# q#) (sumV q# ixVec#)
  where !n@(I# n#) = totalElem sz
        w# = vectorWidth' (Proxy :: Proxy e)
        q# = n# -# (n# `remInt#` w#)
        foldlSlack i acc =
          if i < n
          then foldlSlack (i + 1) (f acc (ixElem i))
          else acc
--{-# INLINE foldSIMD' #-}



-- sumSIMD' :: forall e ix . (Prim e, Index ix, SIMD' e, Num e) => Array VS ix e -> e
-- sumSIMD' = foldSIMD' (+) (plus (Proxy :: Proxy e)) 0
-- --{-# INLINE sumSIMD' #-}


-- --sumByteArrayDoubleX2 :: ByteArray -> Int -> Double
sumSIMD' :: (Index ix) => Array VS ix Double -> Double
sumSIMD' (VSArray c sz ixElem ixVec#) = D# (addRest# (x0# +## x1#) q#)
  where
    !n@(I# n#) = totalElem sz
    q# = n# -# (n# `remInt#` 2#)
    (# x0#, x1# #) = unpackDoubleX2# (goDoubleX2# (broadcastDoubleX2# 0.0##) 0#)
    goDoubleX2# acc# i# =
      case i# <# q# of
        0# -> acc#
        _  -> goDoubleX2# (plusDoubleX2# acc# (ixVec# i#)) (i# +# 2#)
    ixElem# i# = unDouble# (ixElem (I# i#))
    addRest# acc# i# =
      case i# <# n# of
        0# -> acc#
        _  -> addRest# (acc# +## ixElem# i#) (i# +# 1#)

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
