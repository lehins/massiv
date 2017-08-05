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
module Data.Array.Massiv.Delayed.SIMD.Prim1 where

import           Control.Monad.Primitive
import           Data.Array.Massiv.Common
import           Control.Monad.ST                    (runST)
-- import           Data.Array.Massiv.Manifest           (toVector')
import           Data.Array.Massiv.Manifest.Primitive
-- import           Data.Array.Massiv.Mutable            (Target)
import           Data.Monoid                          ((<>))
import           Data.Primitive                       (sizeOf)
import           Data.Primitive.ByteArray
import           Data.Vector.Primitive                as VP
import           GHC.Exts
import           GHC.Int
-- import           GHC.Prim
import Data.Proxy

data VP = VP

data instance Array VP ix Double = VPArray { vpComp        :: Comp
                                           , vpSize        :: ix
                                           , vpIndexElem   :: !(Int# -> Double#)
                                           , vpIndexVector :: !(Int# -> DoubleX2#)
                                           , vpBA          :: {-# UNPACK #-} !ByteArray#
                                           }


-- data instance Array V ix Double = VArray { vComp        :: !Comp
--                                          , vSize        :: !ix
--                                          , vBA          :: !Op Double
--                                          }


type family IxE e :: *
type family Func1E e :: *
type family Func2E e :: *
--type family Broadcast e :: *

type instance IxE Double = Int# -> Double#
type instance Func1E Double = Double# -> Double#
type instance Func2E Double = Double# -> Double# -> Double#
--type instance Broadcast Double = Double -> Double#

type family IxV e :: *
type family Func1V e :: *
type family Func2V e :: *
type family BroadcastV e :: *

--data Int32X4 = Int32X4 Int32X4#

type instance IxV Int32 = Int# -> Int32X4#
type instance Func1V Int32 = Int32X4# -> Int32X4#
type instance Func2V Int32 = Int32X4# -> Int32X4# -> Int32X4#
type instance BroadcastV Int32 = Int32 -> Int32X4#


type instance IxV Double = Int# -> DoubleX2#
type instance Func1V Double = DoubleX2# -> DoubleX2#
type instance Func2V Double = DoubleX2# -> DoubleX2# -> DoubleX2#
type instance BroadcastV Double = Double -> DoubleX2#


class SIMD e where

  vectorWidth# :: proxy e -> Int#

  broadcastV :: e -> IxV e

  liftIxV :: (e -> e) -> IxV e -> IxV e

  composeV :: proxy e -> IxV e -> Func1V e -> IxV e

  compose2V :: proxy e -> IxV e -> IxV e -> Func2V e -> IxV e

  composeE :: proxy e -> IxE e -> Func1E e -> IxE e

  compose2E :: proxy e -> IxE e -> IxE e -> Func2E e -> IxE e

  --foldlV :: proxy e -> Int# -> IxV e -> Func2 e -> Func1 e
  --foldlV :: proxy e -> Fold e

  foldlV :: (e -> e -> e) -> e -> Int# -> IxV e -> Func2V e -> e

  sumV :: Int# -> IxV e -> e

  --compose2 :: proxy e -> IxV e -> IxV e -> Func2 e -> IxV e

  indexByteArrayE :: proxy e -> ByteArray# -> Int# -> IxE e

  indexByteArrayV :: proxy e -> ByteArray# -> Int# -> IxV e

  writeByteArrayE :: proxy e -> IxE e -> MutableByteArray# s -> Int# -> State# s -> State# s

  writeByteArrayV :: proxy e -> IxV e -> MutableByteArray# s -> Int# -> State# s -> State# s

-- unInt32# :: Int32 -> Int#
-- unInt32# (I32# x#) = x#
-- {-# INLINE unInt32# #-}


-- instance SIMD' Int32 where
--   vectorWidth' _ = 4#
--   broadcast (I32# x#) = \ _ -> broadcastInt32X4# x#
--   plus _ = plusInt32X4#
--   liftIxV f ixF = (\i# -> packInt32X4# (applyEach (unpackInt32X4# (ixF i#))))
--     where applyEach (# x0#, x1#, x2#, x3# #) = (# f' x0#, f' x1#, f' x2#, f' x3# #)
--           --{-# INLINE applyEach #-}
--           f' x# = unInt32# (f (I32# x#))
--           --{-# INLINE f' #-}
--   compose _ ixF vF = \ i# -> vF (ixF i#)
--   compose2 _ ixV1 ixV2 fV = \ i# -> fV (ixV1 i#) (ixV2 i#)
--   foldlV f (I32# a#) q# ixF# fV# =
--     case unpackInt32X4# (foldlV# 0# (broadcastInt32X4# a#)) of
--       (# x0#, x1#, x2#, x3# #) -> I32# (f# (f# (f# x0# x1#) x2#) x3#)
--     where f# acc# x# = unInt32# (f (I32# acc#) (I32# x#))
--           {-# INLINE f# #-}
--           w# = vectorWidth' (Proxy :: Proxy Int32)
--           foldlV# i# acc# =
--             case i# <# q# of
--               0# -> acc#
--               _  -> foldlV# (i# +# w#) (fV# acc# (ixF# i#))
--   {-# INLINE foldlV #-}
--   indexByteArray' _ ba# o# = \ i# -> indexInt32ArrayAsInt32X4# ba# (i# +# o#)
--   writeByteArray' _ fV# mba# i# = writeInt32ArrayAsInt32X4# mba# i# (fV# i#)
--   {-# INLINE writeByteArray' #-}

-- unDouble# :: Double -> Double#
-- unDouble# (D# x#) = x#
-- {-# INLINE unDouble# #-}

instance SIMD Double where
  vectorWidth# _ = 2#
  {-# INLINE vectorWidth# #-}
  broadcastV (D# x#) = \ _ -> broadcastDoubleX2# x#
  {-# INLINE broadcastV #-}
  --plus _ = plusDoubleX2#
  --{-# INLINE plus #-}
  -- liftIxV f ixF = (\i# -> packDoubleX2# (applyEach (unpackDoubleX2# (ixF i#))))
  --   where applyEach (# x0#, x1# #) = (# f# x0#, f# x1# #)
  --         --{-# INLINE applyEach #-}
  --         f# x# = unDouble# (f (D# x#))
          --{-# INLINE f# #-}
  --{-# INLINE liftIxV #-}
  composeE _ ixF vF = \ i# -> vF (ixF i#)
  {-# INLINE composeE #-}
  compose2E _ ixE1 ixE2 fE = \ i# -> fE (ixE1 i#) (ixE2 i#)
  {-# INLINE compose2E #-}
  composeV _ ixF vF = \ i# -> vF (ixF i#)
  {-# INLINE composeV #-}
  compose2V _ ixV1 ixV2 fV = \ i# -> fV (ixV1 i#) (ixV2 i#)
  {-# INLINE compose2V #-}
  --foldlV = foldlDoubleX2
  --{-# INLINE foldlV #-}
  --sumV = sumSIMD
  --{-# INLINE sumV #-}
  indexByteArrayE _ ba# o# = indexDoubleArray# ba#
  {-# INLINE indexByteArrayE #-}
  writeByteArrayE _ ixE# mba# i# = writeDoubleArray# mba# i# (ixE# i#)
  {-# INLINE writeByteArrayE #-}
  indexByteArrayV _ ba# o# = indexDoubleArrayAsDoubleX2# ba#
  {-# INLINE indexByteArrayV #-}
  writeByteArrayV _ ixV# mba# i# = writeDoubleArrayAsDoubleX2# mba# i# (ixV# i#)
  {-# INLINE writeByteArrayV #-}



-- sumDoubleX2 :: Int#
--             -> (Int# -> DoubleX2#)
--             -> Double
-- sumDoubleX2 q# ixF# =
--     case unpackDoubleX2# (sumV# 0# (broadcastDoubleX2# 0.0##)) of
--       (# x0#, x1# #) -> D# (x0# +## x1#)
--     where w# = vectorWidth' (Proxy :: Proxy Double)
--           sumV# i# acc# =
--             case i# <# q# of
--               0# -> acc#
--               _  -> sumV# (i# +# w#) (plusDoubleX2# acc# (ixF# i#))
-- --{-# INLINE sumDoubleX2 #-}





-- foldlDoubleX2 :: (Double -> Double -> Double)
--               -> Double
--               -> Int#
--               -> (Int# -> DoubleX2#)
--               -> (DoubleX2# -> DoubleX2# -> DoubleX2#)
--               -> Double
-- foldlDoubleX2 f (D# a#) q# ixF# fV# =
--     case unpackDoubleX2# (foldlV# 0# (broadcastDoubleX2# a#)) of
--       (# x0#, x1# #) -> D# (f# x0# x1#)
--     where f# acc# x# = unDouble# (f (D# acc#) (D# x#))
--           {-# INLINE f# #-}
--           w# = vectorWidth' (Proxy :: Proxy Double)
--           foldlV# i# acc# =
--             case i# <# q# of
--               0# -> acc#
--               _  -> foldlV# (i# +# w#) (fV# acc# (ixF# i#))
--           {-# INLINE foldlV# #-}
-- {-# INLINE foldlDoubleX2 #-}


-- singletonSIMD' :: forall ix e. (SIMD' e, Index ix) => e -> Array VS ix e
-- singletonSIMD' x =
--   VSArray Seq oneIndex (\_ -> x) (broadcast x)
-- {-# INLINE singletonSIMD' #-}


-- mapVector :: forall ix e. SIMD e => Func1E e -> Func1V e -> Array VP ix e -> Array VP ix e
-- mapVector fE fV (VPArray c sz ixE ixV b) =
--   VPArray c sz (composeE (Proxy :: Proxy e) ixE fE) (composeV (Proxy :: Proxy e) ixV fV) b
-- --{-# INLINE mapVector #-}


mapVector :: forall ix. Func1E Double -> Func1V Double -> Array VP ix Double -> Array VP ix Double
mapVector fE fV (VPArray c sz ixE ixV b) =
  VPArray c sz (composeE (Proxy :: Proxy Double) ixE fE) (composeV (Proxy :: Proxy Double) ixV fV) b
-- --{-# INLINE mapVector #-}



-- mapElem' :: forall ix e. SIMD' e => (e -> e) -> Array VS ix e -> Array VS ix e
-- mapElem' fE (VSArray c sz ixE ixV) =
--   VSArray c sz (fE . ixE) (liftIxV fE ixV)
-- {-# INLINE mapElem' #-}


-- zipWithVector :: forall ix e. SIMD e =>
--   Func2E e -> Func2V e -> Array VP ix e -> Array VP ix e -> Array VP ix e
-- zipWithVector fE fV (VPArray c1 sz1 ixE1 ixV1 _) (VPArray c2 _ ixE2 ixV2 b) =
--   let p = (Proxy :: Proxy e)
--   in VPArray (c1 <> c2) sz1 (compose2E p ixE1 ixE2 fE) (compose2V p ixV1 ixV2 fV) b
-- --{-# INLINE zipWithVector #-}

zipWithVector :: forall ix.
  Func2E Double -> Func2V Double -> Array VP ix Double -> Array VP ix Double -> Array VP ix Double
zipWithVector fE fV (VPArray c1 sz1 ixE1 ixV1 _) (VPArray c2 _ ixE2 ixV2 b) =
  --let p = (Proxy :: Proxy Double)
  --in VPArray (c1 <> c2) sz1 (compose2E p ixE1 ixE2 fE) (compose2V p ixV1 ixV2 fV) b
  VPArray (c1 <> c2) sz1 (\ i# -> fE (ixE1 i#) (ixE2 i#)) (\ i# -> fV (ixV1 i#) (ixV2 i#)) b
{-# INLINE zipWithVector #-}

-- instance Index ix => Num (Array VS ix Int32) where
--   (+) = zipWithVector' (+) plusInt32X4#
--   {-# INLINE (+) #-}
--   (-) = zipWithVector' (-) minusInt32X4#
--   {-# INLINE (-) #-}
--   (*) = zipWithVector' (*) timesInt32X4#
--   {-# INLINE (*) #-}
--   negate = mapVector' negate negateInt32X4#
--   {-# INLINE negate #-}
--   abs = mapElem' abs
--   {-# INLINE abs #-}
--   signum = mapElem' signum
--   {-# INLINE signum #-}
--   fromInteger = singletonSIMD' . fromInteger
--   {-# INLINE fromInteger #-}



instance Index ix => Num (Array VP ix Double) where
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





fromByteArray :: forall ix . (Index ix)
              => Comp -> ix -> Int -> ByteArray -> Array VP ix Double
fromByteArray c sz (I# o#) (ByteArray ba#) =
  VPArray c sz ((inline indexDoubleArray#) ba#) ((inline indexDoubleArrayAsDoubleX2#) ba#) ba#
{-# INLINE fromByteArray #-}


delaySIMD :: (Index ix) => Array P ix Double -> Array VP ix Double
delaySIMD (PArray c sz (VP.Vector offset _ ba)) = fromByteArray c sz offset ba
{-# INLINE delaySIMD #-}


computeSIMD :: forall ix . (Index ix) =>
  Array VP ix Double -> (Array P ix Double)
computeSIMD arr@(VPArray c sz ixE# ixV# ba#) = runST $ do
  let !n@(I# n#) = totalElem sz
      vP = Proxy :: Proxy Double
      w# = vectorWidth# vP
      q# = n# -# (n# `remInt#` w#)
  mba@(MutableByteArray mba#) <- newByteArray (n * sizeOf (undefined :: Double))
  let loadVector# i# s# =
        case i# <# q# of
          0# -> s#
          -- _  -> loadVector# (i# +# w#)
          --       (writeDoubleArrayAsDoubleX2# mba# i# (indexDoubleArrayAsDoubleX2# ba# i#) s#)
          -- _  -> loadVector# (i# +# w#)
          --       (writeByteArrayV vP (indexDoubleArrayAsDoubleX2# ba#) mba# i# s#)

          _  -> loadVector# (i# +# w#) (writeDoubleArrayAsDoubleX2# mba# i# (ixV# i#) s#)
          --_  -> loadVector# (i# +# w#) (writeByteArrayV vP ixV# mba# i# s#)
  let loadSlack i# s# =
        case i# <# n# of
          0# -> s#
          -- _  -> loadSlack (i# +# 1#) (writeDoubleArray# mba# i# (indexDoubleArray# ba# i#) s#)
          --_  -> loadSlack (i# +# 1#) (writeByteArrayE vP (indexDoubleArray# ba#) mba# i# s#)
          _  -> loadSlack (i# +# 1#) (writeDoubleArray# mba# i# (ixE# i#) s#)
          --_  -> loadSlack (i# +# 1#) (writeByteArrayE vP ixE# mba# i# s#)
  primitive_ $ \ s# -> loadSlack q# (loadVector# 0# s#)
  ba <- unsafeFreezeByteArray mba
  return $ PArray c sz $ VP.Vector 0 n ba
{-# INLINE computeSIMD #-}


-- foldSIMD' :: forall e ix . (Prim e, Index ix, SIMD' e) =>
--   (e -> e -> e) -> Func2 e -> e -> Array VS ix e -> e
-- foldSIMD' f vF# a (VSArray c sz ixElem ixVec#) =
--   --foldlSlack (I# q#) (foldlV f a q# ixVec# vF#)
--   foldlSlack (I# q#) (sumV q# ixVec#)
--   where !n@(I# n#) = totalElem sz
--         w# = vectorWidth' (Proxy :: Proxy e)
--         q# = n# -# (n# `remInt#` w#)
--         foldlSlack i acc =
--           if i < n
--           then foldlSlack (i + 1) (f acc (ixElem i))
--           else acc
-- --{-# INLINE foldSIMD' #-}



-- sumSIMD' :: forall e ix . (Prim e, Index ix, SIMD' e, Num e) => Array VS ix e -> e
-- sumSIMD' = foldSIMD' (+) (plus (Proxy :: Proxy e)) 0
-- --{-# INLINE sumSIMD' #-}


-- -- --sumByteArrayDoubleX2 :: ByteArray -> Int -> Double
sumSIMD :: (Index ix) => Array VP ix Double -> Double
sumSIMD (VPArray c sz ixE# ixV# ba#) = D# (addRest# (x0# +## x1#) q#)
  where
    !(I# n#) = totalElem sz
    q# = n# -# (n# `remInt#` 2#)
    (# x0#, x1# #) = unpackDoubleX2# (goDoubleX2# (broadcastDoubleX2# 0.0##) 0#)
    goDoubleX2# acc# i# =
      case i# <# q# of
        0# -> acc#
        --_  -> goDoubleX2# (plusDoubleX2# acc# (indexDoubleArrayAsDoubleX2# ba# i#)) (i# +# 2#)
        _  -> goDoubleX2# (plusDoubleX2# acc# ((inline ixV#) i#)) (i# +# 2#)
    addRest# acc# i# =
      case i# <# n# of
        0# -> acc#
        --_  -> addRest# (acc# +## indexDoubleArray# ba# i#) (i# +# 1#)
        _  -> addRest# (acc# +## ixE# i#) (i# +# 1#)
{-# INLINE sumSIMD #-}


-- data Op e = Arr ByteArray#
--           | Plus (Op e) (Op e)
--           | Negate (Op e)


-- sumOp :: (Index ix) => Array VP ix Double -> Double
-- sumOp (VArray _ sz op) = D# (addRest# (x0# +## x1#) q#)
--   where
--     !(I# n#) = totalElem sz
--     q# = n# -# (n# `remInt#` 2#)
--     (# x0#, x1# #) = unpackDoubleX2# (goDoubleX2# (broadcastDoubleX2# 0.0##) 0#)
--     goDoubleX2# acc# i# =
--       case i# <# q# of
--         0# -> acc#
--         _  -> goDoubleX2# (plusDoubleX2# acc#
--                            (plusDoubleX2#
--                             (indexDoubleArrayAsDoubleX2# ba1# i#)
--                             (indexDoubleArrayAsDoubleX2# ba2# i#))) (i# +# 2#)
--         --_  -> goDoubleX2# (plusDoubleX2# acc# (plusDoubleX2# (ixV1# i#) (ixV2# i#))) (i# +# 2#)
--     addRest# acc# i# =
--       case i# <# n# of
--         0# -> acc#
--         _  -> addRest# (acc# +## (indexDoubleArray# ba1# i# +## indexDoubleArray# ba2# i#)) (i# +# 1#)
--         --_  -> addRest# (acc# +## (ixE1# i# +## ixE2# i#)) (i# +# 1#)
-- {-# INLINE sumSIMD2 #-}




sumSIMD2 :: (Index ix) => Array VP ix Double -> Array VP ix Double -> Double
sumSIMD2 (VPArray _ sz ixE1# ixV1# ba1#) (VPArray _ _ ixE2# ixV2# ba2#) = D# (addRest# (x0# +## x1#) q#)
  where
    !(I# n#) = totalElem sz
    q# = n# -# (n# `remInt#` 2#)
    (# x0#, x1# #) = unpackDoubleX2# (goDoubleX2# (broadcastDoubleX2# 0.0##) 0#)
    goDoubleX2# acc# i# =
      case i# <# q# of
        0# -> acc#
        _  -> goDoubleX2# (plusDoubleX2# acc#
                           (plusDoubleX2#
                            (indexDoubleArrayAsDoubleX2# ba1# i#)
                            (indexDoubleArrayAsDoubleX2# ba2# i#))) (i# +# 2#)
        --_  -> goDoubleX2# (plusDoubleX2# acc# (plusDoubleX2# (ixV1# i#) (ixV2# i#))) (i# +# 2#)
    addRest# acc# i# =
      case i# <# n# of
        0# -> acc#
        _  -> addRest# (acc# +## (indexDoubleArray# ba1# i# +## indexDoubleArray# ba2# i#)) (i# +# 1#)
        --_  -> addRest# (acc# +## (ixE1# i# +## ixE2# i#)) (i# +# 1#)
{-# INLINE sumSIMD2 #-}


sumArr :: Index ix => Array P ix Double -> Double
sumArr = sumSIMD . delaySIMD
        -- _  -> addRest# (acc# +## ixElem# i#) (i# +# 1#)
{-# INLINE sumArr #-}

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
