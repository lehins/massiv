{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Graphics.ColorSpace.Binary
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.Binary (
  Bit(..), on, off, isOn, isOff, fromBool, zero, one, bit2bool, bool2bit, toNum, fromNum
  ) where

import           Control.Monad
import           Data.Bits
import           Data.Typeable                (Typeable)
import qualified Data.Vector.Generic          as V
import qualified Data.Vector.Generic.Mutable  as M
import qualified Data.Vector.Unboxed          as U
import           Data.Word                    (Word8)
import           Foreign.Ptr
import           Foreign.Storable
import           Graphics.ColorSpace.Internal
import           Graphics.ColorSpace.X
import           Prelude                      hiding (map)


-- | Under the hood, binary pixels are represented as `Word8`, but can only take
-- values of @0@ or @1@. Use `zero`\/`one` to construct a bit and `on`\/`off` to
-- construct a binary pixel.
newtype Bit = Bit Word8 deriving (Ord, Eq, Typeable)


instance Show Bit where
  show (Bit 0) = "0"
  show _       = "1"


instance Bits Bit where
  (Bit 0) .&. _       = Bit 0
  (Bit 1) .&. (Bit 1) = Bit 1
  _       .&. (Bit 0) = Bit 0
  _       .&. _       = Bit 1
  {-# INLINE (.&.) #-}
  (Bit 1) .|. _       = Bit 1
  (Bit 0) .|. (Bit 0) = Bit 0
  _       .|. _       = Bit 1
  {-# INLINE (.|.) #-}
  (Bit 0) `xor` (Bit 0) = Bit 0
  (Bit 1) `xor` (Bit 1) = Bit 0
  _       `xor` _       = Bit 1
  {-# INLINE xor #-}
  complement (Bit 0) = Bit 1
  complement       _ = Bit 0
  {-# INLINE complement #-}
  shift !b 0 = b
  shift  _ _ = Bit 0
  {-# INLINE shift #-}
  rotate !b _ = b
  {-# INLINE rotate #-}
  zeroBits = Bit 0
  {-# INLINE zeroBits #-}
  bit 0 = Bit 1
  bit _ = Bit 0
  {-# INLINE bit #-}
  testBit (Bit 1) 0 = True
  testBit _       _ = False
  {-# INLINE testBit #-}
  bitSizeMaybe _ = Just 1
  {-# INLINE bitSizeMaybe #-}
  bitSize _ = 1
  {-# INLINE bitSize #-}
  isSigned _ = False
  {-# INLINE isSigned #-}
  popCount (Bit 0) = 0
  popCount _       = 1
  {-# INLINE popCount #-}


instance Bits (Pixel X Bit) where
  (.&.) = liftPx2 (.&.)
  {-# INLINE (.&.) #-}
  (.|.) = liftPx2 (.|.)
  {-# INLINE (.|.) #-}
  xor = liftPx2 xor
  {-# INLINE xor #-}
  complement = liftPx complement
  {-# INLINE complement #-}
  shift !b !n = liftPx (`shift` n) b
  {-# INLINE shift #-}
  rotate !b !n = liftPx (`rotate` n) b
  {-# INLINE rotate #-}
  zeroBits = promote zeroBits
  {-# INLINE zeroBits #-}
  bit = promote . bit
  {-# INLINE bit #-}
  testBit (PixelX (Bit 1)) 0 = True
  testBit _                _ = False
  {-# INLINE testBit #-}
  bitSizeMaybe _ = Just 1
  {-# INLINE bitSizeMaybe #-}
  bitSize _ = 1
  {-# INLINE bitSize #-}
  isSigned _ = False
  {-# INLINE isSigned #-}
  popCount (PixelX (Bit 0)) = 0
  popCount _                = 1
  {-# INLINE popCount #-}

toNum :: Num a => Bit -> a
toNum (Bit 0) = 0
toNum _       = 1
{-# INLINE toNum #-}

fromNum :: (Eq a, Num a) => a -> Bit
fromNum 0 = zero
fromNum _ = one
{-# INLINE fromNum #-}


zero :: Bit
zero = Bit 0
{-# INLINE zero #-}

one :: Bit
one = Bit 1
{-# INLINE one #-}

bool2bit :: Bool -> Bit
bool2bit False = zero
bool2bit True  = one
{-# INLINE bool2bit #-}

bit2bool :: Bit -> Bool
bit2bool (Bit 0) = False
bit2bool _       = True
{-# INLINE bit2bool #-}

-- | Represents value 'True' or @1@ in binary. Often also called a foreground
-- pixel of an object.
on :: Pixel X Bit
on = PixelX one
{-# INLINE on #-}


-- | Represents value 'False' or @0@ in binary. Often also called a background
-- pixel.
off :: Pixel X Bit
off = PixelX zero
{-# INLINE off #-}


-- | Convert a 'Bool' to a binary pixel.
--
-- >>> isOn (fromBool True)
-- True
--
fromBool :: Bool -> Pixel X Bit
fromBool False = off
fromBool True  = on
{-# INLINE fromBool #-}


-- | Test if Pixel's value is 'on'.
isOn :: Pixel X Bit -> Bool
isOn (PixelX (Bit 0)) = False
isOn _                = True
{-# INLINE isOn #-}


-- | Test if Pixel's value is 'off'.
isOff :: Pixel X Bit -> Bool
isOff = not . isOn
{-# INLINE isOff #-}


-- | Values: @0@ and @1@
instance Elevator Bit where
  eToWord8 (Bit 0) = 0
  eToWord8 _       = maxBound
  {-# INLINE eToWord8 #-}
  eToWord16 (Bit 0) = 0
  eToWord16 _       = maxBound
  {-# INLINE eToWord16 #-}
  eToWord32 (Bit 0) = 0
  eToWord32 _       = maxBound
  {-# INLINE eToWord32 #-}
  eToWord64 (Bit 0) = 0
  eToWord64 _       = maxBound
  {-# INLINE eToWord64 #-}
  eToFloat (Bit 0) = 0
  eToFloat _       = 1
  {-# INLINE eToFloat #-}
  eToDouble (Bit 0) = 0
  eToDouble _       = 1
  {-# INLINE eToDouble #-}
  eFromDouble 0 = Bit 0
  eFromDouble _ = Bit 1
  {-# INLINE eFromDouble #-}


instance Num Bit where
  (+) = (.|.)
  {-# INLINE (+) #-}
  -- 0 - 0 = 0
  -- 0 - 1 = 0
  -- 1 - 0 = 1
  -- 1 - 1 = 0
  (Bit 0) - (Bit 0) = Bit 0
  _       - (Bit 0) = Bit 1
  _       - _       = Bit 0
  {-# INLINE (-) #-}
  (*) = (.&.)
  {-# INLINE (*) #-}
  abs         = id
  {-# INLINE abs #-}
  signum      = id
  {-# INLINE signum #-}
  fromInteger 0 = Bit 0
  fromInteger _ = Bit 1
  {-# INLINE fromInteger #-}


instance Storable Bit where

  sizeOf _ = sizeOf (undefined :: Word8)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined :: Word8)
  {-# INLINE alignment #-}
  peek !p = do
    q <- return $ castPtr p
    b <- peek q
    return (Bit b)
  {-# INLINE peek #-}
  poke !p (Bit b) = do
    q <- return $ castPtr p
    poke q b
  {-# INLINE poke #-}


-- | Unboxing of a `Bit`.
instance U.Unbox Bit

newtype instance U.MVector s Bit = MV_Bit (U.MVector s Word8)

instance M.MVector U.MVector Bit where
  basicLength (MV_Bit mvec) = M.basicLength mvec
  {-# INLINE basicLength #-}
  basicUnsafeSlice idx len (MV_Bit mvec) = MV_Bit (M.basicUnsafeSlice idx len mvec)
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_Bit mvec) (MV_Bit mvec') = M.basicOverlaps mvec mvec'
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew len = MV_Bit `liftM` M.basicUnsafeNew len
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeReplicate len (Bit w) = MV_Bit `liftM` M.basicUnsafeReplicate len w
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_Bit mvec) idx = Bit `liftM` M.basicUnsafeRead mvec idx
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_Bit mvec) idx (Bit w) = M.basicUnsafeWrite mvec idx w
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_Bit mvec) = M.basicClear mvec
  {-# INLINE basicClear #-}
  basicSet (MV_Bit mvec) (Bit w) =  M.basicSet mvec w
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV_Bit mvec) (MV_Bit mvec') = M.basicUnsafeCopy mvec mvec'
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_Bit mvec) (MV_Bit mvec') = M.basicUnsafeMove mvec mvec'
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_Bit mvec) len = MV_Bit `liftM` M.basicUnsafeGrow mvec len
  {-# INLINE basicUnsafeGrow #-}
#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_Bit mvec) = M.basicInitialize mvec
  {-# INLINE basicInitialize #-}
#endif


newtype instance U.Vector Bit = V_Bit (U.Vector Word8)

instance V.Vector U.Vector Bit where
  basicUnsafeFreeze (MV_Bit mvec) = V_Bit `liftM` V.basicUnsafeFreeze mvec
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_Bit vec) = MV_Bit `liftM` V.basicUnsafeThaw vec
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_Bit vec) = V.basicLength vec
  {-# INLINE basicLength #-}
  basicUnsafeSlice idx len (V_Bit vec) = V_Bit (V.basicUnsafeSlice idx len vec)
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_Bit vec) idx = Bit `liftM` V.basicUnsafeIndexM vec idx
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_Bit mvec) (V_Bit vec) = V.basicUnsafeCopy mvec vec
  {-# INLINE basicUnsafeCopy #-}
  elemseq (V_Bit vec) (Bit w) = V.elemseq vec w
  {-# INLINE elemseq #-}
