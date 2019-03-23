{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if __GLASGOW_HASKELL__ >= 800
  {-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif
-- |
-- Module      : Graphics.ColorSpace.Elevator
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.Elevator (
  Elevator(..)
  , clamp01
  ) where

import qualified Data.Complex as C
import Data.Int
import Data.Typeable
import Data.Vector.Storable (Storable)
import Data.Vector.Unboxed (Unbox)
import Data.Word
import GHC.Float


-- | A class with a set of convenient functions that allow for changing precision of
-- channels within pixels, while scaling the values to keep them in an appropriate range.
--
-- >>> let rgb = PixelRGB 0.0 0.5 1.0 :: Pixel RGB Double
-- >>> eToWord8 <$> rgb
-- <RGB:(0|128|255)>
-- >>> eToWord16 <$> rgb
-- <RGB:(0|32768|65535)>
--
class (Eq e, Num e, Typeable e, Unbox e, Storable e) => Elevator e where

  -- | Values are scaled to @[0, 255]@ range.
  eToWord8 :: e -> Word8

  -- | Values are scaled to @[0, 65535]@ range.
  eToWord16 :: e -> Word16

  -- | Values are scaled to @[0, 4294967295]@ range.
  eToWord32 :: e -> Word32

  -- | Values are scaled to @[0, 18446744073709551615]@ range.
  eToWord64 :: e -> Word64

  -- | Values are scaled to @[0.0, 1.0]@ range.
  eToFloat :: e -> Float

  -- | Values are scaled to @[0.0, 1.0]@ range.
  eToDouble :: e -> Double

  -- | Values are scaled from @[0.0, 1.0]@ range.
  eFromDouble :: Double -> e


-- | Lower the precision
dropDown :: forall a b. (Integral a, Bounded a, Integral b, Bounded b) => a -> b
dropDown !e = fromIntegral $ fromIntegral e `div` ((maxBound :: a) `div`
                                                   fromIntegral (maxBound :: b))
{-# INLINE dropDown #-}

-- | Increase the precision
raiseUp :: forall a b. (Integral a, Bounded a, Integral b, Bounded b) => a -> b
raiseUp !e = fromIntegral e * ((maxBound :: b) `div` fromIntegral (maxBound :: a))
{-# INLINE raiseUp #-}

-- | Convert to fractional with value less than or equal to 1.
squashTo1 :: forall a b. (Fractional b, Integral a, Bounded a) => a -> b
squashTo1 !e = fromIntegral e / fromIntegral (maxBound :: a)
{-# INLINE squashTo1 #-}

-- | Convert to integral streaching it's value up to a maximum value.
stretch :: forall a b. (RealFrac a, Floating a, Integral b, Bounded b) => a -> b
stretch !e = round (fromIntegral (maxBound :: b) * clamp01 e)
{-# INLINE stretch #-}


-- | Clamp a value to @[0, 1]@ range.
clamp01 :: (Ord a, Floating a) => a -> a
clamp01 !x = min (max 0 x) 1
{-# INLINE clamp01 #-}


-- | Values between @[0, 255]]@
instance Elevator Word8 where
  eToWord8 = id
  {-# INLINE eToWord8 #-}
  eToWord16 = raiseUp
  {-# INLINE eToWord16 #-}
  eToWord32 = raiseUp
  {-# INLINE eToWord32 #-}
  eToWord64 = raiseUp
  {-# INLINE eToWord64 #-}
  eToFloat = squashTo1
  {-# INLINE eToFloat #-}
  eToDouble = squashTo1
  {-# INLINE eToDouble #-}
  eFromDouble = eToWord8
  {-# INLINE eFromDouble #-}


-- | Values between @[0, 65535]]@
instance Elevator Word16 where
  eToWord8 = dropDown
  {-# INLINE eToWord8 #-}
  eToWord16 = id
  {-# INLINE eToWord16 #-}
  eToWord32 = raiseUp
  {-# INLINE eToWord32 #-}
  eToWord64 = raiseUp
  {-# INLINE eToWord64 #-}
  eToFloat = squashTo1
  {-# INLINE eToFloat #-}
  eToDouble = squashTo1
  {-# INLINE eToDouble #-}
  eFromDouble = eToWord16
  {-# INLINE eFromDouble #-}


-- | Values between @[0, 4294967295]@
instance Elevator Word32 where
  eToWord8 = dropDown
  {-# INLINE eToWord8 #-}
  eToWord16 = dropDown
  {-# INLINE eToWord16 #-}
  eToWord32 = id
  {-# INLINE eToWord32 #-}
  eToWord64 = raiseUp
  {-# INLINE eToWord64 #-}
  eToFloat = squashTo1
  {-# INLINE eToFloat #-}
  eToDouble = squashTo1
  {-# INLINE eToDouble #-}
  eFromDouble = eToWord32
  {-# INLINE eFromDouble #-}


-- | Values between @[0, 18446744073709551615]@
instance Elevator Word64 where
  eToWord8 = dropDown
  {-# INLINE eToWord8 #-}
  eToWord16 = dropDown
  {-# INLINE eToWord16 #-}
  eToWord32 = dropDown
  {-# INLINE eToWord32 #-}
  eToWord64 = id
  {-# INLINE eToWord64 #-}
  eToFloat = squashTo1
  {-# INLINE eToFloat #-}
  eToDouble = squashTo1
  {-# INLINE eToDouble #-}
  eFromDouble = eToWord64
  {-# INLINE eFromDouble #-}

-- | Values between @[0, 18446744073709551615]@ on 64bit
instance Elevator Word where
  eToWord8 = dropDown
  {-# INLINE eToWord8 #-}
  eToWord16 = dropDown
  {-# INLINE eToWord16 #-}
  eToWord32 = dropDown
  {-# INLINE eToWord32 #-}
  eToWord64 = fromIntegral
  {-# INLINE eToWord64 #-}
  eToFloat = squashTo1
  {-# INLINE eToFloat #-}
  eToDouble = squashTo1
  {-# INLINE eToDouble #-}
  eFromDouble = stretch . clamp01
  {-# INLINE eFromDouble #-}

-- | Values between @[0, 127]@
instance Elevator Int8 where
  eToWord8 = fromIntegral . max 0
  {-# INLINE eToWord8 #-}
  eToWord16 = raiseUp . max 0
  {-# INLINE eToWord16 #-}
  eToWord32 = raiseUp . max 0
  {-# INLINE eToWord32 #-}
  eToWord64 = raiseUp . max 0
  {-# INLINE eToWord64 #-}
  eToFloat = squashTo1 . max 0
  {-# INLINE eToFloat #-}
  eToDouble = squashTo1 . max 0
  {-# INLINE eToDouble #-}
  eFromDouble = stretch . clamp01
  {-# INLINE eFromDouble #-}


-- | Values between @[0, 32767]@
instance Elevator Int16 where
  eToWord8 = dropDown . max 0
  {-# INLINE eToWord8 #-}
  eToWord16 = fromIntegral . max 0
  {-# INLINE eToWord16 #-}
  eToWord32 = raiseUp . max 0
  {-# INLINE eToWord32 #-}
  eToWord64 = raiseUp . max 0
  {-# INLINE eToWord64 #-}
  eToFloat = squashTo1 . max 0
  {-# INLINE eToFloat #-}
  eToDouble = squashTo1 . max 0
  {-# INLINE eToDouble #-}
  eFromDouble = stretch . clamp01
  {-# INLINE eFromDouble #-}


-- | Values between @[0, 2147483647]@
instance Elevator Int32 where
  eToWord8 = dropDown . max 0
  {-# INLINE eToWord8 #-}
  eToWord16 = dropDown . max 0
  {-# INLINE eToWord16 #-}
  eToWord32 = fromIntegral . max 0
  {-# INLINE eToWord32 #-}
  eToWord64 = raiseUp . max 0
  {-# INLINE eToWord64 #-}
  eToFloat = squashTo1 . max 0
  {-# INLINE eToFloat #-}
  eToDouble = squashTo1 . max 0
  {-# INLINE eToDouble #-}
  eFromDouble = stretch . clamp01
  {-# INLINE eFromDouble #-}


-- | Values between @[0, 9223372036854775807]@
instance Elevator Int64 where
  eToWord8 = dropDown . max 0
  {-# INLINE eToWord8 #-}
  eToWord16 = dropDown . max 0
  {-# INLINE eToWord16 #-}
  eToWord32 = dropDown . max 0
  {-# INLINE eToWord32 #-}
  eToWord64 = fromIntegral . max 0
  {-# INLINE eToWord64 #-}
  eToFloat = squashTo1 . max 0
  {-# INLINE eToFloat #-}
  eToDouble = squashTo1 . max 0
  {-# INLINE eToDouble #-}
  eFromDouble = stretch . clamp01
  {-# INLINE eFromDouble #-}


-- | Values between @[0, 9223372036854775807]@ on 64bit
instance Elevator Int where
  eToWord8 = dropDown . max 0
  {-# INLINE eToWord8 #-}
  eToWord16 = dropDown . max 0
  {-# INLINE eToWord16 #-}
  eToWord32 = dropDown . max 0
  {-# INLINE eToWord32 #-}
  eToWord64 = fromIntegral . max 0
  {-# INLINE eToWord64 #-}
  eToFloat = squashTo1 . max 0
  {-# INLINE eToFloat #-}
  eToDouble = squashTo1 . max 0
  {-# INLINE eToDouble #-}
  eFromDouble = stretch . clamp01
  {-# INLINE eFromDouble #-}


-- | Values between @[0.0, 1.0]@
instance Elevator Float where
  eToWord8 = stretch . clamp01
  {-# INLINE eToWord8 #-}
  eToWord16 = stretch . clamp01
  {-# INLINE eToWord16 #-}
  eToWord32 = stretch . clamp01
  {-# INLINE eToWord32 #-}
  eToWord64 = stretch . clamp01
  {-# INLINE eToWord64 #-}
  eToFloat = id
  {-# INLINE eToFloat #-}
  eToDouble = float2Double
  {-# INLINE eToDouble #-}
  eFromDouble = eToFloat
  {-# INLINE eFromDouble #-}


-- | Values between @[0.0, 1.0]@
instance Elevator Double where
  eToWord8 = stretch . clamp01
  {-# INLINE eToWord8 #-}
  eToWord16 = stretch . clamp01
  {-# INLINE eToWord16 #-}
  eToWord32 = stretch . clamp01
  {-# INLINE eToWord32 #-}
  eToWord64 = stretch . clamp01
  {-# INLINE eToWord64 #-}
  eToFloat = double2Float
  {-# INLINE eToFloat #-}
  eToDouble = id
  {-# INLINE eToDouble #-}
  eFromDouble = id
  {-# INLINE eFromDouble #-}


-- | Discards imaginary part and changes precision of real part.
instance (Num e, Elevator e, RealFloat e) => Elevator (C.Complex e) where
  eToWord8 = eToWord8 . C.realPart
  {-# INLINE eToWord8 #-}
  eToWord16 = eToWord16 . C.realPart
  {-# INLINE eToWord16 #-}
  eToWord32 = eToWord32 . C.realPart
  {-# INLINE eToWord32 #-}
  eToWord64 = eToWord64 . C.realPart
  {-# INLINE eToWord64 #-}
  eToFloat = eToFloat . C.realPart
  {-# INLINE eToFloat #-}
  eToDouble = eToDouble . C.realPart
  {-# INLINE eToDouble #-}
  eFromDouble = (C.:+ 0) . eFromDouble
  {-# INLINE eFromDouble #-}
