{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Graphics.ColorSpace.Internal
-- Copyright   : (c) Alexey Kuleshevich 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.Internal
  ( Pixel
  , ColorSpace(..)
  , AlphaSpace(..)
  , module Graphics.ColorSpace.Elevator
  ) where

import           Control.Applicative
import           Control.DeepSeq              (NFData (rnf), deepseq)
import           Control.Monad                (liftM)
import           Data.Default
import           Data.Foldable
import           Data.Typeable
import qualified Data.Vector.Generic          as V
import qualified Data.Vector.Generic.Mutable  as VM
import qualified Data.Vector.Storable         as VS
import qualified Data.Vector.Unboxed          as VU
import           Graphics.ColorSpace.Elevator


-- | A Pixel family with a color space and a precision of elements.
data family Pixel cs e :: *

class (Eq cs, Enum cs, Show cs, Bounded cs, Typeable cs,
       Functor (Pixel cs), Applicative (Pixel cs), Foldable (Pixel cs),
       Eq (Pixel cs e), VU.Unbox (Components cs e), VS.Storable (Pixel cs e), Elevator e)
      => ColorSpace cs e where

  type Components cs e

  -- | Convert a Pixel to a representation suitable for storage as an unboxed
  -- element, usually a tuple of channels.
  toComponents :: Pixel cs e -> Components cs e

  -- | Convert from an elemnt representation back to a Pixel.
  fromComponents :: Components cs e -> Pixel cs e

  -- | Construt a Pixel by replicating the same value across all of the components.
  promote :: e -> Pixel cs e
  promote = pure
  {-# INLINE promote #-}

  -- | Retrieve Pixel's component value
  getPxC :: Pixel cs e -> cs -> e

  -- | Set Pixel's component value
  setPxC :: Pixel cs e -> cs -> e -> Pixel cs e

  -- | Map a channel aware function over all Pixel's components.
  mapPxC :: (cs -> e -> e) -> Pixel cs e -> Pixel cs e

  -- | Left fold on two pixels a the same time. If accumulator is nutrual to the folding funciton
  -- then it's equivalent to @foldlPx2 f acc px1 px2 == foldl' acc (liftA2 (f acc) px1 px2)@
  foldlPx2 :: (b -> e -> e -> b) -> b -> Pixel cs e -> Pixel cs e -> b

  -- | Map a function over all Pixel's componenets.
  liftPx :: (e -> e) -> Pixel cs e -> Pixel cs e
  liftPx = fmap
  {-# INLINE liftPx #-}

  -- | Zip two Pixels with a function.
  liftPx2 :: (e -> e -> e) -> Pixel cs e -> Pixel cs e -> Pixel cs e
  liftPx2 = liftA2
  {-# INLINE liftPx2 #-}

  -- | Right fold over all Pixel's components.
  foldrPx :: (e -> b -> b) -> b -> Pixel cs e -> b
  foldrPx = foldr'
  {-# INLINE foldrPx #-}

  -- | Left strict fold over all Pixel's components.
  foldlPx :: (b -> e -> b) -> b -> Pixel cs e -> b
  foldlPx = foldl'
  {-# INLINE foldlPx #-}

  foldl1Px :: (e -> e -> e) -> Pixel cs e -> e
  foldl1Px = foldl1
  {-# INLINE foldl1Px #-}

{-# DEPRECATED liftPx "Use `fmap` from `Functor` instead" #-}
{-# DEPRECATED liftPx2 "Use `liftA2` from `Applicative` instead" #-}
{-# DEPRECATED promote "Use `pure` from `Applicative` instead" #-}
{-# DEPRECATED foldlPx "Use `foldl'` from `Foldable` instead" #-}
{-# DEPRECATED foldrPx "Use `foldr'` from `Foldable` instead" #-}
{-# DEPRECATED foldl1Px "Use `foldl1` from `Foldable` instead" #-}

-- | A color space that supports transparency.
class (ColorSpace (Opaque cs) e, ColorSpace cs e) => AlphaSpace cs e where
  -- | A corresponding opaque version of this color space.
  type Opaque cs

  -- | Get an alpha channel of a transparant pixel.
  getAlpha :: Pixel cs e -> e

  -- | Add an alpha channel to an opaque pixel.
  --
  -- @ addAlpha 0 (PixelHSI 1 2 3) == PixelHSIA 1 2 3 0 @
  addAlpha :: e -> Pixel (Opaque cs) e -> Pixel cs e

  -- | Convert a transparent pixel to an opaque one by dropping the alpha
  -- channel.
  --
  -- @ dropAlpha (PixelRGBA 1 2 3 4) == PixelRGB 1 2 3 @
  --
  dropAlpha :: Pixel cs e -> Pixel (Opaque cs) e

instance ColorSpace cs e => Default (Pixel cs e) where

  def = promote 0
  {-# INLINE def #-}


instance ColorSpace cs e => Num (Pixel cs e) where
  (+)         = liftA2 (+)
  {-# INLINE (+) #-}
  (-)         = liftA2 (-)
  {-# INLINE (-) #-}
  (*)         = liftA2 (*)
  {-# INLINE (*) #-}
  abs         = liftA abs
  {-# INLINE abs #-}
  signum      = liftA signum
  {-# INLINE signum #-}
  fromInteger = promote . fromInteger
  {-# INLINE fromInteger #-}


instance (ColorSpace cs e, Fractional e) => Fractional (Pixel cs e) where
  (/)          = liftA2 (/)
  {-# INLINE (/) #-}
  recip        = liftA recip
  {-# INLINE recip #-}
  fromRational = promote . fromRational
  {-# INLINE fromRational #-}


instance (ColorSpace cs e, Floating e) => Floating (Pixel cs e) where
  pi      = promote pi
  {-# INLINE pi #-}
  exp     = liftA exp
  {-# INLINE exp #-}
  log     = liftA log
  {-# INLINE log #-}
  sin     = liftA sin
  {-# INLINE sin #-}
  cos     = liftA cos
  {-# INLINE cos #-}
  asin    = liftA asin
  {-# INLINE asin #-}
  atan    = liftA atan
  {-# INLINE atan #-}
  acos    = liftA acos
  {-# INLINE acos #-}
  sinh    = liftA sinh
  {-# INLINE sinh #-}
  cosh    = liftA cosh
  {-# INLINE cosh #-}
  asinh   = liftA asinh
  {-# INLINE asinh #-}
  atanh   = liftA atanh
  {-# INLINE atanh #-}
  acosh   = liftA acosh
  {-# INLINE acosh #-}

instance (ColorSpace cs e, Bounded e) => Bounded (Pixel cs e) where
  maxBound = promote maxBound
  {-# INLINE maxBound #-}
  minBound = promote minBound
  {-# INLINE minBound #-}

instance (ColorSpace cs e, NFData e) => NFData (Pixel cs e) where

  rnf = foldr' deepseq ()
  {-# INLINE rnf #-}


-- | Unboxing of a `Pixel`.
instance ColorSpace cs e => VU.Unbox (Pixel cs e)

newtype instance VU.MVector s (Pixel cs e) = MV_Pixel (VU.MVector s (Components cs e))

instance ColorSpace cs e => VM.MVector VU.MVector (Pixel cs e) where
  basicLength (MV_Pixel mvec) = VM.basicLength mvec
  {-# INLINE basicLength #-}
  basicUnsafeSlice idx len (MV_Pixel mvec) = MV_Pixel (VM.basicUnsafeSlice idx len mvec)
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_Pixel mvec) (MV_Pixel mvec') = VM.basicOverlaps mvec mvec'
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew len = MV_Pixel `liftM` VM.basicUnsafeNew len
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeReplicate len val = MV_Pixel `liftM` VM.basicUnsafeReplicate len (toComponents val)
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_Pixel mvec) idx = fromComponents `liftM` VM.basicUnsafeRead mvec idx
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_Pixel mvec) idx val = VM.basicUnsafeWrite mvec idx (toComponents val)
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_Pixel mvec) = VM.basicClear mvec
  {-# INLINE basicClear #-}
  basicSet (MV_Pixel mvec) val = VM.basicSet mvec (toComponents val)
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV_Pixel mvec) (MV_Pixel mvec') = VM.basicUnsafeCopy mvec mvec'
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_Pixel mvec) (MV_Pixel mvec') = VM.basicUnsafeMove mvec mvec'
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_Pixel mvec) len = MV_Pixel `liftM` VM.basicUnsafeGrow mvec len
  {-# INLINE basicUnsafeGrow #-}
#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_Pixel mvec) = VM.basicInitialize mvec
  {-# INLINE basicInitialize #-}
#endif


newtype instance VU.Vector (Pixel cs e) = V_Pixel (VU.Vector (Components cs e))

instance (ColorSpace cs e) => V.Vector VU.Vector (Pixel cs e) where
  basicUnsafeFreeze (MV_Pixel mvec) = V_Pixel `liftM` V.basicUnsafeFreeze mvec
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_Pixel vec) = MV_Pixel `liftM` V.basicUnsafeThaw vec
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_Pixel vec) = V.basicLength vec
  {-# INLINE basicLength #-}
  basicUnsafeSlice idx len (V_Pixel vec) = V_Pixel (V.basicUnsafeSlice idx len vec)
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_Pixel vec) idx = fromComponents `liftM` V.basicUnsafeIndexM vec idx
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_Pixel mvec) (V_Pixel vec) = V.basicUnsafeCopy mvec vec
  {-# INLINE basicUnsafeCopy #-}
  elemseq (V_Pixel vec) val = V.elemseq vec (toComponents val)
  {-# INLINE elemseq #-}
