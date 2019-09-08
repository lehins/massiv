{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.ColorSpace.X
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.X
  ( X(..)
  , Pixel(..)
  , toPixelsX
  , fromPixelsX
  ) where

import Data.Bits (Bits)
import Data.Foldable
import Data.Typeable (Typeable)
import Foreign.Ptr
import Foreign.Storable
import Graphics.ColorSpace.Internal
import Prelude as P

-- | This is a single channel colorspace, that is designed to separate Gray
-- level values from other types of colorspace, hence it is not convertible to
-- or from, but rather is here to allow operation on arbirtary single channel
-- images. If you are looking for the actual grayscale color space.
-- 'Graphics.ColorSpace.Luma.Y' should be used instead.
data X = X deriving (Eq, Enum, Bounded, Show, Typeable)


newtype instance  Pixel X e = PixelX
  { getX :: e
  } deriving (Ord, Eq, Enum, Bounded, Real, Integral, RealFrac, RealFloat, Bits)


instance Show e => Show (Pixel X e) where
  show (PixelX g) = "<X:("++show g++")>"


instance Elevator e => ColorSpace X e where
  type Components X e = e

  fromComponents = PixelX
  {-# INLINE fromComponents #-}
  toComponents (PixelX g) = g
  {-# INLINE toComponents #-}
  getPxC (PixelX g) X = g
  {-# INLINE getPxC #-}
  setPxC (PixelX _) X = PixelX
  {-# INLINE setPxC #-}
  mapPxC f (PixelX g) = PixelX (f X g)
  {-# INLINE mapPxC #-}
  foldlPx2 f !z (PixelX g1) (PixelX g2) = f z g1 g2
  {-# INLINE foldlPx2 #-}


instance Functor (Pixel X) where
  fmap f (PixelX g) = PixelX (f g)
  {-# INLINE fmap #-}


instance Applicative (Pixel X) where
  pure = PixelX
  {-# INLINE pure #-}
  (PixelX fg) <*> (PixelX g) = PixelX (fg g)
  {-# INLINE (<*>) #-}


instance Foldable (Pixel X) where
  foldr f !z (PixelX g) = f g z
  {-# INLINE foldr #-}


instance Traversable (Pixel X) where
  traverse f (PixelX x) = PixelX <$> f x
  {-# INLINE traverse #-}


instance Monad (Pixel X) where
  return = PixelX
  {-# INLINE return #-}
  (>>=) (PixelX g) f = f g
  {-# INLINE (>>=) #-}


instance Storable e => Storable (Pixel X e) where

  sizeOf _ = sizeOf (undefined :: e)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined :: e)
  {-# INLINE alignment #-}
  peek !p = do
    let q = castPtr p
    g <- peek q
    return (PixelX g)
  {-# INLINE peek #-}
  poke !p (PixelX g) = do
    let q = castPtr p
    poke q g
  {-# INLINE poke #-}


-- | Separate a Pixel into a list of components with 'X' pixels containing every
-- component from the pixel.
--
-- >>> toPixelsX (PixelRGB 4 5 6)
-- [<X:(4)>,<X:(5)>,<X:(6)>]
--
toPixelsX :: ColorSpace cs e => Pixel cs e -> [Pixel X e]
toPixelsX = fmap PixelX . toList

-- | Combine a list of `X` pixels into a Pixel with a specified channel
-- order. Not the most efficient way to construct a pixel, but might prove
-- useful to someone.
--
-- >>> fromPixelsX [(RedRGB, 3), (BlueRGB, 5), (GreenRGB, 4)]
-- <RGB:(3.0|4.0|5.0)>
-- >>> fromPixelsX $ zip (enumFrom RedRGB) (toPixelsX $ PixelRGB 4 5 6)
-- <RGB:(4.0|5.0|6.0)>
--
fromPixelsX :: ColorSpace cs e => [(cs, Pixel X e)] -> Pixel cs e
fromPixelsX = foldl' f (pure 0) where
  f !px (c, PixelX x) = setPxC px c x
