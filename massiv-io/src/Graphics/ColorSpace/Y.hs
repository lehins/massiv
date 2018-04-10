{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Graphics.ColorSpace.Y
-- Copyright   : (c) Alexey Kuleshevich 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.Y (
  Y(..), YA(..), Pixel(..)
  ) where

import           Data.Foldable
import           Data.Typeable                (Typeable)
import           Foreign.Ptr
import           Foreign.Storable
import           Graphics.ColorSpace.Internal
import           Prelude                      hiding (map)

---------
--- Y ---
---------

-- | Luma or brightness, which is usually denoted as @Y'@.
data Y = LumaY deriving (Eq, Enum, Show, Bounded, Typeable)


newtype instance Pixel Y e = PixelY e deriving (Ord, Eq)

instance Show e => Show (Pixel Y e) where
  show (PixelY g) = "<Luma:("++show g++")>"

instance Elevator e => ColorSpace Y e where
  type Components Y e = e
  fromComponents = PixelY
  {-# INLINE fromComponents #-}
  toComponents (PixelY y) = y
  {-# INLINE toComponents #-}
  getPxC (PixelY y) LumaY = y
  {-# INLINE getPxC #-}
  setPxC _ LumaY y = PixelY y
  {-# INLINE setPxC #-}
  mapPxC f (PixelY y) = PixelY (f LumaY y)
  {-# INLINE mapPxC #-}
  foldlPx2 f !z (PixelY y1) (PixelY y2) = f z y1 y2
  {-# INLINE foldlPx2 #-}


instance Functor (Pixel Y) where
  fmap f (PixelY y) = PixelY (f y)
  {-# INLINE fmap #-}


instance Applicative (Pixel Y) where
  pure = PixelY
  {-# INLINE pure #-}
  (PixelY fy) <*> (PixelY y) = PixelY (fy y)
  {-# INLINE (<*>) #-}


instance Foldable (Pixel Y) where
  foldr f !z (PixelY y) = f y z
  {-# INLINE foldr #-}


instance Monad (Pixel Y) where

  return = PixelY
  {-# INLINE return #-}

  (>>=) (PixelY y) f = f y
  {-# INLINE (>>=) #-}



instance Storable e => Storable (Pixel Y e) where

  sizeOf _ = sizeOf (undefined :: e)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined :: e)
  {-# INLINE alignment #-}
  peek !p = do
    let !q = castPtr p
    y <- peek q
    return (PixelY y)
  {-# INLINE peek #-}
  poke !p (PixelY y) = let !q = castPtr p in poke q y
  {-# INLINE poke #-}




----------
--- YA ---
----------

-- | Luma with Alpha channel.
data YA = LumaYA  -- ^ Luma
        | AlphaYA -- ^ Alpha channel
        deriving (Eq, Enum, Show, Bounded, Typeable)

data instance Pixel YA e = PixelYA !e !e deriving (Eq, Ord)


instance Show e => Show (Pixel YA e) where
  show (PixelYA y a) = "<LumaAlpha:("++show y++"|"++show a++")>"


instance Elevator e => ColorSpace YA e where
  type Components YA e = (e, e)
  fromComponents (y, a) = PixelYA y a
  {-# INLINE fromComponents #-}
  toComponents (PixelYA y a) = (y, a)
  {-# INLINE toComponents #-}
  getPxC (PixelYA y _)  LumaYA = y
  getPxC (PixelYA _ a) AlphaYA = a
  {-# INLINE getPxC #-}
  setPxC (PixelYA _ a) LumaYA  y = PixelYA y a
  setPxC (PixelYA y _) AlphaYA a = PixelYA y a
  {-# INLINE setPxC #-}
  mapPxC f (PixelYA y a) = PixelYA (f LumaYA y) (f AlphaYA a)
  {-# INLINE mapPxC #-}
  foldlPx2 f !z (PixelYA y1 a1) (PixelYA y2 a2) = f (f z y1 y2) a1 a2
  {-# INLINE foldlPx2 #-}


instance Elevator e => AlphaSpace YA e where
  type Opaque YA = Y

  getAlpha (PixelYA _ a) = a
  {-# INLINE getAlpha  #-}
  addAlpha !a (PixelY y) = PixelYA y a
  {-# INLINE addAlpha #-}
  dropAlpha (PixelYA y _) = PixelY y
  {-# INLINE dropAlpha #-}


instance Functor (Pixel YA) where
  fmap f (PixelYA y a) = PixelYA (f y) (f a)
  {-# INLINE fmap #-}


instance Applicative (Pixel YA) where
  pure !e = PixelYA e e
  {-# INLINE pure #-}
  (PixelYA fy fa) <*> (PixelYA y a) = PixelYA (fy y) (fa a)
  {-# INLINE (<*>) #-}


instance Foldable (Pixel YA) where
  foldr f !z (PixelYA y a) = f y (f a z)
  {-# INLINE foldr #-}


instance Storable e => Storable (Pixel YA e) where

  sizeOf _ = 2 * sizeOf (undefined :: e)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined :: e)
  {-# INLINE alignment #-}
  peek !p = do
    q <- return $ castPtr p
    y <- peekElemOff q 0
    a <- peekElemOff q 1
    return (PixelYA y a)
  {-# INLINE peek #-}
  poke !p (PixelYA y a) = do
    q <- return $ castPtr p
    pokeElemOff q 0 y
    pokeElemOff q 1 a
  {-# INLINE poke #-}

