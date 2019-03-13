{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Graphics.ColorSpace.HSI
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.HSI (
  HSI(..), HSIA(..), Pixel(..)
  ) where

import           Data.Foldable
import           Data.Typeable                (Typeable)
import           Foreign.Ptr
import           Foreign.Storable
import           Graphics.ColorSpace.Internal
import           Prelude                      hiding (map)

-----------
--- HSI ---
-----------

-- | Hue, Saturation and Intensity color space.
data HSI = HueHSI -- ^ Hue
         | SatHSI -- ^ Saturation
         | IntHSI -- ^ Intensity
         deriving (Eq, Enum, Show, Bounded, Typeable)

data instance Pixel HSI e = PixelHSI !e !e !e deriving (Eq, Ord)


instance Show e => Show (Pixel HSI e) where
  show (PixelHSI h s i) = "<HSI:("++show h++"|"++show s++"|"++show i++")>"


instance Elevator e => ColorSpace HSI e where
  type Components HSI e = (e, e, e)

  toComponents (PixelHSI h s i) = (h, s, i)
  {-# INLINE toComponents #-}
  fromComponents (h, s, i) = PixelHSI h s i
  {-# INLINE fromComponents #-}
  getPxC (PixelHSI h _ _) HueHSI = h
  getPxC (PixelHSI _ s _) SatHSI = s
  getPxC (PixelHSI _ _ i) IntHSI = i
  {-# INLINE getPxC #-}
  setPxC (PixelHSI _ s i) HueHSI h = PixelHSI h s i
  setPxC (PixelHSI h _ i) SatHSI s = PixelHSI h s i
  setPxC (PixelHSI h s _) IntHSI i = PixelHSI h s i
  {-# INLINE setPxC #-}
  mapPxC f (PixelHSI h s i) = PixelHSI (f HueHSI h) (f SatHSI s) (f IntHSI i)
  {-# INLINE mapPxC #-}
  foldlPx = foldl'
  {-# INLINE foldlPx #-}
  foldlPx2 f !z (PixelHSI h1 s1 i1) (PixelHSI h2 s2 i2) =
    f (f (f z h1 h2) s1 s2) i1 i2
  {-# INLINE foldlPx2 #-}


instance Functor (Pixel HSI) where
  fmap f (PixelHSI h s i) = PixelHSI (f h) (f s) (f i)
  {-# INLINE fmap #-}


instance Applicative (Pixel HSI) where
  pure !e = PixelHSI e e e
  {-# INLINE pure #-}
  (PixelHSI fh fs fi) <*> (PixelHSI h s i) = PixelHSI (fh h) (fs s) (fi i)
  {-# INLINE (<*>) #-}


instance Foldable (Pixel HSI) where
  foldr f !z (PixelHSI h s i) = f h (f s (f i z))
  {-# INLINE foldr #-}


instance Storable e => Storable (Pixel HSI e) where

  sizeOf _ = 3 * sizeOf (undefined :: e)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined :: e)
  {-# INLINE alignment #-}
  peek !p = do
    let !q = castPtr p
    r <- peek q
    g <- peekElemOff q 1
    b <- peekElemOff q 2
    return (PixelHSI r g b)
  {-# INLINE peek #-}
  poke !p (PixelHSI r g b) = do
    let !q =  castPtr p
    poke q r
    pokeElemOff q 1 g
    pokeElemOff q 2 b
  {-# INLINE poke #-}

------------
--- HSIA ---
------------

-- | Hue, Saturation and Intensity color space with Alpha channel.
data HSIA = HueHSIA   -- ^ Hue
          | SatHSIA   -- ^ Saturation
          | IntHSIA   -- ^ Intensity
          | AlphaHSIA -- ^ Alpha
          deriving (Eq, Enum, Show, Bounded, Typeable)


data instance Pixel HSIA e = PixelHSIA !e !e !e !e deriving (Eq, Ord)


instance Show e => Show (Pixel HSIA e) where
  show (PixelHSIA h s i a) = "<HSIA:("++show h++"|"++show s++"|"++show i++"|"++show a++")>"


instance Elevator e => ColorSpace HSIA e where
  type Components HSIA e = (e, e, e, e)

  toComponents (PixelHSIA h s i a) = (h, s, i, a)
  {-# INLINE toComponents #-}
  fromComponents (h, s, i, a) = PixelHSIA h s i a
  {-# INLINE fromComponents #-}
  getPxC (PixelHSIA h _ _ _) HueHSIA   = h
  getPxC (PixelHSIA _ s _ _) SatHSIA   = s
  getPxC (PixelHSIA _ _ i _) IntHSIA   = i
  getPxC (PixelHSIA _ _ _ a) AlphaHSIA = a
  {-# INLINE getPxC #-}
  setPxC (PixelHSIA _ s i a) HueHSIA h   = PixelHSIA h s i a
  setPxC (PixelHSIA h _ i a) SatHSIA s   = PixelHSIA h s i a
  setPxC (PixelHSIA h s _ a) IntHSIA i   = PixelHSIA h s i a
  setPxC (PixelHSIA h s i _) AlphaHSIA a = PixelHSIA h s i a
  {-# INLINE setPxC #-}
  mapPxC f (PixelHSIA h s i a) =
    PixelHSIA (f HueHSIA h) (f SatHSIA s) (f IntHSIA i) (f AlphaHSIA a)
  {-# INLINE mapPxC #-}
  foldlPx2 f !z (PixelHSIA h1 s1 i1 a1) (PixelHSIA h2 s2 i2 a2) =
    f (f (f (f z h1 h2) s1 s2) i1 i2) a1 a2
  {-# INLINE foldlPx2 #-}


instance Elevator e => AlphaSpace HSIA e where
  type Opaque HSIA = HSI

  getAlpha (PixelHSIA _ _ _ a) = a
  {-# INLINE getAlpha #-}
  addAlpha !a (PixelHSI h s i) = PixelHSIA h s i a
  {-# INLINE addAlpha #-}
  dropAlpha (PixelHSIA h s i _) = PixelHSI h s i
  {-# INLINE dropAlpha #-}


instance Functor (Pixel HSIA) where
  fmap f (PixelHSIA h s i a) = PixelHSIA (f h) (f s) (f i) (f a)
  {-# INLINE fmap #-}


instance Applicative (Pixel HSIA) where
  pure !e = PixelHSIA e e e e
  {-# INLINE pure #-}
  (PixelHSIA fh fs fi fa) <*> (PixelHSIA h s i a) = PixelHSIA (fh h) (fs s) (fi i) (fa a)
  {-# INLINE (<*>) #-}


instance Foldable (Pixel HSIA) where
  foldr f !z (PixelHSIA h s i a) = f h (f s (f i (f a z)))
  {-# INLINE foldr #-}


instance Storable e => Storable (Pixel HSIA e) where
  sizeOf _ = 4 * sizeOf (undefined :: e)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined :: e)
  {-# INLINE alignment #-}
  peek !p = do
    let !q = castPtr p
    h <- peek q
    s <- peekElemOff q 1
    i <- peekElemOff q 2
    a <- peekElemOff q 3
    return (PixelHSIA h s i a)
  {-# INLINE peek #-}
  poke !p (PixelHSIA h s i a) = do
    let !q = castPtr p
    poke q h
    pokeElemOff q 1 s
    pokeElemOff q 2 i
    pokeElemOff q 3 a
  {-# INLINE poke #-}
