{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.ColorSpace.RGB
-- Copyright   : (c) Alexey Kuleshevich 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.RGB (
  RGB(..), RGBA(..), Pixel(..)
  ) where

import Prelude hiding (map)
import Foreign.Ptr
import Foreign.Storable
import Data.Typeable (Typeable)
import Graphics.ColorSpace.Internal

-----------
--- RGB ---
-----------

-- | Red, Green and Blue color space.
data RGB = RedRGB
         | GreenRGB
         | BlueRGB deriving (Eq, Enum, Show, Bounded, Typeable)


data instance Pixel RGB e = PixelRGB !e !e !e deriving (Eq, Ord)

instance Show e => Show (Pixel RGB e) where
  show (PixelRGB r g b) = "<RGB:("++show r++"|"++show g++"|"++show b++")>"


instance Elevator e => ColorSpace RGB e where
  type Components RGB e = (e, e, e)

  toComponents (PixelRGB r g b) = (r, g, b)
  {-# INLINE toComponents #-}
  fromComponents !(r, g, b) = PixelRGB r g b
  {-# INLINE fromComponents #-}
  getPxC (PixelRGB r _ _) RedRGB   = r
  getPxC (PixelRGB _ g _) GreenRGB = g
  getPxC (PixelRGB _ _ b) BlueRGB  = b
  {-# INLINE getPxC #-}
  setPxC (PixelRGB _ g b) RedRGB   r = PixelRGB r g b
  setPxC (PixelRGB r _ b) GreenRGB g = PixelRGB r g b
  setPxC (PixelRGB r g _) BlueRGB  b = PixelRGB r g b
  {-# INLINE setPxC #-}
  mapPxC f (PixelRGB r g b) = PixelRGB (f RedRGB r) (f GreenRGB g) (f BlueRGB b)
  {-# INLINE mapPxC #-}
  foldlPx2 f !z (PixelRGB r1 g1 b1) (PixelRGB r2 g2 b2) =
    f (f (f z r1 r2) g1 g2) b1 b2
  {-# INLINE foldlPx2 #-}


instance Functor (Pixel RGB) where
  fmap f (PixelRGB r g b) = PixelRGB (f r) (f g) (f b)
  {-# INLINE fmap #-}


instance Applicative (Pixel RGB) where
  pure !e = PixelRGB e e e
  {-# INLINE pure #-}
  (PixelRGB fr fg fb) <*> (PixelRGB r g b) = PixelRGB (fr r) (fg g) (fb b)
  {-# INLINE (<*>) #-}


instance Foldable (Pixel RGB) where
  foldr f !z (PixelRGB r g b) = f r (f g (f b z))
  {-# INLINE foldr #-}


instance Storable e => Storable (Pixel RGB e) where
  sizeOf _ = 3 * sizeOf (undefined :: e)
  alignment _ = alignment (undefined :: e)
  peek !p = do
    let !q = castPtr p
    r <- peek q
    g <- peekElemOff q 1
    b <- peekElemOff q 2
    return (PixelRGB r g b)
  poke !p (PixelRGB r g b) = do
    let !q = castPtr p
    poke q r
    pokeElemOff q 1 g
    pokeElemOff q 2 b


------------
--- RGBA ---
------------


-- | Red, Green and Blue color space with Alpha channel.
data RGBA = RedRGBA
          | GreenRGBA
          | BlueRGBA
          | AlphaRGBA deriving (Eq, Enum, Show, Bounded, Typeable)

data instance Pixel RGBA e = PixelRGBA !e !e !e !e deriving (Eq, Ord)


instance Show e => Show (Pixel RGBA e) where
  show (PixelRGBA r g b a) = "<RGBA:("++show r++"|"++show g++"|"++show b++"|"++show a++")>"


instance Elevator e => ColorSpace RGBA e where
  type Components RGBA e = (e, e, e, e)

  toComponents (PixelRGBA r g b a) = (r, g, b, a)
  {-# INLINE toComponents #-}
  fromComponents !(r, g, b, a) = PixelRGBA r g b a
  {-# INLINE fromComponents #-}
  getPxC (PixelRGBA r _ _ _) RedRGBA   = r
  getPxC (PixelRGBA _ g _ _) GreenRGBA = g
  getPxC (PixelRGBA _ _ b _) BlueRGBA  = b
  getPxC (PixelRGBA _ _ _ a) AlphaRGBA = a
  {-# INLINE getPxC #-}
  setPxC (PixelRGBA _ g b a) RedRGBA   r = PixelRGBA r g b a
  setPxC (PixelRGBA r _ b a) GreenRGBA g = PixelRGBA r g b a
  setPxC (PixelRGBA r g _ a) BlueRGBA  b = PixelRGBA r g b a
  setPxC (PixelRGBA r g b _) AlphaRGBA a = PixelRGBA r g b a
  {-# INLINE setPxC #-}
  mapPxC f (PixelRGBA r g b a) =
    PixelRGBA (f RedRGBA r) (f GreenRGBA g) (f BlueRGBA b) (f AlphaRGBA a)
  {-# INLINE mapPxC #-}
  foldlPx2 f !z (PixelRGBA r1 g1 b1 a1) (PixelRGBA r2 g2 b2 a2) =
    f (f (f (f z r1 r2) g1 g2) b1 b2) a1 a2
  {-# INLINE foldlPx2 #-}


instance Elevator e => AlphaSpace RGBA e where
  type Opaque RGBA = RGB

  getAlpha (PixelRGBA _ _ _ a) = a
  {-# INLINE getAlpha #-}
  addAlpha !a (PixelRGB r g b) = PixelRGBA r g b a
  {-# INLINE addAlpha #-}
  dropAlpha (PixelRGBA r g b _) = PixelRGB r g b
  {-# INLINE dropAlpha #-}


instance Functor (Pixel RGBA) where
  fmap f (PixelRGBA r g b a) = PixelRGBA (f r) (f g) (f b) (f a)
  {-# INLINE fmap #-}

instance Applicative (Pixel RGBA) where
  pure !e = PixelRGBA e e e e
  {-# INLINE pure #-}
  (PixelRGBA fr fg fb fa) <*> (PixelRGBA r g b a) = PixelRGBA (fr r) (fg g) (fb b) (fa a)
  {-# INLINE (<*>) #-}

instance Foldable (Pixel RGBA) where
  foldr f !z (PixelRGBA r g b a) = f r (f g (f b (f a z)))
  {-# INLINE foldr #-}


instance Storable e => Storable (Pixel RGBA e) where
  sizeOf _ = 4 * sizeOf (undefined :: e)
  alignment _ = alignment (undefined :: e)
  peek p = do
    q <- return $ castPtr p
    r <- peek q
    g <- peekElemOff q 1
    b <- peekElemOff q 2
    a <- peekElemOff q 3
    return (PixelRGBA r g b a)
  poke p (PixelRGBA r g b a) = do
    q <- return $ castPtr p
    poke q r
    pokeElemOff q 1 g
    pokeElemOff q 2 b
    pokeElemOff q 3 a
