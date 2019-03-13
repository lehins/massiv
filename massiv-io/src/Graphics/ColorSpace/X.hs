{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
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

import           Data.Foldable
import           Data.Typeable                (Typeable)
import           Foreign.Ptr
import           Foreign.Storable
import           Graphics.ColorSpace.Internal
import           Prelude                      as P

-- ^ This is a single channel colorspace, that is designed to separate Gray
-- level values from other types of colorspace, hence it is not convertible to
-- or from, but rather is here to allow operation on arbirtary single channel
-- images. If you are looking for a true grayscale colorspace
-- 'Graphics.ColorSpace.Luma.Y' should be used instead.
data X = X deriving (Eq, Enum, Bounded, Show, Typeable)


newtype instance Pixel X e = PixelX { getX :: e } deriving (Ord, Eq)


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
  setPxC (PixelX _) X g = PixelX g
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



-- -- | Apply a left fold to each of the pixels in the image.
-- squashWith :: (Array arr cs e, Array arr X b) =>
--               (b -> e -> b) -> b -> Image arr cs e -> Image arr X b
-- squashWith f !a = I.map (PixelX . foldlPx f a) where
-- {-# INLINE squashWith #-}


-- -- | Combination of zipWith and simultanious left fold on two pixels at the same time.
-- squashWith2 :: (Array arr cs e, Array arr X b) =>
--                (b -> e -> e -> b) -> b -> Image arr cs e -> Image arr cs e -> Image arr X b
-- squashWith2 f !a = I.zipWith (PixelX .:! foldlPx2 f a) where
-- {-# INLINE squashWith2 #-}


-- -- | Separate an image into a list of images with 'X' pixels containing every
-- -- channel from the source image.
-- --
-- -- >>> frog <- readImageRGB "images/frog.jpg"
-- -- >>> let [frog_red, frog_green, frog_blue] = toImagesX frog
-- -- >>> writeImage "images/frog_red.png" $ toImageY frog_red
-- -- >>> writeImage "images/frog_green.jpg" $ toImageY frog_green
-- -- >>> writeImage "images/frog_blue.jpg" $ toImageY frog_blue
-- --
-- -- <<images/frog_red.jpg>> <<images/frog_green.jpg>> <<images/frog_blue.jpg>>
-- --
-- toImagesX :: (Array arr cs e, Array arr X e) => Image arr cs e -> [Image arr X e]
-- toImagesX !img = P.map getCh (enumFrom minBound) where
--   getCh !ch = I.map (PixelX . (`getPxC` ch)) img
--   {-# INLINE getCh #-}
-- {-# INLINE toImagesX #-}


-- -- | Combine a list of images with 'X' pixels into an image of any color
-- -- space, by supplying an order of color space channels.
-- --
-- -- For example here is a frog with swapped 'BlueRGB' and 'GreenRGB' channels.
-- --
-- -- >>> writeImage "images/frog_rbg.jpg" $ fromImagesX [(RedRGB, frog_red), (BlueRGB, frog_green), (GreenRGB, frog_blue)]
-- --
-- -- <<images/frog.jpg>> <<images/frog_rbg.jpg>>
-- --
-- -- It is worth noting though, despite that separating image channels can be
-- -- sometimes pretty useful, exactly the same effect as in example above can be
-- -- achieved in a much simpler and a more efficient way:
-- --
-- -- @ `I.map` (\\(PixelRGB r g b) -> PixelRGB r b g) frog @
-- --
-- fromImagesX :: (Array arr X e, Array arr cs e) =>
--                [(cs, Image arr X e)] -> Image arr cs e
-- fromImagesX = fromXs 0 where
--   updateCh !ch !px (PixelX e) = setPxC px ch e
--   {-# INLINE updateCh #-}
--   fromXs img []          = img
--   fromXs img ((c, i):xs) = fromXs (I.zipWith (updateCh c) img i) xs
--   {-# INLINE fromXs #-}
-- {-# INLINE fromImagesX #-}
