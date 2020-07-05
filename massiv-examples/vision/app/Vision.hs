{-# LANGUAGE DataKinds #-}

module Main where

import Data.Massiv.Array
import Data.Massiv.Array.IO


type SRGBPixel = (SRGB 'Linear)

identity :: Elevator a => Stencil Ix2 (Pixel SRGBPixel a) (Pixel SRGBPixel a)
identity =
  makeStencil sz c $ \get -> get (0 :. 0)
  where
    c = 1 :. 1
    sz = Sz (3 :. 3)
{-# INLINE identity #-}

box :: (Elevator a, Fractional a) => Stencil Ix2 (Pixel SRGBPixel a) (Pixel SRGBPixel a)
box =
  makeStencil sz c $ \get ->
    ( get  (-1 :. -1) + get  (-1 :. 0) + get (-1 :. 1)
    + get  (0 :. -1) + get  (0 :. 0) + get (0 :. 1)
    + get  (1 :. -1) + get  (1 :. 0) + get (1 :. 1)) / 9
  where
    c = 1 :. 1
    sz = Sz (3 :. 3)
{-# INLINE box #-}

main :: IO ()
main = do
  frog <- readImageAuto "files/frog.jpg" :: IO (Image S SRGBPixel Double)

  -- Identity transformation
  writeImageAuto "files/frog_clone.png" $ computeAs S $ mapStencil Edge identity frog
  -- Box filtering (blur)
  writeImageAuto "files/frog_blurred.png" $ computeAs S $ mapStencil Edge box frog

  -- Illustrating how strides work
  -- Simple downsampling leads to aliasing
  writeImageAuto "files/frog_small0.png" $ computeWithStrideAs S (Stride (2 :. 2)) $ mapStencil Edge identity frog
  -- Downsampling after box filtering
  writeImageAuto "files/frog_small1.png" $ computeWithStrideAs S (Stride (2 :. 2)) $ mapStencil Edge box frog
