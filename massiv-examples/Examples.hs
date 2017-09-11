#!/usr/bin/env stack
-- stack --install-ghc exec --package massiv --package massiv-io --package data-default -- ghc -O2 -threaded -rtsopts -with-rtsopts=-N
module Main where


import Data.Array.Massiv as M
import Data.Array.Massiv.Stencil
import Data.Array.Massiv.IO
import Data.Default
import Graphics.ColorSpace


arrLightIx2 :: Comp -> Ix2 -> Array D Ix2 Double
arrLightIx2 comp arrSz = makeArray comp arrSz lightFunc
    where lightFunc (i :. j) = sin (fromIntegral (i ^ (2 :: Int) + j ^ (2 :: Int)) :: Double)
{-# INLINE arrLightIx2 #-}

average3x3Filter :: (Default a, Fractional a) => Border a -> Stencil Ix2 a a
average3x3Filter b = makeStencil b (3 :. 3) (1 :. 1) $ \ get ->
  (  get (-1 :. -1) + get (-1 :. 0) + get (-1 :. 1) +
     get ( 0 :. -1) + get ( 0 :. 0) + get ( 0 :. 1) +
     get ( 1 :. -1) + get ( 1 :. 0) + get ( 1 :. 1)   ) / 9
{-# INLINE average3x3Filter #-}

sum3x3Filter :: (Default a, Fractional a) => Border a -> Stencil Ix2 a a
sum3x3Filter b = mkConvolutionStencil b (3 :. 3) (1 :. 1) $ \ get ->
  get (-1 :. -1) 1 . get (-1 :. 0) 1 . get (-1 :. 1) 1 .
  get ( 0 :. -1) 1 . get ( 0 :. 0) 1 . get ( 0 :. 1) 1 .
  get ( 1 :. -1) 1 . get ( 1 :. 0) 1 . get ( 1 :. 1) 1
{-# INLINE sum3x3Filter #-}


main :: IO ()
main = do
  let arr = arrLightIx2 Par (600 :. 800)
      img = computeAs S $ fmap PixelY arr
  writeImage "files/light.png" img
  writeImage "files/light_avg.png" $ computeAs S $ mapStencil (average3x3Filter Edge) img
