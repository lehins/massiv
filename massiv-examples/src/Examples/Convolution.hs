module Examples.Convolution
    ( arrLightIx2
    , average3x3Filter
    , sum3x3Filter
    ) where

import           Data.Default
import           Data.Massiv.Array
import           Data.Massiv.Array.IO
import           Data.Massiv.Array.Stencil


arrLightIx2 :: Comp -> Ix2 -> Array D Ix2 Double
arrLightIx2 comp arrSz = makeArray comp (Sz arrSz) lightFunc
    where lightFunc (i :. j) = sin (fromIntegral (i ^ (2 :: Int) + j ^ (2 :: Int)) :: Double)
{-# INLINE arrLightIx2 #-}


average3x3Filter :: (Default a, Fractional a) => Stencil Ix2 a a
average3x3Filter = makeStencil (3 :. 3) (1 :. 1) $ \ get ->
  (  get (-1 :. -1) + get (-1 :. 0) + get (-1 :. 1) +
     get ( 0 :. -1) + get ( 0 :. 0) + get ( 0 :. 1) +
     get ( 1 :. -1) + get ( 1 :. 0) + get ( 1 :. 1)   ) / 9
{-# INLINE average3x3Filter #-}


sum3x3Filter :: Fractional a => Stencil Ix2 a a
sum3x3Filter = makeConvolutionStencil (3 :. 3) (1 :. 1) $ \ get ->
  get (-1 :. -1) 1 . get (-1 :. 0) 1 . get (-1 :. 1) 1 .
  get ( 0 :. -1) 1 . get ( 0 :. 0) 1 . get ( 0 :. 1) 1 .
  get ( 1 :. -1) 1 . get ( 1 :. 0) 1 . get ( 1 :. 1) 1
{-# INLINE sum3x3Filter #-}


