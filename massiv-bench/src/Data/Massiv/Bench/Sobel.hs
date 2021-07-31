{-# LANGUAGE BangPatterns #-}
module Data.Massiv.Bench.Sobel
  ( sobelX
  , sobelY
  , sobelOperator
  , sobelBenchGroup
  ) where

import Criterion.Main
import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe
import Data.Massiv.Bench.Common

sobelX :: Num e => Stencil Ix2 e e
sobelX =
  makeUnsafeConvolutionStencil (Sz 3) (1 :. 1) $
  \ f -> f (-1 :. -1) (-1) .
         f ( 0 :. -1) (-2) .
         f ( 1 :. -1) (-1) .
         f (-1 :.  1)   1  .
         f ( 0 :.  1)   2  .
         f ( 1 :.  1)   1
{-# INLINE sobelX #-}


sobelY :: Num e => Stencil Ix2 e e
sobelY =
  makeUnsafeConvolutionStencil (Sz 3) (1 :. 1) $
  \ f -> f (-1 :. -1) (-1) .
         f (-1 :.  0) (-2) .
         f (-1 :.  1) (-1) .
         f ( 1 :. -1)   1  .
         f ( 1 :.  0)   2  .
         f ( 1 :.  1)   1
{-# INLINE sobelY #-}


sobelOperator :: Floating b => Stencil Ix2 b b
sobelOperator = sqrt (sX + sY)
  where
    !sX = fmap (^ (2 :: Int)) sobelX
    !sY = fmap (^ (2 :: Int)) sobelY
{-# INLINE sobelOperator #-}

sobelBenchGroup :: Sz2 -> Benchmark
sobelBenchGroup sz =
  bgroup
    "Sobel"
    [ env (return (arrRLightIx2 S Seq sz)) $ \arr ->
        bgroup
          "Seq"
          [ bench "Horizontal - Massiv" $ whnf (computeAs S . A.mapStencil Edge sobelX) arr
          , bench "Vertical - Massiv" $ whnf (computeAs S . A.mapStencil Edge sobelY) arr
          , bench "Operator - Massiv" $ whnf (computeAs S . A.mapStencil Edge sobelOperator) arr
          ]
    , env (return (arrRLightIx2 S Par sz)) $ \arr ->
        bgroup
          "Par"
          [ bench "Horizontal - Massiv" $ whnf (computeAs S . A.mapStencil Edge sobelX) arr
          , bench "Vertical - Massiv" $ whnf (computeAs S . A.mapStencil Edge sobelY) arr
          , bench "Operator - Massiv" $ whnf (computeAs S . A.mapStencil Edge sobelOperator) arr
          ]
    ]
{-# INLINEABLE sobelBenchGroup #-}
