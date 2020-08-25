{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe as A
import Data.Massiv.Bench as A
import Criterion.Main

avg3x3 :: Fractional a => (Ix2 -> a) -> a
avg3x3 get =
  (get (0 :. 0) + get (0 :. 1) + get (0 :. 2) +
   get (1 :. 0) + get (1 :. 1) + get (1 :. 2) +
   get (2 :. 0) + get (2 :. 1) + get (2 :. 2) ) / 9
{-# INLINE avg3x3 #-}

avg3x3Stencil :: (Default a, Fractional a) => Stencil Ix2 a a
avg3x3Stencil = makeStencil (Sz (3 :. 3)) 0 avg3x3
{-# INLINE avg3x3Stencil #-}

avg3x3StencilUnsafe :: Fractional a => Stencil Ix2 a a
avg3x3StencilUnsafe = makeUnsafeStencil (Sz (3 :. 3)) 0 (const avg3x3)
{-# INLINE avg3x3StencilUnsafe #-}

sum3x3StencilConv :: Fractional a => Stencil Ix2 a a
sum3x3StencilConv = makeConvolutionStencil (Sz2 3 3) 0 $ \ get ->
  get (0 :. 0) 1 . get (0 :. 1) 1 . get (0 :. 2) 1 .
  get (1 :. 0) 1 . get (1 :. 1) 1 . get (1 :. 2) 1 .
  get (2 :. 0) 1 . get (2 :. 1) 1 . get (2 :. 2) 1
{-# INLINE sum3x3StencilConv #-}

sum3x3StencilConvKern :: (Prim a, Fractional a) => Stencil Ix2 a a
sum3x3StencilConvKern =
  makeConvolutionStencilFromKernel $ makeArrayR P Seq (Sz2 3 3) (const 1)
{-# INLINE sum3x3StencilConvKern #-}

main :: IO ()
main = do
  let sz = Sz2 1600 1200
  defaultMain
    [ bgroup
        "Stencil"
        [ env (return (arrRLightIx2 P Seq sz)) $ \arr ->
            bgroup
              "Average Seq"
              [ bench "Array Ix2" $ whnf (computeAs P . A.mapStencil (Fill 0) avg3x3Stencil) arr
              , bench "Array Ix2" $
                whnf
                  (computeAs P . A.applyStencil (samePadding avg3x3Stencil (Fill 0)) avg3x3Stencil)
                  arr
              , bench "Unsafe Stencil Array Ix2" $
                whnf (computeAs P . A.mapStencil (Fill 0) avg3x3StencilUnsafe) arr
              , bench "Unsafe Map Array Ix2" $
                whnf (computeAs P . A.mapStencilUnsafe (Fill 0) 3 0 avg3x3) arr
              , bench "Convolve Array Ix2" $
                whnf (computeAs P . A.mapStencil (Fill 0) (sum3x3StencilConv / 9)) arr
              , bench "Convolve Kernel Array Ix2" $
                whnf (computeAs P . A.mapStencil (Fill 0) (sum3x3StencilConvKern / 9)) arr
              , bench "Foldl Array Ix2" $
                whnf (computeAs P . A.mapStencil (Fill 0) (foldlStencil (+) 0 (Sz 3) / 9)) arr
              , bench "Monoid Array Ix2" $
                whnf (computeAs P . A.mapStencil (Fill 0) (avgStencil (Sz 3))) arr
              ]
        , env (return (arrRLightIx2 P Par sz)) $ \arr ->
            bgroup
              "Average Par"
              [ bench "Array Ix2" $ whnf (computeAs P . A.mapStencil (Fill 0) avg3x3Stencil) arr
              , bench "Convolve Array Ix2" $
                whnf (computeAs P . A.mapStencil (Fill 0) (sum3x3StencilConv / 9)) arr
              , bench "Monoid Array Ix2" $
                whnf (computeAs P . A.mapStencil (Fill 0) (avgStencil (Sz 3))) arr
              ]
        ]
    ]
