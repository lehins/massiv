{-# LANGUAGE BangPatterns #-}
module Main where

import Criterion.Main
import Data.Massiv.Array as A
import Data.Massiv.Bench as A
import Data.Massiv.Bench.Sobel as A
import Prelude as P



main :: IO ()
main = do
  let !sz = Sz2 1600 1200
  defaultMain
    [ bgroup
        "Sobel"
        [ env (return (arrRLightIx2 U Seq sz)) $ \arr ->
            bgroup
              "Seq"
              [ bench "Horizontal - Massiv" $ whnf (computeAs U . A.mapStencil Edge sobelX) arr
              , bench "Vertical - Massiv" $ whnf (computeAs U . A.mapStencil Edge sobelY) arr
              , bench "Operator - Massiv" $ whnf (computeAs U . A.mapStencil Edge sobelOperator) arr
              ]
        , env (return (arrRLightIx2 U Par sz)) $ \arr ->
            bgroup
              "Par"
              [ bench "Horizontal - Massiv" $ whnf (computeAs U . A.mapStencil Edge sobelX) arr
              , bench "Vertical - Massiv" $ whnf (computeAs U . A.mapStencil Edge sobelY) arr
              , bench "Operator - Massiv" $ whnf (computeAs U . A.mapStencil Edge sobelOperator) arr
              ]
        ]
    ]
