{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
module Main where

import           Bench
import           Bench.Massiv.Array           as A
import           Criterion.Main
import           Data.Array.Repa              as R
import           Data.Array.Repa.Stencil      as R
import           Data.Array.Repa.Stencil.Dim2 as R
import           Data.Functor.Identity
import           Prelude                      as P





main :: IO ()
main = do
  let t2 = (1600, 1200) :: (Int, Int)
  defaultMain
    [ bgroup
        "Stencil"
        [ bgroup
            "Average Seq"
            [ env
                (return (computeAs U (arrDLightIx2 Seq (tupleToIx2 t2))))
                (bench "Convolve Array Ix2" .
                 whnf (computeAs U . A.mapStencil Edge average3x3FilterConv))
            , env
                (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
                (bench "Repa DIM2 U" .
                 whnf (computeUnboxedS . mapStencil2 BoundClamp averageStencil))
            ]
        , bgroup
            "Average Par"
            [ env
                (return (computeAs U (arrDLightIx2 Par (tupleToIx2 t2))))
                (bench "Convolve Array Ix2" .
                 whnf (computeAs U . A.mapStencil Edge average3x3FilterConv))
            , env
                (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
                (bench "Repa DIM2 U" .
                 whnf
                   (runIdentity .
                    computeUnboxedP . mapStencil2 BoundClamp averageStencil))
            ]
        ]
    , bgroup
        "Sobel"
        [ bgroup
            "Horizontal"
            [ env
                (return (computeAs U (arrDLightIx2 Seq (tupleToIx2 t2))))
                (bench "Array Ix2 U" .
                 whnf (computeAs U . A.mapStencil Edge sobelX))
            , env
                (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
                (bench "Repa DIM2 U" . whnf (computeUnboxedS . mapSobelRX))
            ]
        , bgroup
            "Vertical"
            [ env
                (return (computeAs U (arrDLightIx2 Seq (tupleToIx2 t2))))
                (bench "Array Ix2 U" .
                 whnf (computeAs U . A.mapStencil Edge sobelY))
            , env
                (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
                (bench "Repa DIM2 U" . whnf (computeUnboxedS . mapSobelRY))
            ]
        , bgroup
            "Operator Fused Seq"
            [ env
                (return (computeAs U (arrDLightIx2 Seq (tupleToIx2 t2))))
                (bench "Array Ix2 U" .
                 whnf (computeAs U . A.mapStencil Edge sobelOperator))
            , env
                (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
                (bench "Repa DIM2 U" . whnf (computeUnboxedS . sobelOperatorR))
            ]
        , bgroup
            "Operator Fused Par"
            [ env
                (return (computeAs U (arrDLightIx2 Par (tupleToIx2 t2))))
                (bench "Array Ix2 U" .
                 whnf (computeAs U . A.mapStencil Edge sobelOperator))
            , env
                (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
                (bench "Repa DIM2 U" .
                 whnf (runIdentity . computeUnboxedP . sobelOperatorR))
            ]
        , bgroup
            "Operator Unfused"
            [ env
                (return (computeAs U (arrDLightIx2 Seq (tupleToIx2 t2))))
                (bench "Array Ix2 U" . whnf (sobelOperatorUnfused Edge))
            , env
                (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
                (bench "Repa DIM2 U" . whnf sobelOperatorRUnfused)
            ]
        ]
    ]
