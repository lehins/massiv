{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
module Main where

import           Bench
import           Criterion.Main
import           Data.Functor.Identity
import           Data.Array.Massiv            as M
import           Data.Array.Massiv.Stencil    as M
import           Data.Array.Repa              as R
import           Data.Array.Repa.Stencil      as R
import           Data.Array.Repa.Stencil.Dim2 as R
import           Data.Default                 (Default)
import           Prelude                      as P





main :: IO ()
main = do
  let t2 = (1600, 1200) :: (Int, Int)
      !arrIx2T = computeAs U (arrDLightIx2T Seq t2) :: M.Array M.U Ix2T Double
      !sobelOpT = sobelOperatorT Edge
  defaultMain
    [ bgroup
        "Stencil"
        [ bgroup
            "Average"
            [ env
                (return (computeAs U (arrDLightIx2 Par (tupleToIx2 t2))))
                (bench "Massiv Parallel" .
                 whnf (computeAs U . mapStencil (average3x3Filter Edge)))
            -- , env
            --     (return (computeAs U (arrDLightIx2 Par (tupleToIx2 t2))))
            --     (bench "Convolve Avg" .
            --      whnf (computeAs U . mapStencil (average3x3FilterConv Edge)))
            -- , env
            --     (return (computeAs U (arrDLightIx2 Par (tupleToIx2 t2))))
            --     (bench "Convolve Sum/avg" .
            --      whnf (computeAs U . fmap (/9) . mapStencil (sum3x3Filter Edge)))
            -- , env
            --     (return (computeAs U (arrDLightIx2 Par (tupleToIx2 t2))))
            --     (bench "Convolve Avg fmap" .
            --      whnf (computeAs U . mapStencil (average3x3FilterConvMap' Edge)))
            -- , env
            --     (return (computeAs U (arrDLightIx2 Par (tupleToIx2 t2))))
            --     (bench "Convolve Avg sMap" .
            --      whnf (computeAs U . mapStencil (average3x3FilterConvMap Edge)))
            -- , env
            --     (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
            --     (bench "Repa DIM2 U" .
            --      whnf (computeUnboxedS . mapStencil2 BoundClamp averageStencil))
            , env
                (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
                (bench "Repa Parallel" .
                 whnf
                   (runIdentity .
                    computeUnboxedP . mapStencil2 BoundClamp averageStencil))
            -- , env
            --     (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
            --     (bench "Repa DIM2 U" .
            --      whnf
            --        (runIdentity .
            --         computeUnboxedP .
            --         R.smap (/ 9) . mapStencil2 BoundClamp sumStencil))
            ]
        ]
    , bgroup
        "Sobel"
        [ bgroup
            "Horizontal"
            [ env
                (return (computeAs U (arrDLightIx2 Seq (tupleToIx2 t2))))
                (bench "Massiv Ix2 U" .
                 whnf (computeAs U . mapStencil (sobelX Edge)))
            , env
                (return (computeAs U (arrDLightIx2T Seq t2)))
                (bench "Massiv Ix2T U" .
                 whnf (computeAs U . mapStencil (sobelTX Edge)))
            , env
                (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
                (bench "Repa DIM2 U" . whnf (computeUnboxedS . mapSobelRX))
            ]
        , bgroup
            "Vertical"
            [ env
                (return (computeAs U (arrDLightIx2 Seq (tupleToIx2 t2))))
                (bench "Massiv Ix2 U" .
                 whnf (computeAs U . mapStencil (sobelY Edge)))
            , env
                (return (computeAs U (arrDLightIx2T Seq t2)))
                (bench "Massiv Ix2T U" .
                 whnf (computeAs U . mapStencil (sobelTY Edge)))
            , env
                (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
                (bench "Repa DIM2 U" . whnf (computeUnboxedS . mapSobelRY))
            ]
        , bgroup
            "Operator Fused"
            [ bench "Massiv Ix2T U (no env)" $
              whnf (computeAs U . mapStencil sobelOpT) arrIx2T
            , env
                (return (computeAs U (arrDLightIx2 Seq (tupleToIx2 t2))))
                (bench "Massiv Ix2 U" .
                 whnf (computeAs U . mapStencil (sobelOperator Edge)))
            , env
                (return (computeAs U (arrDLightIx2T Seq t2)))
                (bench "Massiv Ix2T U" .
                 whnf (computeAs U . mapStencil (sobelOperatorT Edge)))
            , env
                (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
                (bench "Repa DIM2 U" . whnf sobelOperatorR)
            ]
        , bgroup
            "Operator Unfused"
            [ env
                (return (computeAs U (arrDLightIx2 Seq (tupleToIx2 t2))))
                (bench "Massiv Ix2 U" . whnf (sobelOperatorUnfused Edge))
            , env
                (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
                (bench "Repa DIM2 U" . whnf sobelOperatorRUnfused)
            ]
        ]
    ]
