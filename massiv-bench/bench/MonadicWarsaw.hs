{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import           Bench.C
import           Bench.Massiv.Array as A
import           Bench.Repa
import           Bench.Vector
import           Control.Concurrent
import           Criterion.Main
import           Data.Array.Repa as R
import           Data.Array.Repa.Repr.ForeignPtr
import           Data.Array.Repa.Stencil as R
import           Data.Array.Repa.Stencil.Dim2 as R
import           Data.Functor.Identity
import           Data.Massiv.Array.Unsafe as A
import           Data.Monoid
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import           GHC.Exts as GHC
import           Prelude as P

main :: IO ()
main = do
  let t1 = 320000 :: Int
      toDouble :: Int -> Double
      toDouble = fromIntegral
  let t2 = (1600, 1200) :: (Int, Int)
  defaultMain
    [ bgroup
        "Simple"
        [ bgroup
            "Seq"
            [ bench "[Double]" $ whnf (\t -> P.sum (fmap toDouble [0 .. t - 1])) t1
            , bench "Vector U Double" $ whnf (\t -> VU.sum (VU.generate t toDouble)) t1
            , bench "Repa DIM2 D Double" $
              whnf (\t -> R.sumAllS (R.fromFunction (Z R.:. t) (\(Z R.:. i) -> toDouble i))) t1
            , bench "Massiv Ix2 D Double" $ whnf (\t -> A.sum (A.makeArrayR D Seq t toDouble)) t1
            ]
        , bgroup
            "Par"
            [ bench "Repa DIM2 D Double" $
              whnf
                (\t ->
                   runIdentity $ R.sumAllP (R.fromFunction (Z R.:. t) (\(Z R.:. i) -> toDouble i)))
                t1
            , bench "Massiv Ix2 D Double" $ whnf (\t -> A.sum (A.makeArrayR D Par t toDouble)) t1
            ]
        ]
    , bgroup
        "Computed"
        [ bgroup
            "Unboxed"
            [ bgroup
                "Seq"
                [ env (return (vecLight2 t2)) (bench "Vector U" . whnf VU.sum)
                , env
                    (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
                    (bench "Repa DIM2 U" . whnf R.sumAllS)
                , env
                    (return (computeAs U (arrDLightIx2 Seq (tupleToIx2 t2))))
                    (bench "Massiv Ix2 U" . whnf A.sum)
                ]
            , bgroup
                "Par"
                [ env
                    (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
                    (bench "Repa DIM2 U" . whnf (runIdentity . R.sumAllP))
                , env
                    (return (computeAs U (arrDLightIx2 Par (tupleToIx2 t2))))
                    (bench "Massiv U Ix2" . whnf A.sum)
                ]
            ]
        , bgroup
            "Storable"
            [ bgroup
                "Seq"
                [ env (return (VG.convert $ vecLight2 t2)) (bench "Vector S" . whnf VS.sum)
                , env
                    (return (VG.convert $ vecLight2 t2))
                    (bench "Repa DIM2 F" .
                     whnf
                       (R.sumAllS . fromForeignPtr (tupleToSh2 t2) . fst . VS.unsafeToForeignPtr0))
                , env
                    (return (computeAs S (arrDLightIx2 Seq (tupleToIx2 t2))))
                    (bench "Massiv Ix2 S" . whnf A.sum)
                ]
            , bgroup
                "Par"
                [ env
                    (return (VG.convert $ vecLight2 t2))
                    (bench "Repa DIM2 F" .
                     whnf
                       (runIdentity .
                        R.sumAllP . fromForeignPtr (tupleToSh2 t2) . fst . VS.unsafeToForeignPtr0))
                , env
                    (return (computeAs S (arrDLightIx2 Par (tupleToIx2 t2))))
                    (bench "Massiv S Ix2" . whnf A.sum)
                ]
            ]
        , bgroup
            "C"
            [ env
                (return (computeAs S (arrDLightIx2 Seq (tupleToIx2 t2))))
                (bench "Native C" . whnf sumDouble)
            , env (return (VG.convert $ vecLight2 t2)) (bench "Vector S" . whnf VS.sum)
            , env
                (return (computeAs S (arrDLightIx2 Seq (tupleToIx2 t2))))
                (bench "Massiv Ix2 S" . whnf A.sum)
            ]
        ]
    , bgroup
        "mapM"
        [ bgroup
            "Massiv"
            [ env
                (return (computeAs P (arrDLightIx2 Seq (tupleToIx2 t2))))
                (bench "Massiv Ix2 P Seq" . whnf (A.mapM A.P Just))
            , env
                (return (computeAs U (arrDLightIx2 Seq (tupleToIx2 t2))))
                (bench "Massiv Ix2 U Seq" . whnf (A.mapM A.U Just))
            ]
        , env (return (vecLight2 t2)) (bench "Vector U" . whnf (VU.mapM Just))
        ]
    , bgroup
        "Average"
        [ bgroup
            "Seq"
            [ env
                (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
                (bench "Repa DIM2 U" .
                 whnf (computeUnboxedS . mapStencil2 BoundClamp averageStencil))
            , env
                (return (computeAs U (arrDLightIx2 Seq (tupleToIx2 t2))))
                (bench "Massiv Ix2" . whnf (computeAs U . A.mapStencil (average3x3FilterConv Edge)))
            ]
        , bgroup
            "Par"
            [ env
                (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
                (bench "Repa DIM2 U" .
                 whnf (runIdentity . computeUnboxedP . mapStencil2 BoundClamp averageStencil))
            , env
                (return (computeAs U (arrDLightIx2 Par (tupleToIx2 t2))))
                (bench "Massiv Ix2 U" .
                 whnf (computeAs U . A.mapStencil (average3x3FilterConv Edge)))
            ]
        ]
    , bgroup
        "Sobel"
        [ bgroup
            "Seq"
            [ env
                (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
                (bench "Repa DIM2 U" . whnf (computeUnboxedS . sobelOperatorR))
            , env
                (return (computeAs U (arrDLightIx2 Seq (tupleToIx2 t2))))
                (bench "Massiv Ix2 U" . whnf (computeAs U . A.mapStencil (sobelOperator Edge)))
            ]
        , bgroup
            "Par"
            [ env
                (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
                (bench "Repa DIM2 U" . whnf (runIdentity . computeUnboxedP . sobelOperatorR))
            , env
                (return (computeAs U (arrDLightIx2 Par (tupleToIx2 t2))))
                (bench "Massiv Ix2 U" . whnf (computeAs U . A.mapStencil (sobelOperator Edge)))
            ]
        ]
    ]
