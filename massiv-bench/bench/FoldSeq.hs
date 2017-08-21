{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
module Main where

import           Bench.Repa
import           Bench.Massiv as A
import           Bench.Massiv.Auto as M hiding (tupleToIx2)
import           Bench.Vector
import           Criterion.Main
import           Data.Array.Repa       as R
import           Data.Functor.Identity
import           Data.Vector.Unboxed   as VU
import           Prelude               as P

main :: IO ()
main = do
  let t2 = (1600, 1200) :: (Int, Int)
  defaultMain
    [ bgroup
        "Uncomputed"
        [ bgroup
            "foldLeft"
            [ env
                (return (tupleToIx2 t2))
                (bench "Array Ix2 U" . whnf (A.foldlS (+) 0 . arrDLightIx2 Seq))
            , env
                (return (tupleToIx2 t2))
                (bench "Massiv Ix2" . whnf (M.foldlS (+) 0 . massDLightIx2 Seq))
            , env
                (return t2)
                (bench "Vector U" . whnf (VU.foldl' (+) 0 . vecLight2))
            , env
                (return (tupleToSh2 t2))
                (bench "Repa DIM2 U" . whnf (R.foldAllS (+) 0 . arrDLightSh2))
            ]
        , bgroup
            "foldRight"
            [ env
                (return (tupleToIx2 t2))
                (bench "Array Ix2 U" . whnf (A.foldrS (+) 0 . arrDLightIx2 Seq))
            , env
                (return (tupleToIx2 t2))
                (bench "Massiv Ix2" . whnf (M.foldrS (+) 0 . massDLightIx2 Seq))
            , env
                (return t2)
                (bench "Vector U" . whnf (VU.foldr' (+) 0 . vecLight2))
            , env
                (return (tupleToSh2 t2))
                (bench "Repa DIM2 U" . whnf (R.foldAllS (+) 0 . arrDLightSh2))
            ]
        ]
    , bgroup
        "Computed"
        [ bgroup
            "foldLeft"
            [ env
                (return (computeAs U (arrDLightIx2 Seq (tupleToIx2 t2))))
                (bench "Array Ix2 U" . whnf (A.foldlS (+) 0))
            , env
                (return (massDLightIx2 Seq (tupleToIx2 t2)))
                (bench "Massiv Ix2" . whnf (M.foldlS (+) 0))
            , env
                (return (vecLight2 t2))
                (bench "Vector U" . whnf (VU.foldl' (+) 0))
            , env
                (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
                (bench "Repa DIM2 U" . whnf (R.foldAllS (+) 0))
            ]
        , bgroup
            "foldRight"
            [ env
                (return (computeAs U (arrDLightIx2 Seq (tupleToIx2 t2))))
                (bench "Array Ix2 U" . whnf (A.foldrS (+) 0))
            , env
                (return (massDLightIx2 Seq (tupleToIx2 t2)))
                (bench "Massiv Ix2" . whnf (M.foldrS (+) 0))
            , env
                (return (vecLight2 t2))
                (bench "Vector U" . whnf (VU.foldr' (+) 0))
            , env
                (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
                (bench "Repa DIM2 U" . whnf (R.foldAllS (+) 0))
            ]
        ]
    , bgroup
        "Sum (1600x1200)"
        [ bgroup
            "Sequential"
            [ env
                (return (computeAs U (arrDLightIx2 Seq (tupleToIx2 t2))))
                (bench "Array Ix2 U" . whnf A.sum)
            , env
                (return (massDLightIx2 Seq (tupleToIx2 t2)))
                (bench "Massiv Ix2" . whnf M.sum)
            , env
                (return (vecLight2 t2))
                (bench "Vector U" . whnf VU.sum)
            , env
                (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
                (bench "Repa DIM2 U" . whnf R.sumAllS)
            ]
        , bgroup
            "Parallel"
            [ env
                (return (computeAs U (arrDLightIx2 Par (tupleToIx2 t2))))
                (bench "Array Ix2 U" . whnf A.sum)
            , env
                (return (massDLightIx2 Par (tupleToIx2 t2)))
                (bench "Massiv Ix2" . whnf (M.foldlP (+) 0 (+) 0))
            , env
                (return (massDLightIx2 Par (tupleToIx2 t2)))
                (bench "Massiv Ix2 (sum)" . whnf M.sum)
            , env
                (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
                (bench "Repa DIM2 U" . whnf (runIdentity . R.sumAllP))
            ]
        ]
    ]
