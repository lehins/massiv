{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
module Main where

import           Bench
import           Criterion.Main
import           Data.Array.Massiv     as M
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
                (bench "Massiv Ix2 U" . whnf (foldlS (+) 0 . arrDLightIx2 Seq))
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
                (bench "Massiv Ix2 U" . whnf (foldrS (+) 0 . arrDLightIx2 Seq))
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
                (bench "Massiv Ix2 U" . whnf (foldlS (+) 0))
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
                (bench "Massiv Ix2 U" . whnf (foldrS (+) 0))
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
                (bench "Massiv Ix2 U" . whnf (foldlS (+) 0))
            , env
                (return (vecLight2 t2))
                (bench "Vector U" . whnf (VU.foldl' (+) 0))
            , env
                (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
                (bench "Repa DIM2 U" . whnf (R.foldAllS (+) 0))
            ]
        , bgroup
            "Parallel"
            [ env
                (return (computeAs U (arrDLightIx2 Par (tupleToIx2 t2))))
                (bench "Massiv Ix2 U" . whnf (foldlP (+) 0 (+) 0))
            , env
                (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
                (bench "Repa DIM2 U" . whnf (runIdentity . R.foldAllP (+) 0))
            ]
        ]
    ]
