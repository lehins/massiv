{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
module Main where

import           Bench
import           Criterion.Main
import           Data.Array.Massiv   as M
import           Data.Array.Repa     as R
import           Data.Vector.Unboxed as VU
import           Prelude             as P

main :: IO ()
main = do
  let t2 = (1600, 1200) :: (Int, Int)
  defaultMain
    [ bgroup
        "map (+25)"
        [ env (return t2) (bench "Vector U" . whnf (VU.map (+ 25) . vecLight2))
        , env
            (return (tupleToIx2 t2))
            (bench "Massiv Ix2 U" .
             whnf (computeAs U . fmap (+ 25) . arrDLightIx2 Seq))
        , env
            (return (tupleToSh2 t2))
            (bench "Repa DIM2 U" .
             whnf (R.computeUnboxedS . R.map (+ 25) . arrDLightSh2))
        ]
    , bgroup
        "transpose"
        [ env
            (return (tupleToIx2T t2))
            (bench "Massiv Ix2 U" .
             whnf (computeAs U . M.transpose . arrDLightIx2T Seq))
        , env
            (return (tupleToIx2 t2))
            (bench "Massiv Ix2 U" .
             whnf (computeAs U . M.transposeInner . arrDLightIx2 Seq))
        , env
            (return (tupleToSh2 t2))
            (bench "Repa DIM2 U" .
             whnf (R.computeUnboxedS . R.transpose . arrDLightSh2))
        ]
    ]
