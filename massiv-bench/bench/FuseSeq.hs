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
            (return (tupleToIx2 t2))
            (bench "Massiv Ix2 U" .
             whnf (computeAs U . M.transpose . arrDLightIx2 Seq))
        , env
            (return (tupleToIx2 t2))
            (bench "Massiv Ix2 Inner" .
             whnf (computeAs U . M.transposeInner . arrDLightIx2 Seq))
        , env
            (return (tupleToIx2 t2))
            (bench "Massiv Ix2 Outer" .
             whnf (computeAs U . M.transposeOuter . arrDLightIx2 Seq))
        , env
            (return (tupleToSh2 t2))
            (bench "Repa DIM2 U" .
             whnf (R.computeUnboxedS . R.transpose . arrDLightSh2))
        ]
    , bgroup
        "append"
        [ env
            (return t2)
            (bench "Vector U" .
             whnf (\sz -> vecLight2 sz VU.++ vecLight2 sz))
        , env
            (return (tupleToIx2 t2))
            (bench "Massiv Ix2 U" .
             whnf
               (\sz ->
                  computeAs U $
                  M.append' 1 (arrDLightIx2 Seq sz) (arrDLightIx2 Seq sz)))
        , env
            (return (tupleToSh2 t2))
            (bench "Repa Ix2 U" .
             whnf
               (\sz ->
                  R.computeUnboxedS $
                  R.append (arrDLightSh2 sz) (arrDLightSh2 sz)))
        ]
    ]
