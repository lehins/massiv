{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
module Main where

import           Bench.Massiv        as A
import           Bench.Massiv.Auto   as M hiding (tupleToIx2)
import           Bench.Repa
import           Bench.Vector
import           Criterion.Main
--import           Data.Array.Massiv          as M
-- import           Data.Array.Massiv.Common   as A
-- import           Data.Array.Massiv.Manifest as A
-- import           Data.Array.Massiv.Mutable  as A
-- import           Data.Array.Massiv.Ops      as A
import           Data.Array.Repa     as R
import           Data.Vector.Unboxed as VU
import           Prelude             as P

main :: IO ()
main = do
  let t2 = (1600, 1200) :: (Int, Int)
  defaultMain
    [ bgroup
        "map (+25)"
        [ env
            (return (tupleToIx2 t2))
            (bench "Array Ix2 U" .
             whnf (computeAs U . A.map (+ 25) . arrDLightIx2 Seq))
        , env
            (return (tupleToIx2 t2))
            (bench "Massiv Ix2" . whnf (M.map (+ 25) . massDLightIx2 Seq))
        , env (return t2) (bench "Vector U" . whnf (VU.map (+ 25) . vecLight2))
        , env
            (return (tupleToSh2 t2))
            (bench "Repa DIM2 U" .
             whnf (R.computeUnboxedS . R.map (+ 25) . arrDLightSh2))
        ]
    , bgroup
        "zipWith (*) . map (+25)"
        [ env
            (return (tupleToIx2 t2))
            (bench "Array Ix2 U" .
             whnf
               (\sz ->
                  let a = A.map (+ 25) $ arrDLightIx2 Seq sz
                  in computeAs U $ A.zipWith (*) a a))
        , env
            (return (tupleToIx2 t2))
            (bench "Array Ix2 U (compute intermediate)" .
             whnf
               (\sz ->
                  let a = computeAs U $ A.map (+ 25) $ arrDLightIx2 Seq sz
                  in computeAs U $ A.zipWith (*) a a))
        , env
            (return (tupleToIx2 t2))
            (bench "Massiv Ix2" .
             whnf
               (\sz ->
                  let m = M.map (+ 25) $ massDLightIx2 Seq sz
                  in M.zipWith (*) m m))
        , env
            (return t2)
            (bench "Vector U" .
             whnf
               (\sz ->
                  let v = VU.map (+ 25) $ vecLight2 sz
                  in VU.zipWith (*) v v))
        , env
            (return (tupleToSh2 t2))
            (bench "Repa DIM2 U" .
             whnf
               (\sz ->
                  let a = R.map (+ 25) $ arrDLightSh2 sz
                  in R.computeUnboxedS $ R.zipWith (*) a a))
        ]
    , bgroup
        "transpose"
        [ env
            (return (tupleToIx2 t2))
            (bench "Array Ix2 U" .
             whnf (computeAs U . A.transpose . arrDLightIx2 Seq))
        , env
            (return (tupleToIx2 t2))
            (bench "Array Ix2 Inner" .
             whnf (computeAs U . A.transposeInner . arrDLightIx2 Seq))
        , env
            (return (tupleToIx2 t2))
            (bench "Array Ix2 Outer" .
             whnf (computeAs U . A.transposeOuter . arrDLightIx2 Seq))
        , env
            (return (tupleToSh2 t2))
            (bench "Repa DIM2 U" .
             whnf (R.computeUnboxedS . R.transpose . arrDLightSh2))
        ]
    , bgroup
        "append"
        [ env
            (return t2)
            (bench "Vector U" . whnf (\sz -> vecLight2 sz VU.++ vecLight2 sz))
        , env
            (return (tupleToIx2 t2))
            (bench "Array Ix2 U" .
             whnf
               (\sz ->
                  computeAs U $
                  A.append' 1 (arrDLightIx2 Seq sz) (arrDLightIx2 Seq sz)))
        , env
            (return (tupleToSh2 t2))
            (bench "Repa Ix2 U" .
             whnf
               (\sz ->
                  R.computeUnboxedS $
                  R.append (arrDLightSh2 sz) (arrDLightSh2 sz)))
        ]
    ]
