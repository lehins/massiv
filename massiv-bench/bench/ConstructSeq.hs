{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
module Main where

import           Bench
import           Bench.Massiv                           as A
import           Bench.Massiv.Auto                      as M hiding (tupleToIx2)
import           Bench.Repa
import           Bench.Vector
import           Criterion.Main
import qualified Data.Array.Massiv.Manifest.BoxedStrict as BS
import qualified Data.Array.Massiv.Manifest.Prim        as PS
import           Data.Array.Repa                        as R
import           Data.Functor.Identity
import           Prelude                                as P

main :: IO ()
main = do
  let t2 = (1600, 1200) :: (Int, Int)
  defaultMain
    [ bgroup
        "makeArray"
        [ env
            (return (tupleToIx2 t2))
            (bench "Massiv Ix2 D -> U" . whnf (computeAs U . arrDLightIx2 Seq))
        , env
            (return (tupleToIx2 t2))
            (bench "Massiv Ix2 U" . whnf (\sz -> makeArrayR U Seq sz lightFuncIx2))
        , env
            (return (tupleToIx2 t2))
            (bench "Massiv Ix2 U Par" . whnf (\sz -> makeArrayR U Par sz lightFuncIx2))
        , env
            (return (tupleToIx2 t2))
            (bench "Massiv Ix2 P" . whnf (\sz -> makeArrayR P Seq sz lightFuncIx2))
        , env
            (return (tupleToIx2 t2))
            (bench "Massiv Ix2 PS" . whnf (\sz -> makeArrayR PS.P Seq sz lightFuncIx2))
        , env
            (return (tupleToIx2 t2))
            (bench "Massiv Ix2 PS" . whnf (\sz -> makeArrayR PS.P Par sz lightFuncIx2))
        , env
            (return (tupleToIx2 t2))
            (bench "Massiv Ix2 S" . whnf (\sz -> makeArrayR S Seq sz lightFuncIx2))
        , env
            (return (tupleToIx2 t2))
            (bench "Massiv Ix2 B" . nf (\sz -> makeArrayR A.B Seq sz lightFuncIx2))
        , env
            (return (tupleToIx2 t2))
            (bench "Massiv Ix2 BS" . nf (\sz -> makeArrayR BS.B Seq sz lightFuncIx2))
        , env (return t2) (bench "Vector U" . whnf vecLight2)
        , env
            (return (tupleToSh2 t2))
            (bench "Repa DIM2 U" . whnf (R.computeUnboxedS . arrDLightSh2))
        , env
            (return (tupleToSh2 t2))
            (bench "Repa DIM2 U Par" . whnf (runIdentity . R.computeUnboxedP . arrDLightSh2))
        ]
    ]
