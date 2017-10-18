{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
module Main where

import           Bench
-- import           Bench.Massiv                           as A
-- import           Bench.Massiv.Auto                      as M hiding (tupleToIx2)
-- import           Bench.Repa
-- import           Bench.Vector
import           Criterion.Main
import           Data.Array.Repa                        as R
-- import           Data.Functor.Identity
import           Prelude                                as P
import qualified Data.Vector.Unboxed as VU


main :: IO ()
main = do
  let t2 = (1600, 1200) :: (Int, Int)
      ls = toListIx2 (arrDLightIx2 Seq (tupleToIx2 t2))
  defaultMain
    [ bgroup
        "makeArray"
        [ env
            (return (tupleToIx2 t2))
            (bench "Massiv Ix2 D -> U" . whnf (computeAs U . arrDLightIx2 Seq))
        , env
            (return (tupleToIx2 t2))
            (bench "Massiv Ix2 U" . whnf (\sz -> makeArrayR U Seq sz lightFuncIx2))
        -- , env
        --     (return (tupleToIx2 t2))
        --     (bench "Massiv Ix2 U Par" . whnf (\sz -> makeArrayR U Par sz lightFuncIx2))
        , env
            (return (tupleToIx2 t2))
            (bench "Massiv Ix2 P" . whnf (\sz -> makeArrayR P Seq sz lightFuncIx2))
        , env
            (return (tupleToIx2 t2))
            (bench "Massiv Ix2 S" . whnf (\sz -> makeArrayR S Seq sz lightFuncIx2))
        , env
            (return (tupleToIx2 t2))
            (bench "Massiv Ix2 B" . nf (\sz -> makeArrayR B Seq sz lightFuncIx2))
        , env (return t2) (bench "Vector U" . whnf vecLight2)
        , env
            (return (tupleToSh2 t2))
            (bench "Repa DIM2 U" . whnf (R.computeUnboxedS . arrDLightSh2))
        -- , env
        --     (return (tupleToSh2 t2))
        --     (bench "Repa DIM2 U Par" . whnf (runIdentity . R.computeUnboxedP . arrDLightSh2))
        ]
    , bgroup
        "fromList"
        [ bgroup
            "Sequential"
            [ env
                (return ls)
                (bench "Array Ix2 U (A.fromListIx2)" . nf (fromListIx2As U Seq))
            , env
                (return (concat ls))
                (bench "Repa DIM2 U (fromListUnboxed)" . whnf (R.fromListUnboxed (tupleToSh2 t2)))
            , env
                (return (concat ls))
                (bench "Vector U (fromList)" . whnf (VU.fromList))
            -- , env
            --     (return ls)
            --     (bench "Repa DIM2 U" . nf R.toList)
            ]
        ]
    ]
