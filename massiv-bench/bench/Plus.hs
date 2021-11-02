{-# LANGUAGE BangPatterns #-}
module Main where

import Criterion.Main
import Data.Massiv.Array as A
import Data.Massiv.Bench as A
import Prelude as P



main :: IO ()
main = do
  let !sz = Sz2 8000 6000
      arr1 = arrRLightIx2 P Seq sz
      arr2 = arrRHeavyIx2 P Seq sz
  defaultMain
    [ bgroup
        "Plus"
        [ env (return (arr1, arr2)) $ \ ~(a1, a2) ->
            bgroup
              "Seq"
              [ bench "(+) P" $ whnf (a1 !+!) a2
              , bench "zipWith (+)" $ whnf (A.computeAs P . A.zipWith (+) a1) a2
              ]
        , env (return (setComp (ParN 4) arr1, arr2)) $ \ ~(a1, a2) ->
            bgroup
              "ParN 4" -- memory bound, too many workers only make it slower
              [ bench "(+) P" $ whnf (a1 !+!) a2
              , bench "zipWith (+)" $ whnf (A.computeAs P . A.zipWith (+) a1) a2
              ]
        ]
    ]
