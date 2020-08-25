{-# LANGUAGE BangPatterns #-}
module Main where

import Criterion.Main
import Data.Massiv.Array as A
import Data.Massiv.Bench as A
import Prelude as P



main :: IO ()
main = do
  let !sz = Sz2 1600 1200
  defaultMain
    [ bgroup
        "Plus"
        [ env (return (arrRLightIx2 U Seq sz)) $ \arr ->
            let arrD = delay arr
             in bgroup
                  "Seq"
                  [ bench "(+) D" $ whnf (A.computeAs U . (+) arrD) arrD
                  --, bench "(.+)" $ whnf (A.computeAs U . (.+) arr) arr
                  , bench "zipWith (+)" $ whnf (A.computeAs U . A.zipWith (+) arr) arr
                  ]
        , env (return (arrRLightIx2 U Par sz)) $ \arr ->
            let arrD = delay arr
             in bgroup
                  "Par"
                  [ bench "(+) D" $ whnf (A.computeAs U . (+) arrD) arrD
                  --, bench "(.+)" $ whnf (A.computeAs U . (.+) arr) arr
                  , bench "zipWith (+)" $ whnf (A.computeAs U . A.zipWith (+) arr) arr
                  ]
        ]
    ]
