{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Criterion.Main
import Data.Massiv.Array as A
import Data.Massiv.Bench as A
import Prelude as P

main :: IO ()
main = do
  let !sz = Sz (600 :. 1000)
      !strOdd = Stride (3 :. 6)
      !strPowerOf2 = Stride (2 :. 4)
  defaultMain [mkUpsampleBenchGroup "Odd" sz strOdd, mkUpsampleBenchGroup "PowerOf2" sz strPowerOf2]


mkUpsampleBenchGroup :: String -> Sz2 -> Stride Ix2 -> Benchmark
mkUpsampleBenchGroup gname sz str =
  bgroup
    ("Upsample " ++ gname)
    [ env (return (arrRLightIx2 P Seq sz)) $ \arr ->
        bgroup
          "Seq"
          [ bench "upsample" $ whnf (A.computeAs P . upsample 0 str) arr
          ]
    , env (return (arrRLightIx2 P Par sz)) $ \arr ->
        bgroup
          "Par"
          [ bench "upsample" $ whnf (A.computeAs P . upsample 0 str) arr
          ]
    ]
