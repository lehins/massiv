{-# LANGUAGE BangPatterns #-}
module Main where

import Criterion.Main
import Data.Massiv.Array as A
import Data.Massiv.Bench as A
import Data.Semigroup
import Prelude as P


main :: IO ()
main = do
  let !sz = Sz2 1600 1200
  defaultMain
    [ bgroup
        "Min"
        [ env (return (arrRLightIx2 U Seq sz)) $ \arr ->
            bgroup
              "Seq"
              [ bench "minimum" $ whnf A.minimum' arr
              , bench "foldSemi" $ whnf (getMin . foldSemi Min (Min (arr ! 0))) arr
              ]
        , env (return (arrRLightIx2 U Par sz)) $ \arr ->
            bgroup
              "Par"
              [ bench "minimum" $ whnf A.minimum' arr
              , bench "foldSemi" $ whnf (getMin . foldSemi Min (Min (arr ! 0))) arr
              ]
        ]
    , bgroup
        "Max"
        [ env (return (arrRLightIx2 U Seq sz)) $ \arr ->
            bgroup
              "Seq"
              [ bench "maximum" $ whnf A.maximum' arr
              , bench "foldSemi" $ whnf (getMax . foldSemi Max (Max (arr ! 0))) arr
              ]
        , env (return (arrRLightIx2 U Par sz)) $ \arr ->
            bgroup
              "Par"
              [ bench "maximum" $ whnf A.maximum' arr
              , bench "foldSemi" $ whnf (getMax . foldSemi Max (Max (arr ! 0))) arr
              ]
        ]
    ]
