{-# LANGUAGE BangPatterns #-}
module Main where

import Criterion.Main
import Data.Massiv.Array as A
import Data.Massiv.Array.Mutable as A
import Data.Massiv.Bench as A
import Data.Monoid
import Prelude as P


main :: IO ()
main = do
  let !sz = Sz2 1600 1200
  defaultMain
    [ bgroup
        "Thaw"
        [ env (return (arrRLightIx2 P Seq sz)) $ \arr ->
            bgroup
              "Seq"
              [ bench "thawS" $ whnfIO (thawS arr)
              ]
        , env (return (arrRLightIx2 P Par sz)) $ \arr ->
            bgroup
              "Par"
              [ bench "thaw" $ whnfIO (thaw arr)
              ]
        ]
    ]
