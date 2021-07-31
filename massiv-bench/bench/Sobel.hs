{-# LANGUAGE BangPatterns #-}
module Main where

import Criterion.Main
import Data.Massiv.Array as A
import Data.Massiv.Bench as A
import Data.Massiv.Bench.Sobel as A
import Prelude as P



main :: IO ()
main = do
  let !sz = Sz2 1600 1200
  defaultMain [sobelBenchGroup sz]
