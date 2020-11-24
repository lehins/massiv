{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Criterion.Main
import Data.Massiv.Array as A
import Data.Massiv.Bench.Vector


main :: IO ()
main = do
  defaultMain [benchV1 (randomV1 :: Vector P Double), benchVxV (randomVxV :: VxV P Double)]
