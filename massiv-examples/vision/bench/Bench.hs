module Main where

import Criterion.Main
import Lib

main :: IO ()
main = do
  defaultMain
    [ bgroup "Bench"
      [ bench "someFunc" $ nfIO someFunc
      ]
    ]
