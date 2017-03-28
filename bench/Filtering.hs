{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Compute
import           Criterion.Main
import           Data.Array.Massiv                  as M

import qualified Data.Vector.Unboxed                as VU
import           Prelude                            as P



main :: IO ()
main = do
  let !sz = (1600, 1200 :: Int)
      arr = arrMLight :: (Int, Int) -> Array D DIM2 Int
      vec = vecULight :: (Int, Int) -> VU.Vector Int
  defaultMain
    [ bgroup
        "Filter"
        [ bench "Array Massiv" $ whnf (M.filter even . arr) sz
        , bench "Vector Unboxed" $ whnf (VU.filter even . vec) sz
        ]
    ]
