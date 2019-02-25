{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns     #-}
module Main where

import           Criterion.Main
import           Data.Massiv.Array as A
import           Data.Massiv.Array.Numeric.Integral
import           Prelude           as P


gaussian2 :: Floating a => a -> a -> a -> a
gaussian2 stdDev y x = exp (-(x ^ (2 :: Int) + y ^ (2 :: Int)) / var2) / (var2 * pi)
  where
    var2 = 2 * stdDev ^ (2 :: Int)
{-# INLINE gaussian2 #-}

main :: IO ()
main = do
  let f scale (i :. j) = gaussian2 (1 :: Double) (scale i) (scale j)
      {-# INLINE f #-}
      !a = -3
      !d = 1
      !sz = Sz2 10 10
      !n = 100
  defaultMain
    [ bgroup
        "Integral"
        [ bgroup
            "Seq"
            [ bench "midpoint" $ whnf (midpointRule Seq P f a d sz) n
            , bench "trapezoid" $ whnf (trapezoidRule Seq P f a d sz) n
            , bench "simpsons" $ whnf (simpsonsRule Seq P f a d sz) n
            ]
        , bgroup
            "Par"
            [ bench "midpoint" $ whnf (midpointRule Par P f a d sz) n
            , bench "trapezoid" $ whnf (trapezoidRule Par P f a d sz) n
            , bench "simpsons" $ whnf (simpsonsRule Par P f a d sz) n
            ]
        ]
    ]
