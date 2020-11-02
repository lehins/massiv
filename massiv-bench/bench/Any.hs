{-# LANGUAGE BangPatterns #-}
module Main where

import Criterion.Main
import Data.Foldable as F
import Data.Massiv.Array as A
import Data.Massiv.Array.Manifest.Vector as A
import qualified Data.Vector.Primitive as VP



main :: IO ()
main =
  defaultMain
    [ anyBench "None" arrayNone
    , anyBench "Late" arrayLate
    , anyBench "Early" arrayEarly
    ]


anyBench :: String -> Vector P Int -> Benchmark
anyBench name arr =
  bgroup
    name
    [ env (pure (A.toList arr)) $ \ ls -> bench "list" $ whnf (F.any even) ls
    , env (pure (A.toVector arr)) $ \ vec -> bench "vector" $ whnf (VP.any even) vec
    , bench "F.any (DS)" $ whnf (F.any even . toStreamArray) arr
    , bench "sany (DS)" $ whnf (sany even . toStreamArray) arr
    , bench "F.any (D - Seq)" $ whnf (F.any even . delay) arr
    , bench "F.any (D - Par)" $ whnf (F.any even . delay) (setComp Par arr)
    , bench "any (D - Seq)" $ whnf (A.any even . delay) arr
    , bench "any (D - Par)" $ whnf (A.any even . delay) (setComp Par arr)
    ]


-- Has no even numbers will not short-circuit
arrayNone :: Array P Ix1 Int
arrayNone = computeAs P $ A.enumFromStepN Seq 1 2 8000000
{-# NOINLINE arrayNone #-}

-- Has an even number in the middle of the first 1/8th portion of the array. Should
-- short-circuit quickly with or without parallelization
arrayEarly :: Array P Ix1 Int
arrayEarly =
  computeAs P $
  concat' 1 [A.enumFromStepN Seq 1 2 1000000, A.singleton 2, A.enumFromStepN Seq 1 2 6999999]
{-# NOINLINE arrayEarly #-}

-- Has an even number in the beginning of the last 1/8th portion of the array. Should
-- short-circuit quickly with parallelization on 8 cores, but not too fast when compured
-- sequentially
arrayLate :: Array P Ix1 Int
arrayLate =
  computeAs P $
  concat' 1 [A.enumFromStepN Seq 1 2 4000000, A.singleton 2, A.enumFromStepN Seq 1 2 3999999]
{-# NOINLINE arrayLate #-}

