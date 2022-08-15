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
    [ bgroup
        "Any"
        [ anyBench "None" arrayEvenNone
        , anyBench "Late" arrayEvenLate
        , anyBench "Early" arrayEvenEarly
        ]
    , bgroup
        "All"
        [ allBench "None" arrayEvenNone
        , allBench "Late" arrayEvenLate
        , allBench "Early" arrayEvenEarly
        ]
    ]


anyBench :: String -> Vector P Int -> Benchmark
anyBench name arr =
  bgroup
    name
    [ env (pure (A.toList arr)) $ \ ls -> bench "list" $ whnf (F.any even) ls
    , env (pure (A.toVector arr)) $ \ vec -> bench "vector" $ whnf (VP.any even) vec
    , bench "F.any (DS)" $ whnf (F.any even . toStreamArray) arr
    , bench "sany (DS)" $ whnf (sany even) arr
    , bench "F.any (D - Seq)" $ whnf (F.any even . delay) arr
    , bench "F.any (D - Par)" $ whnf (F.any even . delay) (setComp Par arr)
    , bench "any (D - Seq)" $ whnf (A.any even . delay) arr
    , bench "any (D - Par)" $ whnf (A.any even . delay) (setComp Par arr)
    , bench "any (P - Seq)" $ whnf (A.any even) arr
    , bench "any (P - Par)" $ whnf (A.any even) (setComp Par arr)
    ]

allBench :: String -> Vector P Int -> Benchmark
allBench name arr =
  bgroup
    name
    [ env (pure (A.toList arr)) $ \ ls -> bench "list" $ whnf (F.all odd) ls
    , env (pure (A.toVector arr)) $ \ vec -> bench "vector" $ whnf (VP.all odd) vec
    , bench "F.all (DS)" $ whnf (F.all odd . toStreamArray) arr
    , bench "sall (DS)" $ whnf (sall odd) arr
    , bench "F.all (D - Seq)" $ whnf (F.all odd . delay) arr
    , bench "F.all (D - Par)" $ whnf (F.all odd . delay) (setComp Par arr)
    , bench "all (D - Seq)" $ whnf (A.all odd . delay) arr
    , bench "all (D - Par)" $ whnf (A.all odd . delay) (setComp Par arr)
    , bench "all (P - Seq)" $ whnf (A.all odd) arr
    , bench "all (P - Par)" $ whnf (A.all odd) (setComp Par arr)
    ]


-- Has no even numbers will not short-circuit
arrayEvenNone :: Array P Ix1 Int
arrayEvenNone = computeAs P $ A.enumFromStepN Seq 1 2 8000000
{-# NOINLINE arrayEvenNone #-}

-- Has an even number in the middle of the first 1/8th portion of the array. Should
-- short-circuit quickly with or without parallelization
arrayEvenEarly :: Array P Ix1 Int
arrayEvenEarly =
  computeAs P $
  concat' 1 [A.enumFromStepN Seq 1 2 1000000, A.singleton 2, A.enumFromStepN Seq 1 2 6999999]
{-# NOINLINE arrayEvenEarly #-}

-- Has an even number in the beginning of the last 1/8th portion of the array. Should
-- short-circuit quickly with parallelization on 8 cores, but not too fast when compured
-- sequentially
arrayEvenLate :: Array P Ix1 Int
arrayEvenLate =
  computeAs P $
  concat' 1 [A.enumFromStepN Seq 1 2 4000000, A.singleton 2, A.enumFromStepN Seq 1 2 3999999]
{-# NOINLINE arrayEvenLate #-}

