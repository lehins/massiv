{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent
import Criterion.Main
import Data.Bits
import Data.Int
import Data.Massiv.Array as A
import Data.Word
import System.Random

log2i :: Int -> Int
log2i i = 63 - countLeadingZeros (fromIntegral i :: Word64)

main :: IO ()
main = do
  let stdGen = mkStdGen 2023
      -- sizes = [512,4096,32768,262144,2097152]
      sizes = Prelude.take (log2i 32) $ iterate (* 8) 512
  numCaps <- getNumCapabilities
  defaultMain
    [ bgroup
        "Sort"
        [ mkGroups numCaps (compute (uniformArray stdGen Par (Sz sz)))
        | sz <- sizes
        ]
    ]

mkGroups :: Int -> Array S Ix1 Int64 -> Benchmark
mkGroups numCaps !vRand =
  bgroup
    (show (unSz (size vRand)))
    [ mkGroup numCaps "random" (A.delay vRand)
    , mkGroup numCaps "sorted" (A.delay (A.quicksort vRand))
    , mkGroup numCaps "reversed sorted" (A.reverse Dim1 (A.quicksort vRand))
    , mkGroup numCaps "replicated" (A.replicate @D Seq (A.size vRand) 31415)
    ]

mkGroup :: (Size r, Load r Ix1 Int64) => Int -> String -> Array r Ix1 Int64 -> Benchmark
mkGroup numCaps name !v =
  bgroup
    name
    [ env (computeIO v :: IO (Vector P Int64)) $ \vec ->
      bench
        ( "massiv/Array "
            ++ show comp
            ++ if comp == Seq then [] else " (min " ++ w n ++ " " ++ show s ++ ")"
        )
        $ nf A.quicksort (setComp comp vec)
    | (comp, n) <-
        (Seq, 1)
          : ((\n -> (ParN n, n)) <$> Prelude.take (log2i numCaps) (iterate (* 2) 2))
          ++ [(Par, fromIntegral numCaps)]
    ]
  where
    w n = show (log2i (fromIntegral n) + 4)
    s = log2i (unSz (size v)) - 10
