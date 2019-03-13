{-# LANGUAGE BangPatterns #-}
module Main where

import           Criterion.Main
import           Control.DeepSeq
import           Data.Massiv.Bench          as A
import           Data.Massiv.Core.Index
import           Prelude                    as P
import qualified Data.Massiv.Scheduler as S

main :: IO ()
main = 
  defaultMain $ map (mkBench "return" return) [100, 10000, 100000]



mkBench :: (NFData a) => String -> (Int -> IO a) -> Int -> Benchmark
mkBench name f n =
  bgroup
    ("(" ++ show n ++ ") mapConcurrently - action: " ++ name)
    [ bgroup
        "Massiv Scheduler"
        [ bench "Par" $ nfIO (S.mapConcurrently S.Par f [0 .. n])
        , bench "ParN" $ nfIO (S.mapConcurrently (S.ParN 0) f [0 .. n])
        , bench "Seq" $ nfIO (S.mapConcurrently S.Seq f [0 .. n])
        ]
    ]
