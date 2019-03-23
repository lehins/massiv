{-# LANGUAGE BangPatterns #-}
module Main where

import Control.DeepSeq
import Control.Massiv.Scheduler as S
import Criterion.Main
import Prelude as P

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
