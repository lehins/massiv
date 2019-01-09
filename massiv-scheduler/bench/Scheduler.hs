{-# LANGUAGE BangPatterns #-}
module Main where

import           Control.DeepSeq
import           Criterion.Main
import           Data.Massiv.Core.Scheduler as A
import qualified Data.Massiv.Scheduler      as S
import           Prelude                    as P
import qualified UnliftIO.Async             as U (pooledMapConcurrently,
                                                  pooledMapConcurrentlyN)

mapConcurrently :: Foldable t => (a -> IO b) -> t a -> IO [b]
mapConcurrently f ls = withScheduler' [] $ \s -> mapM_ (scheduleWork s . f) ls

mapConcurrentlySeq :: Foldable t => (a -> IO b) -> t a -> IO [b]
mapConcurrentlySeq f ls = withScheduler' [1] $ \s -> mapM_ (scheduleWork s . f) ls



main :: IO ()
main = do
  defaultMain $ map (mkBench "return" return) [100, 10000, 100000]



mkBench :: (NFData a) => String -> (Int -> IO a) -> Int -> Benchmark
mkBench name f n =
  bgroup
    ("(" ++ show n ++ ") mapConcurrently - action: " ++ name)
    [ bgroup
        "UnliftIO"
        [ bench "Par" $ nfIO (U.pooledMapConcurrently f [0 .. n])
        , bench "Seq" $ nfIO (U.pooledMapConcurrentlyN 1 f [0 .. n])
        ]
    , bgroup
        "Massiv Scheduler"
        [ bench "Par" $ nfIO (S.mapConcurrently S.Par f [0 .. n])
        , bench "Seq" $ nfIO (S.mapConcurrently S.Seq f [0 .. n])
        ]
    , bgroup
        "Old Massiv Scheduler"
        [ bench "Par" $ nfIO (mapConcurrently f [0 .. n])
        , bench "Seq" $ nfIO (mapConcurrentlySeq f [0 .. n])
        ]
    ]
