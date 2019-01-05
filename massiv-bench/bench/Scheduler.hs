{-# LANGUAGE BangPatterns #-}
module Main where

import           Criterion.Main
import           Data.Massiv.Bench          as A
import           Data.Massiv.Core.Index
import           Data.Massiv.Core.Scheduler as A
import           Prelude                    as P


mapConcurrently :: Foldable t => (a -> IO b) -> t a -> IO [b]
mapConcurrently f ls = withScheduler' [] $ \s -> mapM_ (scheduleWork s . f) ls



main :: IO ()
main = do
  let !sz@(Sz2 _ k) = Sz2 600 200
  defaultMain
    [ env (return (totalElem sz)) $ \n ->
        bgroup
          "Map"
          [ bench "mapConcurrently (lightFuncIx1)" $
            whnfIO (mapConcurrently (\i -> return $! lightFuncIx1 k i) [0 .. n - 1])
          , bench "mapConcurrently (succ)" $
            whnfIO (mapConcurrently (\i -> return $! succ i) [0 .. n - 1])
          ]
    ]
