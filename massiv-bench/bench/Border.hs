{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Criterion.Main
import Data.Massiv.Core
import Data.Typeable

main :: IO ()
main =
  defaultMain
    [ bgroup
        "handleBorderIndex"
        [ handleBorderIndexBenchGroup $ Fill ()
        , handleBorderIndexBenchGroup Wrap
        , handleBorderIndexBenchGroup Edge
        , handleBorderIndexBenchGroup Reflect
        , handleBorderIndexBenchGroup Continue
        ]
    ]

showBorderType :: Border a -> String
showBorderType = \case
  Fill _ -> "Fill"
  border -> show (() <$ border)

handleBorderIndexBenchGroup :: Border () -> Benchmark
handleBorderIndexBenchGroup border =
  let sz3 :: Sz3
      sz3 = Sz $ pureIndex 4
      d3 = 16 -- out of bounds cells: 46592
      sz2 :: Sz2
      sz2 = Sz $ pureIndex 8
      d2 = 104 -- out of bounds cells: 46592
      sz1 :: Sz1
      sz1 = Sz $ pureIndex 64
      d1 = 23296 -- out of bounds cells: 46592
   in bgroup
        (showBorderType border)
        [ handleBorderIndexBench border sz3 d3
        , handleBorderIndexLinearBench border sz3 d3
        , handleBorderIndexBench border sz2 d2
        , handleBorderIndexLinearBench border sz2 d2
        , handleBorderIndexBench border sz1 d1
        , handleBorderIndexLinearBench border sz1 d1
        ]

handleBorderIndexBench :: Index ix => Border () -> Sz ix -> Int -> Benchmark
handleBorderIndexBench !border !sz !distance =
  let !startIx = liftIndex (subtract distance) zeroIndex
      !endIx = liftIndex (+ distance) (unSz sz)
      !borderIO = pure <$> border
   in bench (show (typeOf sz) ++ ": toLinear " ++ show distance) $
        whnfIO $
          iterA_ startIx endIx (pureIndex 1) (<) (handleBorderIndex borderIO sz ((`seq` pure ()) . toLinearIndex sz))


handleBorderIndexLinearBench :: Index ix => Border () -> Sz ix -> Int -> Benchmark
handleBorderIndexLinearBench !border !sz !distance =
  let !startIx = liftIndex (subtract distance) zeroIndex
      !endIx = liftIndex (+ distance) (unSz sz)
      !borderIO = pure <$> border
   in bench (show (typeOf sz) ++ ": Linear " ++ show distance) $
        whnfIO $
          iterA_ startIx endIx (pureIndex 1) (<) (handleBorderIndexLinear borderIO sz (`seq` pure ()))
