{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main
import Data.Massiv.Array as A
import System.Random

main :: IO ()
main = do
  defaultMain
    [ bgroup
        "Seq"
        [ bgroup
            "randomArray"
            [ randomArrayBench stdGen Seq sz1
            , randomArrayBench stdGen Seq sz2
            , randomArrayBench stdGen Seq sz3
            , randomArrayBench stdGen Seq sz4
            , randomArrayBench stdGen Seq sz5
            ]
        , mkSplitSeedBench "RowMajor" defRowMajor Seq
        , mkSplitSeedBench "RowMajorLinear" defRowMajorLinear Seq
        , mkSplitSeedBench "RowMajorUnbalanced" defRowMajorUnbalanced Seq
        ]
    , bgroup
        "Par"
        [ bgroup
            "randomArray"
            [ randomArrayBench stdGen Par sz1
            , randomArrayBench stdGen Par sz2
            , randomArrayBench stdGen Par sz3
            , randomArrayBench stdGen Par sz4
            , randomArrayBench stdGen Par sz5
            ]
        , mkSplitSeedBench "RowMajor" defRowMajor Par
        , mkSplitSeedBench "RowMajorLinear" defRowMajorLinear Par
        , mkSplitSeedBench "RowMajorUnbalanced" defRowMajorUnbalanced Par
        ]
    ]
  where
    !sz5@(Sz5 n5 n4 n3 n2 n1) = Sz5 100 10 10 10 10
    !sz4 = Sz4 (n5 * n4) n3 n2 n1
    !sz3 = Sz3 (n5 * n4) (n3 * n2) n1
    !sz2 = Sz2 (n5 * n4) (n3 * n2 * n1)
    !sz1 = toLinearSz sz5
    !stdGen = mkStdGen 2022
    mkSplitSeedBench itName it comp =
      bgroup
        itName
        [ bgroup
            "makeSplitSeedArray"
            [ makeSplitSeedArrayBench it stdGen comp sz1
            , makeSplitSeedArrayBench it stdGen comp sz2
            , makeSplitSeedArrayBench it stdGen comp sz3
            , makeSplitSeedArrayBench it stdGen comp sz4
            , makeSplitSeedArrayBench it stdGen comp sz5
            ]
        , bgroup
            "generateSplitSeedArray"
            [ generateSplitSeedArrayBench it stdGen comp sz1
            , generateSplitSeedArrayBench it stdGen comp sz2
            , generateSplitSeedArrayBench it stdGen comp sz3
            , generateSplitSeedArrayBench it stdGen comp sz4
            , generateSplitSeedArrayBench it stdGen comp sz5
            ]
        ]

randomArrayBench :: forall ix. Index ix => StdGen -> Comp -> Sz ix -> Benchmark
randomArrayBench !stdGen !comp !sz =
  bench (show sz) $
    whnfIO
      ( computeIO $
          randomArray stdGen split uniform comp sz
          :: IO (Array P ix Word)
      )

makeSplitSeedArrayBench
  :: forall it ix
   . (Index ix, Iterator it)
  => it
  -> StdGen
  -> Comp
  -> Sz ix
  -> Benchmark
makeSplitSeedArrayBench it !stdGen !comp !sz =
  bench (show sz) $
    whnfIO $
      (computeIO @P $ makeSplitSeedArray @ix @Word it stdGen split comp sz (\_ _ -> uniform))

generateSplitSeedArrayBench
  :: forall it ix
   . (Index ix, Iterator it)
  => it
  -> StdGen
  -> Comp
  -> Sz ix
  -> Benchmark
generateSplitSeedArrayBench it !stdGen !comp sz =
  bench (show sz) $
    nf (\n -> generateSplitSeedArray @P @ix @Word it stdGen (pure . split) comp n (\_ _ -> pure . uniform)) sz
