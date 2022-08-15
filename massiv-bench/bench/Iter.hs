{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad.ST
import Control.Scheduler
import Criterion.Main
import Data.Massiv.Array as A
import Data.Typeable

baseline :: Sz1 -> IO ()
baseline (Sz sz) = loopA_ 0 (< sz) (+ 1) $ \i -> i `seq` pure ()


main :: IO ()
main = do
  let !sz5@(Sz5 n5 n4 n3 n2 n1) = Sz5 50 50 50 50 50
      !sz4 = Sz4 (n5 * n4) n3 n2 n1
      !sz3 = Sz3 (n5 * n4) (n3 * n2) n1
      !sz2 = Sz2 (n5 * n4) (n3 * n2 * n1)
      !sz1 = toLinearSz sz5
  defaultMain
    [ bgroup
        "Seq"
        [ bgroup
            "Full"
            [ bench "baseline" $ whnfIO $ baseline sz1
            , iterFullBench sz1
            , iterFullBench sz2
            , iterFullBench sz3
            , iterFullBench sz4
            , iterFullBench sz5
            ]
        , bgroup
            "Stride"
            [ iterStrideBench (Stride 3) sz1
            , iterStrideBench (Stride 3) sz2
            , iterStrideBench (Stride 3) sz3
            , iterStrideBench (Stride 3) sz4
            , iterStrideBench (Stride 3) sz5
            ]
        ]
    , bgroup
        "Par"
        [ bgroup
            "Full"
            [ iterFullBenchPar sz1
            , iterFullBenchPar sz2
            , iterFullBenchPar sz3
            , iterFullBenchPar sz4
            , iterFullBenchPar sz5
            ]
        , bgroup
            "Stride"
            [ iterStrideBenchPar (Stride 3) sz1
            , iterStrideBenchPar (Stride 3) sz2
            , iterStrideBenchPar (Stride 3) sz3
            , iterStrideBenchPar (Stride 3) sz4
            , iterStrideBenchPar (Stride 3) sz5
            ]
        ]
    ]


iterFullBench :: Index ix => Sz ix -> Benchmark
iterFullBench !sz =
  bgroup
    (show (typeOf sz))
    [ bench "iterFullA_ (RowMajor)" $ whnfIO $
      iterFullA_ defRowMajor zeroIndex sz (\ix -> ix `seq` pure ())
    , bench "iterTargetFullST_ (RowMajor)" $ whnfIO $
      stToIO $ iterTargetFullST_ defRowMajor trivialScheduler_ 0 sz seq2Action
    , bench "iterFullM (RowMajor)" $ whnfIO $
      stToIO $ iterFullM defRowMajor zeroIndex sz () seq2Action
    , bench "iterFullAccST (RowMajor)" $ whnfIO $
      stToIO $ iterFullAccST defRowMajor trivialScheduler_ zeroIndex sz () noopSplit seq2Action
    , bench "iterFullA_ (RowMajorLinear)" $ whnfIO $
      iterFullA_ defRowMajorLinear zeroIndex sz (\ix -> ix `seq` pure ())
    , bench "iterTargetFullST_ (RowMajorLinear)" $ whnfIO $
      stToIO $ iterTargetFullST_ defRowMajorLinear trivialScheduler_ 0 sz seq2Action
    , bench "iterFullM (RowMajorLinear)" $ whnfIO $
      stToIO $ iterFullM defRowMajorLinear zeroIndex sz () seq2Action
    , bench "iterFullAccST (RowMajorLinear)" $ whnfIO $
      stToIO $ iterFullAccST defRowMajorLinear trivialScheduler_ zeroIndex sz () noopSplit seq2Action
    ]

noopSplit :: () -> ST s ((), ())
noopSplit () = pure ((), ())

iterStrideBench :: (Num ix, Index ix) => Stride ix -> Sz ix -> Benchmark
iterStrideBench !stride !sz =
  bgroup
    (show (typeOf sz))
    [ bench "iterTargetFullWithStrideST_ (RowMajor)" $
      whnfIO $
      stToIO $ iterTargetFullWithStrideST_ defRowMajor trivialScheduler_ 0 sz stride seq2Action
    , bench "iterTargetFullWithStrideST_ (RowMajorLinear)" $
      whnfIO $
      stToIO $ iterTargetFullWithStrideST_ defRowMajorLinear trivialScheduler_ 0 sz stride seq2Action
    ]


iterFullBenchPar :: Index ix => Sz ix -> Benchmark
iterFullBenchPar sz =
  bgroup
    (show (typeOf sz))
    [ bench "iterTargetFullST_ (RowMajor)" $ whnfIO $
      withMassivScheduler_ Par $ \ scheduler ->
        stToIO $ iterTargetFullST_ defRowMajor scheduler 0 sz seq2Action
    , bench "iterTargetFullAccST (RowMajor)" $ whnfIO $
      withMassivScheduler_ Par $ \ scheduler ->
        stToIO $ iterTargetFullAccST defRowMajor scheduler 0 sz () noopSplit seq3Action
    , bench "iterFullAccST (RowMajor)" $ whnfIO $
      withMassivScheduler_ Par $ \ scheduler ->
        stToIO $ iterFullAccST defRowMajor scheduler zeroIndex sz () noopSplit seq2Action
    , bench "iterTargetFullST_ (RowMajorLinear)" $ whnfIO $
      withMassivScheduler_ Par $ \ scheduler ->
        stToIO $ iterTargetFullST_ defRowMajorLinear scheduler 0 sz seq2Action
    , bench "iterTargetFullAccST (RowMajorLinear)" $ whnfIO $
      withMassivScheduler_ Par $ \ scheduler ->
        stToIO $ iterTargetFullAccST defRowMajorLinear scheduler 0 sz () noopSplit seq3Action
    , bench "iterFullAccST (RowMajorLinear)" $ whnfIO $
      withMassivScheduler_ Par $ \ scheduler ->
        stToIO $ iterFullAccST defRowMajorLinear scheduler zeroIndex sz () noopSplit seq2Action
    ]

iterStrideBenchPar :: (Num ix, Index ix) => Stride ix -> Sz ix -> Benchmark
iterStrideBenchPar stride sz =
  bgroup
    (show (typeOf sz))
    [ bench "iterTargetFullWithStrideST_ (RowMajor)" $ whnfIO $
      withMassivScheduler_ Par $ \ scheduler ->
        stToIO $ iterTargetFullWithStrideST_ defRowMajor scheduler 0 sz stride seq2Action
    , bench "iterTargetFullWithStrideAccST (RowMajor)" $ whnfIO $
      withMassivScheduler_ Par $ \ scheduler ->
        stToIO $ iterTargetFullWithStrideAccST defRowMajor scheduler 0 sz stride () noopSplit seq3Action
    , bench "iterTargetFullWithStrideST_ (RowMajorLinear)" $ whnfIO $
      withMassivScheduler_ Par $ \ scheduler ->
        stToIO $ iterTargetFullWithStrideST_ defRowMajorLinear scheduler 0 sz stride seq2Action
    , bench "iterTargetFullWithStrideAccST (RowMajorLinear)" $ whnfIO $
      withMassivScheduler_ Par $ \ scheduler ->
        stToIO $
        iterTargetFullWithStrideAccST defRowMajorLinear scheduler 0 sz stride () noopSplit seq3Action
    ]

seq2Action :: Monad m => a -> b -> m ()
seq2Action a b = a `seq` b `seq` pure ()
{-# INLINE seq2Action #-}

seq3Action :: Monad m => a -> b -> c -> m ()
seq3Action a b c = a `seq` b `seq` c `seq` pure ()
{-# INLINE seq3Action #-}



