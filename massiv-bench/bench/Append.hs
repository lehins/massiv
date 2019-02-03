{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Control.Monad            (guard)
import           Criterion.Main
import qualified Data.DList               as DL
import           Data.Massiv.Array        as A
import qualified Data.Foldable            as F
import           Data.Massiv.Array.Unsafe as A
import           Data.Massiv.Bench        as A
import qualified Data.Vector.Primitive    as VP
import           Prelude                  as P

main :: IO ()
main = do
  let !sz = Sz (600 :. 1000)
  defaultMain
    [ mkAppendBenchGroup "LeftToRight" (Dim 1) sz
    , mkAppendBenchGroup "TopToBottom" (Dim 2) sz
    , env (return (computeAs P $ resize' (Sz $ totalElem sz) $ arrRLightIx2 D Seq sz)) $ \arr ->
        bgroup
          "Monoid"
          [ bench "computeDL" $ whnf (A.computeAs P . toLoadArray) arr
          , bench "computeD" $ whnf (A.computeAs P . delay) arr
          , bench "mappend" $ whnf (\a -> A.computeAs P (toLoadArray a <> toLoadArray a)) arr
          , bench "myAppend" $
            whnf (\a -> A.computeAs P (toLoadArray a `myAppend` toLoadArray a)) arr
          -- , bench "mconcat" $
          --   whnf (\a -> A.computeAs P (mconcat [toLoadArray a, toLoadArray a])) arr
          ]
    , bgroup
        "cons"
        [ bench "Array DL Ix1 Int (10000)" $ nf (A.computeAs P . consArray 10000) empty
        , bench "VP.Vector Int (10000)" $ nf (consVector 10000) VP.empty
        , bench "[Int] (10000)" $ nf (consList 10000) []
        ]
    , bgroup
        "snoc"
        [ bench "Array DL Ix1 Int (10000)" $ nf (A.computeAs P . snocArray 10000) empty
        , bench "VP.Vector Int (10000)" $ nf (snocVector 10000) VP.empty
        , bench "DList Int (10000)" $ nf (snocList 10000) DL.empty
        ]
    ]
  where
    consList :: Int -> [Int] -> [Int]
    consList 0 !acc = acc
    consList !n !acc = consList (n - 1) (n : acc)
    consArray :: Int -> Array DL Ix1 Int -> Array DL Ix1 Int
    consArray 0 !acc = acc
    consArray !n !acc = consArray (n - 1) (n `cons` acc)
    consVector :: Int -> VP.Vector Int -> VP.Vector Int
    consVector 0 !acc = acc
    consVector !n !acc = consVector (n - 1) (n `VP.cons` acc)
    snocList :: Int -> DL.DList Int -> [Int]
    snocList 0 !acc = DL.toList acc
    snocList !n !acc = snocList (n - 1) (acc `DL.snoc` n)
    snocArray :: Int -> Array DL Ix1 Int -> Array DL Ix1 Int
    snocArray 0 !acc = acc
    snocArray !n !acc = snocArray (n - 1) (acc `snoc` n)
    snocVector :: Int -> VP.Vector Int -> VP.Vector Int
    snocVector 0 !acc = acc
    snocVector !n !acc = snocVector (n - 1) (acc `VP.snoc` n)

mkAppendBenchGroup :: String -> Dim -> Sz2 -> Benchmark
mkAppendBenchGroup gname dim sz =
  bgroup
    ("Append " ++ gname)
    [ env (return (arrRLightIx2 P Seq sz)) $ \arr ->
        bgroup
          "Seq"
          [ bench "append" $ whnf (A.computeAs P . append' dim arr) arr
          , bench "concat" $ whnf (A.computeAs P . A.concat' dim . (: [arr])) arr
          -- , bench "appendLoad" $ whnf (A.computeAs P . fromJust . appendLoad dim arr) arr
          -- , bench "appendPull" $ whnf (A.computeAs P . fromJust . appendPull dim arr) arr
          ]
    , env (return (arrRLightIx2 P Par sz)) $ \arr ->
        bgroup
          "Par"
          [ bench "append" $ whnf (A.computeAs P . append' dim arr) arr
          , bench "concat" $ whnf (A.computeAs P . A.concat' dim . (: [arr])) arr
          -- , bench "appendLoad" $ whnf (A.computeAs P . fromJust . appendLoad dim arr) arr
          -- , bench "appendPull" $ whnf (A.computeAs P . fromJust . appendPull dim arr) arr
          ]
    ]


myAppend :: (Load r1 Int e, Load r2 Int e) => Array r1 Int e -> Array r2 Int e -> Array DL Int e
myAppend arr1 arr2 =
  let !k = unSz $ size arr1
   in makeLoadArray
        (getComp arr1 <> getComp arr2)
        (SafeSz (k + unSz (size arr2)))
        (\numWorkers scheduleWith dlWrite -> do
           loadArray numWorkers scheduleWith arr1 dlWrite
           loadArray numWorkers scheduleWith arr2 (\ !i -> dlWrite (i + k)))
{-# INLINE myAppend #-}
