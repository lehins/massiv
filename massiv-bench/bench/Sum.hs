{-# LANGUAGE BangPatterns #-}
module Main where

import Criterion.Main
import Data.Massiv.Array as A
import Data.Massiv.Array.SIMD
import Data.Massiv.Bench as A
import Data.Monoid
import Prelude as P


main :: IO ()
main = do
  let !sz = Sz2 1600 12000
      !arrSeq = arrRLightIx2 S Seq sz
      !arrPar = arrRLightIx2 S Par sz
  defaultMain
    [ bgroup
        "Sum"
        [ env (return (arrSeq, computeAs F arrSeq)) $ \ ~(arr, arrV) ->
            bgroup
              "Seq"
              [ bench "foldlS" $ whnf (A.foldlS (+) 0) arr
              , bench "foldrS" $ whnf (A.foldrS (+) 0) arr
              , bench "sum" $ whnf A.sum arr
              , bench "sum'" $ whnf A.sum' arr
              , bench "sum' (SIMD)" $ whnf sum' arrV
              , bench "foldMono" $ whnf (getSum . foldMono Sum) arr
              , bench "foldlS . foldlWithin Dim2" $ whnf (A.foldlS (+) 0 . foldlWithin Dim2 (+) 0) arr
              , bench "foldlS . foldlInner" $ whnf (A.foldlS (+) 0 . foldlInner (+) 0) arr
              ]
        , env (return (arrPar, computeAs F arrPar)) $ \ ~(arr, arrV) ->
            bgroup
              "Par"
              [ bench "foldlP" $ whnfIO (A.foldlP (+) 0 (+) 0 arr)
              , bench "foldrP" $ whnfIO (A.foldrP (+) 0 (+) 0 arr)
              , bench "sum" $ whnf A.sum arr
              , bench "sum'" $ whnf A.sum' arr
              , bench "sum' (SIMD)" $ whnf sum' arrV
              , bench "foldMono" $ whnf (getSum . foldMono Sum) arr
              , bench "foldlS . foldlWithin Dim2" $
                whnfIO (A.foldlP (+) 0 (+) 0 $ foldlWithin Dim2 (+) 0 arr)
              , bench "foldlS . foldlInner" $
                whnfIO (A.foldlP (+) 0 (+) 0 $ foldlInner (+) 0 arr)
              ]
        ]
    , bgroup
        "NumericFloat"
        [ env (return (arrSeq, computeAs F arrSeq)) $ \ ~(arr, arrV) ->
            bgroup
              "Seq"
              [ bench "recip (S)" $ whnf recipA arr
              , bench "recip (F)" $ whnf recipA arrV
              , bench "sqrt (S)" $ whnf sqrtA arr
              , bench "sqrt (F)" $ whnf sqrtA arrV
              ]
        , env (return (arrPar, computeAs F arrPar)) $ \ ~(arr, arrV) ->
            bgroup
              "Par"
              [ bench "recip (S)" $ whnf recipA arr
              , bench "recip (F)" $ whnf recipA arrV
              , bench "sqrt (S)" $ whnf sqrtA arr
              , bench "sqrt (F)" $ whnf sqrtA arrV
              ]
        ]
    ]
