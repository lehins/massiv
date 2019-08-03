{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Criterion.Main
import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe as A
import Data.Massiv.Core.Operations
import Data.Massiv.Array.SIMD
import Data.Massiv.Bench as A
import Data.Monoid
import Prelude as P
import Data.Int

main :: IO ()
main = do
  let !sz = Sz2 1600 1200
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
              , bench "sum (SIMD)" $ whnf A.sum arrV
              , bench "foldMono" $ whnf (getSum . foldMono Sum) arr
              , bench "foldlS . foldlWithin Dim2" $
                whnf (A.foldlS (+) 0 . foldlWithin Dim2 (+) 0) arr
              , bench "foldlS . foldlInner" $ whnf (A.foldlS (+) 0 . foldlInner (+) 0) arr
              ]
        , env (return (arrPar, computeAs F arrPar)) $ \ ~(arr, arrV) ->
            bgroup
              "Par"
              [ bench "foldlP" $ whnfIO (A.foldlP (+) 0 (+) 0 arr)
              , bench "foldrP" $ whnfIO (A.foldrP (+) 0 (+) 0 arr)
              , bench "sum" $ whnf A.sum arr
              , bench "sum (SIMD)" $ whnf A.sum arrV
              , bench "foldMono" $ whnf (getSum . foldMono Sum) arr
              , bench "foldlS . foldlWithin Dim2" $
                whnfIO (A.foldlP (+) 0 (+) 0 $ foldlWithin Dim2 (+) 0 arr)
              , bench "foldlS . foldlInner" $ whnfIO (A.foldlP (+) 0 (+) 0 $ foldlInner (+) 0 arr)
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
    , bgroup
        "RoundNumeric"
        [ env (return (arrSeq, computeAs F arrSeq)) $ \ ~(arr, arrV) ->
            bgroup
              "Seq"
              [ bench "roundPointwise (S)" $ whnf roundPointwiseA arr
              , bench "roundPointwise (F)" $ whnf roundPointwiseA arrV
              ]
        , env (return (arrPar, computeAs F arrPar)) $ \ ~(arr, arrV) ->
            bgroup
              "Par"
              [ bench "roundPointwise (S)" $ whnf roundPointwiseA arr
              , bench "roundPointwise (F)" $ whnf roundPointwiseA arrV
              ]
        ]
    , bgroup
        "Copy"
        [ env (return (arrSeq, computeAs F arrSeq)) $ \ ~(arr, arrV) ->
            bgroup
              "Seq"
              [ bench "clone (S)" $ whnf clone arr
              , bench "clone (F)" $ whnf clone arrV
              , bench "new (S)" $ nfIO (unsafeFreeze Seq =<< new sz :: IO (Array S Ix2 Double))
              , bench "new (F)" $ nfIO (unsafeFreeze Seq =<< new sz :: IO (Array F Ix2 Double))
              ]
        , env (return (arrPar, computeAs F arrPar)) $ \ ~(arr, arrV) ->
            bgroup
              "Par"
              [ bench "clone (S)" $ whnf clone (setComp Par arr)
              , bench "clone (F)" $ whnf clone (setComp Par arrV)
              ]
        ]
    , bgroup
        "New"
        [ env (return (arrSeq, computeAs F arrSeq)) $ \ ~(arr, arrV) ->
            bgroup
              "Seq"
              [ bench "new (S)" $ nfIO (copy' arr :: IO (Array S Ix2 Double))
              , bench "new (F)" $ nfIO (copy' arrV :: IO (Array F Ix2 Double))
              ]
        ]
    ]
  where
    copy' arr = do
      let sz = size arr
      marr <- unsafeNew sz
      unsafeArrayLinearCopy arr 0 marr 0 (SafeSz (totalElem sz))
      --unsafeLinearSet marr 0 (SafeSz (totalElem sz)) 0
      unsafeFreeze Seq marr
    {-# INLINE copy' #-}

roundPointwiseA ::
     (RoundFloatArray r Double Double, Index ix) => Array r ix Double -> Array r ix Double
roundPointwiseA = roundPointwise
{-# INLINE roundPointwiseA #-}
