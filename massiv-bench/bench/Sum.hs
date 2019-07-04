{-# LANGUAGE BangPatterns #-}
module Main where

import Criterion.Main
import Data.Massiv.Array as A
import Data.Massiv.Array.SIMD.Double
import Data.Massiv.Bench as A
import Data.Monoid
import Prelude as P


main :: IO ()
main = do
  let !sz = Sz2 1600 12000
      !arrSeq = arrRLightIx2 P Seq sz
      !arrPar = arrRLightIx2 P Par sz
  defaultMain
    [ bgroup
        "Sum"
        [ env (return (arrSeq, computeAs V arrPar)) $ \ ~(arr, arrV) ->
            bgroup
              "Seq"
              [ bench "foldlS" $ whnf (A.foldlS (+) 0) arr
              , bench "foldrS" $ whnf (A.foldrS (+) 0) arr
              , bench "sum" $ whnf A.sum arr
              , bench "sum'" $ whnfIO (A.sum' arr)
              , bench "sum (SIMD)" $ whnf sumDouble arrV
              , bench "foldMono" $ whnf (getSum . foldMono Sum) arr
              , bench "foldlS . foldlWithin Dim2" $ whnf (A.foldlS (+) 0 . foldlWithin Dim2 (+) 0) arr
              , bench "foldlS . foldlInner" $ whnf (A.foldlS (+) 0 . foldlInner (+) 0) arr
              ]
        , env (return (arrPar, computeAs V arrPar)) $ \ ~(arr, arrV) ->
            bgroup
              "Par"
              [ bench "foldlP" $ whnfIO (A.foldlP (+) 0 (+) 0 arr)
              , bench "foldrP" $ whnfIO (A.foldrP (+) 0 (+) 0 arr)
              , bench "sum" $ whnf A.sum arr
              , bench "sum'" $ whnfIO (A.sum' arr)
              , bench "sum (SIMD)" $ whnfIO (sumSIMD arrV)
              , bench "foldMono" $ whnf (getSum . foldMono Sum) arr
              , bench "foldlS . foldlWithin Dim2" $
                whnfIO (A.foldlP (+) 0 (+) 0 $ foldlWithin Dim2 (+) 0 arr)
              , bench "foldlS . foldlInner" $
                whnfIO (A.foldlP (+) 0 (+) 0 $ foldlInner (+) 0 arr)
              ]
        ]
    ]


sumSIMD :: Array V Ix2 Double -> IO Double
sumSIMD = splitReduce (\_ a -> sumDoubleIO a) (\x y -> pure (x + y)) 0
{-# INLINE sumSIMD #-}
