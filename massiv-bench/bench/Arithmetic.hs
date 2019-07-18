{-# LANGUAGE BangPatterns #-}
module Main where

import Criterion.Main
import Data.Massiv.Array as A
--import Data.Massiv.Core.Operations
import Data.Massiv.Array.SIMD
import Data.Massiv.Bench as A


main :: IO ()
main = do
  let !sz = Sz2 16 16
      !arr1' = arrRLightIx2 S Seq sz
      !arr2' = computeAs S $ transpose arr1'
      !arrV1' = arrRLightIx2 V Seq sz
      !arrV2' = computeAs V $ transpose arr1'
  defaultMain
    [ env (return (arr1', arr2')) $ \ ~(arr1, arr2) ->
        bgroup
          "Addition"
          [ bgroup
              "S (Seq)"
              [ bench "zipWith (+)" $ whnf (computeAs S . A.zipWith (+) arr1) arr2
              , bench "(.+.)" $ whnfIO (computeAs S <$> (toManifest arr1 .+. toManifest arr2))
              ]
          ]
    , env (return (setComp Par arr1', setComp Par arr2')) $ \ ~(arr1, arr2) ->
        bgroup
          "Addition"
          [ bgroup
              "S (Par)"
              [ bench "zipWith (+)" $ whnf (computeAs S . A.zipWith (+) arr1) arr2
              , bench "(.+.)" $ whnfIO (computeAs S <$> (toManifest arr1 .+. toManifest arr2))
              ]
          ]
    , env (return (arrV1', arrV2')) $ \ ~(arr1, arr2) ->
        bgroup
          "Addition"
          [ bgroup
              "V (Seq)"
              [ bench "zipWith (+)" $ whnf (computeAs V . A.zipWith (+) arr1) arr2
              , bench "(.+.)" $ whnfIO (arr1 .+. arr2)
              ]
          ]
    , env (return (setComp Par arrV1', setComp Par arrV2')) $ \ ~(arr1, arr2) ->
        bgroup
          "Addition"
          [ bgroup
              "V (Par)"
              [ bench "zipWith (+)" $ whnf (computeAs V . A.zipWith (+) arr1) arr2
              , bench "(.+.)" $ whnfIO (arr1 .+. arr2)
              ]
          ]
    ]
