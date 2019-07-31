{-# LANGUAGE BangPatterns #-}
module Main where

import Criterion.Main
import Data.Massiv.Array as A
import Data.Massiv.Array.SIMD
import Data.Massiv.Bench as A
import Prelude as P hiding ((<>))


main :: IO ()
main = do
  let !sz = Sz2 600 600
      !arr = arrRLightIx2 S Seq sz
      !arr2 = computeAs S arr
      !arrV = arrRLightIx2 F Seq sz
      !arrV' = computeAs F $ A.transpose arrV
  defaultMain
    [ env (return (computeAs S (A.transpose arr))) $ \ arr' ->
        bgroup
          "Mult"
          [ bgroup
              "Seq"
              [ bench "(|*|) (S)" $ whnfIO (arr |*| arr')
              , bench "(|*|) (S)" $ whnfIO (arrV |*| arrV')
              , bench "multiplyTranspose (S)" $
                whnfIO (computeAs S <$> multiplyTransposed arr arr2)
              , bench "multiplyTranspose' (S)" $
                whnf (computeAs S . multiplyTransposed' arr) arr'
              , bench "multiplyTranspose' (F)" $
                whnf (computeAs F . multiplyTransposed' arrV) arrV'
              ]
          , bgroup
              "Par"
              [ bench "(|*|)" $ whnfIO (setComp Par arr |*| arr')
              , bench "(|*|)" $ whnfIO (setComp Par arrV |*| arrV')
              , bench "fused (|*|)" $ whnfIO (setComp Par arr |*| A.transpose arr)
              , bench "multiplyTranspose (S)" $
                whnfIO (computeAs S <$> multiplyTransposed (setComp Par arr) arr2)
              , bench "multiplyTranspose' (S)" $
                whnf (computeAs S . multiplyTransposed' (setComp Par arr)) arr'
              , bench "multiplyTranspose' (F)" $
                whnf (computeAs F . multiplyTransposed' (setComp Par arrV)) arrV'
              ]
          ]
    ]
