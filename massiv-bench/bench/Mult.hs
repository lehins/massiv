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
     -- !arr2 = computeAs S arr
      !arrV = arrRLightIx2 F Seq sz
     -- !arrV2 = computeAs F arr
      !arrV' = computeAs F $ A.transpose arrV
  defaultMain
    [ env (return (computeAs S (A.transpose arr))) $ \ arr' ->
        bgroup
          "Mult"
          [ bgroup
              "Seq"
              [ bench "(|*|) (S)" $ whnfIO (arr |*| arr')
              , bench "(|*|) (F)" $ whnfIO (arrV |*| arrV')
              , bench "(.*.) (S)" $ whnfIO (arr .*. arr')
              , bench "(.*.) (F)" $ whnfIO (arrV .*. arrV')
              , bench "(./.) (S)" $ whnfIO (arr ./. arr')
              , bench "(./.) (F)" $ whnfIO (arrV ./. arrV')
              ]
          , bgroup
              "Par"
              [ bench "(|*|) (S)" $ whnfIO (setComp Par arr |*| arr')
              , bench "(|*|) (F)" $ whnfIO (setComp Par arrV |*| arrV')
              , bench "(.*.) (S)" $ whnfIO (setComp Par arr .*. arr')
              , bench "(.*.) (F)" $ whnfIO (setComp Par arrV .*. arrV')
              , bench "(./.) (S)" $ whnfIO (setComp Par arr ./. arr')
              , bench "(./.) (F)" $ whnfIO (setComp Par arrV ./. arrV')
              ]
          ]
    ]
