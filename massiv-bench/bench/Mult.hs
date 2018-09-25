{-# LANGUAGE BangPatterns     #-}
module Main where

import           Criterion.Main
import           Data.Massiv.Array as A
import           Data.Massiv.Bench as A
import           Prelude           as P



main :: IO ()
main = do
  let !sz = 600 :. 200
      arr = arrRLightIx2 U Seq sz
  defaultMain
    [ env (return (computeAs U (transpose arr))) $ \arr' ->
        bgroup
          "Mult"
          [ bgroup
              "Seq"
              [bench "(|*|)" $ whnf (A.computeAs U . (|*|) (setComp Seq arr)) arr']
          , bgroup
              "Par"
              [bench "(|*|)" $ whnf (A.computeAs U . (|*|) (setComp Par arr)) arr']
          ]
    ]
