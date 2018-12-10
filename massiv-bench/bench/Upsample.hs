{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Criterion.Main
import           Data.Massiv.Array        as A
import           Data.Massiv.Array.Unsafe as A
import           Data.Massiv.Bench        as A
import           Prelude                  as P

pullUpsample :: Source r ix e => e -> Stride ix -> Array r ix e -> Array D ix e
pullUpsample fillWith safeStride arr =
  makeArrayR D (getComp arr) (liftIndex2 (*) stride (size arr)) $ \ix ->
    if liftIndex2 mod ix stride == zeroIndex
      then unsafeIndex arr (liftIndex2 div ix stride)
      else fillWith
  where
    !stride = unStride safeStride
{-# INLINE pullUpsample #-}


main :: IO ()
main = do
  let !sz = 1600 :. 1200
  defaultMain
    [ bgroup
        "Upsample"
        [ env (return (arrRLightIx2 U Seq sz)) $ \arr ->
            bgroup "Seq"
              [ bench "upsample" $ whnf (A.computeAs U . upsample 0 (Stride 3)) arr
              , bench "pullUpsample" $ whnf (A.computeAs U . pullUpsample 0 (Stride 3)) arr
              ]
        ]
    ]
