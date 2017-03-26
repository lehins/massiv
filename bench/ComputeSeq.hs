{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Compute
import           Criterion.Main
import           Data.Array.Massiv                  as M
import           Data.Array.Massiv.Manifest.Unboxed as M
import           Data.Array.Repa                    as R
import qualified Data.Vector.Unboxed                as VU
import           Prelude                            as P


main :: IO ()
main = do
  let !sz = (1600, 1200 :: Int)
  defaultMain
    [ bgroup
        "Load Light"
        [ bench "Array Massiv" $ whnf (M.computeUnboxedS . arrM) sz
        , bench "Array Repa" $ whnf (R.computeUnboxedS . arrR) sz
        , bench "Vector Unboxed" $ whnf vecU sz
        ]
    , bgroup
        "Load Heavy"
        [ bench "Array Massiv" $ whnf (M.computeUnboxedS . arrM') sz
        , bench "Array Repa" $ whnf (R.computeUnboxedS . arrR') sz
        , bench "Vector Unboxed" $ whnf vecU' sz
        ]
    ,  bgroup
        "Load Windowed"
        [ bench "Array Massiv" $ whnf (M.computeUnboxedS . arrWindowedM) sz
        , bench "Array Repa" $ whnf (R.computeUnboxedS . arrWindowedR) sz
        ]
    , bgroup
        "Fuse"
        [ bench "Array Massiv" $
          whnf (M.computeUnboxedS . mapA (+ 25) . arrM) sz
        , bench "Array Repa" $ whnf (R.computeUnboxedS . R.map (+ 25) . arrR) sz
        , bench "Vector Unboxed" $ whnf (VU.map (+ 25) . vecU) sz
        ]
    ]
