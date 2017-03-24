{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Criterion.Main
import           Prelude               as P

import Data.Array.Massiv
import Data.Array.Massiv.Manifest.Unboxed
import Data.Array.Massiv.Windowed
import qualified Data.Vector.Primitive as VP



main :: IO ()
main = do
  let !n = 3200000 :: Int
  let !vp = (`VP.generate` id)
  let vecVInt sz = makeArray2D sz snd
  let vecVIntW sz = makeArrayWindowed (vecVInt sz) (0, 10) (1, n - 20) (const 10)
  defaultMain
    [ bgroup
        "Sum"
        [ bench "Array Current" $ whnf (computeUnboxedS . mapA (5+) . vecVInt) (10, n)
        , bench "Array Current Windowed" $ whnf (computeUnboxedS . vecVIntW) (10, n)
        , bench "Vector Primitive" $ nf (VP.map (5+) . vp) n
        ]
    ]
