{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Compute
import           Criterion.Main
import           Data.Array.Massiv                  as M

import           Data.Array.Repa                    as R
import qualified Data.Vector.Unboxed                as VU
import           Prelude                            as P


main :: IO ()
main = do
  let !sz = (1600, 1200 :: Int)
  let !ixM = (1000, 999)
      !ixR = (Z :. 1000 :. 999)
      !ix1D = toLinearIndex sz ixM
  let !arrCM = M.computeUnboxedS $ arrM sz
  let !arrCR = R.computeUnboxedS $ arrR sz
  let !vecCU = vecU sz
  defaultMain
    [ -- bgroup
      --   "Unsafe Indexing"
      --   [ bench "Massiv 2D" $ whnf (M.unsafeIndex arrCM) ixM
      --   , bench "Repa 2D" $ whnf (R.unsafeIndex arrCR) ixR
      --   , bench "Vector 1D" $ whnf (VU.unsafeIndex vecCU) ix1D
      --   ]
      -- bgroup
      --   "Safe Indexing"
      --   [ bench "Massiv 2D" $ whnf (M.index arrCM) ixM
      --   , bench "Massiv 2D" $ whnf (\ !(i, j) -> arrCM M.! i M.! j) ixM
      --   , bench "Massiv 2D" $ whnf (M.index2D arrCM) ixM
      --   , bench "Repa 2D" $ whnf (R.index arrCR) ixR
      --   , bench "Vector 1D" $ whnf (vecCU VU.!) ix1D
      --   ]
      -- , bgroup
      --     "Unsafe Linear Indexing"
      --     [ bench "Repa 2D" $ whnf (R.unsafeLinearIndex arrCR) ix1D
      --     , bench "Massiv 2D" $ whnf (M.mUnsafeLinearIndex arrCM) ix1D
      --     , bench "Vector 1D" $ whnf (VU.unsafeIndex vecCU) ix1D
      --     ]
      -- "Load Light"
      -- [ bench "Array Massiv" $ whnf (M.computeUnboxedS . arrM) sz
      -- , bench "Array Repa" $ whnf (R.computeUnboxedS . arrR) sz
      -- , bench "Vector Unboxed" $ whnf vecU sz
      -- ]
    -- , bgroup
    --     "Load Heavy"
    --     [ bench "Array Massiv" $ whnf (M.computeUnboxedS . arrM') sz
    --     , bench "Array Repa" $ whnf (R.computeUnboxedS . arrR') sz
    --     , bench "Vector Unboxed" $ whnf vecU' sz
    --     ]
    --   bgroup
    --     "Load Windowed"
    --     [ bench "Array Massiv" $ whnf (M.computeUnboxedS . arrWindowedM) sz
    --     , bench "Array Repa" $ whnf (R.computeUnboxedS . arrWindowedR) sz
    --     ]
    -- , 
      bgroup
        "Fuse"
        [ bench "Array Massiv" $
          whnf (M.computeUnboxedS . mapA (+ 25) . arrM) sz
        , bench "Array Repa" $ whnf (R.computeUnboxedS . R.map (+ 25) . arrR) sz
        , bench "Vector Unboxed" $ whnf (VU.map (+ 25) . vecU) sz
        ]
    ]
