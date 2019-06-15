{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Criterion.Main
import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe as A
import Data.Massiv.Bench as A
import Prelude as P
import Control.Monad as M (forM_)

main :: IO ()
main = do
  let !sz = Sz (600 :. 1000)
      !strOdd = Stride (3 :. 6)
      !strPowerOf2 = Stride (2 :. 4)
  defaultMain [mkUpsampleBenchGroup "Odd" sz strOdd, mkUpsampleBenchGroup "PowerOf2" sz strPowerOf2]


mkUpsampleBenchGroup :: String -> Sz2 -> Stride Ix2 -> Benchmark
mkUpsampleBenchGroup gname sz str =
  bgroup
    ("Upsample " ++ gname)
    [ env (return (arrRLightIx2 P Seq sz)) $ \arr ->
        bgroup
          "Seq"
          [ bench "upsample" $ whnf (A.computeAs P . upsample 0 str) arr
          , bench "upsample'" $ whnf (A.computeAs P . upsample' 0 str) arr
          ]
    , env (return (arrRLightIx2 P Par sz)) $ \arr ->
        bgroup
          "Par"
          [ bench "upsample" $ whnf (A.computeAs P . upsample 0 str) arr
          ]
    ]


upsample'
  :: Load r ix e => e -> Stride ix -> Array r ix e -> Array DL ix e
upsample' !fillWith safeStride arr
  | safeStride == oneStride = toLoadArray arr
  | otherwise =
    unsafeMakeLoadArray (getComp arr) newsz (Just fillWith) $ \scheduler startAt dlWrite -> do
      -- M.forM_ (defaultElement arr) $ \prevFillWith ->
      --   loopM_
      --     startAt
      --     (< totalElem sz)
      --     (+ 1)
      --     (\i -> dlWrite (adjustLinearStride (i + startAt)) prevFillWith)
      loadArrayM scheduler arr (\i -> dlWrite (adjustLinearStride (i + startAt)))
  where
    adjustLinearStride = toLinearIndex newsz . timesStride . fromLinearIndex sz
    {-# INLINE adjustLinearStride #-}
    timesStride !ix = liftIndex2 (*) stride ix
    {-# INLINE timesStride #-}
    !stride = unStride safeStride
    !sz = size arr
    !newsz = Sz (timesStride $ unSz sz)
{-# INLINE upsample' #-}
