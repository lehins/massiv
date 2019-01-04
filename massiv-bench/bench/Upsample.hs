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


upsampleAlt
  :: (Index (Lower ix), Load r ix e) => e -> Stride ix -> Array r ix e -> Array DL ix e
upsampleAlt fillWith safeStride arr =
  makeLoadArray (getComp arr) newsz $ \numWorkers scheduleWith dlWrite -> do
    iterM_ zeroIndex stride (pureIndex 1) (<) $ \ixs ->
      if ixs == zeroIndex
        then loadArray numWorkers scheduleWith arr $ \ !i -> dlWrite (adjustLinearStride i)
        else let !is = toLinearIndex newsz ixs
              in scheduleWith $
                 loopM_ 0 (< totalElem sz) (+ 1) $ \ !i ->
                   dlWrite (is + adjustLinearStride i) fillWith
  where
    adjustLinearStride = toLinearIndex newsz . timesStride . fromLinearIndex sz
    {-# INLINE adjustLinearStride #-}
    timesStride !ix = liftIndex2 (*) stride ix
    {-# INLINE timesStride #-}
    !stride = unStride safeStride
    !sz = size arr
    !newsz = timesStride sz
{-# INLINE upsampleAlt #-}


main :: IO ()
main = do
  let !sz = 1600 :. 1200
      !str = Stride 3
  defaultMain
    [ bgroup
        "Upsample"
        [ env (return (arrRLightIx2 P Seq sz)) $ \arr ->
            bgroup "Seq"
              [ bench "upsample" $ whnf (A.computeAs P . upsample 0 str) arr
              , bench "upsampleAlt" $ whnf (A.computeAs P . upsampleAlt 0 str) arr
              , bench "pullUpsample" $ whnf (A.computeAs P . pullUpsample 0 str) arr
              ]
        , env (return (arrRLightIx2 P Par sz)) $ \arr ->
            bgroup "Par"
              [ bench "upsample" $ whnf (A.computeAs P . upsample 0 str) arr
              , bench "upsampleAlt" $ whnf (A.computeAs P . upsampleAlt 0 str) arr
              , bench "pullUpsample" $ whnf (A.computeAs P . pullUpsample 0 str) arr
              ]
        ]
    ]
