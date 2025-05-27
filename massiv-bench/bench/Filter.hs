{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Criterion.Main
import Data.Massiv.Array as A
-- import Data.Massiv.Array.Delayed.Stream
import Data.Massiv.Array.Manifest.Vector as A
import Data.Massiv.Bench as A
import qualified Data.Vector.Primitive as VP
import Prelude as P

main :: IO ()
main = do
  let !sz = Sz (600 :. 1000)
      !arrP = computeAs P $ resize' (Sz $ totalElem sz) $ arrRLightIx2 DL Seq sz
  defaultMain
    [ env (pure (A.toVector arrP)) $ \(v :: VP.Vector Double) ->
        bgroup
          "Vector P"
          [ bench "filter > 0" $ nf (VP.filter (> 0)) v
          -- , bench "filterM > 0" $ nf (VP.filterM (\i -> Just (i > 0))) v
          -- , bench "traverse" $ nf (VP.mapM Just) v
          ]
    , env (pure arrP) $ \arr ->
        bgroup "Array" [bench "filterS" $ nf (computeAs P . sfilter (> 0)) arr]
    ]

-- -- Really slow
-- filterA :: Source r ix e => (e -> Bool) -> Array r ix e -> Array DL Ix1 e
-- filterA f = foldlS collect A.empty
--   where
--     collect !acc e
--       | f e = A.snoc acc e
--       | otherwise = acc

-- -- Slow
-- filterA' :: Source r ix e => (e -> Bool) -> Array r ix e -> Array DL Ix1 e
-- filterA' f = foldrS collect A.empty
--   where
--     collect e !acc
--       | f e = A.cons e acc
--       | otherwise = acc
