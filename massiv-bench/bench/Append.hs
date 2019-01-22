{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Control.Monad            (guard)
import           Criterion.Main
import           Data.Massiv.Array        as A
import           Data.Massiv.Array.Unsafe as A
import           Data.Massiv.Bench        as A
import           Data.Maybe
import           Prelude                  as P

main :: IO ()
main = do
  let !sz = Sz (600 :. 1000)
  defaultMain
    [ mkAppendBenchGroup "LeftToRight" (Dim 1) sz
    , mkAppendBenchGroup "TopToBottom" (Dim 2) sz
    ]


mkAppendBenchGroup :: String -> Dim -> Sz2 -> Benchmark
mkAppendBenchGroup gname dim sz =
  bgroup
    ("Append " ++ gname)
    [ env (return (arrRLightIx2 P Seq sz)) $ \ arr ->
        bgroup
          "Seq"
          [ bench "append" $ whnf (A.computeAs P . fromJust . append dim arr) arr
          , bench "concat" $ whnf (A.computeAs P . fromJust . A.concat dim . (:[arr])) arr
          -- , bench "appendLoad" $ whnf (A.computeAs P . fromJust . appendLoad dim arr) arr
          -- , bench "appendPull" $ whnf (A.computeAs P . fromJust . appendPull dim arr) arr
          ]
    , env (return (arrRLightIx2 P Par sz)) $ \arr ->
        bgroup
          "Par"
          [ bench "append" $ whnf (A.computeAs P . fromJust . append dim arr) arr
          , bench "concat" $ whnf (A.computeAs P . fromJust . A.concat dim . (:[arr])) arr
          -- , bench "appendLoad" $ whnf (A.computeAs P . fromJust . appendLoad dim arr) arr
          -- , bench "appendPull" $ whnf (A.computeAs P . fromJust . appendPull dim arr) arr
          ]
    ]




appendLoad :: (Load r1 ix e, Load r2 ix e) =>
              Dim -> Array r1 ix e -> Array r2 ix e -> Maybe (Array DL ix e)
appendLoad n !arr1 !arr2 = do
  let sz1 = size arr1
      sz2 = size arr2
  (k1, _) <- pullOutSz sz1 n
  let k1' = unSz k1
  (k2, _) <- pullOutSz sz2 n
  sz1' <- setSz sz2 n k1
  guard $ sz1 == sz1'
  newSz <- setSz sz1 n (Sz (k1' + unSz k2))
  return $
    makeLoadArray (getComp arr1 <> getComp arr2) newSz $ \numWorkers scheduleWith dlWrite -> do
      loadArray numWorkers scheduleWith arr1 $ \i ->
        dlWrite (toLinearIndex newSz (fromLinearIndex sz1 i))
      loadArray numWorkers scheduleWith arr2 $ \i ->
        let ix = fromLinearIndex sz2 i
            i' = getDim' ix n
            ix' = setDim' ix n (i' + k1')
         in dlWrite (toLinearIndex newSz ix')
{-# INLINE appendLoad #-}


appendPull :: (Source r1 ix e, Source r2 ix e) =>
          Dim -> Array r1 ix e -> Array r2 ix e -> Maybe (Array D ix e)
appendPull n !arr1 !arr2 = do
  let sz1 = size arr1
      sz2 = size arr2
  (k1, _) <- pullOutSz sz1 n
  let k1' = unSz k1
  (k2, _) <- pullOutSz sz2 n
  sz1' <- setSz sz2 n k1
  guard $ sz1 == sz1'
  newSz <- setSz sz1 n (Sz (k1' + unSz k2))
  return $
    makeArray (getComp arr1) newSz $ \ !ix ->
      let k' = getDim' ix n
       in if k' < unSz k1
            then (unsafeIndex arr1 ix)
            else let i = getDim' ix n
                     ix' = setDim' ix n (i - k1')
                  in unsafeIndex arr2 ix'
{-# INLINE appendPull #-}
