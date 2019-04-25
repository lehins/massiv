{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Monad.ST
import Criterion.Main
import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe as A
import Data.Massiv.Bench as A
import Prelude as P
import Control.Scheduler


-- | Scale the array, negate values and create an array with a grid.
zoomWithGridD :: Manifest r Ix2 e => e -> Int -> Array r Ix2 e -> Array D Ix2 e
zoomWithGridD gridVal zoomFactor arr = A.makeArray (getComp arr) sz' getNewElt
  where
    k = zoomFactor + 1
    Sz (m :. n) = size arr
    sz' = Sz (1 + m * k :. 1 + n * k)
    getNewElt (i :. j) =
      if i `mod` k == 0 || j `mod` k == 0
        then gridVal
        else arr `A.unsafeIndex` ((i - 1) `div` k :. (j - 1) `div` k)
{-# INLINE zoomWithGridD #-}

zoomWithGridL ::
     Source r ix e
  => e -- ^ Value to use for the grid
  -> Stride ix -- ^ Scaling factor
  -> Array r ix e -- ^ Source array
  -> Array DL ix e
zoomWithGridL gridVal (Stride zoomFactor) arr =
  unsafeMakeLoadArray Seq newSz (Just gridVal) $ \scheduler _ writeElement -> do
    A.iforSchedulerM_ scheduler arr $ \ !ix !e -> do
      let kix = liftIndex2 (*) ix kx
      A.mapM_ (\ !ix' -> writeElement (toLinearIndex newSz ix') e) $
        range Seq (liftIndex (+1) kix) (liftIndex2 (+) kix kx)
  where
    !kx = liftIndex (+1) zoomFactor
    !lastNewIx = liftIndex2 (*) kx $ unSz (size arr)
    !newSz = Sz (liftIndex (+1) lastNewIx)
{-# INLINE zoomWithGridL #-}


-- | Scale the array, negate values and create an array with a grid.
zoomWithGridL' :: Source r Ix2 e => e -> Int -> Array r Ix2 e -> Array DL Ix2 e
zoomWithGridL' gridVal zoomFactor arr =
  makeLoadArrayS newSz gridVal $ \writeElement -> do
    let writeGrid ix = writeElement ix gridVal
    A.iforM_ arr $ \ix e -> do
      let (i :. j) = ix * (k :. k)
      A.mapM_ (`writeElement` e) $ range Seq (i + 1 :. j + 1) (i + k :. j + k)
  where
    k = zoomFactor + 1
    lastNewIx@(m' :. n') = unSz (size arr) * fromIntegral k
    newSz = 1 + Sz lastNewIx
{-# INLINE zoomWithGridL' #-}



main :: IO ()
main = do
  let !sz = Sz (60 :. 100)
      !arr = arrRLightIx2 P Par sz
      !k = 10
  defaultMain
    [ bgroup
        "Grid"
        [ bench "zoomWithGridD" $ whnf (A.computeS @S . zoomWithGridD 9 k) arr
        , bench "zoomWithGridL" $ whnf (A.computeS @S . zoomWithGridL 9 (Stride (k :. k))) arr
        , bench "zoomWithGridL' Par" $ whnf (A.compute @S . zoomWithGridL 9 (Stride (k :. k))) arr
        , bench "zoomWithGrid" $ whnf (A.computeS @S . zoomWithGrid 9 (Stride (k :. k))) arr
        --, bench "zoomWithGridL'" $ whnf (A.computeAs S . zoomWithGridL' 10 k) arr
        ]
    ]
