{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Data.Array.Massiv.Delayed.WindowedM
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Delayed.WindowedM where

import           Control.Monad               (void, when)
import           Control.Monad.ST
import           Control.Monad.ST.Unsafe
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Delayed
import           Data.Array.Massiv.Manifest.Internal
import           Data.Array.Massiv.Scheduler



data WMD

data instance Array WMD ix e =
  WMDArray { wmdSize :: ix
           , wmdSafeReadIndexM :: forall m . Monad m => (ix -> m e) -> ix -> m e
           , wmdStencilSize :: Maybe ix
           , wmdWindowStartIndex :: !ix
           , wmdWindowSize :: !ix
           , wmdUnsafeReadIndexM :: forall m . Monad m => (ix -> m e) -> ix -> m e
           , wmdDependencies :: Array M Int Int
           }

instance Index ix => Massiv WMD ix where
  size = wmdSize
  {-# INLINE size #-}



instance Load WMD DIM2 where
  loadS wmdArr unsafeRead unsafeWrite = do
    let (WMDArray sz@(m, n) indexB mStencilSz (it, jt) (wm, wn) indexW _deps) =
          wmdArr
        !(ib, jb) = (wm + it, wn + jt)
        !blockHeight = maybe 1 fst mStencilSz
        unsafeReadIx ix = unsafeRead (toLinearIndex sz ix)
    iterM_ (0, 0) (it, n) 1 (<) $ \ !ix ->
      indexB unsafeReadIx ix >>= unsafeWrite (toLinearIndex sz ix)
    iterM_ (it, 0) (ib, jt) 1 (<) $ \ !ix ->
      indexB unsafeReadIx ix >>= unsafeWrite (toLinearIndex sz ix)
    unrollAndJam blockHeight (it, ib) (jt, jb) $ \ !ix ->
      indexW unsafeReadIx ix >>= unsafeWrite (toLinearIndex sz ix)
    iterM_ (it, jb) (ib, n) 1 (<) $ \ !ix ->
      indexB unsafeReadIx ix >>= unsafeWrite (toLinearIndex sz ix)
    iterM_ (ib, 0) (m, n) 1 (<) $ \ !ix ->
      indexB unsafeReadIx ix >>= unsafeWrite (toLinearIndex sz ix)
  {-# INLINE loadS #-}
  loadP wmdArr unsafeRead unsafeWrite = do
    scheduler <- makeScheduler
    let (WMDArray sz@(m, n) indexB mStencilSz (it, jt) (wm, wn) indexW _deps) =
          wmdArr
        !(ib, jb) = (wm + it, wn + jt)
        !blockHeight = maybe 1 fst mStencilSz
        !(chunkHeight, slackHeight) = wm `quotRem` numWorkers scheduler
    let loadBlock !it' !ib' =
          unrollAndJam blockHeight (it', ib') (jt, jb) $ \ !ix ->
            indexW unsafeReadIx ix >>= unsafeWrite (toLinearIndex sz ix)
        {-# INLINE loadBlock #-}
        unsafeReadIx ix = unsafeRead (toLinearIndex sz ix)
    iterM_ (0, 0) (it, n) 1 (<) $ \ !ix ->
      indexB unsafeReadIx ix >>= unsafeWrite (toLinearIndex sz ix)
    iterM_ (it, 0) (ib, jt) 1 (<) $ \ !ix ->
      indexB unsafeReadIx ix >>= unsafeWrite (toLinearIndex sz ix)
    loopM_ 0 (< numWorkers scheduler) (+ 1) $ \ !wid -> do
      let !it' = wid * chunkHeight + it
      submitRequest scheduler $ JobRequest 0 $ loadBlock it' (it' + chunkHeight)
    when (slackHeight > 0) $ do
      let !itSlack = (numWorkers scheduler) * chunkHeight + it
      submitRequest scheduler $
        JobRequest 0 $ loadBlock itSlack (itSlack + slackHeight)
    waitTillDone scheduler
    iterM_ (it, jb) (ib, n) 1 (<) $ \ !ix ->
      indexB unsafeReadIx ix >>= unsafeWrite (toLinearIndex sz ix)
    iterM_ (ib, 0) (m, n) 1 (<) $ \ !ix ->
      indexB unsafeReadIx ix >>= unsafeWrite (toLinearIndex sz ix)
  {-# INLINE loadP #-}

-- instance Load WD DIM3 where
--   loadS = loadWindowedSRec
--   {-# INLINE loadS #-}
--   loadP = loadWindowedPRec
--   {-# INLINE loadP #-}

-- instance Load WD DIM4 where
--   loadS = loadWindowedSRec
--   {-# INLINE loadS #-}
--   loadP = loadWindowedPRec
--   {-# INLINE loadP #-}

-- instance Load WD DIM5 where
--   loadS = loadWindowedSRec
--   {-# INLINE loadS #-}
--   loadP = loadWindowedPRec
--   {-# INLINE loadP #-}

-- loadWindowedSRec
--   :: (Index ix, Load WD (Lower ix)) =>
--      Array WD ix e -> (Int -> ST s e) -> (Int -> e -> ST s ()) -> ST s ()
-- loadWindowedSRec (WDArray (DArray sz indexB) mStencilSz tix wSz indexW) unsafeRead unsafeWrite = do
--   let !szL = snd $ unconsDim sz
--       !(t, tixL) = unconsDim tix
--       !(w, wSzL) = unconsDim wSz
--       !pageElements = totalElem szL
--       -- Are there at least as many pages as there are available workers (2 for borders)
--       unsafeWriteLower i k val = unsafeWrite (k + pageElements * (t + i)) val
--       {-# INLINE unsafeWriteLower #-}
--   iterM_ zeroIndex tix 1 (<) $ \ !ix ->
--       unsafeWrite (toLinearIndex sz ix) (indexB ix)
--   iterM_ (liftIndex2 (+) tix wSz) sz 1 (<) $ \ !ix ->
--       unsafeWrite (toLinearIndex sz ix) (indexB ix)
--   loopM_ t (< (w + t)) (+ 1) $ \ !i ->
--     let !lowerArr =
--           (WDArray
--              (DArray szL (\ !ix -> indexB (consDim i ix)))
--              ((snd . unconsDim) <$> mStencilSz) -- can safely drop the dim, only
--                                                 -- last 2 matter anyways
--              tixL
--              wSzL
--              (\ !ix -> indexW (consDim i ix)))
--     in loadS lowerArr unsafeRead (unsafeWriteLower i)
-- {-# INLINE loadWindowedSRec #-}


-- loadWindowedPRec
--   :: (Index ix, Load WD (Lower ix)) =>
--      Array WD ix e -> (Int -> IO e) -> (Int -> e -> IO ()) -> IO ()
-- loadWindowedPRec (WDArray (DArray sz indexB) mStencilSz tix wSz indexW) unsafeRead unsafeWrite = do
--   scheduler <- makeScheduler
--   let !szL = snd $ unconsDim sz
--       !(t, tixL) = unconsDim tix
--       !(w, wSzL) = unconsDim wSz
--       !pageElements = totalElem szL
--       -- Are there at least as many pages as there are available workers (2 for borders)
--       !enoughPages = w >= (numWorkers scheduler - 2)
--       unsafeWriteLower i k val = unsafeWrite (k + pageElements * (t + i)) val
--       {-# INLINE unsafeWriteLower #-}
--       unsafeWriteLowerST i k = unsafeIOToST . unsafeWriteLower i k
--       {-# INLINE unsafeWriteLowerST #-}
--   submitRequest scheduler $
--     JobRequest 0 $
--     iterM_ zeroIndex tix 1 (<) $ \ !ix ->
--       unsafeWrite (toLinearIndex sz ix) (indexB ix)
--   submitRequest scheduler $
--     JobRequest 0 $
--     iterM_ (liftIndex2 (+) tix wSz) sz 1 (<) $ \ !ix ->
--       unsafeWrite (toLinearIndex sz ix) (indexB ix)
--   loopM_ t (< (w + t)) (+ 1) $ \ !i ->
--     let !lowerArr =
--           (WDArray
--              (DArray szL (\ !ix -> indexB (consDim i ix)))
--              ((snd . unconsDim) <$> mStencilSz) -- can safely drop the dim, only
--                                                 -- last 2 matter anyways
--              tixL
--              wSzL
--              (\ !ix -> indexW (consDim i ix)))
--     in if enoughPages
--          then submitRequest scheduler $
--               JobRequest 0 $
--               stToIO $
--               loadS
--                 lowerArr
--                 (\_ix -> unsafeIOToST $ unsafeRead _ix)
--                 (unsafeWriteLowerST i)
--          else loadP lowerArr unsafeRead (unsafeWriteLower i)
--   waitTillDone scheduler
-- {-# INLINE loadWindowedPRec #-}



unrollAndJam :: Monad m =>
                Int -> (Int, Int) -> (Int, Int) -> ((Int, Int) -> m a) -> m ()
unrollAndJam !bH !(it, ib) !(jt, jb) f = do
  let !bH' = min (max 1 bH) 7
  let f2 !(i, j) = f (i, j) >> f  (i+1, j)
  let f3 !(i, j) = f (i, j) >> f2 (i+1, j)
  let f4 !(i, j) = f (i, j) >> f3 (i+1, j)
  let f5 !(i, j) = f (i, j) >> f4 (i+1, j)
  let f6 !(i, j) = f (i, j) >> f5 (i+1, j)
  let f7 !(i, j) = f (i, j) >> f6 (i+1, j)
  let f' = case bH' of
             1 -> f
             2 -> f2
             3 -> f3
             4 -> f4
             5 -> f5
             6 -> f6
             _ -> f7
  let !ibS = ib - ((ib - it) `mod` bH')
  loopM_ it (< ibS) (+ bH') $ \ !i ->
    loopM_ jt (< jb) (+ 1) $ \ !j ->
      f' (i, j)
  loopM_ ibS (< ib) (+ 1) $ \ !i ->
    loopM_ jt (< jb) (+ 1) $ \ !j ->
      f (i, j)
{-# INLINE unrollAndJam #-}
