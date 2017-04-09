{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Data.Array.Massiv.Delayed.Windowed
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Delayed.Windowed where

import           Control.Monad                  (when)
import           Control.Monad.ST
import           Control.Monad.ST.Unsafe
import           Data.Array.Massiv.Compute.Scheduler
import           Data.Array.Massiv.Delayed
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Compute.Gang


-- | Delayed array with separate function for an inner windowed.
data W


data instance Array W ix e = WArray { wSize :: !ix
                                    , wStencilSize :: Maybe ix
                                      -- ^ Setting this value during stencil
                                      -- application improves cache utilization
                                      -- while computing an array
                                    , wSafeIndexBorder :: ix -> e
                                    , wWindowStartIndex :: !ix
                                    , wWindowSize :: !ix
                                    , wWindowUnsafeIndex :: ix -> e }

data WD

data instance Array WD ix e = WDArray { wdArray :: !(Array D ix e)
                                      , wdStencilSize :: Maybe ix
                                        -- ^ Setting this value during stencil
                                        -- application improves cache utilization
                                        -- while computing an array
                                      , wdWindowStartIndex :: !ix
                                      , wdWindowSize :: !ix
                                      , wdWindowUnsafeIndex :: ix -> e }


instance Index ix => Massiv W ix where
  size = wSize
  {-# INLINE size #-}


instance Index ix => Massiv WD ix where
  size = size . wdArray
  {-# INLINE size #-}



instance Functor (Array W ix) where
  fmap f !arr =
    arr
    { wSafeIndexBorder = f . wSafeIndexBorder arr
    , wWindowUnsafeIndex = f . wWindowUnsafeIndex arr
    }
  {-# INLINE fmap #-}


instance Functor (Array WD ix) where
  fmap f !arr =
    arr
    { wdArray = fmap f (wdArray arr)
    , wdWindowUnsafeIndex = f . wdWindowUnsafeIndex arr
    }
  {-# INLINE fmap #-}

-- | Supply a separate generating function for interior of an array. This is
-- very usful for stencil mapping, where interior function does not perform
-- boundary checks, thus significantly speeding up computation process.
makeArrayWindowed
  :: Source r ix e
  => Array r ix e -- ^ Source array that will have a window inserted into it
  -> ix -- ^ Start index for the window
  -> ix -- ^ Size of the window
  -> (ix -> e) -- ^ Inside window indexing function
  -> Array WD ix e
makeArrayWindowed !arr !wIx !wSz wUnsafeIndex
  | not (isSafeIndex sz wIx) =
    error $
    "Incorrect window starting index: " ++ show wIx ++ " for: " ++ show arr
  | liftIndex2 (+) wIx wSz > sz =
    error $
    "Incorrect window size: " ++
    show wSz ++ " and/or placement: " ++ show wIx ++ " for: " ++ show arr
  | otherwise =
    WDArray
    { wdArray = delay arr
    , wdStencilSize = Nothing
    , wdWindowStartIndex = wIx
    , wdWindowSize = wSz
    , wdWindowUnsafeIndex = wUnsafeIndex
    }
  where sz = size arr
{-# INLINE makeArrayWindowed #-}


instance Load W DIM1 where
  loadS (WArray sz _ indexB it wk indexW) unsafeWrite = do
    iterM_ 0 it 1 (<) $ \ !i ->
      unsafeWrite i (indexB i)
    iterM_ it wk 1 (<) $ \ !i ->
      unsafeWrite i (indexW i)
    iterM_ wk sz 1 (<) $ \ !i ->
      unsafeWrite i (indexB i)
  {-# INLINE loadS #-}
  loadP (WArray sz _ indexB it wk indexW) unsafeWrite = do
    let !gSize = gangSize theGang
        !(chunkHeight, slackHeight) = wk `quotRem` gSize
    let loadBlock !it' !ib' =
          iterM_ it' ib' 1 (<) $ \ !ix ->
            unsafeWrite (toLinearIndex sz ix) (indexW ix)
        {-# INLINE loadBlock #-}
    gangIO theGang $ \ !cix -> do
      let !it' = cix * chunkHeight + it
      loadBlock it' (it' + chunkHeight)
      when (cix == 0) $
        iterM_ 0 it 1 (<) $ \ !ix ->
          unsafeWrite (toLinearIndex sz ix) (indexB ix)
      when (cix == 1 `mod` gSize) $
        iterM_ wk sz 1 (<) $ \ !ix ->
          unsafeWrite (toLinearIndex sz ix) (indexB ix)
      when (cix == 3 `mod` gSize && slackHeight > 0) $ do
        let !itSlack = gSize * chunkHeight + it
        loadBlock itSlack (itSlack + slackHeight)
  {-# INLINE loadP #-}



instance Load W DIM2 where
  loadS (WArray sz@(m, n) mStencilSz indexB (it, jt) (wm, wn) indexW) unsafeWrite = do
    let !(ib, jb) = (wm + it, wn + jt)
        !blockHeight = maybe 1 fst mStencilSz
    iterM_ (0, 0) (it, n) 1 (<) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (indexB ix)
    iterM_ (ib, 0) (m, n) 1 (<) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (indexB ix)
    iterM_ (it, 0) (ib, jt) 1 (<) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (indexB ix)
    iterM_ (it, jb) (ib, n) 1 (<) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (indexB ix)
    unrollAndJam blockHeight (it, ib) (jt, jb) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (indexW ix)
  {-# INLINE loadS #-}
  loadP (WArray sz@(m, n) mStencilSz indexB (it, jt) (wm, wn) indexW) unsafeWrite = do
    let !(ib, jb) = (wm + it, wn + jt)
        !blockHeight = maybe 1 fst mStencilSz
    let !gSize = gangSize theGang
        !(chunkHeight, slackHeight) = wm `quotRem` gSize
    let loadBlock !it' !ib' =
          unrollAndJam blockHeight (it', ib') (jt, jb) $ \ !ix ->
            unsafeWrite (toLinearIndex sz ix) (indexW ix)
        {-# INLINE loadBlock #-}
    gangIO theGang $ \ !cix -> do
      let !it' = cix * chunkHeight + it
      loadBlock it' (it' + chunkHeight)
      when (cix == 0) $
        iterM_ (0, 0) (it, n) 1 (<) $ \ !ix ->
          unsafeWrite (toLinearIndex sz ix) (indexB ix)
      when (cix == 1 `mod` gSize) $
        iterM_ (ib, 0) (m, n) 1 (<) $ \ !ix ->
          unsafeWrite (toLinearIndex sz ix) (indexB ix)
      when (cix == 2 `mod` gSize) $
        iterM_ (it, 0) (ib, jt) 1 (<) $ \ !ix ->
          unsafeWrite (toLinearIndex sz ix) (indexB ix)
      when (cix == 3 `mod` gSize) $
        iterM_ (it, jb) (ib, n) 1 (<) $ \ !ix ->
          unsafeWrite (toLinearIndex sz ix) (indexB ix)
      when (cix == 4 `mod` gSize && slackHeight > 0) $ do
        let !itSlack = gSize * chunkHeight + it
        loadBlock itSlack (itSlack + slackHeight)
  {-# INLINE loadP #-}

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




instance Load WD DIM2 where
  loadS (WDArray (DArray sz@(m, n) indexB) mStencilSz (it, jt) (wm, wn) indexW) unsafeWrite = do
    let !(ib, jb) = (wm + it, wn + jt)
        !blockHeight = maybe 1 fst mStencilSz
    iterM_ (0, 0) (it, n) 1 (<) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (indexB ix)
    iterM_ (ib, 0) (m, n) 1 (<) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (indexB ix)
    iterM_ (it, 0) (ib, jt) 1 (<) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (indexB ix)
    iterM_ (it, jb) (ib, n) 1 (<) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (indexB ix)
    unrollAndJam blockHeight (it, ib) (jt, jb) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (indexW ix)
  {-# INLINE loadS #-}
  loadP (WDArray (DArray sz@(m, n) indexB) mStencilSz (it, jt) (wm, wn) indexW) unsafeWrite = do
    scheduler <- makeScheduler
    let !(ib, jb) = (wm + it, wn + jt)
        !blockHeight = maybe 1 fst mStencilSz
        !(chunkHeight, slackHeight) = wm `quotRem` numWorkers scheduler
    let loadBlock !it' !ib' =
          unrollAndJam blockHeight (it', ib') (jt, jb) $ \ !ix ->
            unsafeWrite (toLinearIndex sz ix) (indexW ix)
        {-# INLINE loadBlock #-}
    submitRequest scheduler $
      JobRequest 0 $
        iterM_ (0, 0) (it, n) 1 (<) $ \ !ix ->
          unsafeWrite (toLinearIndex sz ix) (indexB ix)
    submitRequest scheduler $
      JobRequest 0 $
        iterM_ (ib, 0) (m, n) 1 (<) $ \ !ix ->
          unsafeWrite (toLinearIndex sz ix) (indexB ix)
    submitRequest scheduler $
      JobRequest 0 $
        iterM_ (it, 0) (ib, jt) 1 (<) $ \ !ix ->
          unsafeWrite (toLinearIndex sz ix) (indexB ix)
    submitRequest scheduler $
      JobRequest 0 $
        iterM_ (it, jb) (ib, n) 1 (<) $ \ !ix ->
          unsafeWrite (toLinearIndex sz ix) (indexB ix)
    loopM_ 0 (< numWorkers scheduler) (+ 1) $ \ !wid -> do
      let !it' = wid * chunkHeight + it
      submitRequest scheduler $
        JobRequest 0 $
        loadBlock it' (it' + chunkHeight)
    when (slackHeight > 0) $ do
      let !itSlack = (numWorkers scheduler) * chunkHeight + it
      submitRequest scheduler $
        JobRequest 0 $
        loadBlock itSlack (itSlack + slackHeight)
    waitTillDone scheduler
  {-# INLINE loadP #-}

instance Load WD DIM3 where
  loadS = loadWindowedSRec
  {-# INLINE loadS #-}
  loadP = loadWindowedPRec
  {-# INLINE loadP #-}

instance Load WD DIM4 where
  loadS = loadWindowedSRec
  {-# INLINE loadS #-}
  loadP = loadWindowedPRec
  {-# INLINE loadP #-}

instance Load WD DIM5 where
  loadS = loadWindowedSRec
  {-# INLINE loadS #-}
  loadP = loadWindowedPRec
  {-# INLINE loadP #-}

loadWindowedSRec
  :: (Index ix, Load WD (Lower ix)) =>
     Array WD ix e -> (Int -> e -> ST s ()) -> ST s ()
loadWindowedSRec (WDArray (DArray sz indexB) mStencilSz tix wSz indexW) unsafeWrite = do
  let !szL = snd $ unconsDim sz
      !(t, tixL) = unconsDim tix
      !(w, wSzL) = unconsDim wSz
      !pageElements = totalElem szL
      -- Are there at least as many pages as there are available workers (2 for borders)
      unsafeWriteLower i k val = unsafeWrite (k + pageElements * (t + i)) val
      {-# INLINE unsafeWriteLower #-}
  iterM_ zeroIndex tix 1 (<) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (indexB ix)
  iterM_ (liftIndex2 (+) tix wSz) sz 1 (<) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (indexB ix)
  loopM_ t (< (w + t)) (+ 1) $ \ !i ->
    let !lowerArr =
          (WDArray
             (DArray szL (\ !ix -> indexB (consDim i ix)))
             ((snd . unconsDim) <$> mStencilSz) -- can safely drop the dim, only
                                                -- last 2 matter anyways
             tixL
             wSzL
             (\ !ix -> indexW (consDim i ix)))
    in loadS lowerArr (unsafeWriteLower i)
{-# INLINE loadWindowedSRec #-}


loadWindowedPRec
  :: (Index ix, Load WD (Lower ix)) =>
     Array WD ix e -> (Int -> e -> IO ()) -> IO ()
loadWindowedPRec (WDArray (DArray sz indexB) mStencilSz tix wSz indexW) unsafeWrite = do
  scheduler <- makeScheduler
  let !szL = snd $ unconsDim sz
      !(t, tixL) = unconsDim tix
      !(w, wSzL) = unconsDim wSz
      !pageElements = totalElem szL
      -- Are there at least as many pages as there are available workers (2 for borders)
      !enoughPages = w >= (numWorkers scheduler - 2)
      unsafeWriteLower i k val = unsafeWrite (k + pageElements * (t + i)) val
      {-# INLINE unsafeWriteLower #-}
      unsafeWriteLowerST i k = unsafeIOToST . unsafeWriteLower i k
      {-# INLINE unsafeWriteLowerST #-}
  submitRequest scheduler $
    JobRequest 0 $
    iterM_ zeroIndex tix 1 (<) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (indexB ix)
  submitRequest scheduler $
    JobRequest 0 $
    iterM_ (liftIndex2 (+) tix wSz) sz 1 (<) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (indexB ix)
  loopM_ t (< (w + t)) (+ 1) $ \ !i ->
    let !lowerArr =
          (WDArray
             (DArray szL (\ !ix -> indexB (consDim i ix)))
             ((snd . unconsDim) <$> mStencilSz) -- can safely drop the dim, only
                                                -- last 2 matter anyways
             tixL
             wSzL
             (\ !ix -> indexW (consDim i ix)))
    in if enoughPages
         then submitRequest scheduler $
              JobRequest 0 $ stToIO $ loadS lowerArr (unsafeWriteLowerST i)
         else loadP lowerArr (unsafeWriteLower i)
  waitTillDone scheduler
{-# INLINE loadWindowedPRec #-}
