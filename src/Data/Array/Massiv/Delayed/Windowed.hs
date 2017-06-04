{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
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

import           Control.Monad               (void, when)
import           Control.Monad.ST
import           Control.Monad.ST.Unsafe
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Delayed
import           Data.Array.Massiv.Scheduler


data WD

data instance Array WD ix e = WDArray { wdArray :: !(Array D ix e)
                                      , wdStencilSize :: Maybe ix
                                        -- ^ Setting this value during stencil
                                        -- application improves cache utilization
                                        -- while computing an array
                                      , wdWindowStartIndex :: !ix
                                      , wdWindowSize :: !ix
                                      , wdWindowUnsafeIndex :: ix -> e }

instance Index ix => Massiv WD ix e where
  size = size . wdArray
  {-# INLINE size #-}

  getComp = dComp . wdArray
  {-# INLINE getComp #-}

  setComp c arr = arr { wdArray = (wdArray arr) { dComp = c } }
  {-# INLINE setComp #-}

  unsafeMakeArray c sz f = WDArray (unsafeMakeArray c sz f) Nothing zeroIndex zeroIndex f
  {-# INLINE unsafeMakeArray #-}


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
    "Incorrect window starting index: " ++ show wIx ++ " for: " ++ show (size arr)
  | liftIndex2 (+) wIx wSz > sz =
    error $
    "Incorrect window size: " ++
    show wSz ++ " and/or placement: " ++ show wIx ++ " for: " ++ show (size arr)
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




instance Load WD DIM1 e where
  loadS (WDArray (DArray _ sz indexB) _ it wk indexW) _ unsafeWrite = do
    iterM_ 0 it 1 (<) $ \ !i -> unsafeWrite i (indexB i)
    iterM_ it wk 1 (<) $ \ !i -> unsafeWrite i (indexW i)
    iterM_ wk sz 1 (<) $ \ !i -> unsafeWrite i (indexB i)
  {-# INLINE loadS #-}
  loadP wIds (WDArray (DArray _ sz indexB) _ it wk indexW) _ unsafeWrite = do
    void $
      splitWork wIds wk $ \ !scheduler !chunkLength !totalLength !slackStart -> do
        submitRequest scheduler $
          JobRequest $
          iterM_ 0 it 1 (<) $ \ !ix ->
            unsafeWrite (toLinearIndex sz ix) (indexB ix)
        submitRequest scheduler $
          JobRequest $
          iterM_ wk sz 1 (<) $ \ !ix ->
            unsafeWrite (toLinearIndex sz ix) (indexB ix)
        loopM_ it (< (slackStart + it)) (+ chunkLength) $ \ !start ->
          submitRequest scheduler $
          JobRequest $
          iterM_ start (start + chunkLength) 1 (<) $ \ !k ->
            unsafeWrite k $ indexW k
        submitRequest scheduler $
          JobRequest $
          iterM_ (slackStart + it) (totalLength + it) 1 (<) $ \ !k ->
            unsafeWrite k (indexW k)
  {-# INLINE loadP #-}



instance Load WD DIM2 e where
  loadS (WDArray (DArray _ sz@(m, n) indexB) mStencilSz (it, jt) (wm, wn) indexW) _ unsafeWrite = do
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
  loadP wIds (WDArray (DArray _ sz@(m, n) indexB) mStencilSz (it, jt) (wm, wn) indexW) _ unsafeWrite = do
    scheduler <- makeScheduler wIds
    let !(ib, jb) = (wm + it, wn + jt)
        !blockHeight = maybe 1 fst mStencilSz
        !(chunkHeight, slackHeight) = wm `quotRem` numWorkers scheduler
    let loadBlock !it' !ib' =
          unrollAndJam blockHeight (it', ib') (jt, jb) $ \ !ix ->
            unsafeWrite (toLinearIndex sz ix) (indexW ix)
        {-# INLINE loadBlock #-}
    submitRequest scheduler $
      JobRequest $
        iterM_ (0, 0) (it, n) 1 (<) $ \ !ix ->
          unsafeWrite (toLinearIndex sz ix) (indexB ix)
    submitRequest scheduler $
      JobRequest $
        iterM_ (ib, 0) (m, n) 1 (<) $ \ !ix ->
          unsafeWrite (toLinearIndex sz ix) (indexB ix)
    submitRequest scheduler $
      JobRequest $
        iterM_ (it, 0) (ib, jt) 1 (<) $ \ !ix ->
          unsafeWrite (toLinearIndex sz ix) (indexB ix)
    submitRequest scheduler $
      JobRequest $
        iterM_ (it, jb) (ib, n) 1 (<) $ \ !ix ->
          unsafeWrite (toLinearIndex sz ix) (indexB ix)
    loopM_ 0 (< numWorkers scheduler) (+ 1) $ \ !wid -> do
      let !it' = wid * chunkHeight + it
      submitRequest scheduler $
        JobRequest $
        loadBlock it' (it' + chunkHeight)
    when (slackHeight > 0) $ do
      let !itSlack = (numWorkers scheduler) * chunkHeight + it
      submitRequest scheduler $
        JobRequest $
        loadBlock itSlack (itSlack + slackHeight)
    waitTillDone scheduler
  {-# INLINE loadP #-}

instance Load WD DIM3 e where
  loadS = loadWindowedSRec
  {-# INLINE loadS #-}
  loadP = loadWindowedPRec
  {-# INLINE loadP #-}

instance Load WD DIM4 e where
  loadS = loadWindowedSRec
  {-# INLINE loadS #-}
  loadP = loadWindowedPRec
  {-# INLINE loadP #-}

instance Load WD DIM5 e where
  loadS = loadWindowedSRec
  {-# INLINE loadS #-}
  loadP = loadWindowedPRec
  {-# INLINE loadP #-}

loadWindowedSRec
  :: (Index ix, Load WD (Lower ix) e) =>
     Array WD ix e -> (Int -> ST s e) -> (Int -> e -> ST s ()) -> ST s ()
loadWindowedSRec (WDArray (DArray c sz indexB) mStencilSz tix wSz indexW) unsafeRead unsafeWrite = do
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
             (DArray c szL (\ !ix -> indexB (consDim i ix)))
             ((snd . unconsDim) <$> mStencilSz) -- can safely drop the dim, only
                                                -- last 2 matter anyways
             tixL
             wSzL
             (\ !ix -> indexW (consDim i ix)))
    in loadS lowerArr unsafeRead (unsafeWriteLower i)
{-# INLINE loadWindowedSRec #-}


loadWindowedPRec
  :: (Index ix, Load WD (Lower ix) e) =>
     [Int] -> Array WD ix e -> (Int -> IO e) -> (Int -> e -> IO ()) -> IO ()
loadWindowedPRec wIds (WDArray (DArray c sz indexB) mStencilSz tix wSz indexW) unsafeRead unsafeWrite = do
  scheduler <- makeScheduler wIds
  let !szL = snd $ unconsDim sz
      !(t, tixL) = unconsDim tix
      !(w, wSzL) = unconsDim wSz
      !pageElements = totalElem szL
      unsafeWriteLower i k val = unsafeWrite (k + pageElements * (t + i)) val
      {-# INLINE unsafeWriteLower #-}
      unsafeWriteLowerST i k = unsafeIOToST . unsafeWriteLower i k
      {-# INLINE unsafeWriteLowerST #-}
  submitRequest scheduler $
    JobRequest $
    iterM_ zeroIndex tix 1 (<) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (indexB ix)
  submitRequest scheduler $
    JobRequest $
    iterM_ (liftIndex2 (+) tix wSz) sz 1 (<) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (indexB ix)
  loopM_ t (< (w + t)) (+ 1) $ \ !i ->
    let !lowerArr =
          (WDArray
             (DArray c szL (\ !ix -> indexB (consDim i ix)))
             ((snd . unconsDim) <$> mStencilSz) -- can safely drop the dim, only
                                                -- last 2 matter anyways
             tixL
             wSzL
             (\ !ix -> indexW (consDim i ix)))
    in submitRequest scheduler $
       JobRequest $
       stToIO $
       loadS
         lowerArr
         (\_ix -> unsafeIOToST $ unsafeRead _ix)
         (unsafeWriteLowerST i)
  waitTillDone scheduler
{-# INLINE loadWindowedPRec #-}



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
