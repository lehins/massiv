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


instance Index ix => Massiv W ix where
  size = wSize
  {-# INLINE size #-}



instance Functor (Array W ix) where
  fmap f !arr =
    arr
    { wSafeIndexBorder = f . wSafeIndexBorder arr
    , wWindowUnsafeIndex = f . wWindowUnsafeIndex arr
    }
  {-# INLINE fmap #-}


makeArrayWindowed
  :: Source r ix e
  => Array r ix e -- ^ Source array that will have a window inserted into it
  -> ix -- ^ Start index for the window
  -> ix -> (ix -> e) -> Array W ix e
makeArrayWindowed !arr !wIx !wSz wUnsafeIndex =
  WArray
  { wSize = size arr
  , wStencilSize = Nothing
  , wSafeIndexBorder = unsafeIndex arr
  , wWindowStartIndex = wIx
  , wWindowSize = wSz
  , wWindowUnsafeIndex = wUnsafeIndex
  }
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
