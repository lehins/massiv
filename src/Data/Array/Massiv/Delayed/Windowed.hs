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
  loadS (WArray sz _ indexBorder it wk indexWindow) unsafeWrite = do
    iterateM_ RowMajor 0 it $ \ !i ->
      unsafeWrite i (indexBorder i)
    iterateM_ RowMajor it wk $ \ !i ->
      unsafeWrite i (indexWindow i)
    iterateM_ RowMajor wk sz $ \ !i ->
      unsafeWrite i (indexBorder i)
  {-# INLINE loadS #-}
  loadP WArray {..} unsafeWrite = undefined



instance Load W DIM2 where
  loadS (WArray sz@(m, n) mStencilSz indexB (it, jt) (wm, wn) indexW) unsafeWrite = do
    let !(ib, jb) = (wm + it, wn + jt)
        !blockHeight = maybe 1 fst mStencilSz
    iterateWithLinearM_ RowMajor sz (0, 0) (it, n) $ \ !k !ix ->
      unsafeWrite k (indexB ix)
    iterateWithLinearM_ RowMajor sz (ib, 0) (m, n) $ \ !k !ix ->
      unsafeWrite k (indexB ix)
    iterateWithLinearM_ RowMajor sz (it, 0) (ib, jt) $ \ !k !ix ->
      unsafeWrite k (indexB ix)
    iterateWithLinearM_ RowMajor sz (it, jb) (ib, n) $ \ !k !ix ->
      unsafeWrite k (indexB ix)
    unrollAndJam blockHeight (it, ib) (jt, jb) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (indexW ix)
  {-# INLINE loadS #-}
  loadP (WArray sz@(m, n) mStencilSz indexB (it, jt) (wm, wn) indexW) unsafeWrite = do
    let !(ib, jb) = (wm + it, wn + jt)
        !blockHeight = maybe 1 fst mStencilSz
    let !gSize = gangSize theGang
        !(chunkHeight, slackHeight) = wm `quotRem` gSize
    let iterM_ = iterateWithLinearM_ RowMajor sz
        {-# INLINE iterM_ #-}
    let loadBlock !it' !ib' =
          unrollAndJam blockHeight (it', ib') (jt, jb) $ \ !ix ->
            unsafeWrite (toLinearIndex sz ix) (indexW ix)
        {-# INLINE loadBlock #-}
    gangIO theGang $ \ !cix -> do
      let !it' = cix * chunkHeight + it
      loadBlock it' (it' + chunkHeight)
      when (cix == 0) $ do
        iterM_ (0, 0) (it, n) $ \ !k !ix -> unsafeWrite k (indexB ix)
      when (cix == 1 `mod` gSize) $ do
        iterM_ (ib, 0) (m, n) $ \ !k !ix -> unsafeWrite k (indexB ix)
      when (cix == 2 `mod` gSize) $ do
        iterM_ (it, 0) (ib, jt) $ \ !k !ix ->
          unsafeWrite k (indexB ix)
      when (cix == 3 `mod` gSize) $ do
        iterM_ (it, jb) (ib, n) $ \ !k !ix ->
          unsafeWrite k (indexB ix)
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
  -- when (ibRem > 0) $
  --   unrollAndJam ibRem (ibS, ib) (jt, jb) f
  loopM_ ibS (< ib) (+ 1) $ \ !i ->
    loopM_ jt (< jb) (+ 1) $ \ !j ->
      f (i, j)
{-# INLINE unrollAndJam #-}



  -- loadS (WArray sz@(m, n) indexBorder (it, jt) (wm, wn) indexWindow) unsafeWrite = do
  --   let !(ib, jb) = (wm + it, wn + jt)
  --   iterateWithLinearM_ RowMajor sz (0, 0) (it, n) $ \ !k !ix ->
  --     unsafeWrite k (indexBorder ix)
  --   iterateWithLinearM_ RowMajor sz (ib, 0) (m, n) $ \ !k !ix ->
  --     unsafeWrite k (indexBorder ix)
  --   iterateWithLinearM_ RowMajor sz (it, 0) (ib, jt) $ \ !k !ix ->
  --     unsafeWrite k (indexBorder ix)
  --   iterateWithLinearM_ RowMajor sz (it, jb) (ib, n) $ \ !k !ix ->
  --     unsafeWrite k (indexBorder ix)
  --   -- iterateWithLinearM_ RowMajor sz t b $ \ !k !ix ->
  --   --   unsafeWrite k (indexWindow ix)
  --   let !ibS = ib - ((ib - it) `mod` 3)
  --   loopM_ it (< ibS) (+ 3) $ \ !i -> do
  --     loopM_ jt (< jb) (+ 1) $ \ !j -> do
  --       let !ix0 = (i, j)
  --       let !ix1 = (i + 1, j)
  --       let !ix2 = (i + 2, j)
  --       unsafeWrite (toLinearIndex sz ix0) (indexWindow ix0)
  --       unsafeWrite (toLinearIndex sz ix1) (indexWindow ix1)
  --       unsafeWrite (toLinearIndex sz ix2) (indexWindow ix2)
  --   loopM_ ibS (< ib) (+ 1) $ \ !i -> do
  --     loopM_ jt (< jb) (+ 1) $ \ !j -> do
  --       unsafeWrite (toLinearIndex sz (i, j)) (indexWindow (i, j))
