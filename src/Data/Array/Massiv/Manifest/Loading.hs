{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- |
-- Module      : Data.Array.Massiv.Manifest.Loading
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Manifest.Loading where

import           Control.DeepSeq                 (NFData, deepseq)
import           Control.Monad                   (when)
import           Control.Monad.ST                (ST)

import           Data.Array.Massiv.Index
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Delayed
import           Data.Array.Massiv.Manifest
import           Data.Array.Massiv.Manifest.Gang
import           Data.Array.Massiv.Windowed
import           Data.STRef
import qualified Data.Vector.Generic             as VG
import qualified Data.Vector.Generic.Mutable     as MVG


import           GHC.Base                        (quotRemInt)


class Iterator i ix where
  iterateM_
    :: Monad m
    => i -- ^ Iterator
    -> ix -- ^ From
    -> ix -- ^ To
    -> (ix -> m ())
    -> m ()
  iterateWithLinearM_
    :: i -- ^ Iterator
    -> ix -- ^ Dimensions
    -> ix -- ^ From
    -> ix -- ^ To
    -> (Int -> ix -> ST s ())
    -> ST s ()


data RowMajor = RowMajor

instance Iterator RowMajor DIM2 where
  iterateM_ _ (m0, n0) (m1, n1) f = do
    loopM_ m0 (< m1) (+ 1) $ \ !i -> do
      loopM_ n0 (< n1) (+ 1) $ \ !j -> do f (i, j)
  {-# INLINE iterateM_ #-}

  iterateWithLinearM_ _ (_, n) !(m0, n0) !(m1, n1) f = do
    kRef <- newSTRef (n*m0)
    loopM_ m0 (< m1) (+ 1) $ \ !i -> do
      k <- readSTRef kRef
      loopM_ n0 (< n1) (+ 1) $ \ !j -> do
        f (k + j) (i, j)
      writeSTRef kRef (k + n)
  {-# INLINE iterateWithLinearM_ #-}

iterateWithLinear4M_
  :: (Int, Int)
  -> (Int, Int)
  -> (Int, Int)
  -> (Int -> (Int, Int) -> ST s ())
  -> ST s ()
iterateWithLinear4M_ sz@(_, n) !(m0, n0) !(m1, n1) f = do
  kRef <- newSTRef (n * n0)
  let m1' = m1 `mod` 4
  loopM_ m0 (< m1') (+ 4) $ \ !i -> do
    k <- readSTRef kRef
    loopM_ n0 (< n1) (+ 1) $ \ !j ->
      let !k' = k + j
      in f  k'      (i,     j) >>
         f (k' + 1) (i + 1, j) >>
         f (k' + 2) (i + 2, j) >>
         f (k' + 3) (i + 3, j)
    writeSTRef kRef (k + n)
  when (m1' > 0) $ iterateWithLinearM_ RowMajor sz (m1', n0) (m1, n1) f
{-# INLINE iterateWithLinear4M_ #-}


class Massiv r ix => Load r ix where
  loadS
    :: Array r ix e
    -> (Int -> e -> ST s ()) -- ^ Write element
    -> ST s ()

  loadP
    :: Array r ix e
    -> (Int -> e -> ST s ()) -- ^ Write element
    -> ST s ()


instance Load D DIM1 where
  loadS (DArray k f) unsafeWrite = do
    loopM_ 0 (< k) (+ 1) $ \ !i -> unsafeWrite i (f i)
  loadP (DArray k f) unsafeWrite = do
    let !gSize = gangSize theGang
        !(chunkLength, slackLength) = k `quotRemInt` gSize
    gangST theGang $ \ !tid -> do
      let !start = tid * chunkLength
          !end = start + chunkLength
      loopM_ start (< end) (+ 1) $ \ !i -> unsafeWrite i (f i)
    loopM_ (k - slackLength) (< k) (+ 1) $ \ !i -> unsafeWrite i (f i)

instance Load D DIM2 where
  loadS (DArray (m, n) f) unsafeWrite =
    iterateLinearM_ (m, n) (0, 0) (m, n) $ \ !k !ix -> unsafeWrite k (f ix)
    -- iterateWithLinearM_ RowMajor (m, n) (0, 0) (m, n) $ \ !k !ix ->
    --   unsafeWrite k (f ix)
    -- loopM_ 0 (< n) (+ 1) $ \ !j -> do
    --   loopM_ 0 (< m) (+ 1) $ \ !i ->
    --     unsafeWrite (toLinearIndex sz (i, j)) (f (i, j))
  {-# INLINE loadS #-}
  loadP arr@(DArray arrSize f) unsafeWrite = do
    let !gSize = gangSize theGang
        !totalLength = length arr
        !(chunkLength, slackLength) = totalLength `quotRemInt` gSize
    gangST theGang $ \ !tid ->
      let !start = tid * chunkLength
          !start2D = fromLinearIndex arrSize start
          !end2D = fromLinearIndex arrSize (start + chunkLength)
      in iterateWithLinearM_ RowMajor arrSize start2D end2D $ \ !k !ix ->
           unsafeWrite k (f ix)
    loopM_ (totalLength - slackLength) (< totalLength) (+ 1) $ \ !i ->
      unsafeWrite i (unsafeLinearIndex arr i)


iterateLinearM_
  :: (Int, Int)
  -> (Int, Int)
  -> (Int, Int)
  -> (Int -> (Int, Int) -> ST s ())
  -> ST s ()
iterateLinearM_ (_, n) !(m0, n0) !(m1, n1) f = do
  kRef <- newSTRef (n * m0)
  loopM_ m0 (< m1) (+ 1) $ \ !i -> do
    k <- readSTRef kRef
    loopM_ n0 (< n1) (+ 1) $ \ !j -> f (k + j) (i, j)
    writeSTRef kRef (k + n)
{-# INLINE iterateLinearM_ #-}


instance Load W DIM2 where
  loadS WArray {..} unsafeWrite = do
    let !(m, n) = wSize
        !(it, jt) = wWindowStartIndex
        !(wm, wn) = wWindowSize
        !(ib, jb) = (wm + it, wn + jt)
    let iterM_ = iterateLinearM_ (m, n)
        {-# INLINE iterM_ #-}
    iterM_ (0, 0) (it, n) $ \ !k !ix -> unsafeWrite k (wSafeIndexBorder ix)
    iterM_ (ib, 0) (m, n) $ \ !k !ix -> unsafeWrite k (wSafeIndexBorder ix)
    iterM_ (it, 0) (ib, jt) $ \ !k !ix -> unsafeWrite k (wSafeIndexBorder ix)
    iterM_ (it, jb) (ib, n) $ \ !k !ix -> unsafeWrite k (wSafeIndexBorder ix)
    iterM_ (it, jt) (ib, jb) $ \ !k !ix -> unsafeWrite k (wWindowUnsafeIndex ix)

  {-# INLINE loadS #-}
  loadP WArray {..} unsafeWrite = do
    let !(m, n) = wSize
        !(it, jt) = wWindowStartIndex
        !(wm, wn) = wWindowSize
        !(ib, jb) = (wm + it, wn + jt)
    let !gSize = gangSize theGang
        !(chunkHeight, slackHeight) = wm `divMod` gSize
    let iterM_ = iterateLinearM_ (m, n)
    let loadRows !it' !ib' =
          iterM_ (it', jt) (ib', jb) $ \ !k !ix ->
            unsafeWrite k (wWindowUnsafeIndex ix)
        {-# INLINE loadRows #-}
    gangST theGang $ \ !cix -> do
      let !it' = cix * chunkHeight + it
      loadRows it' (it' + chunkHeight)
      when (cix == 0) $ do
        iterM_ (0, 0) (it, n) $ \ !k !ix -> unsafeWrite k (wSafeIndexBorder ix)
      when (cix == 1 `mod` gSize) $ do
        iterM_ (ib, 0) (m, n) $ \ !k !ix -> unsafeWrite k (wSafeIndexBorder ix)
      when (cix == 2 `mod` gSize) $ do
        iterM_ (it, 0) (ib, jt) $ \ !k !ix -> unsafeWrite k (wSafeIndexBorder ix)
      when (cix == 3 `mod` gSize) $ do
        iterM_ (it, jb) (ib, n) $ \ !k !ix -> unsafeWrite k (wSafeIndexBorder ix)
      when (cix == 4 `mod` gSize && slackHeight > 0) $ do
        let !itSlack = gSize * chunkHeight + it
        loadRows itSlack (itSlack + slackHeight)

data V (v :: * -> *) = V


data Computation
  = Sequential
  | Parallel

compute
  :: forall proxy r ix v e.
     (Load r ix, NFData (v e), VG.Vector v e)
  => Computation -> proxy v -> Array r ix e -> Array M ix e
compute !comp _ !arr =
  vector `deepseq` MArray szArr (VG.unsafeIndex vector)
  where
    !szArr = size arr
    vector :: v e
    vector = VG.create generateArray
    generateArray :: ST s (VG.Mutable v s e)
    generateArray = do
      let !k = totalElem szArr
      mv <- MVG.unsafeNew k
      case comp of
        Parallel   -> loadP arr (MVG.unsafeWrite mv)
        Sequential -> loadS arr (MVG.unsafeWrite mv)
      return mv
    {-# INLINE generateArray #-}
{-# INLINE compute #-}
