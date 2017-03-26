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
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Delayed
import           Data.Array.Massiv.Manifest
import           Data.Array.Massiv.Manifest.Gang
import           Data.Array.Massiv.Windowed
import qualified Data.Vector.Generic             as VG
import qualified Data.Vector.Generic.Mutable     as MVG
import           GHC.Base                        (quotRemInt)
import           System.IO.Unsafe                (unsafePerformIO)


class Index ix => Iterator i ix where

  ifoldl :: Source r ix => i -> (b -> ix -> e -> b) -> b -> Array r ix e -> b

  ifoldr :: Source r ix => i -> (ix -> e -> b -> b) -> b -> Array r ix e -> b

  iterateM_
    :: Monad m
    => i -- ^ Iterator
    -> ix -- ^ From
    -> ix -- ^ To
    -> (ix -> m ())
    -> m ()
  iterateLinearM_
    :: Monad m
    => i -- ^ Iterator
    -> ix -- ^ Dimensions
    -> Int -- ^ From
    -> Int -- ^ To
    -> (Int -> ix -> m ())
    -> m ()
  iterateWithLinearM_
    :: Monad m
    => i -- ^ Iterator
    -> ix -- ^ Dimensions
    -> ix -- ^ From
    -> ix -- ^ To
    -> (Int -> ix -> m ())
    -> m ()
  iterateWithLinearM_ it !sz !start !end f =
    iterateM_ it start end $ \ !ix -> f (toLinearIndex sz ix) ix
  {-# INLINE iterateWithLinearM_ #-}


data RowMajor = RowMajor


instance Iterator RowMajor DIM1 where
  iterateM_ _ !k0 !k1 f = loopM_ k0 (< k1) (+ 1) $ \ !i -> f i
  {-# INLINE iterateM_ #-}

  iterateLinearM_ = iterateWithLinearM_
  {-# INLINE iterateLinearM_ #-}


instance Iterator RowMajor DIM2 where
  ifoldr f acc arr =
    loop 0 (< m) (+ 1) acc $ \ !i !accO ->
      loop 0 (< n) (+ 1) accO $ \ !j !accI ->
        let !ix = (i, j)
        in f ix (unsafeIndex arr ix) accI
  {-# INLINE ifoldr #-}
  ifoldl f acc arr =
    loop (m - 1) (>= 0) (subtract 1) acc $ \ !i !accO ->
      loop (n - 1) (>= 0) (subtract 1) accO $ \ !j !accI ->
        let !ix = (i, j)
        in f ix (unsafeIndex arr ix) accI
  {-# INLINE ifoldl #-}
  iterateM_ _ !(m0, n0) !(m1, n1) f =
    loopM_ m0 (< m1) (+ 1) $ \ !i -> loopM_ n0 (< n1) (+ 1) $ \ !j -> f (i, j)
  {-# INLINE iterateM_ #-}
  iterateLinearM_ _ !sz@(_, n) !k0 !k1 f = do
    let !(si, sj) = fromLinearIndex sz k0
    let !(ei, ej) = fromLinearIndex sz (k1 - 1)
    if si == ei || k0 >= k1
      then loopM_ k0 (< k1) (+ 1) $ \ !k -> f k (si, sj + k - k0)
      else do
        loopM_ sj (< n) (+ 1) $ \ !j -> f (k0 + j - sj) (si, j)
        loopM_ (si + 1) (< ei) (+ 1) $ \ !i ->
          loopM_ 0 (< n) (+ 1) $ \ !j ->
            let !ix = (i, j)
            in f (toLinearIndex sz ix) ix
        let !ej' = ej + 1
        loopM_ 0 (< ej') (+ 1) $ \ !j -> f (k1 + j - ej') (ei, j)
  {-# INLINE iterateLinearM_ #-}


class Massiv r ix => Load r ix where
  loadS
    :: Array r ix e
    -> (Int -> e -> ST s ()) -- ^ Write element
    -> ST s ()

  loadP
    :: Array r ix e
    -> (Int -> e -> IO ()) -- ^ Write element
    -> IO ()


instance Load D DIM1 where
  loadS (DArray sz f) unsafeWrite = do
    iterateWithLinearM_ RowMajor sz 0 sz $ \ !k !ix ->
      unsafeWrite k (f ix)
  {-# INLINE loadS #-}
  loadP arr@(DArray arrSize f) unsafeWrite = do
    let !gSize = gangSize theGang
        !totalLength = totalElem arrSize
        !(chunkLength, slackLength) = totalLength `quotRemInt` gSize
    gangIO theGang $ \ !tid ->
      let !start = tid * chunkLength
          !end = start + chunkLength
      in do
        iterateLinearM_ RowMajor arrSize start end $ \ !k !ix -> do
          unsafeWrite k $ f ix
    loopM_ (totalLength - slackLength) (< totalLength) (+ 1) $ \ !i ->
      unsafeWrite i (unsafeLinearIndex arr i)
  {-# INLINE loadP #-}

instance Load D DIM2 where
  loadS (DArray sz f) unsafeWrite =
    iterateWithLinearM_ RowMajor sz (0, 0) sz $ \ !k !ix ->
      unsafeWrite k (f ix)
  {-# INLINE loadS #-}
  loadP arr@(DArray arrSize f) unsafeWrite = do
    let !gSize = gangSize theGang
        !totalLength = totalElem arrSize
        !(chunkLength, slackLength) = totalLength `quotRemInt` gSize
    gangIO theGang $ \ !tid ->
      let !start = tid * chunkLength
          !end = start + chunkLength
      in do
        iterateLinearM_ RowMajor arrSize start end $ \ !k !ix -> do
          unsafeWrite k $ f ix
    loopM_ (totalLength - slackLength) (< totalLength) (+ 1) $ \ !i ->
      unsafeWrite i (unsafeLinearIndex arr i)
  {-# INLINE loadP #-}


-- iterateWithLinearIO_
--   :: (Int, Int)
--   -> Int
--   -> Int
--   -> (Int -> (Int, Int) -> IO ())
--   -> IO ()
-- iterateWithLinearIO_ !(m, n) !k0 !k1 f = do
--   let !(si, sj) = fromLinearIndex (m, n) k0
--   let !(ei, ej) = fromLinearIndex (m, n) (k1 - 1)
--   if si == ei || k0 >= k1
--     then loopM_ k0 (< k1) (+ 1) $ \ !k -> f k (si, sj + k - k0)
--     else do
--       loopM_ sj (< n) (+ 1) $ \ !j -> f (k0 + j - sj) (si, j)
--       kRef <- newIORef (n * (si + 1))
--       loopM_ (si + 1) (< ei) (+ 1) $ \ !i -> do
--         kRow <- readIORef kRef
--         loopM_ 0 (< n) (+ 1) $ \ !j -> f (kRow + j) (i, j)
--         writeIORef kRef (kRow + n)
--       let !ej' = ej + 1
--       loopM_ 0 (< ej') (+ 1) $ \ !j -> f (k1 + j - ej') (ei, j)
-- {-# INLINE iterateWithLinearIO_ #-}

-- iterateWithLinearST_
--   :: (Int, Int)
--   -> Int
--   -> Int
--   -> (Int -> (Int, Int) -> ST s ())
--   -> ST s ()
-- iterateWithLinearST_ !(m, n) !k0 !k1 f = do
--   let !(si, sj) = fromLinearIndex (m, n) k0
--   let !(ei, ej) = fromLinearIndex (m, n) (k1 - 1)
--   if si == ei || k0 >= k1
--     then loopM_ k0 (< k1) (+ 1) $ \ !k -> f k (si, sj + k - k0)
--     else do
--       loopM_ sj (< n) (+ 1) $ \ !j -> f (k0 + j - sj) (si, j)
--       kRef <- newSTRef (n * (si + 1))
--       loopM_ (si + 1) (< ei) (+ 1) $ \ !i -> do
--         kRow <- readSTRef kRef
--         loopM_ 0 (< n) (+ 1) $ \ !j -> f (kRow + j) (i, j)
--         writeSTRef kRef (kRow + n)
--       let !ej' = ej + 1
--       loopM_ 0 (< ej') (+ 1) $ \ !j -> f (k1 + j - ej') (ei, j)
-- {-# INLINE iterateWithLinearST_ #-}



instance Load W DIM2 where
  loadS WArray {..} unsafeWrite = do
    let !(m, n) = wSize
        !(it, jt) = wWindowStartIndex
        !(wm, wn) = wWindowSize
        !(ib, jb) = (wm + it, wn + jt)
    let iterM_ = iterateWithLinearM_ RowMajor wSize
        {-# INLINE iterM_ #-}
    iterM_ (0, 0) (it, n) $ \ !k !ix -> unsafeWrite k (wSafeIndexBorder ix)
    iterM_ (ib, 0) (m, n) $ \ !k !ix -> unsafeWrite k (wSafeIndexBorder ix)
    iterM_ (it, 0) (ib, jt) $ \ !k !ix -> unsafeWrite k (wSafeIndexBorder ix)
    iterM_ (it, jb) (ib, n) $ \ !k !ix -> unsafeWrite k (wSafeIndexBorder ix)
    iterM_ (it, jt) (ib, jb) $ \ !k !ix ->
      {-# SCC "writeWindow" #-} unsafeWrite k (wWindowUnsafeIndex ix)
    -- loopM_ 0 (< n) (+ 1) $ \ !j -> do
    --     loopM_ 0 (< it) (+ 1) $ \ !i -> do
    --       unsafeWrite (toLinearIndex wSize (i, j)) (wSafeIndexBorder (i, j))
    --     loopM_ ib (< m) (+ 1) $ \ !i -> do
    --       unsafeWrite (toLinearIndex wSize (i, j)) (wSafeIndexBorder (i, j))
    -- loopM_ it (< ib) (+ 1) $ \ !i -> do
    --     loopM_ 0 (< jt) (+ 1) $ \ !j -> do
    --       unsafeWrite (toLinearIndex wSize (i, j)) (wSafeIndexBorder (i, j))
    --     loopM_ jt (< jb) (+ 1) $ \ !j -> do
    --       unsafeWrite (toLinearIndex wSize (i, j)) (wWindowUnsafeIndex (i, j))
    --     loopM_ jb (< n) (+ 1) $ \ !j -> do
    --       unsafeWrite (toLinearIndex wSize (i, j)) (wSafeIndexBorder (i, j))

  {-# INLINE loadS #-}
  loadP WArray {..} unsafeWrite = do
    let !(m, n) = wSize
        !(it, jt) = wWindowStartIndex
        !(wm, wn) = wWindowSize
        !(ib, jb) = (wm + it, wn + jt)
    let !gSize = gangSize theGang
        !(chunkHeight, slackHeight) = wm `quotRemInt` gSize
    let iterM_ = iterateWithLinearM_ RowMajor wSize
        {-# INLINE iterM_ #-}
    let loadRows !it' !ib' =
          iterM_ (it', jt) (ib', jb) $ \ !k !ix ->
            unsafeWrite k (wWindowUnsafeIndex ix)
        {-# INLINE loadRows #-}
    gangIO theGang $ \ !cix -> do
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
  {-# INLINE loadP #-}

data V (v :: * -> *) = V


data Computation
  = Sequential
  | Parallel

compute
  :: forall r ix v e.
     (Load r ix, NFData (v e), VG.Vector v e)
  => Computation -> V v -> Array r ix e -> Array M ix e
compute Sequential _ !arr =
  MArray szArr (VG.unsafeIndex vector)
  where
    !szArr = size arr
    vector :: v e
    !vector = VG.create generateArray
    generateArray :: ST s (VG.Mutable v s e)
    generateArray = do
      mv <- MVG.unsafeNew (totalElem szArr)
      loadS arr (MVG.unsafeWrite mv)
      return mv
    {-# INLINE generateArray #-}
compute Parallel _ !arr =
  vector `deepseq` MArray (size arr) (VG.unsafeIndex vector)
  where
    vector :: v e
    !vector = loadVectorParallel arr
{-# INLINE compute #-}


loadVectorParallel :: (Load r ix, VG.Vector v a) => Array r ix a -> v a
loadVectorParallel arr = unsafePerformIO $ do
  let !k = totalElem (size arr)
  mv <- MVG.unsafeNew k
  loadP arr (\ !val -> MVG.unsafeWrite mv val)
  VG.unsafeFreeze mv
{-# NOINLINE loadVectorParallel #-}



computeP
  :: forall r ix v e.
     (Load r ix, NFData (v e), VG.Vector v e)
  => V v -> Array r ix e -> IO (Array M ix e)
computeP _ !arr = do
  vector <- loadVectorParallelIO arr :: IO (v e)
  return (vector `deepseq` MArray (size arr) (VG.unsafeIndex vector))
{-# INLINE computeP #-}


loadVectorParallelIO :: (Load r ix, VG.Vector v a) => Array r ix a -> IO (v a)
loadVectorParallelIO arr = do
  let !k = totalElem (size arr)
  mv <- MVG.unsafeNew k
  loadP arr (\ !val -> MVG.unsafeWrite mv val)
  VG.unsafeFreeze mv
{-# INLINE loadVectorParallelIO #-}


-- imapMaybeA :: forall r ix v e.
--            (Load r ix, NFData (v e), VG.Vector v e)
--         => V v -> (ix -> e -> Maybe b) -> Array r ix e -> IO (Array M ix e)
-- imapMaybeA _ f arr = do
--   MArray szArr (VG.unsafeIndex vector)
--   where
--     !ixData = ifoldl RowMajor predAcc (0, []) arr
--     predAcc !acc@(k, ls) ix v = case f ix v of
--                                   Nothing -> acc
--                                   Just v' -> (k+1, v':ls)
--     {-# INLINE predAcc #-}
--     !szArr = size arr
--     vector :: v e
--     !vector = VG.create generateArray
--     generateArray :: ST s (VG.Mutable v s e)
--     generateArray = do
--       mv <- MVG.unsafeNew (totalElem szArr)
--       loadS arr (MVG.unsafeWrite mv)
--       return mv
--     {-# INLINE generateArray #-}
