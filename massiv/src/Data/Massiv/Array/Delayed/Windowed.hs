{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DeriveGeneric         #-}
-- |
-- Module      : Data.Massiv.Array.Delayed.Windowed
-- Copyright   : (c) Alexey Kuleshevich 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Delayed.Windowed
  ( DW(..)
  , Array(..)
  , makeWindowedArray
  , unsafeBackpermuteDW
  ) where

import           Control.Monad                          (when)
import           Data.Massiv.Array.Delayed.Internal
import           Data.Massiv.Array.Manifest.Boxed
import           Data.Massiv.Array.Manifest.Internal
import           Data.Massiv.Core
import           Data.Massiv.Core.Common
import           Data.Massiv.Core.List                  (showArray)
import           Data.Massiv.Core.Scheduler
import           Data.Proxy                             (Proxy (..))
import           Data.Typeable                          (showsTypeRep, typeRep)
import           GHC.Generics hiding (D)

-- | Delayed Windowed Array representation.
data DW = DW deriving Generic

type instance EltRepr DW ix = D

data instance Array DW ix e = DWArray { wdArray :: !(Array D ix e)
                                      , wdStencilSize :: Maybe ix
                                        -- ^ Setting this value during stencil
                                        -- application improves cache utilization
                                        -- while computing an array
                                      , wdWindowStartIndex :: !ix
                                      , wdWindowSize :: !ix
                                      , wdWindowUnsafeIndex :: ix -> e } deriving Generic

instance {-# OVERLAPPING #-} (Show e, Ragged L ix e, Load DW ix e) =>
  Show (Array DW ix e) where
  show arr = showArray (showsTypeRep (typeRep (Proxy :: Proxy DW)) " ") (computeAs B arr)


instance Index ix => Construct DW ix e where
  getComp = dComp . wdArray
  {-# INLINE getComp #-}

  setComp c arr = arr { wdArray = (wdArray arr) { dComp = c } }
  {-# INLINE setComp #-}

  unsafeMakeArray c sz f = DWArray (unsafeMakeArray c sz f) Nothing zeroIndex zeroIndex f
  {-# INLINE unsafeMakeArray #-}


-- | Any resize or extract on Windowed Array will hurt the performance.
instance Index ix => Size DW ix e where
  size = size . wdArray
  {-# INLINE size #-}
  unsafeResize sz DWArray {..} =
    let dArr = unsafeResize sz wdArray
    in DWArray
       { wdArray = dArr
       , wdStencilSize = Nothing
       , wdWindowStartIndex = zeroIndex
       , wdWindowSize = zeroIndex
       , wdWindowUnsafeIndex = evaluateAt dArr
       }
  unsafeExtract sIx newSz = unsafeExtract sIx newSz . wdArray


instance Functor (Array DW ix) where
  fmap f !arr =
    arr
    { wdArray = fmap f (wdArray arr)
    , wdWindowUnsafeIndex = f . wdWindowUnsafeIndex arr
    }
  {-# INLINE fmap #-}


-- | Supply a separate generating function for interior of an array. This is
-- very usful for stencil mapping, where interior function does not perform
-- boundary checks, thus significantly speeding up computation process.
--
-- @since 0.1.3
makeWindowedArray
  :: Source r ix e
  => Array r ix e -- ^ Source array that will have a window inserted into it
  -> ix -- ^ Start index for the window
  -> ix -- ^ Size of the window
  -> (ix -> e) -- ^ Inside window indexing function
  -> Array DW ix e
makeWindowedArray !arr !wIx !wSz wUnsafeIndex
  | not (isSafeIndex sz wIx) =
    error $
    "Incorrect window starting index: " ++ show wIx ++ " for: " ++ show (size arr)
  | liftIndex2 (+) wIx wSz > sz =
    error $
    "Incorrect window size: " ++
    show wSz ++ " and/or placement: " ++ show wIx ++ " for: " ++ show (size arr)
  | otherwise =
    DWArray
    { wdArray = delay arr
    , wdStencilSize = Nothing
    , wdWindowStartIndex = wIx
    , wdWindowSize = wSz
    , wdWindowUnsafeIndex = wUnsafeIndex
    }
  where sz = size arr
{-# INLINE makeWindowedArray #-}


-- | Backpermute a windowed array. If index mappings aren't correct reading memory out of bounds is
-- very likely.
--
-- __Note__: windowStartIndex is mapped to the new windowStartIndex using the "old to new index"
-- map.  This means that the order of the elements should be preserved, or performance will take a
-- major hit.
--
-- __Important__: This function is still experimental and can be removed in any future minor
-- release.
--
-- @since 0.2.0
unsafeBackpermuteDW ::
     Index ix
  => (ix -> ix) -- ^ map from old to new index
  -> (ix -> ix) -- ^ map from new to old index
  -> ix -- ^ Size of resulting array
  -> Array DW ix a
  -> Array DW ix a
unsafeBackpermuteDW toNewIndex toOldIndex sz DWArray {..} =
  DWArray
    { wdArray =
        DArray {dComp = dComp wdArray, dSize = sz, dUnsafeIndex = dUnsafeIndex wdArray . toOldIndex}
    , wdStencilSize = wdStencilSize
    , wdWindowStartIndex = newWindowStartIndex
    , wdWindowSize =
        liftIndex2
          (-)
          (toNewIndex (liftIndex2 (+) wdWindowStartIndex wdWindowSize))
          newWindowStartIndex
    , wdWindowUnsafeIndex = wdWindowUnsafeIndex . toOldIndex
    }
  where
    !newWindowStartIndex = toNewIndex wdWindowStartIndex
{-# INLINE unsafeBackpermuteDW #-}


instance {-# OVERLAPPING #-} Load DW Ix1 e where
  loadS (DWArray (DArray _ sz indexB) _ it wk indexW) _ unsafeWrite = do
    iterM_ 0  it (pureIndex 1) (<) $ \ !i -> unsafeWrite i (indexB i)
    iterM_ it wk (pureIndex 1) (<) $ \ !i -> unsafeWrite i (indexW i)
    iterM_ wk sz (pureIndex 1) (<) $ \ !i -> unsafeWrite i (indexB i)
  {-# INLINE loadS #-}
  loadP wIds (DWArray (DArray _ sz indexB) _ it wk indexW) _ unsafeWrite = do
      divideWork_ wIds wk $ \ !scheduler !chunkLength !totalLength !slackStart -> do
        scheduleWork scheduler $
          iterM_ 0 it (pureIndex 1) (<) $ \ !ix ->
            unsafeWrite (toLinearIndex sz ix) (indexB ix)
        scheduleWork scheduler $
          iterM_ wk sz (pureIndex 1) (<) $ \ !ix ->
            unsafeWrite (toLinearIndex sz ix) (indexB ix)
        loopM_ it (< (slackStart + it)) (+ chunkLength) $ \ !start ->
          scheduleWork scheduler $
          iterM_ start (start + chunkLength) (pureIndex 1) (<) $ \ !k ->
            unsafeWrite k $ indexW k
        scheduleWork scheduler $
          iterM_ (slackStart + it) (totalLength + it) (pureIndex 1) (<) $ \ !k ->
            unsafeWrite k (indexW k)
  {-# INLINE loadP #-}



instance {-# OVERLAPPING #-} Load DW Ix2 e where
  loadS arr _ unsafeWrite = do
    let (DWArray (DArray _ sz@(m :. n) indexB) mStencilSz (it :. jt) (wm :. wn) indexW) =
          arr
    let (ib :. jb) = (wm + it) :. (wn + jt)
        blockHeight = case mStencilSz of
                        Just (i :. _) -> i
                        _             -> 1
    iterM_ (0 :. 0) (it :. n) (pureIndex 1) (<) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (indexB ix)
    iterM_ (ib :. 0) (m :. n) (pureIndex 1) (<) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (indexB ix)
    iterM_ (it :. 0) (ib :. jt) (pureIndex 1) (<) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (indexB ix)
    iterM_ (it :. jb) (ib :. n) (pureIndex 1) (<) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (indexB ix)
    unrollAndJam blockHeight (it :. ib) (jt :. jb) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (indexW ix)
  {-# INLINE loadS #-}
  loadP wIds arr _ unsafeWrite = do
    let (DWArray (DArray _ sz@(m :. n) indexB) mStencilSz (it :. jt) (wm :. wn) indexW) = arr
    withScheduler_ wIds $ \scheduler -> do
      let (ib :. jb) = (wm + it) :. (wn + jt)
          !blockHeight = case mStencilSz of
                           Just (i :. _) -> i
                           _             -> 1
          !(chunkHeight, slackHeight) = wm `quotRem` numWorkers scheduler
      let loadBlock !it' !ib' =
            unrollAndJam blockHeight (it' :. ib') (jt :. jb) $ \ !ix ->
              unsafeWrite (toLinearIndex sz ix) (indexW ix)
          {-# INLINE loadBlock #-}
      scheduleWork scheduler $
        iterM_ (0 :. 0) (it :. n) (pureIndex 1) (<) $ \ !ix ->
          unsafeWrite (toLinearIndex sz ix) (indexB ix)
      scheduleWork scheduler $
        iterM_ (ib :. 0) (m :. n) (pureIndex 1) (<) $ \ !ix ->
          unsafeWrite (toLinearIndex sz ix) (indexB ix)
      scheduleWork scheduler $
        iterM_ (it :. 0) (ib :. jt) (pureIndex 1) (<) $ \ !ix ->
          unsafeWrite (toLinearIndex sz ix) (indexB ix)
      scheduleWork scheduler $
        iterM_ (it :. jb) (ib :. n) (pureIndex 1) (<) $ \ !ix ->
          unsafeWrite (toLinearIndex sz ix) (indexB ix)
      loopM_ 0 (< numWorkers scheduler) (+ 1) $ \ !wid -> do
        let !it' = wid * chunkHeight + it
        scheduleWork scheduler $ loadBlock it' (it' + chunkHeight)
      when (slackHeight > 0) $ do
        let !itSlack = (numWorkers scheduler) * chunkHeight + it
        scheduleWork scheduler $
          loadBlock itSlack (itSlack + slackHeight)
  {-# INLINE loadP #-}


-- instance {-# OVERLAPPING #-} Load DW Ix3 e where
--   loadS = loadWindowedSRec
--   {-# INLINE loadS #-}
--   loadP = loadWindowedPRec
--   {-# INLINE loadP #-}


instance {-# OVERLAPPABLE #-} (Index ix, Load DW (Lower ix) e) => Load DW ix e where
  loadS = loadWindowedSRec
  {-# INLINE loadS #-}
  loadP = loadWindowedPRec
  {-# INLINE loadP #-}


loadWindowedSRec :: (Index ix, Load DW (Lower ix) e, Monad m) =>
  Array DW ix e -> (Int -> m e) -> (Int -> e -> m ()) -> m ()
loadWindowedSRec (DWArray darr mStencilSz tix wSz indexW) _unsafeRead unsafeWrite = do
  let DArray _ sz indexB = darr
      !szL = tailDim sz
      !bix = liftIndex2 (+) tix wSz
      !(t, tixL) = unconsDim tix
      !pageElements = totalElem szL
      unsafeWriteLower i k val = unsafeWrite (k + pageElements * i) val
      {-# INLINE unsafeWriteLower #-}
  iterM_ zeroIndex tix (pureIndex 1) (<) $ \ !ix ->
    unsafeWrite (toLinearIndex sz ix) (indexB ix)
  iterM_ bix sz (pureIndex 1) (<) $ \ !ix ->
    unsafeWrite (toLinearIndex sz ix) (indexB ix)
  loopM_ t (< headDim bix) (+ 1) $ \ !i ->
    let !lowerArr =
          (DWArray
             (DArray Seq szL (indexB . consDim i))
             (tailDim <$> mStencilSz) -- can safely drop the dim, only
                                      -- last 2 matter anyways
             tixL
             (tailDim wSz)
             (indexW . consDim i))
    in loadS lowerArr _unsafeRead (unsafeWriteLower i)
{-# INLINE loadWindowedSRec #-}


loadWindowedPRec :: (Index ix, Load DW (Lower ix) e) =>
  [Int] -> Array DW ix e -> (Int -> IO e) -> (Int -> e -> IO ()) -> IO ()
loadWindowedPRec wIds (DWArray darr mStencilSz tix wSz indexW) _unsafeRead unsafeWrite = do
  withScheduler_ wIds $ \ scheduler -> do
    let DArray _ sz indexB = darr
        !szL = tailDim sz
        !bix = liftIndex2 (+) tix wSz
        !(t, tixL) = unconsDim tix
        !pageElements = totalElem szL
        unsafeWriteLower i k = unsafeWrite (k + pageElements * i)
        {-# INLINE unsafeWriteLower #-}
    scheduleWork scheduler $
      iterM_ zeroIndex tix (pureIndex 1) (<) $ \ !ix ->
        unsafeWrite (toLinearIndex sz ix) (indexB ix)
    scheduleWork scheduler $
      iterM_ bix sz (pureIndex 1) (<) $ \ !ix ->
        unsafeWrite (toLinearIndex sz ix) (indexB ix)
    loopM_ t (< headDim bix) (+ 1) $ \ !i ->
      let !lowerArr =
            (DWArray
               (DArray Seq szL (indexB . consDim i))
               (tailDim <$> mStencilSz) -- can safely drop the dim, only
                                        -- last 2 matter anyways
               tixL
               (tailDim wSz)
               (indexW . consDim i))
      in scheduleWork scheduler $
         loadS
           lowerArr
           (_unsafeRead)
           (unsafeWriteLower i)
{-# INLINE loadWindowedPRec #-}



unrollAndJam :: Monad m =>
                Int -> Ix2 -> Ix2 -> (Ix2 -> m a) -> m ()
unrollAndJam !bH (it :. ib) (jt :. jb) f = do
  let !bH' = min (max 1 bH) 7
  let f2 (i :. j) = f (i :. j) >> f  ((i + 1) :. j)
  let f3 (i :. j) = f (i :. j) >> f2 ((i + 1) :. j)
  let f4 (i :. j) = f (i :. j) >> f3 ((i + 1) :. j)
  let f5 (i :. j) = f (i :. j) >> f4 ((i + 1) :. j)
  let f6 (i :. j) = f (i :. j) >> f5 ((i + 1) :. j)
  let f7 (i :. j) = f (i :. j) >> f6 ((i + 1) :. j)
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
      f' (i :. j)
  loopM_ ibS (< ib) (+ 1) $ \ !i ->
    loopM_ jt (< jb) (+ 1) $ \ !j ->
      f (i :. j)
{-# INLINE unrollAndJam #-}


-- TODO: Implement Hilbert curve


instance {-# OVERLAPPING #-} Load DW Ix2T e where
  loadS arr _ unsafeWrite = do
    let (DWArray (DArray _ sz@(m, n) indexB) mStencilSz (it, jt) (wm, wn) indexW) =
          arr
    let (ib, jb) = (wm + it, wn + jt)
        blockHeight = case mStencilSz of
                        Just (i, _) -> i
                        _           -> 1
    iterM_ (0, 0) (it, n) (pureIndex 1) (<) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (indexB ix)
    iterM_ (ib, 0) (m, n) (pureIndex 1) (<) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (indexB ix)
    iterM_ (it, 0) (ib, jt) (pureIndex 1) (<) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (indexB ix)
    iterM_ (it, jb) (ib, n) (pureIndex 1) (<) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (indexB ix)
    unrollAndJamT blockHeight (it, ib) (jt, jb) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (indexW ix)
  {-# INLINE loadS #-}
  loadP wIds arr _ unsafeWrite = do
    let (DWArray (DArray _ sz@(m, n) indexB) mStencilSz (it, jt) (wm, wn) indexW) = arr
    withScheduler_ wIds $ \ scheduler -> do
      let (ib, jb) = (wm + it, wn + jt)
          blockHeight = case mStencilSz of
                          Just (i, _) -> i
                          _           -> 1
          !(chunkHeight, slackHeight) = wm `quotRem` numWorkers scheduler
      let loadBlock !it' !ib' =
            unrollAndJamT blockHeight (it', ib') (jt, jb) $ \ !ix ->
              unsafeWrite (toLinearIndex sz ix) (indexW ix)
          {-# INLINE loadBlock #-}
      scheduleWork scheduler $
        iterM_ (0, 0) (it, n) (pureIndex 1) (<) $ \ !ix ->
          unsafeWrite (toLinearIndex sz ix) (indexB ix)
      scheduleWork scheduler $
        iterM_ (ib, 0) (m, n) (pureIndex 1) (<) $ \ !ix ->
          unsafeWrite (toLinearIndex sz ix) (indexB ix)
      scheduleWork scheduler $
        iterM_ (it, 0) (ib, jt) (pureIndex 1) (<) $ \ !ix ->
          unsafeWrite (toLinearIndex sz ix) (indexB ix)
      scheduleWork scheduler $
        iterM_ (it, jb) (ib, n) (pureIndex 1) (<) $ \ !ix ->
          unsafeWrite (toLinearIndex sz ix) (indexB ix)
      loopM_ 0 (< numWorkers scheduler) (+ 1) $ \ !wid -> do
        let !it' = wid * chunkHeight + it
        scheduleWork scheduler $ loadBlock it' (it' + chunkHeight)
      when (slackHeight > 0) $ do
        let !itSlack = (numWorkers scheduler) * chunkHeight + it
        scheduleWork scheduler $ loadBlock itSlack (itSlack + slackHeight)
  {-# INLINE loadP #-}



unrollAndJamT :: Monad m =>
                Int -> Ix2T -> Ix2T -> (Ix2T -> m a) -> m ()
unrollAndJamT !bH (it, ib) (jt, jb) f = do
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
{-# INLINE unrollAndJamT #-}
