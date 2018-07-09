{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
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

-- | Delayed Windowed Array representation.
data DW = DW

type instance EltRepr DW ix = D

data instance Array DW ix e = DWArray { dwArray :: !(Array D ix e)
                                      , dwStencilSize :: Maybe ix
                                        -- ^ Setting this value during stencil
                                        -- application improves cache utilization
                                        -- while computing an array
                                      , dwWindowStartIndex :: !ix
                                      , dwWindowSize :: !ix
                                      , dwStride :: !ix
                                      , dwWindowUnsafeIndex :: ix -> e }


instance {-# OVERLAPPING #-} (Show e, Ragged L ix e, Load DW ix e) =>
  Show (Array DW ix e) where
  show arr = showArray (showsTypeRep (typeRep (Proxy :: Proxy DW)) " ") (computeAs B arr)


instance Index ix => Construct DW ix e where
  getComp = dComp . dwArray
  {-# INLINE getComp #-}

  setComp c arr = arr { dwArray = (dwArray arr) { dComp = c } }
  {-# INLINE setComp #-}

  unsafeMakeArray c sz f =
    DWArray (unsafeMakeArray c sz f) Nothing zeroIndex zeroIndex (pureIndex 1) f
  {-# INLINE unsafeMakeArray #-}


-- | Any resize or extract on Windowed Array will loose all of the optimizations, thus hurt the
-- performance.
instance Index ix => Size DW ix e where
  size arr =
    liftIndex (+ 1) $ liftIndex2 div (liftIndex (subtract 1) (size (dwArray arr))) (dwStride arr)
  {-# INLINE size #-}
  unsafeResize _sz DWArray {..} = undefined -- TODO: drop strides and use the delayed
    -- let dArr = unsafeResize sz dwArray
    -- in DWArray
    --    { dwArray = dArr
    --    , dwStencilSize = Nothing
    --    , dwWindowStartIndex = zeroIndex
    --    , dwWindowSize = zeroIndex
    --    , dwStride = pureIndex 1
    --    , dwWindowUnsafeIndex = evaluateAt dArr
    --    }
  unsafeExtract sIx newSz = unsafeExtract sIx newSz . dwArray


instance Functor (Array DW ix) where
  fmap f !arr =
    arr
    { dwArray = fmap f (dwArray arr)
    , dwWindowUnsafeIndex = f . dwWindowUnsafeIndex arr
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
    { dwArray = delay arr
    , dwStencilSize = Nothing
    , dwWindowStartIndex = wIx
    , dwWindowSize = wSz
    , dwStride = pureIndex 1
    , dwWindowUnsafeIndex = wUnsafeIndex
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
    { dwArray =
        DArray {dComp = dComp dwArray, dSize = sz, dUnsafeIndex = dUnsafeIndex dwArray . toOldIndex}
    , dwStencilSize = dwStencilSize
    , dwWindowStartIndex = newWindowStartIndex
    , dwWindowSize =
        liftIndex2
          (-)
          (toNewIndex (liftIndex2 (+) dwWindowStartIndex dwWindowSize))
          newWindowStartIndex
    , dwStride = dwStride
    , dwWindowUnsafeIndex = dwWindowUnsafeIndex . toOldIndex
    }
  where
    !newWindowStartIndex = toNewIndex dwWindowStartIndex
{-# INLINE unsafeBackpermuteDW #-}


instance {-# OVERLAPPING #-} Load DW Ix1 e where
  loadS (DWArray (DArray _ sz indexB) _ it wk stride indexW) _ unsafeWrite = do
    iterM_ 0  it stride (<) $ \ !i -> unsafeWrite i (indexB i)
    iterM_ it wk stride (<) $ \ !i -> unsafeWrite i (indexW i)
    iterM_ wk sz stride (<) $ \ !i -> unsafeWrite i (indexB i)
  {-# INLINE loadS #-}
  loadP wIds (DWArray (DArray _ sz indexB) _ it wk stride indexW) _ unsafeWrite = do
      divideWork_ wIds wk $ \ !scheduler !chunkLength !totalLength !slackStart -> do
        scheduleWork scheduler $
          iterM_ 0 it stride (<) $ \ !ix ->
            unsafeWrite (toLinearIndex sz ix) (indexB ix)
        scheduleWork scheduler $
          iterM_ wk sz stride (<) $ \ !ix ->
            unsafeWrite (toLinearIndex sz ix) (indexB ix)
        loopM_ it (< (slackStart + it)) (+ chunkLength) $ \ !start ->
          scheduleWork scheduler $
          iterM_ start (start + chunkLength) (pureIndex 1) (<) $ \ !k ->
            unsafeWrite k $ indexW k
        scheduleWork scheduler $
          iterM_ (slackStart + it) (totalLength + it) stride (<) $ \ !k ->
            unsafeWrite k (indexW k)
  {-# INLINE loadP #-}



instance {-# OVERLAPPING #-} Load DW Ix2 e where
  loadS arr _ unsafeWrite = do
    let DWArray (DArray _ (m :. n) indexB) mStencilSz (it :. jt) (wm :. wn) stride indexW = arr
    let ib :. jb = (wm + it) :. (wn + jt)
        !blockHeight =
          case mStencilSz of
            Just (i :. _) -> min (max 1 i) 7
            _ -> 1
        is :. js = stride
        !strideSz = size arr
        strideStart ix = ix + (liftIndex2 mod (stride - liftIndex2 mod ix stride) stride)
        {-# INLINE strideStart #-}
        toLinearIndexStride ix = toLinearIndex strideSz (liftIndex2 div ix stride)
        {-# INLINE toLinearIndexStride #-}
        --writeB !ix = unsafeWrite (toLinearIndexStride ix) (indexB ix)
    -- iterM_ (0 :. 0) sz stride (<) $ \ !ix -> unsafeWrite (toLinearIndexStride ix) (indexB ix)
    iterM_ (0 :. 0) (it :. n) stride (<) $ \ !ix -> unsafeWrite (toLinearIndexStride ix) (indexB ix)
    iterM_ (strideStart (ib :. 0)) (m :. n) stride (<) $ \ !ix ->
      unsafeWrite (toLinearIndexStride ix) (indexB ix)
    iterM_ (strideStart (it :. 0)) (ib :. jt) stride (<) $ \ !ix ->
      unsafeWrite (toLinearIndexStride ix) (indexB ix)
    iterM_ (strideStart (it :. jb)) (ib :. n) stride (<) $ \ !ix ->
      unsafeWrite (toLinearIndexStride ix) (indexB ix)
    if is > 1 -- Turn off unrolling for vertical strides
      then iterM_ (strideStart (it :. jt)) (ib :. jb) stride (<) $ \ !ix ->
             unsafeWrite (toLinearIndexStride ix) (indexW ix)
      else
      -- unrollAndJam blockHeight (it :. ib) (jt :. jb) stride $ \ !ix ->
      --        unsafeWrite (toLinearIndexStride ix) (indexW ix)
      unrollAndJam' blockHeight (strideStart (it :. jt)) (ib :. jb) js $ \ !ix ->
             unsafeWrite (toLinearIndexStride ix) (indexW ix)
  {-# INLINE loadS #-}
  --
  loadP wIds arr _ unsafeWrite = do
    let DWArray (DArray _ sz@(m :. n) indexB) mStencilSz (it :. jt) (wm :. wn) stride indexW = arr
    withScheduler_ wIds $ \scheduler -> do
      let ib :. jb = (wm + it) :. (wn + jt)
          !blockHeight =
            case mStencilSz of
              Just (i :. _) -> i
              _ -> 1
          !(chunkHeight, slackHeight) = wm `quotRem` numWorkers scheduler
      let loadBlock !it' !ib' =
            unrollAndJam blockHeight (it' :. ib') (jt :. jb) stride $ \ !ix ->
              unsafeWrite (toLinearIndex sz ix) (indexW ix)
          {-# INLINE loadBlock #-}
      scheduleWork scheduler $
        iterM_ (0 :. 0) (it :. n) stride (<) $ \ !ix ->
          unsafeWrite (toLinearIndex sz ix) (indexB ix)
      scheduleWork scheduler $
        iterM_ (ib :. 0) (m :. n) stride (<) $ \ !ix ->
          unsafeWrite (toLinearIndex sz ix) (indexB ix)
      scheduleWork scheduler $
        iterM_ (it :. 0) (ib :. jt) stride (<) $ \ !ix ->
          unsafeWrite (toLinearIndex sz ix) (indexB ix)
      scheduleWork scheduler $
        iterM_ (it :. jb) (ib :. n) stride (<) $ \ !ix ->
          unsafeWrite (toLinearIndex sz ix) (indexB ix)
      loopM_ 0 (< numWorkers scheduler) (+ 1) $ \ !wid -> do
        let !it' = wid * chunkHeight + it
        scheduleWork scheduler $ loadBlock it' (it' + chunkHeight)
      when (slackHeight > 0) $ do
        let !itSlack = (numWorkers scheduler) * chunkHeight + it
        scheduleWork scheduler $ loadBlock itSlack (itSlack + slackHeight)
  {-# INLINE loadP #-}


instance {-# OVERLAPPABLE #-} (Index ix, Load DW (Lower ix) e) => Load DW ix e where
  loadS = loadWindowedSRec
  {-# INLINE loadS #-}
  loadP = loadWindowedPRec
  {-# INLINE loadP #-}


loadWindowedSRec :: (Index ix, Load DW (Lower ix) e, Monad m) =>
  Array DW ix e -> (Int -> m e) -> (Int -> e -> m ()) -> m ()
loadWindowedSRec (DWArray darr mStencilSz tix wSz stride indexW) _unsafeRead unsafeWrite = do
  let DArray _ sz indexB = darr
      !szL = tailDim sz
      !bix = liftIndex2 (+) tix wSz
      !(s, sIxL) = unconsDim stride
      !(t, tixL) = unconsDim tix
      !pageElements = totalElem szL
      unsafeWriteLower i k val = unsafeWrite (k + pageElements * i) val
      {-# INLINE unsafeWriteLower #-}
  iterM_ zeroIndex tix stride (<) $ \ !ix ->
    unsafeWrite (toLinearIndex sz ix) (indexB ix)
  iterM_ bix sz stride (<) $ \ !ix ->
    unsafeWrite (toLinearIndex sz ix) (indexB ix)
  loopM_ t (< headDim bix) (+ s) $ \ !i ->
    let !lowerArr =
          (DWArray
             (DArray Seq szL (indexB . consDim i))
             (tailDim <$> mStencilSz) -- can safely drop the dim, only
                                      -- last 2 matter anyways
             tixL
             (tailDim wSz)
             sIxL
             (indexW . consDim i))
    in loadS lowerArr _unsafeRead (unsafeWriteLower i)
{-# INLINE loadWindowedSRec #-}


loadWindowedPRec :: (Index ix, Load DW (Lower ix) e) =>
  [Int] -> Array DW ix e -> (Int -> IO e) -> (Int -> e -> IO ()) -> IO ()
loadWindowedPRec wIds (DWArray darr mStencilSz tix wSz stride indexW) _unsafeRead unsafeWrite = do
  withScheduler_ wIds $ \ scheduler -> do
    let DArray _ sz indexB = darr
        !szL = tailDim sz
        !bix = liftIndex2 (+) tix wSz
        !(s, sIxL) = unconsDim stride
        !(t, tixL) = unconsDim tix
        !pageElements = totalElem szL
        unsafeWriteLower i k = unsafeWrite (k + pageElements * i)
        {-# INLINE unsafeWriteLower #-}
    scheduleWork scheduler $
      iterM_ zeroIndex tix stride (<) $ \ !ix ->
        unsafeWrite (toLinearIndex sz ix) (indexB ix)
    scheduleWork scheduler $
      iterM_ bix sz stride (<) $ \ !ix ->
        unsafeWrite (toLinearIndex sz ix) (indexB ix)
    loopM_ t (< headDim bix) (+ s) $ \ !i ->
      let !lowerArr =
            (DWArray
               (DArray Seq szL (indexB . consDim i))
               (tailDim <$> mStencilSz) -- can safely drop the dim, only
                                        -- last 2 matter anyways
               tixL
               (tailDim wSz)
               sIxL
               (indexW . consDim i))
      in scheduleWork scheduler $
         loadS
           lowerArr
           (_unsafeRead)
           (unsafeWriteLower i)
{-# INLINE loadWindowedPRec #-}


unrollAndJam' :: Monad m =>
                 Int -- ^ Block height
              -> Ix2 -- ^ Top corner
              -> Ix2 -- ^ Bottom corner
              -> Int -- ^ Column Stride
              -> (Ix2 -> m a) -- ^ Writing function
              -> m ()
unrollAndJam' !bH (it :. jt) (ib :. jb) js f = do
  let f2 (i :. j) = f (i :. j) >> f  ((i + 1) :. j)
  let f3 (i :. j) = f (i :. j) >> f2 ((i + 1) :. j)
  let f4 (i :. j) = f (i :. j) >> f3 ((i + 1) :. j)
  let f5 (i :. j) = f (i :. j) >> f4 ((i + 1) :. j)
  let f6 (i :. j) = f (i :. j) >> f5 ((i + 1) :. j)
  let f7 (i :. j) = f (i :. j) >> f6 ((i + 1) :. j)
  let f' = case bH of
             1 -> f
             2 -> f2
             3 -> f3
             4 -> f4
             5 -> f5
             6 -> f6
             _ -> f7
  let !ibS = ib - ((ib - it) `mod` bH)
  loopM_ it (< ibS) (+ bH) $ \ !i ->
    loopM_ jt (< jb) (+ js) $ \ !j ->
      f' (i :. j)
  loopM_ ibS (< ib) (+ 1) $ \ !i ->
    loopM_ jt (< jb) (+ js) $ \ !j ->
      f (i :. j)
{-# INLINE unrollAndJam' #-}


unrollAndJam :: Monad m =>
                Int -> Ix2 -> Ix2 -> Ix2 -> (Ix2 -> m a) -> m ()
unrollAndJam !bH (it :. ib) (jt :. jb) (is :. js) f = do
  let !bH' = (min (max 1 bH) 7)
      !bH'' = bH' * is
  let f2 (i :. j) = f (i :. j) >> f  ((i + is) :. j)
  let f3 (i :. j) = f (i :. j) >> f2 ((i + is) :. j)
  let f4 (i :. j) = f (i :. j) >> f3 ((i + is) :. j)
  let f5 (i :. j) = f (i :. j) >> f4 ((i + is) :. j)
  let f6 (i :. j) = f (i :. j) >> f5 ((i + is) :. j)
  let f7 (i :. j) = f (i :. j) >> f6 ((i + is) :. j)
  let f' = case bH' of
             1 -> f
             2 -> f2
             3 -> f3
             4 -> f4
             5 -> f5
             6 -> f6
             _ -> f7
  let !ibS = ib - ((ib - it) `mod` bH'')
  loopM_ it (< ibS) (+ bH'') $ \ !i ->
    loopM_ jt (< jb) (+ js) $ \ !j ->
      f' (i :. j)
  loopM_ ibS (< ib) (+ is) $ \ !i ->
    loopM_ jt (< jb) (+ js) $ \ !j ->
      f (i :. j)
{-# INLINE unrollAndJam #-}


-- TODO: Implement Hilbert curve


instance {-# OVERLAPPING #-} Load DW Ix2T e where
  loadS arr _ unsafeWrite = do
    let (DWArray (DArray _ sz@(m, n) indexB) mStencilSz (it, jt) (wm, wn) stride indexW) =
          arr
    let (ib, jb) = (wm + it, wn + jt)
        blockHeight = case mStencilSz of
                        Just (i, _) -> i
                        _           -> 1
    iterM_ (0, 0) (it, n) stride (<) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (indexB ix)
    iterM_ (ib, 0) (m, n) stride (<) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (indexB ix)
    iterM_ (it, 0) (ib, jt) stride (<) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (indexB ix)
    iterM_ (it, jb) (ib, n) stride (<) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (indexB ix)
    unrollAndJam blockHeight (it :. ib) (jt :. jb) (toIx2 stride) $ \ !ix ->
      unsafeWrite (toLinearIndex sz (fromIx2 ix)) (indexW (fromIx2 ix))
  {-# INLINE loadS #-}
  loadP wIds arr _ unsafeWrite = do
    let (DWArray (DArray _ sz@(m, n) indexB) mStencilSz (it, jt) (wm, wn) stride indexW) = arr
    withScheduler_ wIds $ \ scheduler -> do
      let (ib, jb) = (wm + it, wn + jt)
          blockHeight = case mStencilSz of
                          Just (i, _) -> i
                          _           -> 1
          !(chunkHeight, slackHeight) = wm `quotRem` numWorkers scheduler
      let loadBlock !it' !ib' =
            unrollAndJam blockHeight (it' :. ib') (jt :. jb) (toIx2 stride) $ \ !ix ->
              unsafeWrite (toLinearIndex sz (fromIx2 ix)) (indexW (fromIx2 ix))
          {-# INLINE loadBlock #-}
      scheduleWork scheduler $
        iterM_ (0, 0) (it, n) stride (<) $ \ !ix ->
          unsafeWrite (toLinearIndex sz ix) (indexB ix)
      scheduleWork scheduler $
        iterM_ (ib, 0) (m, n) stride (<) $ \ !ix ->
          unsafeWrite (toLinearIndex sz ix) (indexB ix)
      scheduleWork scheduler $
        iterM_ (it, 0) (ib, jt) stride (<) $ \ !ix ->
          unsafeWrite (toLinearIndex sz ix) (indexB ix)
      scheduleWork scheduler $
        iterM_ (it, jb) (ib, n) stride (<) $ \ !ix ->
          unsafeWrite (toLinearIndex sz ix) (indexB ix)
      loopM_ 0 (< numWorkers scheduler) (+ 1) $ \ !wid -> do
        let !it' = wid * chunkHeight + it
        scheduleWork scheduler $ loadBlock it' (it' + chunkHeight)
      when (slackHeight > 0) $ do
        let !itSlack = (numWorkers scheduler) * chunkHeight + it
        scheduleWork scheduler $ loadBlock itSlack (itSlack + slackHeight)
  {-# INLINE loadP #-}



-- unrollAndJamT :: Monad m =>
--                 Int -> Ix2T -> Ix2T -> (Ix2T -> m a) -> m ()
-- unrollAndJamT !bH (it, ib) (jt, jb) f = do
--   let !bH' = min (max 1 bH) 7
--   let f2 !(i, j) = f (i, j) >> f  (i+1, j)
--   let f3 !(i, j) = f (i, j) >> f2 (i+1, j)
--   let f4 !(i, j) = f (i, j) >> f3 (i+1, j)
--   let f5 !(i, j) = f (i, j) >> f4 (i+1, j)
--   let f6 !(i, j) = f (i, j) >> f5 (i+1, j)
--   let f7 !(i, j) = f (i, j) >> f6 (i+1, j)
--   let f' = case bH' of
--              1 -> f
--              2 -> f2
--              3 -> f3
--              4 -> f4
--              5 -> f5
--              6 -> f6
--              _ -> f7
--   let !ibS = ib - ((ib - it) `mod` bH')
--   loopM_ it (< ibS) (+ bH') $ \ !i ->
--     loopM_ jt (< jb) (+ 1) $ \ !j ->
--       f' (i, j)
--   loopM_ ibS (< ib) (+ 1) $ \ !i ->
--     loopM_ jt (< jb) (+ 1) $ \ !j ->
--       f (i, j)
-- {-# INLINE unrollAndJamT #-}
