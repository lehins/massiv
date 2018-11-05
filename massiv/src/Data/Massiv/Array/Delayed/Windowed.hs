{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
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
  , Window(..)
  , getWindow
  , makeWindowedArray
  ) where

import           Control.Monad                       (when)
import           Data.Massiv.Array.Delayed.Internal
import           Data.Massiv.Array.Manifest.Boxed
import           Data.Massiv.Array.Manifest.Internal
import           Data.Massiv.Core
import           Data.Massiv.Core.Common
import           Data.Massiv.Core.List               (showArray)
import           Data.Massiv.Core.Scheduler
import           Data.Maybe                          (fromMaybe)
import           Data.Proxy                          (Proxy (..))
import           Data.Typeable                       (showsTypeRep, typeRep)
import           GHC.TypeLits

-- | Delayed Windowed Array representation.
data DW = DW

type instance EltRepr DW ix = D

data Window ix e = Window { windowStart :: !ix
                          -- ^ Index of where window will start at.
                          , windowSize  :: !ix
                          -- ^ Size of the window
                          , windowIndex :: ix -> e
                          -- ^ Indexing function for the window
                          }

instance Functor (Window ix) where
  fmap f arr@Window{windowIndex} = arr { windowIndex = f . windowIndex }

data instance Array DW ix e = DWArray { dwArray :: !(Array D ix e)
                                      , dwStencilSize :: !(Maybe ix)
                                        -- ^ Setting this value during stencil
                                        -- application improves cache utilization
                                        -- while computing an array
                                      , dwWindow :: !(Maybe (Window ix e))
                                      }

instance {-# OVERLAPPING #-} (Show e, Ragged L ix e, Load DW ix e) =>
  Show (Array DW ix e) where
  show arr = showArray (showsTypeRep (typeRep (Proxy :: Proxy DW)) " ") (computeAs B arr)


instance Index ix => Construct DW ix e where
  getComp = dComp . dwArray
  {-# INLINE getComp #-}

  setComp c arr = arr { dwArray = (dwArray arr) { dComp = c } }
  {-# INLINE setComp #-}

  unsafeMakeArray c sz f =
    DWArray (unsafeMakeArray c sz f) Nothing Nothing
  {-# INLINE unsafeMakeArray #-}


-- | Any resize or extract on Windowed Array will loose the interior window and all other
-- optimizations, thus hurting the performance a lot.
instance Index ix => Size DW ix e where
  size = dSize . dwArray
  {-# INLINE size #-}
  unsafeResize sz arr = arr { dwArray = unsafeResize sz (dwArray arr)
                            , dwWindow = Nothing
                            , dwStencilSize = Nothing }
  unsafeExtract sIx newSz = unsafeExtract sIx newSz . dwArray


instance Functor (Array DW ix) where
  fmap f arr@DWArray{dwArray, dwWindow} =
    arr
    { dwArray = fmap f dwArray
    , dwWindow = fmap f <$> dwWindow
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
makeWindowedArray !arr !windowStart !windowSize windowIndex
  | not (isSafeIndex sz windowStart) =
    error $
    "makeWindowedArray: Incorrect window starting index: (" ++
    show windowStart ++ ") for array size: (" ++ show (size arr) ++ ")"
  | totalElem windowSize == 0 =
    error $
    "makeWindowedArray: Window can't hold any elements with this size: (" ++ show windowSize ++ ")"
  | not (isSafeIndex (liftIndex (+ 1) sz) (liftIndex2 (+) windowStart windowSize)) =
    error $
    "makeWindowedArray: Incorrect window size: (" ++
    show windowSize ++
    ") and/or starting index: (" ++
    show windowStart ++ ") for array size: (" ++ show (size arr) ++ ")"
  | otherwise =
    DWArray
      { dwArray = delay arr
      , dwStencilSize = Nothing
      , dwWindow = Just $! Window {..}
      }
  where
    sz = size arr
{-# INLINE makeWindowedArray #-}


-- | Get the `Window` from the Windowed array.
--
-- @since 0.2.1
getWindow :: Array DW ix e -> Maybe (Window ix e)
getWindow = dwWindow
{-# INLINE getWindow #-}


zeroWindow :: Index ix => Window ix e
zeroWindow = Window zeroIndex zeroIndex windowError
{-# INLINE zeroWindow #-}

windowError :: a
windowError = error "Impossible: index of zeroWindow"
{-# NOINLINE windowError #-}


loadWithIx1 ::
     (Monad m)
  => (m () -> m ())
  -> Array DW Ix1 e
  -> (Ix1 -> e -> m a)
  -> m ((Ix1, Ix1) -> m (), (Ix1, Ix1))
loadWithIx1 with (DWArray (DArray _ sz indexB) _ window) unsafeWrite = do
  let Window it wk indexW = fromMaybe zeroWindow window
      wEnd = it + wk
  with $ iterM_ 0 it 1 (<) $ \ !i -> unsafeWrite i (indexB i)
  with $ iterM_ wEnd sz 1 (<) $ \ !i -> unsafeWrite i (indexB i)
  return (\(from, to) -> with $ iterM_ from to 1 (<) $ \ !i -> unsafeWrite i (indexW i), (it, wEnd))
{-# INLINE loadWithIx1 #-}


instance Load DW Ix1 e where
  loadS arr _ unsafeWrite = loadWithIx1 id arr unsafeWrite >>= uncurry ($)
  {-# INLINE loadS #-}
  loadP wIds arr _ unsafeWrite =
    withScheduler_ wIds $ \scheduler -> do
      (loadWindow, (wStart, wEnd)) <- loadWithIx1 (scheduleWork scheduler) arr unsafeWrite
      let (chunkHeight, slackHeight) = (wEnd - wStart) `quotRem` numWorkers scheduler
      loopM_ 0 (< numWorkers scheduler) (+ 1) $ \ !wid ->
        let !it' = wid * chunkHeight + wStart
         in loadWindow (it', it' + chunkHeight)
      when (slackHeight > 0) $
        let !itSlack = numWorkers scheduler * chunkHeight + wStart
         in loadWindow (itSlack, itSlack + slackHeight)
  {-# INLINE loadP #-}
  loadArray numWorkers' scheduleWork' arr =
    loadArrayWithStride numWorkers' scheduleWork' oneStride (size arr) arr
  {-# INLINE loadArray #-}
  loadArrayWithStride numWorkers' scheduleWork' stride sz arr _ unsafeWrite = do
      (loadWindow, (wStart, wEnd)) <- loadArrayWithIx1 scheduleWork' arr stride sz unsafeWrite
      let (chunkHeight, slackHeight) = (wEnd - wStart) `quotRem` numWorkers'
      loopM_ 0 (< numWorkers') (+ 1) $ \ !wid ->
        let !it' = wid * chunkHeight + wStart
         in loadWindow (it', it' + chunkHeight)
      when (slackHeight > 0) $
        let !itSlack = numWorkers' * chunkHeight + wStart
         in loadWindow (itSlack, itSlack + slackHeight)
  {-# INLINE loadArrayWithStride #-}

loadArrayWithIx1 ::
     (Monad m)
  => (m () -> m ())
  -> Array DW Ix1 e
  -> Stride Ix1
  -> Ix1
  -> (Ix1 -> e -> m a)
  -> m ((Ix1, Ix1) -> m (), (Ix1, Ix1))
loadArrayWithIx1 with (DWArray (DArray _ arrSz indexB) _ window) stride _ unsafeWrite = do
  let Window it wk indexW = fromMaybe zeroWindow window
      wEnd = it + wk
      strideIx = unStride stride
  with $ iterM_ 0 it strideIx (<) $ \ !i -> unsafeWrite (i `div` strideIx) (indexB i)
  with $
    iterM_ (strideStart stride wEnd) arrSz strideIx (<) $ \ !i ->
      unsafeWrite (i `div` strideIx) (indexB i)
  return
    ( \(from, to) ->
        with $
        iterM_ (strideStart stride from) to strideIx (<) $ \ !i ->
          unsafeWrite (i `div` strideIx) (indexW i)
    , (it, wEnd))
{-# INLINE loadArrayWithIx1 #-}



loadWithIx2 ::
     Monad m
  => (m () -> m ())
  -> Array DW Ix2 t1
  -> (Int -> t1 -> m ())
  -> m (Ix2 -> m (), Ix2)
loadWithIx2 with arr unsafeWrite = do
  let DWArray (DArray _ (m :. n) indexB) mStencilSize window = arr
  let Window (it :. jt) (wm :. wn) indexW = fromMaybe zeroWindow window
  let ib :. jb = (wm + it) :. (wn + jt)
      !blockHeight =
        case mStencilSize of
          Just (i :. _) -> min (max 1 i) 7
          _             -> 1
      stride = oneStride
      !sz = strideSize stride $ size arr
      writeB !ix = unsafeWrite (toLinearIndex sz ix) (indexB ix)
      {-# INLINE writeB #-}
      writeW !ix = unsafeWrite (toLinearIndex sz ix) (indexW ix)
      {-# INLINE writeW #-}
  with $ iterM_ (0 :. 0) (it :. n) (1 :. 1) (<) writeB
  with $ iterM_ (ib :. 0) (m :. n) (1 :. 1) (<) writeB
  with $ iterM_ (it :. 0) (ib :. jt) (1 :. 1) (<) writeB
  with $ iterM_ (it :. jb) (ib :. n) (1 :. 1) (<) writeB
  let f (it' :. ib') = with $ unrollAndJam blockHeight (it' :. jt) (ib' :. jb) 1 writeW
      {-# INLINE f #-}
  return (f, it :. ib)
{-# INLINE loadWithIx2 #-}


instance Load DW Ix2 e where
  loadS arr _ unsafeWrite = loadWithIx2 id arr unsafeWrite >>= uncurry ($)
  {-# INLINE loadS #-}
  --
  loadP wIds arr _ unsafeWrite =
    withScheduler_ wIds $ \scheduler -> do
      (loadWindow, it :. ib) <- loadWithIx2 (scheduleWork scheduler) arr unsafeWrite
      let !(chunkHeight, slackHeight) = (ib - it) `quotRem` numWorkers scheduler
      loopM_ 0 (< numWorkers scheduler) (+ 1) $ \ !wid ->
        let !it' = wid * chunkHeight + it
         in loadWindow (it' :. (it' + chunkHeight))
      when (slackHeight > 0) $
        let !itSlack = numWorkers scheduler * chunkHeight + it
         in loadWindow (itSlack :. (itSlack + slackHeight))
  {-# INLINE loadP #-}
  loadArray numWorkers' scheduleWork' arr =
    loadArrayWithStride numWorkers' scheduleWork' oneStride (size arr) arr
  {-# INLINE loadArray #-}
  loadArrayWithStride numWorkers' scheduleWork' stride sz arr _ unsafeWrite = do
    (loadWindow, it :. ib) <- loadArrayWithIx2 scheduleWork' arr stride sz unsafeWrite
    let !(chunkHeight, slackHeight) = (ib - it) `quotRem` numWorkers'
    loopM_ 0 (< numWorkers') (+ 1) $ \ !wid ->
      let !it' = wid * chunkHeight + it
       in loadWindow (it' :. (it' + chunkHeight))
    when (slackHeight > 0) $
      let !itSlack = numWorkers' * chunkHeight + it
       in loadWindow (itSlack :. (itSlack + slackHeight))
  {-# INLINE loadArrayWithStride #-}

loadArrayWithIx2 ::
     Monad m
  => (m () -> m ())
  -> Array DW Ix2 e
  -> Stride Ix2
  -> Ix2
  -> (Int -> e -> m ())
  -> m (Ix2 -> m (), Ix2)
loadArrayWithIx2 with arr stride sz unsafeWrite = do
  let DWArray (DArray _ (m :. n) indexB) mStencilSize window = arr
  let Window (it :. jt) (wm :. wn) indexW = fromMaybe zeroWindow window
  let ib :. jb = (wm + it) :. (wn + jt)
      !blockHeight =
        case mStencilSize of
          Just (i :. _) -> min (max 1 i) 7
          _             -> 1
      strideIx@(is :. js) = unStride stride
      writeB !ix = unsafeWrite (toLinearIndexStride stride sz ix) (indexB ix)
      {-# INLINE writeB #-}
      writeW !ix = unsafeWrite (toLinearIndexStride stride sz ix) (indexW ix)
      {-# INLINE writeW #-}
  with $ iterM_ (0 :. 0) (it :. n) strideIx (<) writeB
  with $ iterM_ (strideStart stride (ib :. 0)) (m :. n) strideIx (<) writeB
  with $ iterM_ (strideStart stride (it :. 0)) (ib :. jt) strideIx (<) writeB
  with $ iterM_ (strideStart stride (it :. jb)) (ib :. n) strideIx (<) writeB
  f <-
    if is > 1 -- Turn off unrolling for vertical strides
      then return $ \(it' :. ib') ->
             iterM_ (strideStart stride (it' :. jt)) (ib' :. jb) strideIx (<) writeW
      else return $ \(it' :. ib') ->
             unrollAndJam blockHeight (strideStart stride (it' :. jt)) (ib' :. jb) js writeW
  return (f, it :. ib)
{-# INLINE loadArrayWithIx2 #-}



instance (Index (IxN n), Load DW (Ix (n - 1)) e) => Load DW (IxN n) e where
  loadS = loadWithIxN id
  {-# INLINE loadS #-}
  loadP wIds arr unsafeRead unsafeWrite =
    withScheduler_ wIds $ \scheduler ->
      loadWithIxN (scheduleWork scheduler) arr unsafeRead unsafeWrite
  {-# INLINE loadP #-}
  loadArray numWorkers' scheduleWork' arr =
    loadArrayWithStride numWorkers' scheduleWork' oneStride (size arr) arr
  {-# INLINE loadArray #-}
  loadArrayWithStride = loadArrayWithIxN
  {-# INLINE loadArrayWithStride #-}

loadArrayWithIxN ::
     (Index ix, Monad m, Load DW (Lower ix) e)
  => Int
  -> (m () -> m ())
  -> Stride ix
  -> ix
  -> Array DW ix e
  -> (Int -> m e)
  -> (Int -> e -> m ())
  -> m ()
loadArrayWithIxN numWorkers' scheduleWork' stride szResult arr unsafeRead unsafeWrite = do
  let DWArray darr mStencilSize window  = arr
      DArray {dSize = szSource, dIndex = indexBorder} = darr
      Window {windowStart, windowSize, windowIndex = indexWindow} = fromMaybe zeroWindow window
      !(headSourceSize, lowerSourceSize) = unconsDim szSource
      !lowerSize = tailDim szResult
      !(s, lowerStrideIx) = unconsDim $ unStride stride
      !(curWindowStart, lowerWindowStart) = unconsDim windowStart
      !curWindowEnd = curWindowStart + headDim windowSize
      !pageElements = totalElem lowerSize
      -- can safely drop the dim, only last 2 matter anyways
      !mLowerStencilSize = fmap tailDim mStencilSize
      loadLower !i =
        let !lowerWindow =
              Window
                { windowStart = lowerWindowStart
                , windowSize = tailDim windowSize
                , windowIndex = indexWindow . consDim i
                }
            !lowerArr =
              DWArray
                { dwArray = DArray Seq lowerSourceSize (indexBorder . consDim i)
                , dwStencilSize = mLowerStencilSize
                , dwWindow = Just lowerWindow
                }
         in loadArrayWithStride
              numWorkers'
              scheduleWork'
              (Stride lowerStrideIx)
              lowerSize
              lowerArr
              (\k -> unsafeRead (k + pageElements * (i `div` s)))
              (\k -> unsafeWrite (k + pageElements * (i `div` s)))
      {-# NOINLINE loadLower #-}
  loopM_ 0 (< headDim windowStart) (+ s) loadLower
  loopM_ (strideStart (Stride s) curWindowStart) (< curWindowEnd) (+ s) loadLower
  loopM_ (strideStart (Stride s) curWindowEnd) (< headSourceSize) (+ s) loadLower
{-# INLINE loadArrayWithIxN #-}



loadWithIxN ::
     (Index ix, Monad m, Load DW (Lower ix) e)
  => (m () -> m ())
  -> Array DW ix e
  -> (Int -> m e)
  -> (Int -> e -> m ())
  -> m ()
loadWithIxN with arr unsafeRead unsafeWrite = do
  let DWArray darr mStencilSize window = arr
      DArray {dSize = sz, dIndex = indexBorder} = darr
      Window {windowStart, windowSize, windowIndex = indexWindow} = fromMaybe zeroWindow window
      !szL = tailDim sz
      !windowEnd = liftIndex2 (+) windowStart windowSize
      !(t, windowStartL) = unconsDim windowStart
      !pageElements = totalElem szL
      -- can safely drop the dim, only last 2 matter anyways
      !stencilSizeLower = fmap tailDim mStencilSize
      loadLower !i =
        let !lowerWindow =
              Window
                { windowStart = windowStartL
                , windowSize = tailDim windowSize
                , windowIndex = indexWindow . consDim i
                }
            !lowerArr =
              DWArray
                { dwArray = DArray Seq szL (indexBorder . consDim i)
                , dwStencilSize = stencilSizeLower
                , dwWindow = Just lowerWindow
                }
         in with $
            loadS
              lowerArr
              (\k -> unsafeRead (k + pageElements * i))
              (\k -> unsafeWrite (k + pageElements * i))
      {-# NOINLINE loadLower #-}
  loopM_ 0 (< headDim windowStart) (+ 1) loadLower
  loopM_ t (< headDim windowEnd) (+ 1) loadLower
  loopM_ (headDim windowEnd) (< headDim sz) (+ 1) loadLower
{-# INLINE loadWithIxN #-}



unrollAndJam :: Monad m =>
                 Int -- ^ Block height
              -> Ix2 -- ^ Top corner
              -> Ix2 -- ^ Bottom corner
              -> Int -- ^ Column Stride
              -> (Ix2 -> m ()) -- ^ Writing function
              -> m ()
unrollAndJam !bH (it :. jt) (ib :. jb) js f = do
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
{-# INLINE unrollAndJam #-}



-- TODO: Implement Hilbert curve

toIx2Window :: Window Ix2T e -> Window Ix2 e
toIx2Window Window {..} =
  Window
    { windowStart = toIx2 windowStart
    , windowSize = toIx2 windowSize
    , windowIndex = windowIndex . fromIx2
    }
{-# INLINE toIx2Window #-}

toIx2ArrayDW :: Array DW Ix2T e -> Array DW Ix2 e
toIx2ArrayDW DWArray {dwArray, dwStencilSize, dwWindow} =
  DWArray
    { dwArray = dwArray {dIndex = dIndex dwArray . fromIx2, dSize = toIx2 (dSize dwArray)}
    , dwStencilSize = fmap toIx2 dwStencilSize
    , dwWindow = fmap toIx2Window dwWindow
    }
{-# INLINE toIx2ArrayDW #-}


instance Load DW Ix2T e where
  loadS arr = loadS (toIx2ArrayDW arr)
  {-# INLINE loadS #-}
  loadP wIds arr = loadP wIds (toIx2ArrayDW arr)
  {-# INLINE loadP #-}
  loadArray numWorkers' scheduleWork' arr =
    loadArrayWithStride numWorkers' scheduleWork' oneStride (size arr) arr
  {-# INLINE loadArray #-}
  loadArrayWithStride numWorkers' scheduleWork' stride sz arr =
    loadArrayWithStride
      numWorkers'
      scheduleWork'
      (Stride $ toIx2 $ unStride stride)
      (toIx2 sz)
      (toIx2ArrayDW arr)
  {-# INLINE loadArrayWithStride #-}
