{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Massiv.Array.Delayed.Windowed
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Delayed.Windowed
  ( DW(..)
  , Array(..)
  , Window(..)
  , insertWindow
  , getWindow
  , dropWindow
  , makeWindowedArray
  ) where

import Control.Exception (Exception(..))
import Control.Scheduler (trivialScheduler_)
import Control.Monad (when)
import Data.Massiv.Array.Delayed.Pull
import Data.Massiv.Array.Manifest.Boxed
import Data.Massiv.Array.Manifest.Internal
import Data.Massiv.Core
import Data.Massiv.Core.Common
import Data.Massiv.Core.Index.Internal (Sz(..))
import Data.Massiv.Core.List (L, showArrayList, showsArrayPrec)
import Data.Maybe (fromMaybe)
import GHC.TypeLits

-- | Delayed Windowed Array representation.
data DW = DW

type instance EltRepr DW ix = D

data Window ix e = Window { windowStart     :: !ix
                          -- ^ Index of where window will start at.
                          , windowSize      :: !(Sz ix)
                          -- ^ Size of the window
                          , windowIndex     :: ix -> e
                          -- ^ Indexing function for the window
                          , windowUnrollIx2 :: !(Maybe Int)
                          -- ^ Setting this value during stencil application improves cache
                          -- utilization by unrolling the loop for Ix2 and higher dimensions.
                          -- Has no affect on arrays with one dimension.
                          }

instance Functor (Window ix) where
  fmap f arr@Window{windowIndex} = arr { windowIndex = f . windowIndex }

data instance Array DW ix e = DWArray { dwArray :: !(Array D ix e)
                                      , dwWindow :: !(Maybe (Window ix e))
                                      }

instance (Ragged L ix e, Load DW ix e, Show e) => Show (Array DW ix e) where
  showsPrec = showsArrayPrec (computeAs B)
  showList = showArrayList


instance Index ix => Construct DW ix e where

  setComp c arr = arr { dwArray = (dwArray arr) { dComp = c } }
  {-# INLINE setComp #-}

  makeArray c sz f = DWArray (makeArray c sz f) Nothing
  {-# INLINE makeArray #-}


-- TODO: adjust in response to Window
-- instance Index ix => Extract DW ix e where
--   unsafeExtract sIx newSz = unsafeExtract sIx newSz . dwArray
--   {-# INLINE unsafeExtract #-}


instance Functor (Array DW ix) where
  fmap f arr@DWArray{dwArray, dwWindow} =
    arr
    { dwArray = fmap f dwArray
    , dwWindow = fmap f <$> dwWindow
    }
  {-# INLINE fmap #-}


--
--
-- @since 0.3.0
-- _makeWindowedArrayM
--   :: Source r ix e
--   => Array r ix e -- ^ Source array that will have a window inserted into it
--   -> ix -- ^ Start index for the window
--   -> Sz ix -- ^ Size of the window
--   -> (ix -> e) -- ^ Inside window indexing function
--   -> Array DW ix e
-- _makeWindowedArrayM !arr !windowStart !windowSize windowIndex
--   | not (isSafeIndex sz windowStart) =
--     error $
--     "makeWindowedArray: Incorrect window starting index: (" ++
--     show windowStart ++ ") for array size: (" ++ show (size arr) ++ ")"
--   | totalElem windowSize == 0 =
--     error $
--     "makeWindowedArray: Window can't hold any elements with this size: (" ++ show windowSize ++ ")"
--   | not
--      (isSafeIndex
--         (Sz (liftIndex (+ 1) (unSz sz)))
--         (liftIndex2 (+) windowStart (unSz windowSize))) =
--     error $
--     "makeWindowedArray: Incorrect window size: (" ++
--     show windowSize ++
--     ") and/or starting index: (" ++
--     show windowStart ++ ") for array size: (" ++ show (size arr) ++ ")"
--   | otherwise =
--     DWArray {dwArray = delay arr, dwWindow = Just $! Window {..}}
--   where
--     windowUnrollIx2 = Nothing
--     sz = size arr
-- {-# INLINE _makeWindowedArrayM #-}

-- | Construct a delayed windowed array by supply a separate element producing function for the
-- interior of an array. This is very usful for stencil mapping, where interior function does not
-- perform boundary checks, thus significantly speeding up computation process.
--
-- @since 0.1.3
makeWindowedArray
  :: Source r ix e
  => Array r ix e -- ^ Source array that will have a window inserted into it
  -> ix -- ^ Start index for the window
  -> Sz ix -- ^ Size of the window
  -> (ix -> e) -- ^ Indexing function foto use inside window
  -> Array DW ix e
makeWindowedArray !arr wStart wSize wIndex =
  insertWindow (delay arr) $
  Window {windowStart = wStart, windowSize = wSize, windowIndex = wIndex, windowUnrollIx2 = Nothing}
{-# INLINE makeWindowedArray #-}

-- | Inserts a `Window` into a delayed array while scaling the window down if it doesn't fit inside
-- that array.
--
-- @since 0.3.0
insertWindow
  :: Source D ix e
  => Array D ix e -- ^ Source array that will have a window inserted into it
  -> Window ix e -- ^ Window to place inside the delayed array
  -> Array DW ix e
insertWindow !arr !window =
  DWArray
    { dwArray = delay arr
    , dwWindow =
        Just $!
        Window
          { windowStart = unSz (Sz (liftIndex2 min wStart (liftIndex (subtract 1) sz)))
          , windowSize = Sz (liftIndex2 min wSize (liftIndex2 (-) sz wStart))
          , windowIndex = wIndex
          , windowUnrollIx2 = wUnrollIx2
          }
    }
  where
    Sz sz = size arr
    Window { windowStart = wStart
           , windowSize = Sz wSize
           , windowIndex = wIndex
           , windowUnrollIx2 = wUnrollIx2
           } = window
{-# INLINE insertWindow #-}


-- | Get the `Window` from a windowed array.
--
-- @since 0.2.1
getWindow :: Array DW ix e -> Maybe (Window ix e)
getWindow = dwWindow
{-# INLINE getWindow #-}

-- | Drop the `Window` from a windowed array.
--
-- @since 0.3.0
dropWindow :: Array DW ix e -> Array D ix e
dropWindow = dwArray
{-# INLINE dropWindow #-}


zeroWindow :: Index ix => Window ix e
zeroWindow = Window zeroIndex zeroSz windowError Nothing
{-# INLINE zeroWindow #-}

data EmptyWindowException = EmptyWindowException deriving (Eq, Show)

instance Exception EmptyWindowException where

  displayException _ = "Index of zero size Window"

windowError :: a
windowError = throwImpossible EmptyWindowException
{-# NOINLINE windowError #-}


loadWithIx1 ::
     (Monad m)
  => (m () -> m ())
  -> Array DW Ix1 e
  -> (Ix1 -> e -> m a)
  -> m (Ix1 -> Ix1 -> m (), Ix1, Ix1)
loadWithIx1 with (DWArray (DArray _ sz indexB) mWindow) uWrite = do
  let Window it wk indexW _ = fromMaybe zeroWindow mWindow
      wEnd = it + unSz wk
  with $ iterM_ 0 it 1 (<) $ \ !i -> uWrite i (indexB i)
  with $ iterM_ wEnd (unSz sz) 1 (<) $ \ !i -> uWrite i (indexB i)
  return (\from to -> with $ iterM_ from to 1 (<) $ \ !i -> uWrite i (indexW i), it, wEnd)
{-# INLINE loadWithIx1 #-}


instance Load DW Ix1 e where
  size = dSize . dwArray
  {-# INLINE size #-}
  getComp = dComp . dwArray
  {-# INLINE getComp #-}
  loadArrayM scheduler arr uWrite = do
    (loadWindow, wStart, wEnd) <- loadWithIx1 (scheduleWork scheduler) arr uWrite
    let (chunkWidth, slackWidth) = (wEnd - wStart) `quotRem` numWorkers scheduler
    loopM_ 0 (< numWorkers scheduler) (+ 1) $ \ !wid ->
      let !it' = wid * chunkWidth + wStart
       in loadWindow it' (it' + chunkWidth)
    when (slackWidth > 0) $
      let !itSlack = numWorkers scheduler * chunkWidth + wStart
       in loadWindow itSlack (itSlack + slackWidth)
  {-# INLINE loadArrayM #-}

instance StrideLoad DW Ix1 e where
  loadArrayWithStrideM scheduler stride sz arr uWrite = do
      (loadWindow, (wStart, wEnd)) <- loadArrayWithIx1 (scheduleWork scheduler) arr stride sz uWrite
      let (chunkWidth, slackWidth) = (wEnd - wStart) `quotRem` numWorkers scheduler
      loopM_ 0 (< numWorkers scheduler) (+ 1) $ \ !wid ->
        let !it' = wid * chunkWidth + wStart
         in loadWindow (it', it' + chunkWidth)
      when (slackWidth > 0) $
        let !itSlack = numWorkers scheduler * chunkWidth + wStart
         in loadWindow (itSlack, itSlack + slackWidth)
  {-# INLINE loadArrayWithStrideM #-}

loadArrayWithIx1 ::
     (Monad m)
  => (m () -> m ())
  -> Array DW Ix1 e
  -> Stride Ix1
  -> Sz1
  -> (Ix1 -> e -> m a)
  -> m ((Ix1, Ix1) -> m (), (Ix1, Ix1))
loadArrayWithIx1 with (DWArray (DArray _ arrSz indexB) mWindow) stride _ uWrite = do
  let Window it wk indexW _ = fromMaybe zeroWindow mWindow
      wEnd = it + unSz wk
      strideIx = unStride stride
  with $ iterM_ 0 it strideIx (<) $ \ !i -> uWrite (i `div` strideIx) (indexB i)
  with $
    iterM_ (strideStart stride wEnd) (unSz arrSz) strideIx (<) $ \ !i ->
      uWrite (i `div` strideIx) (indexB i)
  return
    ( \(from, to) ->
        with $
        iterM_ (strideStart stride from) to strideIx (<) $ \ !i ->
          uWrite (i `div` strideIx) (indexW i)
    , (it, wEnd))
{-# INLINE loadArrayWithIx1 #-}



loadWithIx2 ::
     Monad m
  => (m () -> m ())
  -> Array DW Ix2 t1
  -> (Int -> t1 -> m ())
  -> m (Ix2 -> m (), Ix2)
loadWithIx2 with arr uWrite = do
  let DWArray (DArray _ (Sz (m :. n)) indexB) window = arr
  let Window (it :. jt) (Sz (wm :. wn)) indexW mUnrollHeight = fromMaybe zeroWindow window
  let ib :. jb = (wm + it) :. (wn + jt)
      !blockHeight = maybe 1 (min 7 . max 1) mUnrollHeight
      stride = oneStride
      !sz = strideSize stride $ size arr
      writeB !ix = uWrite (toLinearIndex sz ix) (indexB ix)
      {-# INLINE writeB #-}
      writeW !ix = uWrite (toLinearIndex sz ix) (indexW ix)
      {-# INLINE writeW #-}
  with $ iterM_ (0 :. 0) (it :. n) (1 :. 1) (<) writeB
  with $ iterM_ (ib :. 0) (m :. n) (1 :. 1) (<) writeB
  with $ iterM_ (it :. 0) (ib :. jt) (1 :. 1) (<) writeB
  with $ iterM_ (it :. jb) (ib :. n) (1 :. 1) (<) writeB
  let f (it' :. ib') = with $ unrollAndJam blockHeight (it' :. jt) (ib' :. jb) 1 writeW
      {-# INLINE f #-}
  return (f, it :. ib)
{-# INLINE loadWithIx2 #-}

loadArrayWithIx2 ::
     Monad m
  => (m () -> m ())
  -> Array DW Ix2 e
  -> Stride Ix2
  -> Sz2
  -> (Int -> e -> m ())
  -> m (Ix2 -> m (), Ix2)
loadArrayWithIx2 with arr stride sz uWrite = do
  let DWArray (DArray _ (Sz (m :. n)) indexB) window = arr
  let Window (it :. jt) (Sz (wm :. wn)) indexW mUnrollHeight = fromMaybe zeroWindow window
  let ib :. jb = (wm + it) :. (wn + jt)
      !blockHeight = maybe 1 (min 7 . max 1) mUnrollHeight
      strideIx@(is :. js) = unStride stride
      writeB !ix = uWrite (toLinearIndexStride stride sz ix) (indexB ix)
      {-# INLINE writeB #-}
      writeW !ix = uWrite (toLinearIndexStride stride sz ix) (indexW ix)
      {-# INLINE writeW #-}
  with $ iterM_ (0 :. 0) (it :. n) strideIx (<) writeB
  with $ iterM_ (strideStart stride (ib :. 0)) (m :. n) strideIx (<) writeB
  with $ iterM_ (strideStart stride (it :. 0)) (ib :. jt) strideIx (<) writeB
  with $ iterM_ (strideStart stride (it :. jb)) (ib :. n) strideIx (<) writeB
  f <-
    if is > 1 || blockHeight <= 1 -- Turn off unrolling for vertical strides
      then return $ \(it' :. ib') ->
             iterM_ (strideStart stride (it' :. jt)) (ib' :. jb) strideIx (<) writeW
      else return $ \(it' :. ib') ->
             unrollAndJam blockHeight (strideStart stride (it' :. jt)) (ib' :. jb) js writeW
  return (f, it :. ib)
{-# INLINE loadArrayWithIx2 #-}


loadWindowIx2 :: Monad m => Int -> (Ix2 -> m ()) -> Ix2 -> m ()
loadWindowIx2 nWorkers loadWindow (it :. ib) = do
  let !(chunkHeight, slackHeight) = (ib - it) `quotRem` nWorkers
  loopM_ 0 (< nWorkers) (+ 1) $ \ !wid ->
    let !it' = wid * chunkHeight + it
     in loadWindow (it' :. (it' + chunkHeight))
  when (slackHeight > 0) $
    let !itSlack = nWorkers * chunkHeight + it
     in loadWindow (itSlack :. (itSlack + slackHeight))
{-# INLINE loadWindowIx2 #-}


instance Load DW Ix2 e where
  size = dSize . dwArray
  {-# INLINE size #-}
  getComp = dComp . dwArray
  {-# INLINE getComp #-}
  loadArrayM scheduler arr uWrite =
    loadWithIx2 (scheduleWork scheduler) arr uWrite >>=
    uncurry (loadWindowIx2 (numWorkers scheduler))
  {-# INLINE loadArrayM #-}

instance StrideLoad DW Ix2 e where
  loadArrayWithStrideM scheduler stride sz arr uWrite =
    loadArrayWithIx2 (scheduleWork scheduler) arr stride sz uWrite >>=
    uncurry (loadWindowIx2 (numWorkers scheduler))
  {-# INLINE loadArrayWithStrideM #-}


instance (Index (IxN n), Load DW (Ix (n - 1)) e) => Load DW (IxN n) e where
  size = dSize . dwArray
  {-# INLINE size #-}
  getComp = dComp . dwArray
  {-# INLINE getComp #-}
  loadArrayM scheduler = loadWithIxN (scheduleWork scheduler)
  {-# INLINE loadArrayM #-}

instance (Index (IxN n), StrideLoad DW (Ix (n - 1)) e) => StrideLoad DW (IxN n) e where
  loadArrayWithStrideM = loadArrayWithIxN
  {-# INLINE loadArrayWithStrideM #-}

loadArrayWithIxN ::
     (Index ix, Monad m, StrideLoad DW (Lower ix) e)
  => Scheduler m ()
  -> Stride ix
  -> Sz ix
  -> Array DW ix e
  -> (Int -> e -> m ())
  -> m ()
loadArrayWithIxN scheduler stride szResult arr uWrite = do
  let DWArray darr window = arr
      DArray {dSize = szSource, dIndex = indexBorder} = darr
      Window {windowStart, windowSize, windowIndex, windowUnrollIx2} = fromMaybe zeroWindow window
      !(headSourceSize, lowerSourceSize) = unconsSz szSource
      !lowerSize = snd $ unconsSz szResult
      !(s, lowerStrideIx) = unconsDim $ unStride stride
      !(curWindowStart, lowerWindowStart) = unconsDim windowStart
      !(headWindowSz, tailWindowSz) = unconsSz windowSize
      !curWindowEnd = curWindowStart + unSz headWindowSz
      !pageElements = totalElem lowerSize
      loadLower !i =
        let !lowerWindow =
              Window
                { windowStart = lowerWindowStart
                , windowSize = tailWindowSz
                , windowIndex = windowIndex . consDim i
                , windowUnrollIx2 = windowUnrollIx2
                }
            !lowerArr =
              DWArray
                { dwArray = DArray Seq lowerSourceSize (indexBorder . consDim i)
                , dwWindow = Just lowerWindow
                }
         in loadArrayWithStrideM
              scheduler
              (Stride lowerStrideIx)
              lowerSize
              lowerArr
              (\k -> uWrite (k + pageElements * (i `div` s)))
      {-# NOINLINE loadLower #-}
  loopM_ 0 (< headDim windowStart) (+ s) loadLower
  loopM_ (strideStart (Stride s) curWindowStart) (< curWindowEnd) (+ s) loadLower
  loopM_ (strideStart (Stride s) curWindowEnd) (< unSz headSourceSize) (+ s) loadLower
{-# INLINE loadArrayWithIxN #-}



loadWithIxN ::
     (Index ix, Monad m, Load DW (Lower ix) e)
  => (m () -> m ())
  -> Array DW ix e
  -> (Int -> e -> m ())
  -> m ()
loadWithIxN with arr uWrite = do
  let DWArray darr window = arr
      DArray {dSize = sz, dIndex = indexBorder} = darr
      Window {windowStart, windowSize, windowIndex, windowUnrollIx2} = fromMaybe zeroWindow window
      !(si, szL) = unconsSz sz
      !windowEnd = liftIndex2 (+) windowStart (unSz windowSize)
      !(t, windowStartL) = unconsDim windowStart
      !pageElements = totalElem szL
      loadLower !i =
        let !lowerWindow =
              Window
                { windowStart = windowStartL
                , windowSize = snd $ unconsSz windowSize
                , windowIndex = windowIndex . consDim i
                , windowUnrollIx2 = windowUnrollIx2
                }
            !lowerArr =
              DWArray
                { dwArray = DArray Seq szL (indexBorder . consDim i)
                , dwWindow = Just lowerWindow
                }
         in with $ loadArrayM trivialScheduler_ lowerArr (\k -> uWrite (k + pageElements * i))
      {-# NOINLINE loadLower #-}
  loopM_ 0 (< headDim windowStart) (+ 1) loadLower
  loopM_ t (< headDim windowEnd) (+ 1) loadLower
  loopM_ (headDim windowEnd) (< unSz si) (+ 1) loadLower
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
    , windowSize = SafeSz (toIx2 $ unSz windowSize)
    , windowIndex = windowIndex . fromIx2
    , windowUnrollIx2 = windowUnrollIx2
    }
{-# INLINE toIx2Window #-}

toIx2ArrayDW :: Array DW Ix2T e -> Array DW Ix2 e
toIx2ArrayDW DWArray {dwArray, dwWindow} =
  DWArray
    { dwArray =
        dwArray {dIndex = dIndex dwArray . fromIx2, dSize = SafeSz (toIx2 (unSz (dSize dwArray)))}
    , dwWindow = fmap toIx2Window dwWindow
    }
{-# INLINE toIx2ArrayDW #-}


instance Load DW Ix2T e where
  size = dSize . dwArray
  {-# INLINE size #-}
  getComp = dComp . dwArray
  {-# INLINE getComp #-}
  loadArrayM scheduler arr =
    loadArrayWithStrideM scheduler oneStride (size arr) arr
  {-# INLINE loadArrayM #-}

instance StrideLoad DW Ix2T e where
  loadArrayWithStrideM scheduler stride sz arr =
    loadArrayWithStrideM
      scheduler
      (Stride $ toIx2 $ unStride stride)
      (SafeSz (toIx2 (unSz sz)))
      (toIx2ArrayDW arr)
  {-# INLINE loadArrayWithStrideM #-}

instance Load DW Ix3T e where
  size = dSize . dwArray
  {-# INLINE size #-}
  getComp = dComp . dwArray
  {-# INLINE getComp #-}
  loadArrayM scheduler arr =
    loadArrayWithStrideM scheduler oneStride (size arr) arr
  {-# INLINE loadArrayM #-}

instance StrideLoad DW Ix3T e where
  loadArrayWithStrideM = loadArrayWithIxN
  {-# INLINE loadArrayWithStrideM #-}


instance Load DW Ix4T e where
  size = dSize . dwArray
  {-# INLINE size #-}
  getComp = dComp . dwArray
  {-# INLINE getComp #-}
  loadArrayM scheduler arr = loadArrayWithStrideM scheduler oneStride (size arr) arr
  {-# INLINE loadArrayM #-}

instance StrideLoad DW Ix4T e where
  loadArrayWithStrideM = loadArrayWithIxN
  {-# INLINE loadArrayWithStrideM #-}


instance Load DW Ix5T e where
  size = dSize . dwArray
  {-# INLINE size #-}
  getComp = dComp . dwArray
  {-# INLINE getComp #-}
  loadArrayM scheduler arr = loadArrayWithStrideM scheduler oneStride (size arr) arr
  {-# INLINE loadArrayM #-}
instance StrideLoad DW Ix5T e where
  loadArrayWithStrideM = loadArrayWithIxN
  {-# INLINE loadArrayWithStrideM #-}
