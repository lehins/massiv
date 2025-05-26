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
-- Copyright   : (c) Alexey Kuleshevich 2018-2025
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
module Data.Massiv.Array.Delayed.Windowed (
  DW (..),
  Array (..),
  Window (..),
  insertWindow,
  getWindow,
  dropWindow,
  makeWindowedArray,
) where

import Control.Monad (when)
import Control.Scheduler (trivialScheduler_)
import Data.Massiv.Array.Delayed.Pull
import Data.Massiv.Array.Manifest.Boxed
import Data.Massiv.Array.Manifest.Internal
import Data.Massiv.Core
import Data.Massiv.Core.Common
import Data.Massiv.Core.List (showArrayList, showsArrayPrec)
import Data.Maybe (fromMaybe)
import GHC.Base (modInt)
import GHC.TypeLits

-- | Delayed Windowed Array representation.
data DW = DW

data Window ix e = Window
  { windowStart :: !ix
  -- ^ Index of where window will start at.
  , windowSize :: !(Sz ix)
  -- ^ Size of the window
  , windowIndex :: ix -> e
  -- ^ Indexing function for the window
  , windowUnrollIx2 :: !(Maybe Int)
  -- ^ Setting this value during stencil application improves cache
  -- utilization by unrolling the loop for Ix2 and higher dimensions.
  -- Has no affect on arrays with one dimension.
  }

instance Functor (Window ix) where
  fmap f arr@Window{windowIndex} = arr{windowIndex = f . windowIndex}

data instance Array DW ix e = DWArray
  { dwComp :: !Comp
  , dwSize :: !(Sz ix)
  , dwIndex :: ix -> e
  , dwWindow :: !(Maybe (Window ix e))
  }

instance (Ragged L ix e, Load DW ix e, Show e) => Show (Array DW ix e) where
  showsPrec = showsArrayPrec (computeAs B)
  showList = showArrayList

instance Strategy DW where
  setComp c arr = arr{dwComp = c}
  {-# INLINE setComp #-}
  getComp = dwComp
  {-# INLINE getComp #-}
  repr = DW

instance Functor (Array DW ix) where
  fmap f arr@DWArray{dwIndex, dwWindow} =
    arr
      { dwIndex = f . dwIndex
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
  :: (Index ix, Source r e)
  => Array r ix e
  -- ^ Source array that will have a window inserted into it
  -> ix
  -- ^ Start index for the window
  -> Sz ix
  -- ^ Size of the window
  -> (ix -> e)
  -- ^ Indexing function foto use inside window
  -> Array DW ix e
makeWindowedArray !arr wStart wSize wIndex =
  insertWindow (delay arr) $
    Window{windowStart = wStart, windowSize = wSize, windowIndex = wIndex, windowUnrollIx2 = Nothing}
{-# INLINE makeWindowedArray #-}

-- | Inserts a `Window` into a delayed array while scaling the window down if it doesn't fit inside
-- that array.
--
-- @since 0.3.0
insertWindow
  :: (Index ix)
  => Array D ix e
  -- ^ Source array that will have a window inserted into it
  -> Window ix e
  -- ^ Window to place inside the delayed array
  -> Array DW ix e
insertWindow !arr !window =
  DWArray
    { dwComp = getComp arr
    , dwSize = arrSize
    , dwIndex = unsafeIndex arr
    , dwWindow =
        Just $!
          Window
            { windowStart = wStart'
            , windowSize = Sz (liftIndex2 min wSize (liftIndex2 (-) sz wStart'))
            , windowIndex = wIndex
            , windowUnrollIx2 = wUnrollIx2
            }
    }
  where
    wStart' = unSz (Sz (liftIndex2 min wStart (liftIndex (subtract 1) sz)))
    arrSize@(Sz sz) = size arr
    Window
      { windowStart = wStart
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
dropWindow DWArray {..} =
  DArray
  { dComp = dwComp
  , dSize = dwSize
  , dPrefIndex = PrefIndex dwIndex
  }
{-# INLINE dropWindow #-}

zeroWindow :: Index ix => Window ix e
zeroWindow = Window zeroIndex zeroSz windowError Nothing
{-# INLINE zeroWindow #-}

data EmptyWindowException = EmptyWindowException deriving (Eq, Show)

instance Exception EmptyWindowException where
  displayException _ = "Index of zero size Window"

windowError :: ix -> a
windowError _ = throwImpossible EmptyWindowException
{-# NOINLINE windowError #-}

loadWithIx1
  :: (Monad m)
  => (m () -> m ())
  -> Array DW Ix1 e
  -> (Ix1 -> e -> m a)
  -> m (Ix1 -> Ix1 -> m (), Ix1, Ix1)
loadWithIx1 with (DWArray _ sz uIndex mWindow) uWrite = do
  let Window it wk indexW _ = fromMaybe zeroWindow mWindow
      wEnd = it + unSz wk
  with $ iterA_ 0 it 1 (<) $ \ !i -> uWrite i (uIndex i)
  with $ iterA_ wEnd (unSz sz) 1 (<) $ \ !i -> uWrite i (uIndex i)
  return (\from to -> with $ iterA_ from to 1 (<) $ \ !i -> uWrite i (indexW i), it, wEnd)
{-# INLINE loadWithIx1 #-}

instance Index ix => Shape DW ix where
  maxLinearSize = Just . linearSize
  {-# INLINE maxLinearSize #-}
  linearSize = SafeSz . totalElem . dwSize
  {-# INLINE linearSize #-}
  outerSize = dwSize
  {-# INLINE outerSize #-}

instance Load DW Ix1 e where
  makeArray c sz f = DWArray c sz f Nothing
  {-# INLINE makeArray #-}
  iterArrayLinearST_ scheduler arr uWrite = do
    (loadWindow, wStart, wEnd) <- loadWithIx1 (scheduleWork scheduler) arr uWrite
    let (chunkWidth, slackWidth) = (wEnd - wStart) `quotRem` numWorkers scheduler
    loopA_ 0 (< numWorkers scheduler) (+ 1) $ \ !wid ->
      let !it' = wid * chunkWidth + wStart
       in loadWindow it' (it' + chunkWidth)
    when (slackWidth > 0) $
      let !itSlack = numWorkers scheduler * chunkWidth + wStart
       in loadWindow itSlack (itSlack + slackWidth)
  {-# INLINE iterArrayLinearST_ #-}

instance StrideLoad DW Ix1 e where
  iterArrayLinearWithStrideST_ scheduler stride sz arr uWrite = do
    (loadWindow, (wStart, wEnd)) <- loadArrayWithIx1 (scheduleWork scheduler) arr stride sz uWrite
    let (chunkWidth, slackWidth) = (wEnd - wStart) `quotRem` numWorkers scheduler
    loopA_ 0 (< numWorkers scheduler) (+ 1) $ \ !wid ->
      let !it' = wid * chunkWidth + wStart
       in loadWindow (it', it' + chunkWidth)
    when (slackWidth > 0) $
      let !itSlack = numWorkers scheduler * chunkWidth + wStart
       in loadWindow (itSlack, itSlack + slackWidth)
  {-# INLINE iterArrayLinearWithStrideST_ #-}

loadArrayWithIx1
  :: (Monad m)
  => (m () -> m ())
  -> Array DW Ix1 e
  -> Stride Ix1
  -> Sz1
  -> (Ix1 -> e -> m a)
  -> m ((Ix1, Ix1) -> m (), (Ix1, Ix1))
loadArrayWithIx1 with (DWArray _ arrSz uIndex mWindow) stride _ uWrite = do
  let Window it wk indexW _ = fromMaybe zeroWindow mWindow
      wEnd = it + unSz wk
      strideIx = unStride stride
  with $ iterA_ 0 it strideIx (<) $ \ !i -> uWrite (i `div` strideIx) (uIndex i)
  with $
    iterA_ (strideStart stride wEnd) (unSz arrSz) strideIx (<) $ \ !i ->
      uWrite (i `div` strideIx) (uIndex i)
  return
    ( \(from, to) ->
        with $
          iterA_ (strideStart stride from) to strideIx (<) $ \ !i ->
            uWrite (i `div` strideIx) (indexW i)
    , (it, wEnd)
    )
{-# INLINE loadArrayWithIx1 #-}

loadWithIx2
  :: Monad m
  => (m () -> m ())
  -> Array DW Ix2 t1
  -> (Int -> t1 -> m ())
  -> m (Ix2 -> m (), Ix2)
loadWithIx2 with arr uWrite = do
  let DWArray _ (Sz (m :. n)) uIndex window = arr
      Window (it :. jt) (Sz (wm :. wn)) indexW mUnrollHeight = fromMaybe zeroWindow window
      ib :. jb = (wm + it) :. (wn + jt)
      !blockHeight = maybe 1 (min 7 . max 1) mUnrollHeight
      stride = oneStride
      !sz = strideSize stride $ outerSize arr
      writeB !ix = uWrite (toLinearIndex sz ix) (uIndex ix)
      {-# INLINE writeB #-}
      writeW !ix = uWrite (toLinearIndex sz ix) (indexW ix)
      {-# INLINE writeW #-}
  with $ iterA_ (0 :. 0) (it :. n) (1 :. 1) (<) writeB
  with $ iterA_ (ib :. 0) (m :. n) (1 :. 1) (<) writeB
  with $ iterA_ (it :. 0) (ib :. jt) (1 :. 1) (<) writeB
  with $ iterA_ (it :. jb) (ib :. n) (1 :. 1) (<) writeB
  let f (it' :. ib') = with $ unrollAndJam blockHeight (it' :. jt) (ib' :. jb) 1 writeW
      {-# INLINE f #-}
  return (f, it :. ib)
{-# INLINE loadWithIx2 #-}

loadArrayWithIx2
  :: Monad m
  => (m () -> m ())
  -> Array DW Ix2 e
  -> Stride Ix2
  -> Sz2
  -> (Int -> e -> m ())
  -> m (Ix2 -> m (), Ix2)
loadArrayWithIx2 with arr stride sz uWrite = do
  let DWArray _ (Sz (m :. n)) uIndex window = arr
      Window (it :. jt) (Sz (wm :. wn)) indexW mUnrollHeight = fromMaybe zeroWindow window
      ib :. jb = (wm + it) :. (wn + jt)
      !blockHeight = maybe 1 (min 7 . max 1) mUnrollHeight
      strideIx@(is :. js) = unStride stride
      writeB !ix = uWrite (toLinearIndexStride stride sz ix) (uIndex ix)
      {-# INLINE writeB #-}
      writeW !ix = uWrite (toLinearIndexStride stride sz ix) (indexW ix)
      {-# INLINE writeW #-}
  with $ iterA_ (0 :. 0) (it :. n) strideIx (<) writeB
  with $ iterA_ (strideStart stride (ib :. 0)) (m :. n) strideIx (<) writeB
  with $ iterA_ (strideStart stride (it :. 0)) (ib :. jt) strideIx (<) writeB
  with $ iterA_ (strideStart stride (it :. jb)) (ib :. n) strideIx (<) writeB
  let f (it' :. ib')
        | is > 1 || blockHeight <= 1 =
            -- Turn off unrolling for vertical strides
            iterA_ (strideStart stride (it' :. jt)) (ib' :. jb) strideIx (<) writeW
        | otherwise =
            unrollAndJam blockHeight (strideStart stride (it' :. jt)) (ib' :. jb) js writeW
      {-# INLINE f #-}
  return (with . f, it :. ib)
{-# INLINE loadArrayWithIx2 #-}

loadWindowIx2 :: Monad m => Int -> (Ix2 -> m ()) -> Ix2 -> m ()
loadWindowIx2 nWorkers loadWindow (it :. ib) = do
  let !(chunkHeight, slackHeight) = (ib - it) `quotRem` nWorkers
  loopA_ 0 (< nWorkers) (+ 1) $ \ !wid ->
    let !it' = wid * chunkHeight + it
     in loadWindow (it' :. (it' + chunkHeight))
  when (slackHeight > 0) $
    let !itSlack = nWorkers * chunkHeight + it
     in loadWindow (itSlack :. (itSlack + slackHeight))
{-# INLINE loadWindowIx2 #-}

instance Load DW Ix2 e where
  makeArray c sz f = DWArray c sz f Nothing
  {-# INLINE makeArray #-}
  iterArrayLinearST_ scheduler arr uWrite =
    loadWithIx2 (scheduleWork scheduler) arr uWrite
      >>= uncurry (loadWindowIx2 (numWorkers scheduler))
  {-# INLINE iterArrayLinearST_ #-}

instance StrideLoad DW Ix2 e where
  iterArrayLinearWithStrideST_ scheduler stride sz arr uWrite =
    loadArrayWithIx2 (scheduleWork scheduler) arr stride sz uWrite
      >>= uncurry (loadWindowIx2 (numWorkers scheduler))
  {-# INLINE iterArrayLinearWithStrideST_ #-}

instance (Index (IxN n), Load DW (Ix (n - 1)) e) => Load DW (IxN n) e where
  makeArray c sz f = DWArray c sz f Nothing
  {-# INLINE makeArray #-}
  iterArrayLinearST_ = loadWithIxN
  {-# INLINE iterArrayLinearST_ #-}

instance (Index (IxN n), StrideLoad DW (Ix (n - 1)) e) => StrideLoad DW (IxN n) e where
  iterArrayLinearWithStrideST_ = loadArrayWithIxN
  {-# INLINE iterArrayLinearWithStrideST_ #-}

loadArrayWithIxN
  :: (Index ix, StrideLoad DW (Lower ix) e)
  => Scheduler s ()
  -> Stride ix
  -> Sz ix
  -> Array DW ix e
  -> (Int -> e -> ST s ())
  -> ST s ()
loadArrayWithIxN scheduler stride szResult arr uWrite = do
  let DWArray _ sz uIndex window = arr
      Window{windowStart, windowSize, windowIndex, windowUnrollIx2} = fromMaybe zeroWindow window
      !(!headSourceSize, !lowerSourceSize) = unconsSz sz
      !lowerSize = snd $ unconsSz szResult
      !(!s, !lowerStrideIx) = unconsDim $ unStride stride
      !(!curWindowStart, lowerWindowStart) = unconsDim windowStart
      !(!headWindowSz, tailWindowSz) = unconsSz windowSize
      !curWindowEnd = curWindowStart + unSz headWindowSz
      !pageElements = totalElem lowerSize
      lowerWindow =
        Window
          { windowStart = lowerWindowStart
          , windowSize = tailWindowSz
          , windowIndex = \_ -> error "Window index uninitialized"
          , windowUnrollIx2 = windowUnrollIx2
          }
      mkLowerWindow !i =
        lowerWindow
          { windowIndex = windowIndex . consDim i
          }
      loadLower mkWindow !i =
        let !lowerArray =
               DWArray
                 { dwComp = Seq
                 , dwSize = lowerSourceSize
                 , dwIndex = uIndex . consDim i
                 , dwWindow = mkWindow i
                 }
            !innerScheduler =
              if numWorkers scheduler <= unSz (strideSize (Stride s) headSourceSize)
                then trivialScheduler_
                else scheduler
        in scheduleWork_ scheduler $
             iterArrayLinearWithStrideST_ innerScheduler (Stride lowerStrideIx) lowerSize lowerArray $ \k ->
               uWrite (k + pageElements * (i `div` s))
      {-# INLINE loadLower #-}
  loopA_ 0 (< headDim windowStart) (+ s) (loadLower (const Nothing))
  loopA_
    (strideStart (Stride s) curWindowStart)
    (< curWindowEnd)
    (+ s)
    (loadLower (Just . mkLowerWindow))
  loopA_ (strideStart (Stride s) curWindowEnd) (< unSz headSourceSize) (+ s) (loadLower (const Nothing))
{-# INLINE loadArrayWithIxN #-}

loadWithIxN
  :: (Index ix, Load DW (Lower ix) e)
  => Scheduler s ()
  -> Array DW ix e
  -> (Int -> e -> ST s ())
  -> ST s ()
loadWithIxN scheduler arr uWrite = do
  let DWArray _ sz uIndex window = arr
      Window{windowStart, windowSize, windowIndex, windowUnrollIx2} = fromMaybe zeroWindow window
      !(!si, !szL) = unconsSz sz
      !windowEnd = liftIndex2 (+) windowStart (unSz windowSize)
      !(!t, windowStartL) = unconsDim windowStart
      !pageElements = totalElem szL
      lowerWindow =
        Window
          { windowStart = windowStartL
          , windowSize = snd $ unconsSz windowSize
          , windowIndex = \_ -> error "Window index uninitialized"
          , windowUnrollIx2 = windowUnrollIx2
          }
      mkLowerWindow !i =
        lowerWindow
          { windowIndex = windowIndex . consDim i
          }
      loadLower mkWindow !i =
        let !lowerArray =
              DWArray
                { dwComp = Seq
                , dwSize = szL
                , dwIndex = uIndex . consDim i
                , dwWindow = mkWindow i
                }
            !innerScheduler =
              if numWorkers scheduler <= unSz si
                then trivialScheduler_
                else scheduler
         in scheduleWork_ scheduler $
              iterArrayLinearST_ innerScheduler lowerArray (\k -> uWrite (k + pageElements * i))
      {-# INLINE loadLower #-}
  loopA_ 0 (< headDim windowStart) (+ 1) (loadLower (const Nothing))
  loopA_ t (< headDim windowEnd) (+ 1) (loadLower (Just . mkLowerWindow))
  loopA_ (headDim windowEnd) (< unSz si) (+ 1) (loadLower (const Nothing))
{-# INLINE loadWithIxN #-}

unrollAndJam
  :: Monad m
  => Int
  -- ^ Block height
  -> Ix2
  -- ^ Top corner
  -> Ix2
  -- ^ Bottom corner
  -> Int
  -- ^ Column Stride
  -> (Ix2 -> m ())
  -- ^ Writing function
  -> m ()
unrollAndJam !bH (it :. jt) (ib :. jb) js f = do
  let f2 (i :. j) = f (i :. j) >> f ((i + 1) :. j)
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
  let !ibS = ib - ((ib - it) `modInt` bH)
  loopA_ it (< ibS) (+ bH) $ \ !i ->
    loopA_ jt (< jb) (+ js) $ \ !j ->
      f' (i :. j)
  loopA_ ibS (< ib) (+ 1) $ \ !i ->
    loopA_ jt (< jb) (+ js) $ \ !j ->
      f (i :. j)
{-# INLINE unrollAndJam #-}

-- TODO: Implement Hilbert curve
