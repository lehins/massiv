{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
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
  , Window(..)
  , getStride
  , setStride
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
                                      , dwStride :: !ix
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
    DWArray (unsafeMakeArray c sz f) Nothing Nothing (pureIndex 1)
  {-# INLINE unsafeMakeArray #-}


-- | Any resize or extract on Windowed Array will loose all of the optimizations, thus hurt the
-- performance.
instance Index ix => Size DW ix e where
  --size arr = liftIndex2 div (size (dwArray arr)) (dwStride arr)
  size DWArray {dwArray, dwStride} = strideSize dwStride (size dwArray)
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
      , dwStride = pureIndex 1
      }
  where
    sz = size arr
{-# INLINE makeWindowedArray #-}


-- | Specify which elements should be computed and which ones are to be ignored. Eg. to keep all of
-- the elements with even indices set the stride to @`pureIndex` 2@, but if all you want is to drop
-- all rows divisible by 5, for instance, then you'd @`setStride` (5 :. 1)@.
--
-- @since 0.2.1
setStride ::
     Index ix
  => ix -- ^ Stride
  -> Array DW ix e
  -> Array DW ix e
setStride stride warr = warr { dwStride = liftIndex (max 1) stride }
{-# INLINE setStride #-}

-- | Get the stride from the Windowed array.
--
-- @since 0.2.1
getStride :: Array DW ix e -> ix
getStride = dwStride
{-# INLINE getStride #-}



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


strideStart :: Index ix => ix -> ix -> ix
strideStart stride ix =
  liftIndex2
    (+)
    ix
    (liftIndex2 mod (liftIndex2 subtract (liftIndex2 mod ix stride) stride) stride)
{-# INLINE strideStart #-}

strideSize :: Index ix => ix -> ix -> ix
strideSize stride sz = liftIndex (+ 1) $ liftIndex2 div (liftIndex (subtract 1) sz) stride
{-# INLINE strideSize #-}


loadWithIx1 ::
     (Monad m)
  => (m () -> m ())
  -> Array DW Ix1 e
  -> (Ix1 -> e -> m a)
  -> m ((Ix1, Ix1) -> m (), (Ix1, Ix1))
loadWithIx1 with (DWArray (DArray _ sz indexB) _ window stride) unsafeWrite = do
  let Window it wk indexW = fromMaybe zeroWindow window
      wEnd = it + wk
  with $ iterM_ 0 it stride (<) $ \ !i -> unsafeWrite (i `div` stride) (indexB i)
  with $
    iterM_ (strideStart stride wEnd) sz stride (<) $ \ !i -> unsafeWrite (i `div` stride) (indexB i)
  return
    ( \(from, to) ->
        with $ iterM_ (strideStart stride from) to stride (<) $ \ !i ->
          unsafeWrite (i `div` stride) (indexW i)
    , (it, wEnd))
{-# INLINE loadWithIx1 #-}


instance {-# OVERLAPPING #-} Load DW Ix1 e where
  loadS arr _ unsafeWrite = loadWithIx1 id arr unsafeWrite >>= uncurry ($)
  {-# INLINE loadS #-}
  loadP wIds arr _ unsafeWrite =
    withScheduler_ wIds $ \scheduler -> do
      (loadWindow, (wStart, wEnd)) <- loadWithIx1 (scheduleWork scheduler) arr unsafeWrite
      let !(chunkHeight, slackHeight) = (wEnd - wStart) `quotRem` numWorkers scheduler
      loopM_ 0 (< numWorkers scheduler) (+ 1) $ \ !wid ->
        let !it' = wid * chunkHeight + wStart
         in loadWindow (it', it' + chunkHeight)
      when (slackHeight > 0) $
        let !itSlack = numWorkers scheduler * chunkHeight + wStart
         in loadWindow (itSlack, itSlack + slackHeight)
  {-# INLINE loadP #-}


toLinearIndexStride :: Index ix => ix -> ix -> ix -> Int
toLinearIndexStride stride sz ix = toLinearIndex sz (liftIndex2 div ix stride)
{-# INLINE toLinearIndexStride #-}


loadWithIx2 ::
     (Monad m)
  => (m () -> m ())
  -> Array DW Ix2 t1
  -> (Int -> t1 -> m ())
  -> m (Ix2 -> m (), Ix2)
loadWithIx2 with arr unsafeWrite = do
  let DWArray (DArray _ (m :. n) indexB) mStencilSz window stride = arr
  let Window (it :. jt) (wm :. wn) indexW = fromMaybe zeroWindow window
  let ib :. jb = (wm + it) :. (wn + jt)
      !blockHeight =
        case mStencilSz of
          Just (i :. _) -> min (max 1 i) 7
          _             -> 1
      is :. js = stride
      !sz = size arr
      writeB !ix = unsafeWrite (toLinearIndexStride stride sz ix) (indexB ix)
      {-# INLINE writeB #-}
      writeW !ix = unsafeWrite (toLinearIndexStride stride sz ix) (indexW ix)
      {-# INLINE writeW #-}
  with $ iterM_ (0 :. 0) (it :. n) stride (<) writeB
  with $ iterM_ (strideStart stride (ib :. 0)) (m :. n) stride (<) writeB
  with $ iterM_ (strideStart stride (it :. 0)) (ib :. jt) stride (<) writeB
  with $ iterM_ (strideStart stride (it :. jb)) (ib :. n) stride (<) writeB
  f <-
    if is > 1 -- Turn off unrolling for vertical strides
      then return $ \(it' :. ib') ->
             with $ iterM_ (strideStart stride (it' :. jt)) (ib' :. jb) stride (<) writeW
      else return $ \(it' :. ib') ->
             with $ unrollAndJam blockHeight (strideStart stride (it' :. jt)) (ib' :. jb) js writeW
  return (f, it :. ib)
{-# INLINE loadWithIx2 #-}


instance {-# OVERLAPPING #-} Load DW Ix2 e where
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

instance {-# OVERLAPPABLE #-} (Index ix, Load DW (Lower ix) e) => Load DW ix e where
  loadS = loadWithIxN id
  {-# INLINE loadS #-}
  loadP wIds arr unsafeRead unsafeWrite =
    withScheduler_ wIds $ \scheduler ->
      loadWithIxN (scheduleWork scheduler) arr unsafeRead unsafeWrite
  {-# INLINE loadP #-}



loadWithIxN ::
     (Index ix, Monad m, Load DW (Lower ix) e)
  => (m () -> m ())
  -> Array DW ix e
  -> (Int -> m e)
  -> (Int -> e -> m ())
  -> m ()
loadWithIxN with arr unsafeRead unsafeWrite = do
  let DWArray darr mStencilSz window stride = arr
      DArray {dSize = sz, dIndex = indexBorder} = darr
      Window {windowStart, windowSize, windowIndex = indexWindow} = fromMaybe zeroWindow window
      !szL = tailDim sz
      !windowEnd = liftIndex2 (+) windowStart windowSize
      !(s, sIxL) = unconsDim stride
      !(t, windowStartL) = unconsDim windowStart
      !pageElements = totalElem $ strideSize sIxL szL
      -- can safely drop the dim, only last 2 matter anyways
      !stencilSizeLower = fmap tailDim mStencilSz
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
                , dwStride = sIxL
                }
         in with $
            loadS
              lowerArr
              (\k -> unsafeRead (k + pageElements * (i `div` s)))
              (\k -> unsafeWrite (k + pageElements * (i `div` s)))
      {-# INLINE loadLower #-}
  loopM_ 0 (< headDim windowStart) (+ s) loadLower
  loopM_ (strideStart s t) (< headDim windowEnd) (+ s) loadLower
  loopM_ (strideStart s (headDim windowEnd)) (< headDim sz) (+ s) loadLower
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
toIx2ArrayDW DWArray {dwArray, dwStencilSize, dwWindow, dwStride} =
  DWArray
    { dwArray = dwArray {dIndex = dIndex dwArray . fromIx2, dSize = toIx2 (dSize dwArray)}
    , dwStencilSize = fmap toIx2 dwStencilSize
    , dwWindow = fmap toIx2Window dwWindow
    , dwStride = toIx2 dwStride
    }
{-# INLINE toIx2ArrayDW #-}


-- toIxWindow :: (ix' -> ix) -> (ix -> ix') -> Window ix' e -> Window ix e
-- toIxWindow toIx fromIx Window {..} =
--   Window
--     { windowStart = toIx windowStart
--     , windowSize = toIx windowSize
--     , windowIndex = windowIndex . fromIx
--     }
-- {-# INLINE toIxWindow #-}

-- toIxArrayDW :: (ix' -> ix) -> (ix -> ix') -> Array DW ix' e -> Array DW ix e
-- toIxArrayDW toIx fromIx DWArray {dwArray, dwStencilSize, dwWindow, dwStride} =
--   DWArray
--     { dwArray = dwArray {dIndex = dIndex dwArray . fromIx, dSize = toIx (dSize dwArray)}
--     , dwStencilSize = fmap toIx dwStencilSize
--     , dwWindow = fmap (toIxWindow toIx fromIx) dwWindow
--     , dwStride = toIx dwStride
--     }
-- {-# INLINE toIxArrayDW #-}



instance {-# OVERLAPPING #-} Load DW Ix2T e where
  loadS arr = loadS (toIx2ArrayDW arr)
  {-# INLINE loadS #-}
  loadP wIds arr = loadP wIds (toIx2ArrayDW arr)
  {-# INLINE loadP #-}

-- instance {-# OVERLAPPING #-} Load DW Ix2T e where
--   loadS arr = loadS (toIxArrayDW toIx2 fromIx2 arr)
--   {-# INLINE loadS #-}
--   loadP wIds arr = loadP wIds (toIxArrayDW toIx2 fromIx2 arr)
--   {-# INLINE loadP #-}

-- instance {-# OVERLAPPING #-} Load DW Ix2T e where
--   loadS arr = loadS (toIxArrayDW toIx2 fromIx2 arr)
--   {-# INLINE loadS #-}
--   loadP wIds arr = loadP wIds (toIxArrayDW toIx2 fromIx2 arr)
--   {-# INLINE loadP #-}

-- instance {-# OVERLAPPING #-} Load DW Ix5T e where
--   loadS arr = loadS (toIxArrayDW toIx5 fromIx5 arr)
--   {-# INLINE loadS #-}
--   loadP wIds arr = loadP wIds (toIxArrayDW toIx5 fromIx5 arr)
--   {-# INLINE loadP #-}


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





-- ALternative separation of jamming the urolled
-- unroll :: Monad m => Int -> (Ix2 -> m b) -> Ix2 -> m b
-- unroll bH f =
--   let f2 (i :. j) = f (i :. j) >> f ((i + 1) :. j)
--       {-# INLINE f2 #-}
--       f3 (i :. j) = f (i :. j) >> f2 ((i + 1) :. j)
--       {-# INLINE f3 #-}
--       f4 (i :. j) = f (i :. j) >> f3 ((i + 1) :. j)
--       {-# INLINE f4 #-}
--       f5 (i :. j) = f (i :. j) >> f4 ((i + 1) :. j)
--       {-# INLINE f5 #-}
--       f6 (i :. j) = f (i :. j) >> f5 ((i + 1) :. j)
--       {-# INLINE f6 #-}
--       f7 (i :. j) = f (i :. j) >> f6 ((i + 1) :. j)
--       {-# INLINE f7 #-}
--    in case bH of
--         1 -> f
--         2 -> f2
--         3 -> f3
--         4 -> f4
--         5 -> f5
--         6 -> f6
--         _ -> f7
-- {-# INLINE unroll #-}

-- jam :: Monad m =>
--        Int -- ^ Block height
--     -> Ix2 -- ^ Top corner
--     -> Ix2 -- ^ Bottom corner
--     -> Int -- ^ Column Stride
--     -> (Ix2 -> m a) -- ^ Unrolled writing function
--     -> (Ix2 -> m a) -- ^ Writing function
--     -> m ()
-- jam !bH (it :. jt) (ib :. jb) js f' f = do
--   let !ibS = ib - ((ib - it) `mod` bH)
--   loopM_ it (< ibS) (+ bH) $ \ !i ->
--     loopM_ jt (< jb) (+ js) $ \ !j ->
--       f' (i :. j)
--   loopM_ ibS (< ib) (+ 1) $ \ !i ->
--     loopM_ jt (< jb) (+ js) $ \ !j ->
--       f (i :. j)
-- {-# INLINE jam #-}
