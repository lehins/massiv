{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- |
-- Module      : Data.Array.Massiv.Ops.Construct
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Ops.Construct
  ( -- * Construction
    makeArray1D
  , makeArray2D
  , makeArray3D
  -- * Enumeration
  , range
  , rangeStep
  , enumFromN
  , enumFromStepN
  -- * Conversion
  -- ** From List
  , fromList1D
  , fromListAs1D
  , fromList2D
  , fromListAs2D
  , fromList3D
  , fromListAs3D
  -- ** To List
  --
  -- Conversion to List is done sequentially regardless of the internal
  -- computation type, because constructing nested lists in parallel turns out
  -- to be slower then doing so sequentially.
  , toList1D
  , toList2D
  , toList3D
  ) where

import           Control.Monad                  (unless)
import           Control.Monad.ST               (runST)
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Common.Shape (Shape (R), Slice, (!>))
import           Data.Array.Massiv.Delayed      (D)
import           Data.Array.Massiv.Ops.Fold     (foldrFB)
import           Prelude                        as P
--import           Data.Array.Massiv.Manifest     (B)
import           Data.Array.Massiv.Mutable      (Mutable (unsafeFreeze, unsafeNew),
                                                 Mutable (unsafeLinearWrite))
import           Data.Array.Massiv.Scheduler
import           Data.Maybe                     (listToMaybe)
import           GHC.Base                       (build)
import           System.IO.Unsafe               (unsafePerformIO)
--import Control.DeepSeq (NFData, deepseq)


makeArray1D :: Comp -> DIM1 -> (Int -> e) -> Array D DIM1 e
makeArray1D = makeArray
{-# INLINE makeArray1D #-}


makeArray2D :: Comp -> DIM2 -> ((Int, Int) -> e) -> Array D DIM2 e
makeArray2D = makeArray
{-# INLINE makeArray2D #-}

makeArray3D :: Comp -> DIM3 -> ((Int, Int, Int) -> e) -> Array D DIM3 e
makeArray3D = makeArray
{-# INLINE makeArray3D #-}




-- | Create a vector with a range of @Int@s incremented by 1.
-- @range k0 k1 == rangeStep k0 k1 1@
--
-- >>> toList $ range 1 6
-- [1,2,3,4,5]
-- >>> toList $ range (-2) 3
-- [-2,-1,0,1,2]
range :: Int -> Int -> Array D DIM1 Int
range !from !to = makeArray1D Seq (max 0 (to - from)) (+ from)
{-# INLINE range #-}



rangeStep :: Int -- ^ Start
          -> Int -- ^ Step (Can't be zero)
          -> Int -- ^ End
          -> Array D DIM1 Int
rangeStep !from !step !to
  | step == 0 = error "rangeStep: Step can't be zero"
  | otherwise =
    let (sz, r) = (to - from) `divMod` step
    in makeArray1D Seq (sz + signum r) (\i -> from + i * step)
{-# INLINE rangeStep #-}


-- |
--
-- >>> toList $ enumFromN 5 3
-- [5,6,7]
--
enumFromN :: Num e =>
             e -- ^ Start value
          -> Int -- ^ Length of resulting array
          -> Array D DIM1 e
enumFromN !from !sz = makeArray1D Seq sz $ \ i -> fromIntegral i + from
{-# INLINE enumFromN #-}

-- |
--
-- >>> toList $ enumFromStepN 1 0.1 5
-- [1.0,1.1,1.2,1.3,1.4]
--
enumFromStepN :: Num e =>
                 e -- ^ Start value
              -> e -- ^ Step value
              -> Int -- ^ Length of resulting array
              -> Array D DIM1 e
enumFromStepN !from !step !sz = makeArray1D Seq sz $ \ i -> from + fromIntegral i * step
{-# INLINE enumFromStepN #-}


fromList1D :: Mutable r DIM1 e => Comp -> [e] -> Array r DIM1 e
fromList1D Seq  = fromListS1D
fromList1D comp = setComp comp . fromListS1D
{-# INLINE fromList1D #-}


-- | fromListAs1D U Par [1,2,3]
fromListAs1D :: Mutable r DIM1 e => r -> Comp -> [e] -> Array r DIM1 e
fromListAs1D _ = fromList1D
{-# INLINE fromListAs1D #-}


fromList2D :: Mutable r DIM2 e => Comp -> [[e]] -> Array r DIM2 e
fromList2D Seq          = fromListS2D
fromList2D (ParOn wIds) = fromListP2D wIds
{-# INLINE fromList2D #-}


fromListAs2D :: Mutable r DIM2 e => r -> Comp -> [[e]] -> Array r DIM2 e
fromListAs2D _ = fromList2D
{-# INLINE fromListAs2D #-}



fromList3D :: Mutable r DIM3 e => Comp -> [[[e]]] -> Array r DIM3 e
fromList3D Seq          = fromListS3D
fromList3D (ParOn wIds) = fromListP3D wIds
{-# INLINE fromList3D #-}


fromListAs3D :: Mutable r DIM3 e => r -> Comp -> [[[e]]] -> Array r DIM3 e
fromListAs3D _ = fromList3D
{-# INLINE fromListAs3D #-}


loadList1D :: Monad m => Int -> Int -> (Int -> t -> m a) -> [t] -> m ()
loadList1D start end uWrite xs = do
  leftOver <- loopM start (< end) (+ 1) xs $ \ i xs' ->
    case xs' of
      (y:ys) -> uWrite i y >> return ys
      []     -> error $ "Row is too short"
  unless (null leftOver) $ error "Row is too long"
{-# INLINE loadList1D #-}


fromListS1D :: Mutable r DIM1 e => [e] -> Array r DIM1 e
fromListS1D xs =
  runST $ do
    let !k = length xs
    mArr <- unsafeNew k
    loadList1D 0 k (unsafeLinearWrite mArr) xs
    unsafeFreeze Seq mArr
{-# INLINE fromListS1D #-}


loadListUsing2D
  :: Monad m
  => Int -> Int -> Int -> (Int -> e -> m ()) -> (m () -> m ()) -> [[e]] -> m ()
loadListUsing2D start end step uWrite using xs = do
  leftOver <-
    loopM start (< end) (+ step) xs $ \ !i ys ->
      case ys of
        (x:xs') -> do
          using (loadList1D i (i + step) uWrite x)
          return xs'
        [] -> error "Column is too short."
  unless (P.null leftOver || P.all P.null leftOver) $ error "Column is too long."
{-# INLINE loadListUsing2D #-}


fromListS2D :: Mutable r DIM2 e => [[e]] -> Array r DIM2 e
fromListS2D xs =
  runST $ do
    let sz@(m, n) = (length xs, maybe 0 length $ listToMaybe xs)
    mArr <- unsafeNew sz
    loadListUsing2D 0 (m * n) n (unsafeLinearWrite mArr) id xs
    unsafeFreeze Seq mArr
{-# INLINE fromListS2D #-}



loadListUsing3D :: Monad m =>
  Int -> Int -> Int -> Int -> (Int -> e -> m ()) -> (m () -> m ()) -> [[[e]]] -> m ()
loadListUsing3D start end step lowerStep uWrite using xs = do
  leftOver <-
    loopM start (< end) (+ step) xs $ \ !i zs ->
      case zs of
        [] -> error "Page is too short"
        (y:ys) -> do
          loadListUsing2D i (i + step) lowerStep uWrite using y
          return ys
  unless
    (P.null leftOver ||
     (P.all (length (head leftOver) ==) (P.map length leftOver) &&
      P.null (concat (concat leftOver)))) $
    error "Page is too long."
{-# INLINE loadListUsing3D #-}


fromListS3D :: Mutable r DIM3 e => [[[e]]] -> Array r DIM3 e
fromListS3D xs =
  runST $ do
    let mFirstRow = listToMaybe xs
    let !sz@(l, m, n) =
          ( length xs
          , maybe 0 length mFirstRow
          , maybe 0 length (mFirstRow >>= listToMaybe))
    mArr <- unsafeNew (l, m, n)
    let !step = m * n
    loadListUsing3D 0 (totalElem sz) step n (unsafeLinearWrite mArr) id xs
    unsafeFreeze Seq mArr
{-# INLINE fromListS3D #-}


-- | Convert a list of lists into a 2 dimensional array in parallel, while
-- splitting loading of all rows among all cores/capabilites.
fromListP2D :: Mutable r DIM2 e => [Int] -> [[e]] -> Array r DIM2 e
fromListP2D wIds xs =
  unsafePerformIO $ do
    let sz@(m, n) = (length xs, maybe 0 length $ listToMaybe xs)
    scheduler <- makeScheduler wIds
    mArr <- unsafeNew sz
    loadListUsing2D
      0
      (m * n)
      n
      (unsafeLinearWrite mArr)
      (submitRequest scheduler . JobRequest)
      xs
    waitTillDone scheduler
    unsafeFreeze Par mArr
{-# INLINE fromListP2D #-}



fromListP3D :: Mutable r DIM3 e => [Int] -> [[[e]]] -> Array r DIM3 e
fromListP3D wIds xs =
  unsafePerformIO $ do
    let mFirstRow = listToMaybe xs
    let !sz@(l, m, n) =
          ( length xs
          , maybe 0 length mFirstRow
          , maybe 0 length (mFirstRow >>= listToMaybe))
    scheduler <- makeScheduler wIds
    mArr <- unsafeNew (l, m, n)
    let !step = m * n
    loadListUsing3D
      0
      (totalElem sz)
      step
      n
      (unsafeLinearWrite mArr)
      (submitRequest scheduler . JobRequest)
      xs
    waitTillDone scheduler
    unsafeFreeze Par mArr
{-# INLINE fromListP3D #-}


-- | Convert an array into a list.
--
-- >>> toList1D $ makeArray2D (2, 3) id
-- [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
--
toList1D :: Source r ix e => Array r ix e -> [e]
toList1D !arr = build (\ c n -> foldrFB c n arr)
{-# INLINE toList1D #-}


-- | Convert an array with at least 2 dimensions into a list of lists. Inner
-- dimensions will get flattened into a list.
--
-- ==== __Examples__
--
-- >>> toList2D $ makeArray2D Seq (2, 3) id
-- [[(0,0),(0,1),(0,2)],[(1,0),(1,1),(1,2)]]
-- >>> toList2D $ makeArray3D Seq (2, 1, 3) id
-- [[(0,0,0),(0,0,1),(0,0,2)],[(1,0,0),(1,0,1),(1,0,2)]]
--
toList2D :: Slice r ix e => Array r ix e -> [[e]]
toList2D !arr = build $ \ c n -> foldrFB c n $ fmap toList1D $ makeArray1D Seq k (arr !>)
  where !k = fst $ unconsDim $ size arr
{-# INLINE toList2D #-}


toList3D :: (Slice (R r) (Lower ix) e, Slice r ix e) => Array r ix e -> [[[e]]]
toList3D !arr = build $ \ c n -> foldrFB c n $ fmap toList2D $ makeArray1D Seq k (arr !>)
  where !k = fst $ unconsDim $ size arr
{-# INLINE toList3D #-}


-- toListP2D
--   :: forall r ix e . Slice r ix e => Array r ix e -> [[e]]
-- toListP2D !arr = unsafePerformIO $ do
--   arrLs <- sequenceP $
--     makeArray1D k (return . toListS1D . (arr !>))
--   return $ toListS1D (arrLs :: Array B DIM1 [e])
--   where
--     !k = fst $ unconsDim $ size arr
-- {-# INLINE toListP2D #-}


-- toListP3D
--   :: forall r ix e . (Slice (R r) (Lower ix) e, Slice r ix e) => Array r ix e -> [[[e]]]
-- toListP3D !arr = unsafePerformIO $ do
--   arrLs <- sequenceP $
--     makeArray1D k (return . toListS2D . (arr !>))
--   return $ toListS1D (arrLs :: Array B DIM1 [[e]])
--   where
--     !k = fst $ unconsDim $ size arr
-- {-# INLINE toListP3D #-}


-- toListP2D :: (NFData e, Slice r ix e) => Array r ix e -> [[e]]
-- toListP2D !arr = unsafePerformIO $ foldrP (++) [] (:) [] $ fmap toListS1D $ makeArray1D k (arr !>)
--   where !k = fst $ unconsDim $ size arr
-- {-# INLINE toListP2D #-}

-- toListP2D' :: (NFData e, Slice r ix e) => Array r ix e -> IO [[e]]
-- toListP2D' = foldrP' (:) []
-- {-# INLINE toListP2D' #-}

-- toListP2D' :: (NFData e, Index (Lower ix), Source r ix e) => Array r ix e -> [[e]]
-- toListP2D' !arr =
--   concat $ unsafePerformIO $
--   foldrP (:) [] (:) [] $
--   fmap toListS1D $
--   makeArray1D m (\ !i -> DArray szL (\ !ix -> unsafeIndex arr (consDim i ix)))
--   where
--     !(m, szL) = unconsDim (size arr)
-- {-# INLINE toListP2D' #-}


-- toListP3D :: (NFData e, Slice (R r) (Lower ix) e, Slice r ix e) =>
--              Array r ix e -> [[[e]]]
-- toListP3D !arr = unsafePerformIO $ foldrP (++) [] (:) [] $ fmap toListS2D $ makeArray1D k (arr !>)
--   where !k = fst $ unconsDim $ size arr
-- {-# INLINE toListP3D #-}
