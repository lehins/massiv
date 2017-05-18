{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  , fromListS1D
  , fromListS1D'
  , fromListS2D
  , fromListS2D'
  , fromListS3D
  , fromListS3D'
  , toListS1D
  , toListS2D
  , toListS3D
  -- , toListP2D
  -- , toListP2D'
  -- , toListP3D
  ) where

--import           Control.DeepSeq                (NFData)
import           Control.Monad                  (unless, void)
import           Control.Monad.ST               (runST)
import           Data.Array.Massiv.Common       (Array, DIM1, DIM2, DIM3,
                                                 Index (consDim, totalElem, unconsDim),
                                                 Lower,
                                                 Massiv (makeArray, size),
                                                 Source (unsafeIndex, unsafeLinearIndex),
                                                 loopM)
import           Data.Array.Massiv.Common.Shape (Shape (R), Slice, (!>))
import           Data.Array.Massiv.Delayed      (Array (DArray), D)
import           Data.Array.Massiv.Mutable      (Mutable (unsafeFreeze, unsafeLinearWrite, unsafeNew))
import           Data.Array.Massiv.Ops.Fold     (foldrFB)
import           Data.Maybe                     (listToMaybe)
--import           System.IO.Unsafe               (unsafePerformIO)
import GHC.Base (build)

makeArray1D :: Int -> (Int -> e) -> Array D DIM1 e
makeArray1D = makeArray
{-# INLINE makeArray1D #-}


makeArray2D :: (Int, Int) -> ((Int, Int) -> e) -> Array D DIM2 e
makeArray2D = makeArray
{-# INLINE makeArray2D #-}

makeArray3D :: (Int, Int, Int) -> ((Int, Int, Int) -> e) -> Array D DIM3 e
makeArray3D = makeArray
{-# INLINE makeArray3D #-}




-- | Create a Vector with a range of @Int@s incremented by 1.
-- @range k0 k1 == rangeStep k0 k1 1@
--
-- >>> toList $ range 1 6
-- [1,2,3,4,5]
-- >>> toList $ range (-2) 3
-- [-2,-1,0,1,2]
range :: Int -> Int -> Array D DIM1 Int
range !k0 !k1 = makeArray1D (max 0 (k1 - k0)) (+ k0)
{-# INLINE range #-}


rangeStep :: Int -> Int -> Int -> Array D DIM1 Int
rangeStep !k0 !k1 !step = makeArray1D ((k1 - k0) `div` step) (\ !i -> k0 + i*step)
{-# INLINE rangeStep #-}


-- |
--
-- >>> toList $ enumFromN 5 3
-- [5,6,7]
enumFromN :: Num e => e -> Int -> Array D DIM1 e
enumFromN !s !k = DArray k $ \ !i -> fromIntegral i + s
{-# INLINE enumFromN #-}

-- |
--
-- >>> toList $ enumFromStepN 1 0.1 5
-- [1.0,1.1,1.2,1.3,1.4]
enumFromStepN :: Num e => e -> e -> Int -> Array D DIM1 e
enumFromStepN !s !step !k = DArray k $ \ !i -> fromIntegral i * step + s
{-# INLINE enumFromStepN #-}


loadList1D :: Monad m => Int -> Int -> (Int -> t -> m a) -> [t] -> m ()
loadList1D start end uWrite xs = do
  leftOver <- loopM start (< end) (+ 1) xs $ \ !i xs' ->
    case xs' of
      []     -> error $ "Row is too short"
      (y:ys) -> uWrite i y >> return ys
  unless (null leftOver) $ error "Row is too long"
{-# INLINE loadList1D #-}


fromListS1D :: Mutable r DIM1 e => [e] -> Array r DIM1 e
fromListS1D xs =
  runST $ do
    let !k = length xs
    mArr <- unsafeNew k
    loadList1D 0 k (unsafeLinearWrite mArr) xs
    unsafeFreeze mArr
{-# INLINE fromListS1D #-}


fromListS1D' :: Mutable r DIM1 e => r -> [e] -> Array r DIM1 e
fromListS1D' _ = fromListS1D
{-# INLINE fromListS1D' #-}


loadList2D :: Monad m => Int -> Int -> Int -> (Int -> t -> m a) -> [[t]] -> m ()
loadList2D start end step uWrite xs = do
  leftOver <-
    loopM start (< end) (+ step) xs $
    (\ !i ys ->
       case ys of
            []      -> error "Column is too short."
            (x:xs') -> loadList1D i (i + step) uWrite x >> return xs')
  unless (null leftOver) $ error "Column is too long."
{-# INLINE loadList2D #-}


fromListS2D :: Mutable r DIM2 e => [[e]] -> Array r DIM2 e
fromListS2D xs =
  runST $ do
    let (m, n) = (length xs, maybe 0 length $ listToMaybe xs)
    mArr <- unsafeNew (m, n)
    loadList2D 0 (m*n) n (unsafeLinearWrite mArr) xs
    unsafeFreeze mArr
{-# INLINE fromListS2D #-}


fromListS2D' :: Mutable r DIM2 e => r -> [[e]] -> Array r DIM2 e
fromListS2D' _ = fromListS2D
{-# INLINE fromListS2D' #-}


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
    void $
      loopM 0 (< totalElem sz) (+ step) xs $ \ !i (y:ys) ->
        loadList2D i (i + step) n (unsafeLinearWrite mArr) y >> return ys
    unsafeFreeze mArr
{-# INLINE fromListS3D #-}


fromListS3D' :: Mutable r DIM3 e => r -> [[[e]]] -> Array r DIM3 e
fromListS3D' _ = fromListS3D
{-# INLINE fromListS3D' #-}

-- toListS1D' :: Source r ix e => Array r ix e -> [e]
-- toListS1D' = foldrS (:) []
-- {-# INLINE toListS1D' #-}

-- | Convert an array into a list.
--
-- >>> toListS1D $ makeArray2D (2, 3) id
-- [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
--
toListS1D :: Source r ix e => Array r ix e -> [e]
toListS1D !arr = build (\ c n -> foldrFB c n arr)
{-# INLINE toListS1D #-}


-- | Convert an array with at least 2 dimensions into a list of lists. Inner
-- dimensions will get flattened into a list.
--
-- ==== __Examples__
--
-- >>> toListS2D $ makeArray2D (2, 3) id
-- [[(0,0),(0,1),(0,2)],[(1,0),(1,1),(1,2)]]
-- >>> toListS2D $ makeArray3D (2, 1, 3) id
-- [[(0,0,0),(0,0,1),(0,0,2)],[(1,0,0),(1,0,1),(1,0,2)]]
--
toListS2D :: Slice r ix e => Array r ix e -> [[e]]
toListS2D !arr = build $ \ c n -> foldrFB c n $ fmap toListS1D $ makeArray1D k (arr !>)
  where !k = fst $ unconsDim $ size arr
{-# INLINE toListS2D #-}


toListS3D :: (Slice (R r) (Lower ix) e, Slice r ix e) => Array r ix e -> [[[e]]]
toListS3D !arr = build $ \ c n -> foldrFB c n $ fmap toListS2D $ makeArray1D k (arr !>)
  where !k = fst $ unconsDim $ size arr
{-# INLINE toListS3D #-}


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
