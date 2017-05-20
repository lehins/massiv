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
  -- ** From List Sequentially
  , fromListS1D
  , fromListAsS1D
  , fromListS2D
  , fromListAsS2D
  , fromListS3D
  , fromListAsS3D
  -- ** From List in Parallel
  , fromListP2D
  , fromListAsP2D
  , fromListP3D
  , fromListAsP3D
  -- ** To List Sequentially
  , toListS1D
  , toListS2D
  , toListS3D
  -- ** To List in Parallel
  , toListP2D
  -- , toListP2D'
  -- , toListP3D
  ) where

--import           Control.DeepSeq                (NFData)
import           Control.Monad                  (unless, void)
import           Control.Monad.ST               (runST)
import           Data.Array.Massiv.Common       (Array, DIM1, DIM2, DIM3,
                                                 Index (totalElem, unconsDim),
                                                 Lower,
                                                 Massiv (makeArray, size),
                                                 Source, loopM)
import           Data.Array.Massiv.Common.Ops   (foldrFB)
import           Data.Array.Massiv.Common.Shape (Shape (R), Slice, (!>))
import           Data.Array.Massiv.Delayed      (Array (DArray), D)
import           Data.Array.Massiv.Manifest     (B)
import           Data.Array.Massiv.Mutable      (Mutable (unsafeFreeze, unsafeLinearWrite, unsafeNew),
                                                 sequenceP)
import           Data.Array.Massiv.Scheduler
import           Data.Maybe                     (listToMaybe)
import           GHC.Base                       (build)
import           System.IO.Unsafe               (unsafePerformIO)

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


fromListAsS1D :: Mutable r DIM1 e => r -> [e] -> Array r DIM1 e
fromListAsS1D _ = fromListS1D
{-# INLINE fromListAsS1D #-}


loadListUsing2D :: Monad m =>
  Int -> Int -> Int -> (Int -> e -> m ()) -> (m () -> m ()) -> [[e]] -> m ()
loadListUsing2D start end step uWrite using xs = do
  leftOver <-
    loopM start (< end) (+ step) xs $
    (\ !i ys ->
       case ys of
            []      -> error "Column is too short."
            (x:xs') -> do
              using (loadList1D i (i + step) uWrite x)
              return xs'
    )
  unless (null leftOver) $ error "Column is too long."
{-# INLINE loadListUsing2D #-}


fromListS2D :: Mutable r DIM2 e => [[e]] -> Array r DIM2 e
fromListS2D xs =
  runST $ do
    let (m, n) = (length xs, maybe 0 length $ listToMaybe xs)
    mArr <- unsafeNew (m, n)
    loadListUsing2D 0 (m*n) n (unsafeLinearWrite mArr) id xs
    unsafeFreeze mArr
{-# INLINE fromListS2D #-}


fromListAsS2D :: Mutable r DIM2 e => r -> [[e]] -> Array r DIM2 e
fromListAsS2D _ = fromListS2D
{-# INLINE fromListAsS2D #-}


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
        loadListUsing2D i (i + step) n (unsafeLinearWrite mArr) id y >> return ys
    unsafeFreeze mArr
{-# INLINE fromListS3D #-}


fromListAsS3D :: Mutable r DIM3 e => r -> [[[e]]] -> Array r DIM3 e
fromListAsS3D _ = fromListS3D
{-# INLINE fromListAsS3D #-}



-- | Convert a list of lists into a 2 dimensional array in parallel.
fromListP2D :: Mutable r DIM2 e => [[e]] -> Array r DIM2 e
fromListP2D xs =
  unsafePerformIO $ do
    let (m, n) = (length xs, maybe 0 length $ listToMaybe xs)
    scheduler <- makeScheduler []
    mArr <- unsafeNew (m, n)
    loadListUsing2D 0 (m*n) n (unsafeLinearWrite mArr) (submitRequest scheduler . JobRequest) xs
    waitTillDone scheduler
    unsafeFreeze mArr
{-# INLINE fromListP2D #-}


fromListAsP2D :: Mutable r DIM2 e => r -> [[e]] -> Array r DIM2 e
fromListAsP2D _ = fromListP2D
{-# INLINE fromListAsP2D #-}


fromListP3D :: Mutable r DIM3 e => [[[e]]] -> Array r DIM3 e
fromListP3D xs =
  unsafePerformIO $ do
    let mFirstRow = listToMaybe xs
    let !sz@(l, m, n) =
          ( length xs
          , maybe 0 length mFirstRow
          , maybe 0 length (mFirstRow >>= listToMaybe))
    scheduler <- makeScheduler []
    mArr <- unsafeNew (l, m, n)
    let !step = m * n
    void $
      loopM 0 (< totalElem sz) (+ step) xs $ \i (y:ys) -> do
        loadListUsing2D
          i
          (i + step)
          n
          (unsafeLinearWrite mArr)
          (submitRequest scheduler . JobRequest)
          y
        return ys
    waitTillDone scheduler
    unsafeFreeze mArr
{-# INLINE fromListP3D #-}


fromListAsP3D :: Mutable r DIM3 e => r -> [[[e]]] -> Array r DIM3 e
fromListAsP3D _ = fromListP3D
{-# INLINE fromListAsP3D #-}



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


toListP2D
  :: forall r ix e . Slice r ix e => Array r ix e -> IO [[e]]
toListP2D !arr = do
  arrLs <- sequenceP $
    makeArray1D k (return . toListS1D . (arr !>))
  return $ toListS1D (arrLs :: Array B DIM1 [e])
  where
    !k = fst $ unconsDim $ size arr
{-# INLINE toListP2D #-}


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
