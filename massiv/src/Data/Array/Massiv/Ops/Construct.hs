{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
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
    makeArray
  , makeArrayR
  -- * Enumeration
  , range
  , rangeStep
  , enumFromN
  , enumFromStepN
  -- * Conversion
  -- ** From List
  , fromListIx1
  , fromListIx1As
  , fromListIx2
  , fromListIx2As
  , fromListIx3
  , fromListIx3As
  -- ** To List
  --
  -- Conversion to List is done sequentially regardless of the internal
  -- computation type, because constructing nested lists in parallel turns out
  -- to be slower then doing so sequentially.
  , toListIx1
  , toListIx2
  , toListIx3
  ) where

import           Control.Monad                  (unless)
import           Control.Monad.ST               (runST)
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Common.Shape (Shape (R), Slice, (!>))
import           Data.Array.Massiv.Delayed      (D(..))
import           Data.Array.Massiv.Ops.Fold     (foldrFB)
import           Prelude                        as P
import           Data.Array.Massiv.Mutable      (Mutable (unsafeFreeze, unsafeNew),
                                                 Mutable (unsafeLinearWrite))
import           Data.Array.Massiv.Scheduler
import           Data.Maybe                     (listToMaybe)
import           GHC.Base                       (build)
import           System.IO.Unsafe               (unsafePerformIO)



makeArray :: Construct r ix e => Comp -> ix -> (ix -> e) -> Array r ix e
makeArray !c = unsafeMakeArray c . liftIndex (max 0)
{-# INLINE makeArray #-}


makeArrayR :: Construct r ix e => r -> Comp -> ix -> (ix -> e) -> Array r ix e
makeArrayR _ = makeArray
{-# INLINE makeArrayR #-}


-- | Create a vector with a range of @Int@s incremented by 1.
-- @range k0 k1 == rangeStep k0 k1 1@
--
-- >>> toList $ range 1 6
-- [1,2,3,4,5]
-- >>> toList $ range (-2) 3
-- [-2,-1,0,1,2]
range :: Int -> Int -> Array D Ix1 Int
range !from !to = makeArray Seq (max 0 (to - from)) (+ from)
{-# INLINE range #-}



rangeStep :: Int -- ^ Start
          -> Int -- ^ Step (Can't be zero)
          -> Int -- ^ End
          -> Array D Ix1 Int
rangeStep !from !step !to
  | step == 0 = error "rangeStep: Step can't be zero"
  | otherwise =
    let (sz, r) = (to - from) `divMod` step
    in makeArray Seq (sz + signum r) (\i -> from + i * step)
{-# INLINE rangeStep #-}


-- |
--
-- >>> toList $ enumFromN 5 3
-- [5,6,7]
--
enumFromN :: Num e =>
             e -- ^ Start value
          -> Int -- ^ Length of resulting array
          -> Array D Ix1 e
enumFromN !from !sz = makeArray Seq sz $ \ i -> fromIntegral i + from
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
              -> Array D Ix1 e
enumFromStepN !from !step !sz = makeArray Seq sz $ \ i -> from + fromIntegral i * step
{-# INLINE enumFromStepN #-}


fromListIx1 :: Mutable r Ix1 e => Comp -> [e] -> Array r Ix1 e
fromListIx1 Seq  = fromListSIx1
fromListIx1 comp = setComp comp . fromListSIx1
{-# INLINE fromListIx1 #-}


-- | fromListIx1As U Par [1,2,3]
fromListIx1As :: Mutable r Ix1 e => r -> Comp -> [e] -> Array r Ix1 e
fromListIx1As _ = fromListIx1
{-# INLINE fromListIx1As #-}


fromListIx2 :: Mutable r Ix2 e => Comp -> [[e]] -> Array r Ix2 e
fromListIx2 Seq          = fromListSIx2
fromListIx2 (ParOn wIds) = fromListPIx2 wIds
{-# INLINE fromListIx2 #-}


fromListIx2As :: Mutable r Ix2 e => r -> Comp -> [[e]] -> Array r Ix2 e
fromListIx2As _ = fromListIx2
{-# INLINE fromListIx2As #-}



fromListIx3 :: Mutable r Ix3 e => Comp -> [[[e]]] -> Array r Ix3 e
fromListIx3 Seq          = fromListSIx3
fromListIx3 (ParOn wIds) = fromListPIx3 wIds
{-# INLINE fromListIx3 #-}


fromListIx3As :: Mutable r Ix3 e => r -> Comp -> [[[e]]] -> Array r Ix3 e
fromListIx3As _ = fromListIx3
{-# INLINE fromListIx3As #-}


loadListIx1 :: Monad m => Int -> Int -> (Int -> t -> m a) -> [t] -> m ()
loadListIx1 start end uWrite xs = do
  leftOver <- loopM start (< end) (+ 1) xs $ \ i xs' ->
    case xs' of
      (y:ys) -> uWrite i y >> return ys
      []     -> error $ "Row is too short"
  unless (null leftOver) $ error "Row is too long"
{-# INLINE loadListIx1 #-}


fromListSIx1 :: Mutable r Ix1 e => [e] -> Array r Ix1 e
fromListSIx1 xs =
  runST $ do
    let !k = length xs
    mArr <- unsafeNew k
    loadListIx1 0 k (unsafeLinearWrite mArr) xs
    unsafeFreeze Seq mArr
{-# INLINE fromListSIx1 #-}


loadListUsingIx2
  :: Monad m
  => Int -> Int -> Int -> (Int -> e -> m ()) -> (m () -> m ()) -> [[e]] -> m ()
loadListUsingIx2 start end step uWrite using xs = do
  leftOver <-
    loopM start (< end) (+ step) xs $ \ !i ys ->
      case ys of
        (x:xs') -> do
          using (loadListIx1 i (i + step) uWrite x)
          return xs'
        [] -> error "Column is too short."
  unless (P.null leftOver || P.all P.null leftOver) $ error "Column is too long."
{-# INLINE loadListUsingIx2 #-}


fromListSIx2 :: Mutable r Ix2 e => [[e]] -> Array r Ix2 e
fromListSIx2 xs =
  runST $ do
    let sz@(m :. n) = (length xs :. maybe 0 length (listToMaybe xs))
    mArr <- unsafeNew sz
    loadListUsingIx2 0 (m * n) n (unsafeLinearWrite mArr) id xs
    unsafeFreeze Seq mArr
{-# INLINE fromListSIx2 #-}



loadListUsingIx3 :: Monad m =>
  Int -> Int -> Int -> Int -> (Int -> e -> m ()) -> (m () -> m ()) -> [[[e]]] -> m ()
loadListUsingIx3 start end step lowerStep uWrite using xs = do
  leftOver <-
    loopM start (< end) (+ step) xs $ \ !i zs ->
      case zs of
        [] -> error "Page is too short"
        (y:ys) -> do
          loadListUsingIx2 i (i + step) lowerStep uWrite using y
          return ys
  unless
    (P.null leftOver ||
     (P.all (length (head leftOver) ==) (P.map length leftOver) &&
      P.null (concat (concat leftOver)))) $
    error "Page is too long."
{-# INLINE loadListUsingIx3 #-}


fromListSIx3 :: Mutable r Ix3 e => [[[e]]] -> Array r Ix3 e
fromListSIx3 xs =
  runST $ do
    let mFirstRow = listToMaybe xs
    let sz@(_ :> m :. n) =
          (length xs :> maybe 0 length mFirstRow :.
           maybe 0 length (mFirstRow >>= listToMaybe))
    mArr <- unsafeNew sz
    loadListUsingIx3 0 (totalElem sz) (m * n) n (unsafeLinearWrite mArr) id xs
    unsafeFreeze Seq mArr
{-# INLINE fromListSIx3 #-}


-- | Convert a list of lists into a 2 dimensional array in parallel, while
-- splitting loading of all rows among all cores/capabilites.
fromListPIx2 :: Mutable r Ix2 e => [Int] -> [[e]] -> Array r Ix2 e
fromListPIx2 wIds xs =
  unsafePerformIO $ do
    let sz@(m :. n) = (length xs :. maybe 0 length (listToMaybe xs))
    scheduler <- makeScheduler wIds
    mArr <- unsafeNew sz
    loadListUsingIx2
      0
      (m * n)
      n
      (unsafeLinearWrite mArr)
      (submitRequest scheduler . JobRequest)
      xs
    waitTillDone scheduler
    unsafeFreeze Par mArr
{-# INLINE fromListPIx2 #-}



fromListPIx3 :: Mutable r Ix3 e => [Int] -> [[[e]]] -> Array r Ix3 e
fromListPIx3 wIds xs =
  unsafePerformIO $ do
    let mFirstRow = listToMaybe xs
    let sz@(l :> m :. n) =
          (length xs :> maybe 0 length mFirstRow :.
           maybe 0 length (mFirstRow >>= listToMaybe))
    scheduler <- makeScheduler wIds
    mArr <- unsafeNew (l :> m :. n)
    loadListUsingIx3
      0
      (totalElem sz)
      (m * n)
      n
      (unsafeLinearWrite mArr)
      (submitRequest scheduler . JobRequest)
      xs
    waitTillDone scheduler
    unsafeFreeze Par mArr
{-# INLINE fromListPIx3 #-}


-- | Convert an array into a list.
--
-- >>> toListIx1 $ makeArrayIx2 (2, 3) id
-- [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
--
toListIx1 :: Source r ix e => Array r ix e -> [e]
toListIx1 !arr = build (\ c n -> foldrFB c n arr)
{-# INLINE toListIx1 #-}


-- | Convert an array with at least 2 dimensions into a list of lists. Inner
-- dimensions will get flattened into a list.
--
-- ==== __Examples__
--
-- >>> toListIx2 $ makeArrayIx2 Seq (2, 3) id
-- [[(0,0),(0,1),(0,2)],[(1,0),(1,1),(1,2)]]
-- >>> toListIx2 $ makeArrayIx3 Seq (2, 1, 3) id
-- [[(0,0,0),(0,0,1),(0,0,2)],[(1,0,0),(1,0,1),(1,0,2)]]
--
toListIx2 :: Slice r ix e => Array r ix e -> [[e]]
toListIx2 !arr = build $ \ c n -> foldrFB c n $ fmap toListIx1 $ makeArrayR D Seq k (arr !>)
  where !k = fst $ unconsDim $ size arr
{-# INLINE toListIx2 #-}


toListIx3 :: (Slice (R r) (Lower ix) e, Slice r ix e) => Array r ix e -> [[[e]]]
toListIx3 !arr = build $ \ c n -> foldrFB c n $ fmap toListIx2 $ makeArrayR D Seq k (arr !>)
  where !k = fst $ unconsDim $ size arr
{-# INLINE toListIx3 #-}


-- toListPIx2
--   :: forall r ix e . Slice r ix e => Array r ix e -> [[e]]
-- toListPIx2 !arr = unsafePerformIO $ do
--   arrLs <- sequenceP $
--     makeArrayIx1 k (return . toListSIx1 . (arr !>))
--   return $ toListSIx1 (arrLs :: Array B Ix1 [e])
--   where
--     !k = fst $ unconsDim $ size arr
-- {-# INLINE toListPIx2 #-}


-- toListPIx3
--   :: forall r ix e . (Slice (R r) (Lower ix) e, Slice r ix e) => Array r ix e -> [[[e]]]
-- toListPIx3 !arr = unsafePerformIO $ do
--   arrLs <- sequenceP $
--     makeArrayIx1 k (return . toListSIx2 . (arr !>))
--   return $ toListSIx1 (arrLs :: Array B Ix1 [[e]])
--   where
--     !k = fst $ unconsDim $ size arr
-- {-# INLINE toListPIx3 #-}


-- toListPIx2 :: (NFData e, Slice r ix e) => Array r ix e -> [[e]]
-- toListPIx2 !arr = unsafePerformIO $ foldrP (++) [] (:) [] $ fmap toListSIx1 $ makeArrayIx1 k (arr !>)
--   where !k = fst $ unconsDim $ size arr
-- {-# INLINE toListPIx2 #-}

-- toListPIx2' :: (NFData e, Slice r ix e) => Array r ix e -> IO [[e]]
-- toListPIx2' = foldrP' (:) []
-- {-# INLINE toListPIx2' #-}

-- toListPIx2' :: (NFData e, Index (Lower ix), Source r ix e) => Array r ix e -> [[e]]
-- toListPIx2' !arr =
--   concat $ unsafePerformIO $
--   foldrP (:) [] (:) [] $
--   fmap toListSIx1 $
--   makeArrayIx1 m (\ !i -> DArray szL (\ !ix -> unsafeIndex arr (consDim i ix)))
--   where
--     !(m, szL) = unconsDim (size arr)
-- {-# INLINE toListPIx2' #-}


-- toListPIx3 :: (NFData e, Slice (R r) (Lower ix) e, Slice r ix e) =>
--              Array r ix e -> [[[e]]]
-- toListPIx3 !arr = unsafePerformIO $ foldrP (++) [] (:) [] $ fmap toListSIx2 $ makeArrayIx1 k (arr !>)
--   where !k = fst $ unconsDim $ size arr
-- {-# INLINE toListPIx3 #-}
