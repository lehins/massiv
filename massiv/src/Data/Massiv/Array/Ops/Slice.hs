{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Massiv.Array.Ops.Slice
-- Copyright   : (c) Alexey Kuleshevich 2018-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Ops.Slice
  (
  -- ** From the outside
    (!>)
  , (!?>)
  , (??>)
  -- ** From the inside
  , (<!)
  , (<!?)
  , (<??)
  -- ** From within
  , (<!>)
  , (<!?>)
  , (<??>)
  -- ** Many slices
  , outerSlices
  , innerSlices
  , withinSlices
  , withinSlicesM
  ) where

import Control.Monad (unless)
import Data.Massiv.Array.Delayed.Pull
import Data.Massiv.Core.Common


infixl 4 !>, !?>, ??>, <!, <!?, <??, <!>, <!?>, <??>


-- | /O(1)/ - Slices the array from the outside. For 2-dimensional array this will
-- be equivalent of taking a row. Throws an error when index is out of bounds.
--
-- ===__Examples__
--
-- You could say that slicing from outside is synonymous to slicing from the end or slicing at the
-- highermost dimension. For example with rank-3 arrays outer slice would be equivalent to getting a
-- page:
--
-- >>> import Data.Massiv.Array
-- >>> arr = makeArrayR U Seq (Sz (3 :> 2 :. 4)) fromIx3
-- >>> arr
-- Array U Seq (Sz (3 :> 2 :. 4))
--   [ [ [ (0,0,0), (0,0,1), (0,0,2), (0,0,3) ]
--     , [ (0,1,0), (0,1,1), (0,1,2), (0,1,3) ]
--     ]
--   , [ [ (1,0,0), (1,0,1), (1,0,2), (1,0,3) ]
--     , [ (1,1,0), (1,1,1), (1,1,2), (1,1,3) ]
--     ]
--   , [ [ (2,0,0), (2,0,1), (2,0,2), (2,0,3) ]
--     , [ (2,1,0), (2,1,1), (2,1,2), (2,1,3) ]
--     ]
--   ]
-- >>> arr !> 2
-- Array M Seq (Sz (2 :. 4))
--   [ [ (2,0,0), (2,0,1), (2,0,2), (2,0,3) ]
--   , [ (2,1,0), (2,1,1), (2,1,2), (2,1,3) ]
--   ]
--
-- There is nothing wrong with chaining, mixing and matching slicing operators, or even using them
-- to index arrays:
--
-- >>> arr !> 2 !> 0 !> 3
-- (2,0,3)
-- >>> arr !> 2 <! 3 ! 0
-- (2,0,3)
-- >>> (arr !> 2 !> 0 !> 3) == (arr ! 2 :> 0 :. 3)
-- True
--
--
-- @since 0.1.0
(!>) :: OuterSlice r ix e => Array r ix e -> Int -> Elt r ix e
(!>) !arr !ix = either throw id (arr !?> ix)
{-# INLINE (!>) #-}


-- | /O(1)/ - Just like `!>` slices the array from the outside, but returns
-- `Nothing` when index is out of bounds.
--
-- @since 0.1.0
(!?>) :: (MonadThrow m, OuterSlice r ix e) => Array r ix e -> Int -> m (Elt r ix e)
(!?>) !arr !i
  | isSafeIndex sz i = pure $ unsafeOuterSlice arr i
  | otherwise = throwM $ IndexOutOfBoundsException sz i
  where
    !sz = fst (unconsSz (size arr))
{-# INLINE (!?>) #-}


-- | /O(1)/ - Safe slicing continuation from the outside. Similarly to (`!>`) slices the array from
-- the outside, but takes `Maybe` array as input and returns `Nothing` when index is out of bounds.
--
-- ===__Examples__
--
-- >>> import Data.Massiv.Array
-- >>> arr = makeArrayR U Seq (Sz (3 :> 2 :. 4)) fromIx3
-- >>> arr !?> 2 ??> 0 ??> 3 :: Maybe Ix3T
-- Just (2,0,3)
-- >>> arr !?> 2 ??> 0 ??> -1 :: Maybe Ix3T
-- Nothing
-- >>> arr !?> 2 ??> -10 ?? 1
-- *** Exception: IndexOutOfBoundsException: -10 is not safe for (Sz1 2)
--
-- @since 0.1.0
(??>) :: (MonadThrow m, OuterSlice r ix e) => m (Array r ix e) -> Int -> m (Elt r ix e)
(??>) marr !ix = marr >>= (!?> ix)
{-# INLINE (??>) #-}


-- | /O(1)/ - Safe slice from the inside
--
-- @since 0.1.0
(<!?) :: (MonadThrow m, InnerSlice r ix e) => Array r ix e -> Int -> m (Elt r ix e)
(<!?) !arr !i
  | isSafeIndex m i = pure $ unsafeInnerSlice arr sz i
  | otherwise = throwM $ IndexOutOfBoundsException m i
  where
    !sz@(_, m) = unsnocSz (size arr)
{-# INLINE (<!?) #-}


-- | /O(1)/ - Similarly to (`!>`) slice an array from an opposite direction.
--
-- @since 0.1.0
(<!) :: InnerSlice r ix e => Array r ix e -> Int -> Elt r ix e
(<!) !arr !ix =
  case arr <!? ix of
    Right res -> res
    Left exc  -> throw exc
{-# INLINE (<!) #-}


-- | /O(1)/ - Safe slicing continuation from the inside
--
-- @since 0.1.0
(<??) :: (MonadThrow m, InnerSlice r ix e) => m (Array r ix e) -> Int -> m (Elt r ix e)
(<??) marr !ix = marr >>= (<!? ix)
{-# INLINE (<??) #-}


-- | /O(1)/ - Same as (`<!>`), but fails gracefully with a `Nothing`, instead of an error
--
-- @since 0.1.0
(<!?>) :: (MonadThrow m, Slice r ix e) => Array r ix e -> (Dim, Int) -> m (Elt r ix e)
(<!?>) !arr (dim, i) = do
  (m, szl) <- pullOutSzM (size arr) dim
  unless (isSafeIndex m i) $ throwM $ IndexOutOfBoundsException m i
  cutSz <- insertSzM szl dim oneSz
  internalInnerSlice dim cutSz arr i
{-# INLINE (<!?>) #-}


internalInnerSlice ::
     (MonadThrow m, Slice r ix e) => Dim -> Sz ix -> Array r ix e -> Int -> m (Elt r ix e)
internalInnerSlice dim cutSz arr i = do
  start <- setDimM zeroIndex dim i
  unsafeSlice arr start cutSz dim
{-# INLINE internalInnerSlice #-}



-- prop> arr !> i == arr <!> (dimensions (size arr), i)
-- prop> arr <! i == arr <!> (1,i)
--
-- | /O(1)/ - Slices the array in any available dimension. Throws an error when
-- index is out of bounds or dimensions is invalid.
--
-- @since 0.1.0
(<!>) :: Slice r ix e => Array r ix e -> (Dim, Int) -> Elt r ix e
(<!>) !arr !dix =
  case arr <!?> dix of
    Right res -> res
    Left exc  -> throw exc
{-# INLINE (<!>) #-}


-- | /O(1)/ - Safe slicing continuation from within.
--
-- @since 0.1.0
(<??>) :: (MonadThrow m, Slice r ix e) => m (Array r ix e) -> (Dim, Int) -> m (Elt r ix e)
(<??>) !marr !ix = marr >>= (<!?> ix)
{-# INLINE (<??>) #-}

-- | Create a delayed array of outer slices.
--
-- ====__Examples__
--
-- >>> import Data.Massiv.Array as A
-- >>> A.mapM_ print $ outerSlices (0 ..: (3 :. 2))
-- Array D Seq (Sz1 2)
--   [ 0 :. 0, 0 :. 1 ]
-- Array D Seq (Sz1 2)
--   [ 1 :. 0, 1 :. 1 ]
-- Array D Seq (Sz1 2)
--   [ 2 :. 0, 2 :. 1 ]
--
-- @since 0.5.4
outerSlices :: OuterSlice r ix e => Array r ix e -> Array D Ix1 (Elt r ix e)
outerSlices arr = makeArray Seq k (unsafeOuterSlice arr)
  where
    (k, _) = unconsSz $ size arr
-- TODO: move setComp to Load
-- outerSlices arr = makeArray (getComp arr) k (unsafeOuterSlice arr')
--   where
--     arr' = setComp Seq arr
--     (k, _) = unconsSz $ size arr
{-# INLINE outerSlices #-}


-- | Create a delayed array of inner slices.
--
-- ====__Examples__
--
-- >>> import Data.Massiv.Array as A
-- >>> A.mapM_ print $ innerSlices (0 ..: (3 :. 2))
-- Array D Seq (Sz1 3)
--   [ 0 :. 0, 1 :. 0, 2 :. 0 ]
-- Array D Seq (Sz1 3)
--   [ 0 :. 1, 1 :. 1, 2 :. 1 ]
--
-- @since 0.5.4
innerSlices :: InnerSlice r ix e => Array r ix e -> Array D Ix1 (Elt r ix e)
innerSlices arr = makeArray Seq k (unsafeInnerSlice arr sz)
  where
    sz@(_, k) = unsnocSz $ size arr
-- TODO: move setComp to Load
-- innerSlices arr = makeArray (getComp arr) k (unsafeInnerSlice arr' sz)
--   where
--     arr' = setComp Seq arr
--     sz@(_, k) = unsnocSz $ size arr
{-# INLINE innerSlices #-}

-- | Create a delayed array of slices from within. Checks dimension at compile time.
--
-- ====__Examples__
--
-- >>> import Data.Massiv.Array as A
-- >>> arr = fromIx3 <$> (0 ..: (4 :> 3 :. 2))
-- >>> print arr
-- Array D Seq (Sz (4 :> 3 :. 2))
--   [ [ [ (0,0,0), (0,0,1) ]
--     , [ (0,1,0), (0,1,1) ]
--     , [ (0,2,0), (0,2,1) ]
--     ]
--   , [ [ (1,0,0), (1,0,1) ]
--     , [ (1,1,0), (1,1,1) ]
--     , [ (1,2,0), (1,2,1) ]
--     ]
--   , [ [ (2,0,0), (2,0,1) ]
--     , [ (2,1,0), (2,1,1) ]
--     , [ (2,2,0), (2,2,1) ]
--     ]
--   , [ [ (3,0,0), (3,0,1) ]
--     , [ (3,1,0), (3,1,1) ]
--     , [ (3,2,0), (3,2,1) ]
--     ]
--   ]
-- >>> A.mapM_ print $ withinSlices Dim2 arr
-- Array D Seq (Sz (4 :. 2))
--   [ [ (0,0,0), (0,0,1) ]
--   , [ (1,0,0), (1,0,1) ]
--   , [ (2,0,0), (2,0,1) ]
--   , [ (3,0,0), (3,0,1) ]
--   ]
-- Array D Seq (Sz (4 :. 2))
--   [ [ (0,1,0), (0,1,1) ]
--   , [ (1,1,0), (1,1,1) ]
--   , [ (2,1,0), (2,1,1) ]
--   , [ (3,1,0), (3,1,1) ]
--   ]
-- Array D Seq (Sz (4 :. 2))
--   [ [ (0,2,0), (0,2,1) ]
--   , [ (1,2,0), (1,2,1) ]
--   , [ (2,2,0), (2,2,1) ]
--   , [ (3,2,0), (3,2,1) ]
--   ]
--
-- @since 0.5.4
withinSlices ::
     (IsIndexDimension ix n, Slice r ix e)
  => Dimension n
  -> Array r ix e
  -> Array D Ix1 (Elt r ix e)
withinSlices dim = either throwImpossible id . withinSlicesM (fromDimension dim)
{-# INLINE withinSlices #-}


-- | Create a delayed array of slices from within. Same as `withinSlices`, but throws an
-- error on invalid dimension.
--
-- /__Throws Exceptions__/: `IndexDimensionException`
--
-- @since 0.5.4
withinSlicesM :: (MonadThrow m, Slice r ix e) => Dim -> Array r ix e -> m (Array D Ix1 (Elt r ix e))
withinSlicesM dim arr = do
  (k, szl) <- pullOutSzM (size arr) dim
  cutSz <- insertSzM szl dim oneSz
  pure $ makeArray Seq k (either throwImpossible id . internalInnerSlice dim cutSz arr)
{-# INLINE withinSlicesM #-}
