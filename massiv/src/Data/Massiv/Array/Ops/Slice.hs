{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Massiv.Array.Ops.Slice
-- Copyright   : (c) Alexey Kuleshevich 2018-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
module Data.Massiv.Array.Ops.Slice (
  -- ** From the outside
  (!>),
  (!?>),
  (??>),

  -- ** From the inside
  (<!),
  (<!?),
  (<??),

  -- ** From within
  (<!>),
  (<!?>),
  (<??>),

  -- ** Many slices
  outerSlices,
  innerSlices,
  withinSlices,
  withinSlicesM,
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
-- Array U Seq (Sz (2 :. 4))
--   [ [ (2,0,0), (2,0,1), (2,0,2), (2,0,3) ]
--   , [ (2,1,0), (2,1,1), (2,1,2), (2,1,3) ]
--   ]
--
-- There is nothing wrong with chaining, mixing and matching slicing operators:
--
-- >>> arr !> 2 !> 0 ! 3
-- (2,0,3)
-- >>> evaluateM (arr !> 2 <! 3) 0
-- (2,0,3)
-- >>> (arr !> 2 !> 0 ! 3) == (arr ! 2 :> 0 :. 3)
-- True
--
--
-- @since 0.1.0
(!>)
  :: forall r ix e
   . (HasCallStack, Index ix, Index (Lower ix), Source r e)
  => Array r ix e
  -> Int
  -> Array r (Lower ix) e
(!>) !arr !ix = throwEither (arr !?> ix)
{-# INLINE (!>) #-}

-- | /O(1)/ - Just like `!>` slices the array from the outside, but returns
-- `Nothing` when index is out of bounds.
--
-- @since 0.1.0
(!?>)
  :: forall r ix e m
   . (MonadThrow m, Index ix, Index (Lower ix), Source r e)
  => Array r ix e
  -> Int
  -> m (Array r (Lower ix) e)
(!?>) !arr !i = do
  let (k, szL) = unconsSz (size arr)
  unless (isSafeIndex k i) $ throwM $ IndexOutOfBoundsException k i
  pure $ unsafeOuterSlice arr szL i
{-# INLINE (!?>) #-}

-- | /O(1)/ - Safe slicing continuation from the outside. Similarly to (`!>`) slices the array from
-- the outside, but takes `Maybe` array as input and returns `Nothing` when index is out of bounds.
--
-- ===__Examples__
--
-- >>> import Data.Massiv.Array
-- >>> arr = makeArrayR U Seq (Sz (3 :> 2 :. 4)) fromIx3
-- >>> arr !?> 2 ??> 0 ?? 3 :: Maybe Ix3T
-- Just (2,0,3)
-- >>> arr !?> 2 ??> 0 ?? -1 :: Maybe Ix3T
-- Nothing
-- >>> arr !?> 2 ??> -10 ?? 1
-- *** Exception: IndexOutOfBoundsException: -10 is not safe for (Sz1 2)
--
-- @since 0.1.0
(??>)
  :: forall r ix e m
   . (MonadThrow m, Index ix, Index (Lower ix), Source r e)
  => m (Array r ix e)
  -> Int
  -> m (Array r (Lower ix) e)
(??>) marr !ix = marr >>= (!?> ix)
{-# INLINE (??>) #-}

-- | /O(1)/ - Safe slice from the inside
--
-- @since 0.1.0
(<!?)
  :: forall r ix e m
   . (MonadThrow m, Index ix, Source r e)
  => Array r ix e
  -> Int
  -> m (Array D (Lower ix) e)
(<!?) !arr !i = do
  let (szL, m) = unsnocSz (size arr)
  unless (isSafeIndex m i) $ throwM $ IndexOutOfBoundsException m i
  pure $ unsafeInnerSlice arr szL i
{-# INLINE (<!?) #-}

-- | /O(1)/ - Similarly to (`!>`) slice an array from an opposite direction.
--
-- @since 0.1.0
(<!)
  :: forall r ix e
   . (HasCallStack, Index ix, Source r e)
  => Array r ix e
  -> Int
  -> Array D (Lower ix) e
(<!) !arr !ix = throwEither (arr <!? ix)
{-# INLINE (<!) #-}

-- | /O(1)/ - Safe slicing continuation from the inside
--
-- @since 0.1.0
(<??)
  :: forall r ix e m
   . (MonadThrow m, Index ix, Source r e)
  => m (Array r ix e)
  -> Int
  -> m (Array D (Lower ix) e)
(<??) marr !ix = marr >>= (<!? ix)
{-# INLINE (<??) #-}

-- | /O(1)/ - Same as (`<!>`), but fails gracefully with a `Nothing`, instead of an error
--
-- @since 0.1.0
(<!?>)
  :: forall r ix e m
   . (MonadThrow m, Index ix, Index (Lower ix), Source r e)
  => Array r ix e
  -> (Dim, Int)
  -> m (Array D (Lower ix) e)
(<!?>) !arr (dim, i) = do
  (m, szl) <- pullOutSzM (size arr) dim
  unless (isSafeIndex m i) $ throwM $ IndexOutOfBoundsException m i
  cutSz <- insertSzM szl dim oneSz
  internalInnerSlice dim cutSz arr i
{-# INLINE (<!?>) #-}

internalInnerSlice
  :: (MonadThrow m, Index ix, Index (Lower ix), Source r e)
  => Dim
  -> Sz ix
  -> Array r ix e
  -> Ix1
  -> m (Array D (Lower ix) e)
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
(<!>)
  :: forall r ix e
   . (HasCallStack, Index ix, Index (Lower ix), Source r e)
  => Array r ix e
  -> (Dim, Int)
  -> Array D (Lower ix) e
(<!>) !arr !dix = throwEither (arr <!?> dix)
{-# INLINE (<!>) #-}

-- | /O(1)/ - Safe slicing continuation from within.
--
-- @since 0.1.0
(<??>)
  :: forall r ix e m
   . (MonadThrow m, Index ix, Index (Lower ix), Source r e)
  => m (Array r ix e)
  -> (Dim, Int)
  -> m (Array D (Lower ix) e)
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
outerSlices
  :: forall r ix e
   . (Index ix, Index (Lower ix), Source r e)
  => Array r ix e
  -> Array D Ix1 (Array r (Lower ix) e)
outerSlices arr = makeArray (getComp arr) k (unsafeOuterSlice (setComp Seq arr) szL)
  where
    (k, szL) = unconsSz $ size arr
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
innerSlices
  :: forall r ix e
   . (Index ix, Source r e)
  => Array r ix e
  -> Array D Ix1 (Array D (Lower ix) e)
innerSlices arr = makeArray (getComp arr) k (unsafeInnerSlice (setComp Seq arr) szL)
  where
    (szL, k) = unsnocSz $ size arr
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
withinSlices
  :: forall n r ix e
   . (IsIndexDimension ix n, Index (Lower ix), Source r e)
  => Dimension n
  -> Array r ix e
  -> Array D Ix1 (Array D (Lower ix) e)
withinSlices dim = either throwImpossible id . withinSlicesM (fromDimension dim)
{-# INLINE withinSlices #-}

-- | Create a delayed array of slices from within. Same as `withinSlices`, but throws an
-- error on invalid dimension.
--
-- /__Throws Exceptions__/: `IndexDimensionException`
--
-- @since 0.5.4
withinSlicesM
  :: forall r ix e m
   . (MonadThrow m, Index ix, Index (Lower ix), Source r e)
  => Dim
  -> Array r ix e
  -> m (Array D Ix1 (Array D (Lower ix) e))
withinSlicesM dim arr = do
  (k, szl) <- pullOutSzM (size arr) dim
  cutSz <- insertSzM szl dim oneSz
  pure $ makeArray Seq k (either throwImpossible id . internalInnerSlice dim cutSz arr)
{-# INLINE withinSlicesM #-}
