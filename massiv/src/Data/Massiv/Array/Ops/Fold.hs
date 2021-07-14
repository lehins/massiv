{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Massiv.Array.Ops.Fold
-- Copyright   : (c) Alexey Kuleshevich 2018-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Ops.Fold
  (
  -- ** Unstructured folds

  -- $unstruct_folds
    fold
  , ifoldMono
  , foldMono
  , ifoldSemi
  , foldSemi
  , foldOuterSlice
  , ifoldOuterSlice
  , foldInnerSlice
  , ifoldInnerSlice
  , minimumM
  , minimum'
  , maximumM
  , maximum'
  , sum
  , product
  , and
  , or
  , all
  , any
  , elem
  , eqArrays
  , compareArrays

  -- ** Single dimension folds
  -- *** Safe inner most
  --
  -- Folding along the inner most dimension will always be faster when compared to doing the same
  -- operation along any other dimension, this is due to the fact that inner most folds follow the
  -- memory layout of data.
  , ifoldlInner
  , foldlInner
  , ifoldrInner
  , foldrInner
  , foldInner
  -- *** Type safe within
  , ifoldlWithin
  , foldlWithin
  , ifoldrWithin
  , foldrWithin
  , foldWithin
  -- *** Partial within
  , ifoldlWithin'
  , foldlWithin'
  , ifoldrWithin'
  , foldrWithin'
  , foldWithin'

  -- ** Sequential folds

  -- $seq_folds

  , foldlS
  , foldrS
  , ifoldlS
  , ifoldrS

  -- *** Monadic
  , foldlM
  , foldrM
  , foldlM_
  , foldrM_
  , ifoldlM
  , ifoldrM
  , ifoldlM_
  , ifoldrM_

  -- *** Special folds
  , foldrFB
  , lazyFoldlS
  , lazyFoldrS

  -- ** Parallel folds

  -- $par_folds

  , foldlP
  , foldrP
  , ifoldlP
  , ifoldrP
  , ifoldlIO
  , ifoldrIO
  -- , splitReduce
  ) where

import Data.Massiv.Array.Delayed.Pull
import Data.Massiv.Array.Ops.Construct
import Data.Massiv.Array.Ops.Fold.Internal
import Data.Massiv.Core
import Data.Massiv.Core.Common
import Prelude hiding (all, and, any, foldl, foldr, map, maximum, minimum, or, product, sum, elem)


-- | /O(n)/ - Monoidal fold over an array with an index aware function. Also known as reduce.
--
-- @since 0.2.4
ifoldMono ::
     (Index ix, Source r e, Monoid m)
  => (ix -> e -> m) -- ^ Convert each element of an array to an appropriate `Monoid`.
  -> Array r ix e -- ^ Source array
  -> m
ifoldMono f = ifoldlInternal (\a ix e -> a `mappend` f ix e) mempty mappend mempty
{-# INLINE ifoldMono #-}


-- | /O(n)/ - Semigroup fold over an array with an index aware function.
--
-- @since 0.2.4
ifoldSemi ::
     (Index ix, Source r e, Semigroup m)
  => (ix -> e -> m) -- ^ Convert each element of an array to an appropriate `Semigroup`.
  -> m -- ^ Initial element that must be neutral to the (`<>`) function.
  -> Array r ix e -- ^ Source array
  -> m
ifoldSemi f m = ifoldlInternal (\a ix e -> a <> f ix e) m (<>) m
{-# INLINE ifoldSemi #-}


-- | /O(n)/ - Semigroup fold over an array.
--
-- @since 0.1.6
foldSemi ::
     (Index ix, Source r e, Semigroup m)
  => (e -> m) -- ^ Convert each element of an array to an appropriate `Semigroup`.
  -> m -- ^ Initial element that must be neutral to the (`<>`) function.
  -> Array r ix e -- ^ Source array
  -> m
foldSemi f m = foldlInternal (\a e -> a <> f e) m (<>) m
{-# INLINE foldSemi #-}


-- | Left fold along a specified dimension with an index aware function.
--
-- @since 0.2.4
ifoldlWithin :: (Index (Lower ix), IsIndexDimension ix n, Source r e) =>
  Dimension n -> (ix -> a -> e -> a) -> a -> Array r ix e -> Array D (Lower ix) a
ifoldlWithin dim = ifoldlWithin' (fromDimension dim)
{-# INLINE ifoldlWithin #-}


-- | Left fold along a specified dimension.
--
-- ====__Example__
--
-- >>> import Data.Massiv.Array
-- >>> :set -XTypeApplications
-- >>> arr = makeArrayLinear @U Seq (Sz (2 :. 5)) id
-- >>> arr
-- Array U Seq (Sz (2 :. 5))
--   [ [ 0, 1, 2, 3, 4 ]
--   , [ 5, 6, 7, 8, 9 ]
--   ]
-- >>> foldlWithin Dim1 (flip (:)) [] arr
-- Array D Seq (Sz1 2)
--   [ [4,3,2,1,0], [9,8,7,6,5] ]
-- >>> foldlWithin Dim2 (flip (:)) [] arr
-- Array D Seq (Sz1 5)
--   [ [5,0], [6,1], [7,2], [8,3], [9,4] ]
--
-- @since 0.2.4
foldlWithin :: (Index (Lower ix), IsIndexDimension ix n, Source r e) =>
  Dimension n -> (a -> e -> a) -> a -> Array r ix e -> Array D (Lower ix) a
foldlWithin dim f = ifoldlWithin dim (const f)
{-# INLINE foldlWithin #-}


-- | Right fold along a specified dimension with an index aware function.
--
-- @since 0.2.4
ifoldrWithin :: (Index (Lower ix), IsIndexDimension ix n, Source r e) =>
  Dimension n -> (ix -> e -> a -> a) -> a -> Array r ix e -> Array D (Lower ix) a
ifoldrWithin dim = ifoldrWithin' (fromDimension dim)
{-# INLINE ifoldrWithin #-}


-- | Right fold along a specified dimension.
--
-- @since 0.2.4
foldrWithin :: (Index (Lower ix), IsIndexDimension ix n, Source r e) =>
  Dimension n -> (e -> a -> a) -> a -> Array r ix e -> Array D (Lower ix) a
foldrWithin dim f = ifoldrWithin dim (const f)
{-# INLINE foldrWithin #-}


-- | Similar to `ifoldlWithin`, except that dimension is specified at a value level, which means it
-- will throw an exception on an invalid dimension.
--
-- @since 0.2.4
ifoldlWithin' :: (HasCallStack, Index (Lower ix), Index ix, Source r e) =>
  Dim -> (ix -> a -> e -> a) -> a -> Array r ix e -> Array D (Lower ix) a
ifoldlWithin' dim f acc0 arr =
  makeArray (getComp arr) (SafeSz szl) $ \ixl ->
    iter
      (insertDim' ixl dim 0)
      (insertDim' ixl dim (k - 1))
      (pureIndex 1)
      (<=)
      acc0
      (\ix acc' -> f ix acc' (unsafeIndex arr ix))
  where
    SafeSz sz = size arr
    (k, szl) = pullOutDim' sz dim
{-# INLINE ifoldlWithin' #-}


-- | Similar to `foldlWithin`, except that dimension is specified at a value level, which means it will
-- throw an exception on an invalid dimension.
--
-- @since 0.2.4
foldlWithin' :: (HasCallStack, Index (Lower ix), Index ix, Source r e) =>
  Dim -> (a -> e -> a) -> a -> Array r ix e -> Array D (Lower ix) a
foldlWithin' dim f = ifoldlWithin' dim (const f)
{-# INLINE foldlWithin' #-}


-- | Similar to `ifoldrWithin`, except that dimension is specified at a value level, which means it
-- will throw an exception on an invalid dimension.
--
--
-- @since 0.2.4
ifoldrWithin' :: (HasCallStack, Index (Lower ix), Index ix, Source r e) =>
  Dim -> (ix -> e -> a -> a) -> a -> Array r ix e -> Array D (Lower ix) a
ifoldrWithin' dim f acc0 arr =
  makeArray (getComp arr) (SafeSz szl) $ \ixl ->
    iter
      (insertDim' ixl dim (k - 1))
      (insertDim' ixl dim 0)
      (pureIndex (-1))
      (>=)
      acc0
      (\ix acc' -> f ix (unsafeIndex arr ix) acc')
  where
    SafeSz sz = size arr
    (k, szl) = pullOutDim' sz dim
{-# INLINE ifoldrWithin' #-}

-- | Similar to `foldrWithin`, except that dimension is specified at a value level, which means it
-- will throw an exception on an invalid dimension.
--
-- @since 0.2.4
foldrWithin' :: (HasCallStack, Index (Lower ix), Index ix, Source r e) =>
  Dim -> (e -> a -> a) -> a -> Array r ix e -> Array D (Lower ix) a
foldrWithin' dim f = ifoldrWithin' dim (const f)
{-# INLINE foldrWithin' #-}


-- | Left fold over the inner most dimension with index aware function.
--
-- @since 0.2.4
ifoldlInner :: (Index (Lower ix), Index ix, Source r e) =>
  (ix -> a -> e -> a) -> a -> Array r ix e -> Array D (Lower ix) a
ifoldlInner = ifoldlWithin' 1
{-# INLINE ifoldlInner #-}

-- | Left fold over the inner most dimension.
--
-- @since 0.2.4
foldlInner :: (Index (Lower ix), Index ix, Source r e) =>
  (a -> e -> a) -> a -> Array r ix e -> Array D (Lower ix) a
foldlInner = foldlWithin' 1
{-# INLINE foldlInner #-}

-- | Right fold over the inner most dimension with index aware function.
--
-- @since 0.2.4
ifoldrInner :: (Index (Lower ix), Index ix, Source r e) =>
  (ix -> e -> a -> a) -> a -> Array r ix e -> Array D (Lower ix) a
ifoldrInner = ifoldrWithin' 1
{-# INLINE ifoldrInner #-}

-- | Right fold over the inner most dimension.
--
-- @since 0.2.4
foldrInner :: (Index (Lower ix), Index ix, Source r e) =>
  (e -> a -> a) -> a -> Array r ix e -> Array D (Lower ix) a
foldrInner = foldrWithin' 1
{-# INLINE foldrInner #-}

-- | Monoidal fold over the inner most dimension.
--
-- @since 0.4.3
foldInner :: (Monoid e, Index (Lower ix), Index ix, Source r e) => Array r ix e -> Array D (Lower ix) e
foldInner = foldlInner mappend mempty
{-# INLINE foldInner #-}

-- | Monoidal fold over some internal dimension.
--
-- @since 0.4.3
foldWithin ::
     (Source r a, Monoid a, Index (Lower ix), IsIndexDimension ix n)
  => Dimension n
  -> Array r ix a
  -> Array D (Lower ix) a
foldWithin dim = foldlWithin dim mappend mempty
{-# INLINE foldWithin #-}

-- | Monoidal fold over some internal dimension. This is a pratial function and will
-- result in `IndexDimensionException` if supplied dimension is invalid.
--
-- @since 0.4.3
foldWithin' ::
     (HasCallStack, Index ix, Source r a, Monoid a, Index (Lower ix))
  => Dim
  -> Array r ix a
  -> Array D (Lower ix) a
foldWithin' dim = foldlWithin' dim mappend mempty
{-# INLINE foldWithin' #-}


-- | Reduce each outer slice into a monoid and mappend results together
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Array as A
-- >>> import Data.Monoid (Product(..))
-- >>> arr = computeAs P $ iterateN (Sz2 2 3) (+1) (10 :: Int)
-- >>> arr
-- Array P Seq (Sz (2 :. 3))
--   [ [ 11, 12, 13 ]
--   , [ 14, 15, 16 ]
--   ]
-- >>> getProduct $ foldOuterSlice (\row -> Product (A.sum row)) arr
-- 1620
-- >>> (11 + 12 + 13) * (14 + 15 + 16) :: Int
-- 1620
--
-- @since 0.4.3
foldOuterSlice ::
     (Index ix, Index (Lower ix), Source r e, Monoid m)
  => (Array r (Lower ix) e -> m)
  -> Array r ix e
  -> m
foldOuterSlice f = ifoldOuterSlice (const f)
{-# INLINE foldOuterSlice #-}


-- | Reduce each outer slice into a monoid with an index aware function and mappend results
-- together
--
-- @since 0.4.3
ifoldOuterSlice ::
     (Index ix, Index (Lower ix), Source r e, Monoid m)
  => (Ix1 -> Array r (Lower ix) e -> m)
  -> Array r ix e
  -> m
ifoldOuterSlice f arr = foldMono g $ range (getComp arr) 0 k
  where
    (Sz1 k, szL) = unconsSz $ size arr
    g i = f i (unsafeOuterSlice arr szL i)
    {-# INLINE g #-}
{-# INLINE ifoldOuterSlice #-}


-- | Reduce each inner slice into a monoid and mappend results together
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Array as A
-- >>> import Data.Monoid (Product(..))
-- >>> arr = computeAs P $ iterateN (Sz2 2 3) (+1) (10 :: Int)
-- >>> arr
-- Array P Seq (Sz (2 :. 3))
--   [ [ 11, 12, 13 ]
--   , [ 14, 15, 16 ]
--   ]
-- >>> getProduct $ foldInnerSlice (\column -> Product (A.sum column)) arr
-- 19575
-- >>> (11 + 14) * (12 + 15) * (13 + 16) :: Int
-- 19575
--
-- @since 0.4.3
foldInnerSlice ::
     (Source r e, Index ix, Monoid m) => (Array D (Lower ix) e -> m) -> Array r ix e -> m
foldInnerSlice f = ifoldInnerSlice (const f)
{-# INLINE foldInnerSlice #-}


-- | Reduce each inner slice into a monoid with an index aware function and mappend
-- results together
--
-- @since 0.4.3
ifoldInnerSlice ::
     (Source r e, Index ix, Monoid m) => (Ix1 -> Array D (Lower ix) e -> m) -> Array r ix e -> m
ifoldInnerSlice f arr = foldMono g $ range (getComp arr) 0 (unSz k)
  where
    (szL, !k) = unsnocSz (size arr)
    g i = f i (unsafeInnerSlice arr szL i)
    {-# INLINE g #-}
{-# INLINE ifoldInnerSlice #-}

-- | /O(n)/ - Compute maximum of all elements.
--
-- @since 0.3.0
maximumM :: (Raises m, Shape r ix, Source r e, Ord e) => Array r ix e -> m e
maximumM arr =
  if isNull arr
    then raiseM (SizeEmptyException (size arr))
    else let !e0 = unsafeIndex arr zeroIndex
          in pure $ foldlInternal max e0 max e0 arr
{-# INLINE maximumM #-}


-- | /O(n)/ - Compute maximum of all elements.
--
-- @since 0.3.0
maximum' ::
     forall r ix e. (HasCallStack, Shape r ix, Source r e, Ord e)
  => Array r ix e
  -> e
maximum' = raiseLeftImprecise . maximumM
{-# INLINE maximum' #-}


-- | /O(n)/ - Compute minimum of all elements.
--
-- @since 0.3.0
minimumM :: (Raises m, Shape r ix, Source r e, Ord e) => Array r ix e -> m e
minimumM arr =
  if isNull arr
    then raiseM (SizeEmptyException (size arr))
    else let !e0 = unsafeIndex arr zeroIndex
          in pure $ foldlInternal min e0 min e0 arr
{-# INLINE minimumM #-}

-- | /O(n)/ - Compute minimum of all elements.
--
-- @since 0.3.0
minimum' :: forall r ix e. (HasCallStack, Shape r ix, Source r e, Ord e) => Array r ix e -> e
minimum' = raiseLeftImprecise . minimumM
{-# INLINE minimum' #-}


-- -- | /O(n)/ - Compute sum of all elements.
-- --
-- -- @since 0.1.0
-- sum' ::
--      forall r ix e. (Index ix, Source r e, Numeric r e)
--   => Array r ix e
--   -> IO e
-- sum' = splitReduce (\_ -> pure . sumArray) (\x y -> pure (x + y)) 0
-- {-# INLINE sum' #-}

-- | /O(n)/ - Compute sum of all elements.
--
-- @since 0.1.0
sum :: (Index ix, Source r e, Num e) => Array r ix e -> e
sum = foldlInternal (+) 0 (+) 0
{-# INLINE sum #-}


-- | /O(n)/ - Compute product of all elements.
--
-- @since 0.1.0
product :: (Index ix, Source r e, Num e) => Array r ix e -> e
product = foldlInternal (*) 1 (*) 1
{-# INLINE product #-}


-- | /O(n)/ - Compute conjunction of all elements.
--
-- @since 0.1.0
and :: (Index ix, Source r Bool) => Array r ix Bool -> Bool
and = all id
{-# INLINE and #-}


-- | /O(n)/ - Compute disjunction of all elements.
--
-- @since 0.1.0
or :: (Index ix, Source r Bool) => Array r ix Bool -> Bool
or = any id
{-# INLINE or #-}


-- | /O(n)/ - Determines whether all elements of the array satisfy a predicate.
--
-- @since 0.1.0
all :: (Index ix, Source r e) => (e -> Bool) -> Array r ix e -> Bool
all f = not . any (not . f)
{-# INLINE all #-}

-- | /O(n)/ - Determines whether an element is present in the array.
--
-- @since 0.5.5
elem :: (Eq e, Index ix, Source r e) => e -> Array r ix e -> Bool
elem e = any (e ==)
{-# INLINE elem #-}


{- $unstruct_folds

Functions in this section will fold any `Source` array with respect to the inner
`Comp`utation strategy setting.

-}


{- $seq_folds

Functions in this section will fold any `Source` array sequentially, regardless of the inner
`Comp`utation strategy setting.

-}


{- $par_folds

__Note__ It is important to compile with @-threaded -with-rtsopts=-N@ flags, otherwise there will be
no parallelization.

Functions in this section will fold any `Source` array in parallel, regardless of the inner
`Comp`utation strategy setting. All of the parallel structured folds are performed inside `IO`
monad, because referential transparency can't generally be preserved and results will depend on the
number of cores/capabilities that computation is being performed on.

In contrast to sequential folds, each parallel folding function accepts two functions and two
initial elements as arguments. This is necessary because an array is first split into chunks, which
folded individually on separate cores with the first function, and the results of those folds are
further folded with the second function.

-}
