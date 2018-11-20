{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Massiv.Array.Ops.Fold
-- Copyright   : (c) Alexey Kuleshevich 2018
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
  , minimum
  , maximum
  , sum
  , product
  , and
  , or
  , all
  , any

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
  -- *** Type safe
  , ifoldlWithin
  , foldlWithin
  , ifoldrWithin
  , foldrWithin
  -- *** Partial
  , ifoldlWithin'
  , foldlWithin'
  , ifoldrWithin'
  , foldrWithin'

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
  , foldlOnP
  , ifoldlIO
  , foldrOnP
  , ifoldlOnP
  , ifoldrOnP
  , ifoldrIO
  ) where

import           Data.Massiv.Array.Delayed.Internal
import           Data.Massiv.Array.Ops.Fold.Internal
import           Data.Massiv.Array.Ops.Map           (imap, map)
import           Data.Massiv.Core
import           Data.Massiv.Core.Common
import           Data.Semigroup
import           Prelude                             hiding (all, and, any,
                                                      foldl, foldr, map,
                                                      maximum, minimum, or,
                                                      product, sum)

-- | /O(n)/ - Monoidal fold over an array with an index aware function. Also known as reduce.
--
-- @since 0.2.4
ifoldMono ::
     (Source r ix e, Monoid m)
  => (ix -> e -> m) -- ^ Convert each element of an array to an appropriate `Monoid`.
  -> Array r ix e -- ^ Source array
  -> m
ifoldMono f = foldlInternal mappend mempty mappend mempty . imap f
{-# INLINE ifoldMono #-}


-- | /O(n)/ - Monoidal fold over an array. Also known as reduce.
--
-- @since 0.1.4
foldMono ::
     (Source r ix e, Monoid m)
  => (e -> m) -- ^ Convert each element of an array to an appropriate `Monoid`.
  -> Array r ix e -- ^ Source array
  -> m
foldMono f = foldlInternal mappend mempty mappend mempty . map f
{-# INLINE foldMono #-}


-- | /O(n)/ - Semigroup fold over an array with an index aware function.
--
-- @since 0.2.4
ifoldSemi ::
     (Source r ix e, Semigroup m)
  => (ix -> e -> m) -- ^ Convert each element of an array to an appropriate `Semigroup`.
  -> m -- ^ Initial element that must be neutral to the (`<>`) function.
  -> Array r ix e -- ^ Source array
  -> m
ifoldSemi f m = foldlInternal (<>) m (<>) m . imap f
{-# INLINE ifoldSemi #-}


-- | /O(n)/ - Semigroup fold over an array.
--
-- @since 0.1.6
foldSemi ::
     (Source r ix e, Semigroup m)
  => (e -> m) -- ^ Convert each element of an array to an appropriate `Semigroup`.
  -> m -- ^ Initial element that must be neutral to the (`<>`) function.
  -> Array r ix e -- ^ Source array
  -> m
foldSemi f m = foldlInternal (<>) m (<>) m . map f
{-# INLINE foldSemi #-}


-- | Left fold along a specified dimension with an index aware function.
--
-- @since 0.2.4
ifoldlWithin :: (Index (Lower ix), IsIndexDimension ix n, Source r ix e) =>
  Dimension n -> (ix -> a -> e -> a) -> a -> Array r ix e -> Array D (Lower ix) a
ifoldlWithin dim = ifoldlWithin' (fromDimension dim)
{-# INLINE ifoldlWithin #-}


-- | Left fold along a specified dimension.
--
-- ====__Example__
--
-- >>> let arr = makeArrayR U Seq (2 :. 5) (toLinearIndex (2 :. 5))
-- >>> arr
-- (Array U Seq (2 :. 5)
--   [ [ 0,1,2,3,4 ]
--   , [ 5,6,7,8,9 ]
--   ])
-- >>> foldlWithin Dim1 (flip (:)) [] arr
-- (Array D Seq (2)
--   [ [4,3,2,1,0],[9,8,7,6,5] ])
-- >>> foldlWithin Dim2 (flip (:)) [] arr
-- (Array D Seq (5)
--   [ [5,0],[6,1],[7,2],[8,3],[9,4] ])
--
-- @since 0.2.4
foldlWithin :: (Index (Lower ix), IsIndexDimension ix n, Source r ix e) =>
  Dimension n -> (a -> e -> a) -> a -> Array r ix e -> Array D (Lower ix) a
foldlWithin dim f = ifoldlWithin dim (const f)
{-# INLINE foldlWithin #-}


-- | Right fold along a specified dimension with an index aware function.
--
-- @since 0.2.4
ifoldrWithin :: (Index (Lower ix), IsIndexDimension ix n, Source r ix e) =>
  Dimension n -> (ix -> e -> a -> a) -> a -> Array r ix e -> Array D (Lower ix) a
ifoldrWithin dim = ifoldrWithin' (fromDimension dim)
{-# INLINE ifoldrWithin #-}


-- | Right fold along a specified dimension.
--
-- @since 0.2.4
foldrWithin :: (Index (Lower ix), IsIndexDimension ix n, Source r ix e) =>
  Dimension n -> (e -> a -> a) -> a -> Array r ix e -> Array D (Lower ix) a
foldrWithin dim f = ifoldrWithin dim (const f)
{-# INLINE foldrWithin #-}


-- | Similar to `ifoldlWithin`, except that dimension is specified at a value level, which means it
-- will throw an exception on an invalid dimension.
--
-- @since 0.2.4
ifoldlWithin' :: (Index (Lower ix), Source r ix e) =>
  Dim -> (ix -> a -> e -> a) -> a -> Array r ix e -> Array D (Lower ix) a
ifoldlWithin' dim f acc0 arr =
  unsafeMakeArray (getComp arr) szl $ \ixl ->
    iter
      (insertDim' ixl dim 0)
      (insertDim' ixl dim (k - 1))
      (pureIndex 1)
      (<=)
      acc0
      (\ix acc' -> f ix acc' (unsafeIndex arr ix))
  where
    sz = size arr
    (k, szl) = pullOutDim' sz dim
{-# INLINE ifoldlWithin' #-}


-- | Similar to `foldlWithin`, except that dimension is specified at a value level, which means it will
-- throw an exception on an invalid dimension.
--
-- @since 0.2.4
foldlWithin' :: (Index (Lower ix), Source r ix e) =>
  Dim -> (a -> e -> a) -> a -> Array r ix e -> Array D (Lower ix) a
foldlWithin' dim f = ifoldlWithin' dim (const f)
{-# INLINE foldlWithin' #-}


-- | Similar to `ifoldrWithin`, except that dimension is specified at a value level, which means it
-- will throw an exception on an invalid dimension.
--
--
-- @since 0.2.4
ifoldrWithin' :: (Index (Lower ix), Source r ix e) =>
  Dim -> (ix -> e -> a -> a) -> a -> Array r ix e -> Array D (Lower ix) a
ifoldrWithin' dim f acc0 arr =
  unsafeMakeArray (getComp arr) szl $ \ixl ->
    iter
      (insertDim' ixl dim (k - 1))
      (insertDim' ixl dim 0)
      (pureIndex (-1))
      (>=)
      acc0
      (\ix acc' -> f ix (unsafeIndex arr ix) acc')
  where
    sz = size arr
    (k, szl) = pullOutDim' sz dim
{-# INLINE ifoldrWithin' #-}

-- | Similar to `foldrWithin`, except that dimension is specified at a value level, which means it
-- will throw an exception on an invalid dimension.
--
-- @since 0.2.4
foldrWithin' :: (Index (Lower ix), Source r ix e) =>
  Dim -> (e -> a -> a) -> a -> Array r ix e -> Array D (Lower ix) a
foldrWithin' dim f = ifoldrWithin' dim (const f)
{-# INLINE foldrWithin' #-}


-- | Left fold over the inner most dimension with index aware function.
--
-- @since 0.2.4
ifoldlInner :: (Index (Lower ix), Source r ix e) =>
  (ix -> a -> e -> a) -> a -> Array r ix e -> Array D (Lower ix) a
ifoldlInner = ifoldlWithin' 1
{-# INLINE ifoldlInner #-}

-- | Left fold over the inner most dimension.
--
-- @since 0.2.4
foldlInner :: (Index (Lower ix), Source r ix e) =>
  (a -> e -> a) -> a -> Array r ix e -> Array D (Lower ix) a
foldlInner = foldlWithin' 1
{-# INLINE foldlInner #-}

-- | Right fold over the inner most dimension with index aware function.
--
-- @since 0.2.4
ifoldrInner :: (Index (Lower ix), Source r ix e) =>
  (ix -> e -> a -> a) -> a -> Array r ix e -> Array D (Lower ix) a
ifoldrInner = ifoldrWithin' 1
{-# INLINE ifoldrInner #-}

-- | Right fold over the inner most dimension.
--
-- @since 0.2.4
foldrInner :: (Index (Lower ix), Source r ix e) =>
  (e -> a -> a) -> a -> Array r ix e -> Array D (Lower ix) a
foldrInner = foldrWithin' 1
{-# INLINE foldrInner #-}


-- | /O(n)/ - Compute maximum of all elements.
maximum :: (Source r ix e, Ord e) =>
           Array r ix e -> e
maximum = \arr ->
  if isEmpty arr
    then error "Data.Massiv.Array.maximum - empty"
    else fold max (evaluateAt arr zeroIndex) arr
{-# INLINE maximum #-}


-- | /O(n)/ - Compute minimum of all elements.
minimum :: (Source r ix e, Ord e) =>
           Array r ix e -> e
minimum = \arr ->
  if isEmpty arr
    then error "Data.Massiv.Array.minimum - empty"
    else fold min (evaluateAt arr zeroIndex) arr
{-# INLINE minimum #-}


-- | /O(n)/ - Compute sum of all elements.
sum :: (Source r ix e, Num e) =>
        Array r ix e -> e
sum = fold (+) 0
{-# INLINE sum #-}


-- | /O(n)/ - Compute product of all elements.
product :: (Source r ix e, Num e) =>
            Array r ix e -> e
product = fold (*) 1
{-# INLINE product #-}


-- | /O(n)/ - Compute conjunction of all elements.
and :: (Source r ix Bool) =>
       Array r ix Bool -> Bool
and = fold (&&) True
{-# INLINE and #-}


-- | /O(n)/ - Compute disjunction of all elements.
or :: Source r ix Bool =>
      Array r ix Bool -> Bool
or = fold (||) False
{-# INLINE or #-}


-- | Determines whether all element of the array satisfy the predicate.
all :: Source r ix e =>
       (e -> Bool) -> Array r ix e -> Bool
all f = foldlInternal (\acc el -> acc && f el) True (&&) True
{-# INLINE all #-}

-- | Determines whether any element of the array satisfies the predicate.
any :: Source r ix e =>
       (e -> Bool) -> Array r ix e -> Bool
any f = foldlInternal (\acc el -> acc || f el) False (||) False
{-# INLINE any #-}


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
