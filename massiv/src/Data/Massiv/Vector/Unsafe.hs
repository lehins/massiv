{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Data.Massiv.Vector.Unsafe
-- Copyright   : (c) Alexey Kuleshevich 2020-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Vector.Unsafe
  (
  -- * Vector
  -- ** Accessors
  -- *** Indexing
    unsafeHead
  , unsafeLast
  -- *** Monadic Indexing
  , unsafeIndexM
  , unsafeHeadM
  , unsafeLastM
  -- *** Slicing
  , unsafeInit
  , unsafeTail
  , unsafeTake
  , unsafeDrop
  -- -- ** Modifying
  -- -- *** Bulk updates
  -- , unsafeUpdate
  -- , unsafeUpdate_
  -- -- *** Accumulation
  -- , unsafeAccum
  -- , unsafeAccumulate_
  -- , unsafeBackpermute
  -- -- ** Predicates
  -- , unsafePartition
  -- ** Unbounded streams
  , unsafeUnfoldrN
  , unsafeUnfoldrNM
  , unsafeFromListN
  ) where

import Data.Coerce
import Data.Massiv.Core.Common
import Data.Massiv.Array.Delayed.Stream
import qualified Data.Massiv.Vector.Stream as S

-- ========= --
-- Accessors --
-- ========= --

--------------
-- Indexing --
--------------


-- |
--
-- @since 0.5.0
unsafeHead :: Source r e => Vector r e -> e
unsafeHead = (`unsafeLinearIndex` 0)
{-# INLINE unsafeHead #-}

-- |
--
-- @since 0.5.0
unsafeLast :: Source r e => Vector r e -> e
unsafeLast v = unsafeLinearIndex v (max 0 (unSz (size v) - 1))
{-# INLINE unsafeLast #-}

----------------------
-- Monadic indexing --
----------------------

-- |
--
-- @since 0.5.0
unsafeIndexM :: (Source r e, Monad m) => Vector r e -> Ix1 -> m e
unsafeIndexM v i = pure $! unsafeLinearIndex v i
{-# INLINE unsafeIndexM #-}


-- |
--
-- @since 0.5.0
unsafeHeadM :: (Monad m, Source r e) => Vector r e -> m e
unsafeHeadM v = pure $! unsafeHead v
{-# INLINE unsafeHeadM #-}

-- |
--
-- @since 0.5.0
unsafeLastM :: (Monad m, Source r e) => Vector r e -> m e
unsafeLastM v = pure $! unsafeLast v
{-# INLINE unsafeLastM #-}


-------------
-- Slicing --
-------------


-- |
--
-- @since 0.5.0
unsafeInit :: Source r e => Vector r e -> Vector r e
unsafeInit v = unsafeLinearSlice 0 (SafeSz (coerce (size v) - 1)) v
{-# INLINE unsafeInit #-}


-- |
--
-- @since 0.5.0
unsafeTail :: Source r e => Vector r e -> Vector r e
unsafeTail = unsafeDrop oneSz
{-# INLINE unsafeTail #-}


-- |
--
-- @since 0.5.0
unsafeTake :: Source r e => Sz1 -> Vector r e -> Vector r e
unsafeTake = unsafeLinearSlice 0
{-# INLINE unsafeTake #-}

-- |
--
-- @since 0.5.0
unsafeDrop :: Source r e => Sz1 -> Vector r e -> Vector r e
unsafeDrop (Sz d) v = unsafeLinearSlice d (SafeSz (coerce (size v) - d)) v
{-# INLINE unsafeDrop #-}


-- | /O(n)/ - Convert a list of a known length to a delayed stream vector.
--
-- /Unsafe/ - This function is unsafe because it will allocate enough space in memory for
-- @n@ elements ahead of time, regardless of the actual size of the list. Supplying @n@
-- that is too big will result in an asynchronous `Control.Exception.Base.HeapOverflow`
-- exception.
--
-- @since 0.5.1
unsafeFromListN :: Sz1 -> [e] -> Vector DS e
unsafeFromListN n = fromSteps . S.unsafeFromListN n
{-# INLINE unsafeFromListN #-}

-- | /O(n)/ - Right unfolding function with at most @n@ number of elements.
--
-- /Unsafe/ - This function is unsafe because it will allocate enough space in memory for
-- @n@ elements ahead of time, regardless of when unfolding function returns a
-- `Nothing`. Supplying @n@ that is too big will result in an asynchronous
-- `Control.Exception.Base.HeapOverflow` exception.
--
-- @since 0.5.1
unsafeUnfoldrN ::
     Sz1
  -- ^ @n@ - maximum number of elements that the vector will have
  -> (s -> Maybe (e, s))
  -- ^ Unfolding function. Stops when `Nothing` is returned or maximum number of elements
  -- is reached.
  -> s -- ^ Inititial element.
  -> Vector DS e
unsafeUnfoldrN n f = DSArray . S.unsafeUnfoldrN n f
{-# INLINE unsafeUnfoldrN #-}



-- | /O(n)/ - Same as `unsafeUnfoldrN`, but with monadic generating function.
--
-- /Unsafe/ - This function is unsafe because it will allocate enough space in memory for
-- @n@ elements ahead of time, regardless of when unfolding function returns a
-- `Nothing`. Supplying @n@ that is too big will result in an asynchronous
-- `Control.Exception.Base.HeapOverflow` exception.
--
-- @since 0.5.1
unsafeUnfoldrNM :: Monad m => Sz1 -> (s -> m (Maybe (e, s))) -> s -> m (Vector DS e)
unsafeUnfoldrNM n f = fromStepsM . S.unsafeUnfoldrNM n f
{-# INLINE unsafeUnfoldrNM #-}
