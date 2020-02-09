{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Data.Massiv.Vector.Unsafe
-- Copyright   : (c) Alexey Kuleshevich 2020
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
  ) where

import Data.Coerce
import Data.Massiv.Core.Common

-- ========= --
-- Accessors --
-- ========= --

--------------
-- Indexing --
--------------


-- |
--
-- @since 0.5.0
unsafeHead :: Source r Ix1 e => Vector r e -> e
unsafeHead = (`unsafeLinearIndex` 0)
{-# INLINE unsafeHead #-}

-- |
--
-- @since 0.5.0
unsafeLast :: Source r Ix1 e => Vector r e -> e
unsafeLast v = unsafeLinearIndex v (max 0 (unSz (size v) - 1))
{-# INLINE unsafeLast #-}

----------------------
-- Monadic indexing --
----------------------

-- |
--
-- @since 0.5.0
unsafeIndexM :: (Source r Ix1 e, Monad m) => Vector r e -> Ix1 -> m e
unsafeIndexM v i = pure $! unsafeLinearIndex v i
{-# INLINE unsafeIndexM #-}


-- |
--
-- @since 0.5.0
unsafeHeadM :: Monad m => Source r Ix1 e => Vector r e -> m e
unsafeHeadM v = pure $! unsafeHead v
{-# INLINE unsafeHeadM #-}

-- |
--
-- @since 0.5.0
unsafeLastM :: Monad m => Source r Ix1 e => Vector r e -> m e
unsafeLastM v = pure $! unsafeLast v
{-# INLINE unsafeLastM #-}


-------------
-- Slicing --
-------------


-- |
--
-- @since 0.5.0
unsafeInit :: Source r Ix1 e => Vector r e -> Vector r e
unsafeInit v = unsafeLinearSlice 0 (SafeSz (coerce (size v) - 1)) v
{-# INLINE unsafeInit #-}


-- |
--
-- @since 0.5.0
unsafeTail :: Source r Ix1 e => Vector r e -> Vector r e
unsafeTail = unsafeDrop 1
{-# INLINE unsafeTail #-}


-- |
--
-- @since 0.5.0
unsafeTake :: Source r Ix1 e => Sz1 -> Vector r e -> Vector r e
unsafeTake = unsafeLinearSlice 0
{-# INLINE unsafeTake #-}

-- |
--
-- @since 0.5.0
unsafeDrop :: Source r Ix1 e => Sz1 -> Vector r e -> Vector r e
unsafeDrop (Sz d) v = unsafeLinearSlice d (SafeSz (coerce (size v) - d)) v
{-# INLINE unsafeDrop #-}

