{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
-- |
-- Module      : Data.Massiv.Array.Unsafe
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Unsafe
  ( -- * Creation
    unsafeMakeLoadArray
    -- * Indexing
  , Sz(SafeSz)
  , Stride(SafeStride)
  , unsafeIndex
  , unsafeLinearIndex
  , unsafeLinearIndexM
    -- * Manipulations
  , unsafeBackpermute
  , unsafeResize
  , unsafeExtract
  , unsafeTransform
  , unsafeTransform2
  , unsafeLiftArray2
    -- * Slicing
  , unsafeSlice
  , unsafeOuterSlice
  , unsafeInnerSlice
  , unsafeLinearSlice
  , unsafeMutableSlice
    -- * Mutable interface
  , unsafeThaw
  , unsafeFreeze
  , unsafeNew
    -- ** Read
  , unsafeRead
  , unsafeLinearRead
    -- ** Write
  , unsafeWrite
  , unsafeLinearWrite
    -- ** Modify
  , unsafeModify
  , unsafeLinearModify
    -- ** Swap
  , unsafeSwap
  , unsafeLinearSwap
    -- ** Range modification
  , unsafeLinearSet
  , unsafeLinearCopy
  , unsafeArrayLinearCopy
    -- ** Resizing
  , unsafeLinearShrink
  , unsafeLinearGrow
    -- * Pointer access
  , unsafeWithPtr
  , unsafeArrayToForeignPtr
  , unsafeMArrayToForeignPtr
  , unsafeArrayFromForeignPtr
  , unsafeArrayFromForeignPtr0
  , unsafeMArrayFromForeignPtr
  , unsafeMArrayFromForeignPtr0
    -- ** Atomic Operations
  , unsafeAtomicReadIntArray
  , unsafeAtomicWriteIntArray
  , unsafeAtomicModifyIntArray
  , unsafeAtomicAddIntArray
  , unsafeAtomicSubIntArray
  , unsafeAtomicAndIntArray
  , unsafeAtomicNandIntArray
  , unsafeAtomicOrIntArray
  , unsafeAtomicXorIntArray
  , unsafeCasIntArray
    -- ** Other operations
  , unsafeUnstablePartitionRegionM
  ) where

import Data.Massiv.Array.Delayed.Pull (D)
import Data.Massiv.Array.Delayed.Push (unsafeMakeLoadArray)
import Data.Massiv.Array.Manifest.Primitive
import Data.Massiv.Array.Manifest.Storable
import Data.Massiv.Core.Common
import Data.Massiv.Core.Index.Internal (Sz(SafeSz))
import Data.Massiv.Core.Index.Stride (Stride(SafeStride))
import Data.Massiv.Array.Ops.Sort (unsafeUnstablePartitionRegionM)

unsafeBackpermute :: (Source r' ix' e, Index ix) =>
                     Sz ix -> (ix -> ix') -> Array r' ix' e -> Array D ix e
unsafeBackpermute !sz ixF !arr =
  makeArray (getComp arr) sz $ \ !ix -> unsafeIndex arr (ixF ix)
{-# INLINE unsafeBackpermute #-}

-- | Same `Data.Array.transform'`, except no bounds checking is performed, thus making it faster,
-- but unsafe.
--
-- @since 0.3.0
unsafeTransform ::
     (Source r' ix' e', Index ix)
  => (Sz ix' -> (Sz ix, a))
  -> (a -> (ix' -> e') -> ix -> e)
  -> Array r' ix' e'
  -> Array D ix e
unsafeTransform getSz get arr = makeArray (getComp arr) sz (get a (unsafeIndex arr))
  where
    (sz, a) = getSz (size arr)
{-# INLINE unsafeTransform #-}

-- | Same `Data.Array.transform2'`, except no bounds checking is performed, thus making it faster,
-- but unsafe.
--
-- @since 0.3.0
unsafeTransform2 ::
     (Source r1 ix1 e1, Source r2 ix2 e2, Index ix)
  => (Sz ix1 -> Sz ix2 -> (Sz ix, a))
  -> (a -> (ix1 -> e1) -> (ix2 -> e2) -> ix -> e)
  -> Array r1 ix1 e1
  -> Array r2 ix2 e2
  -> Array D ix e
unsafeTransform2 getSz get arr1 arr2 =
  makeArray (getComp arr1 <> getComp arr2) sz (get a (unsafeIndex arr1) (unsafeIndex arr2))
  where
    (sz, a) = getSz (size arr1) (size arr2)
{-# INLINE unsafeTransform2 #-}
