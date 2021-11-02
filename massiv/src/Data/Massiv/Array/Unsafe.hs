{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : Data.Massiv.Array.Unsafe
-- Copyright   : (c) Alexey Kuleshevich 2018-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Unsafe
  ( -- * Creation
    unsafeMakeLoadArray
  , unsafeMakeLoadArrayAdjusted
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
    -- * Slicing
  , unsafeSlice
  , unsafeOuterSlice
  , unsafeInnerSlice
  , unsafeLinearSlice
    -- * Mutable interface
  , unsafeResizeMArray
  , unsafeLinearSliceMArray
  , unsafeThaw
  , unsafeFreeze
  , unsafeNew
  , unsafeLoadIntoST
  , unsafeLoadIntoIO
  , unsafeLoadIntoS
  , unsafeLoadIntoM
  , unsafeCreateArray
  , unsafeCreateArray_
  , unsafeCreateArrayS
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
  , unsafeMallocMArray
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
  , coerceBoxedArray
  , coerceNormalBoxedArray
  , unsafeUnstablePartitionRegionM
  , module Data.Massiv.Vector.Unsafe
  , module Data.Massiv.Array.Stencil.Unsafe
    -- * Constructors
  , Array(PArray, SArray, UArray, BArray, BLArray, BNArray, DArray, DLArray, DSArray, DIArray, DWArray)
  , MArray(MPArray, MSArray, MUArray, MBArray, MBLArray, MBNArray)
  ) where

import Data.Massiv.Array.Delayed.Interleaved (Array(DIArray))
import Data.Massiv.Array.Delayed.Pull (D, unsafeExtract, unsafeInnerSlice,
                                       unsafeSlice)
import Data.Massiv.Array.Delayed.Push (Array(DLArray), unsafeMakeLoadArray,
                                       unsafeMakeLoadArrayAdjusted)
import Data.Massiv.Array.Delayed.Stream (Array(DSArray))
import Data.Massiv.Array.Delayed.Windowed (Array(DWArray))
import Data.Massiv.Array.Manifest.Boxed
import Data.Massiv.Array.Manifest.Internal
import Data.Massiv.Array.Manifest.Primitive
import Data.Massiv.Array.Manifest.Storable
import Data.Massiv.Array.Manifest.Unboxed
import Data.Massiv.Array.Mutable.Internal
import Data.Massiv.Array.Ops.Sort (unsafeUnstablePartitionRegionM)
import Data.Massiv.Array.Stencil.Unsafe
import Data.Massiv.Core.Common
import Data.Massiv.Core.Index.Stride (Stride(SafeStride))
import Data.Massiv.Vector.Unsafe


unsafeBackpermute :: (Index ix', Source r' e, Index ix) =>
                     Sz ix -> (ix -> ix') -> Array r' ix' e -> Array D ix e
unsafeBackpermute !sz ixF !arr = makeArray (getComp arr) sz (unsafeIndex arr . ixF)
{-# INLINE unsafeBackpermute #-}

-- | Same 'Data.Array.transform'', except no bounds checking is performed, thus making it faster,
-- but unsafe.
--
-- @since 0.3.0
unsafeTransform ::
     (Index ix', Source r' e', Index ix)
  => (Sz ix' -> (Sz ix, a))
  -> (a -> (ix' -> e') -> ix -> e)
  -> Array r' ix' e'
  -> Array D ix e
unsafeTransform getSz get arr = makeArray (getComp arr) sz (get a (unsafeIndex arr))
  where
    (sz, a) = getSz (size arr)
{-# INLINE unsafeTransform #-}

-- | Same 'Data.Array.transform2'', except no bounds checking is performed, thus making it faster,
-- but unsafe.
--
-- @since 0.3.0
unsafeTransform2 ::
     (Index ix1, Source r1 e1, Index ix2, Source r2 e2, Index ix)
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
