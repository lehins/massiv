{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Massiv.Array.Manifest
-- Copyright   : (c) Alexey Kuleshevich 2018-2025
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
module Data.Massiv.Array.Manifest (
  -- * Manifest
  Manifest,

  -- ** Generate
  generateArray,
  generateArrayLinear,
  generateArrayS,
  generateArrayLinearS,
  generateSplitSeedArray,

  -- ** Stateful worker threads
  generateArrayWS,
  generateArrayLinearWS,

  -- ** Unfold
  unfoldrPrimM_,
  iunfoldrPrimM_,
  unfoldrPrimM,
  iunfoldrPrimM,
  unfoldlPrimM_,
  iunfoldlPrimM_,
  unfoldlPrimM,
  iunfoldlPrimM,

  -- ** Mapping
  forPrimM,
  forPrimM_,
  iforPrimM,
  iforPrimM_,
  iforLinearPrimM,
  iforLinearPrimM_,
  for2PrimM_,
  ifor2PrimM_,

  -- * Boxed
  B (..),
  BL (..),
  BN (..),
  N,
  pattern N,
  Uninitialized (..),

  -- ** Access
  findIndex,

  -- ** Conversion
  -- $boxed_conversion_note
  toLazyArray,
  evalLazyArray,
  forceLazyArray,
  unwrapNormalForm,
  evalNormalForm,

  -- *** Primitive Boxed Array
  unwrapLazyArray,
  wrapLazyArray,
  unwrapArray,
  evalArray,
  unwrapMutableArray,
  unwrapMutableLazyArray,
  evalMutableArray,
  unwrapNormalFormArray,
  evalNormalFormArray,
  unwrapNormalFormMutableArray,
  evalNormalFormMutableArray,

  -- *** Boxed Vector
  toBoxedVector,
  toBoxedMVector,
  fromBoxedVector,
  fromBoxedMVector,
  evalBoxedVector,
  evalBoxedMVector,

  -- * Primitive
  P (..),
  Prim,

  -- ** Conversion

  -- *** Primitive ByteArray
  toByteArray,
  toByteArrayM,
  unwrapByteArray,
  unwrapByteArrayOffset,
  fromByteArray,
  fromByteArrayM,
  fromByteArrayOffsetM,
  toMutableByteArray,
  unwrapMutableByteArray,
  unwrapMutableByteArrayOffset,
  fromMutableByteArray,
  fromMutableByteArrayM,
  fromMutableByteArrayOffsetM,

  -- *** Primitive Vector
  toPrimitiveVector,
  toPrimitiveMVector,
  fromPrimitiveVector,
  fromPrimitiveMVector,

  -- * Storable
  S (..),
  Storable,
  mallocCompute,
  mallocCopy,

  -- ** Conversion

  -- *** Storable Vector
  toStorableVector,
  toStorableMVector,
  fromStorableVector,
  fromStorableMVector,

  -- *** Direct Pointer Access
  withPtr,

  -- * Unboxed
  U (..),
  Unbox,

  -- ** Conversion

  -- *** Unboxed Vector
  toUnboxedVector,
  toUnboxedMVector,
  fromUnboxedVector,
  fromUnboxedMVector,

  -- * ByteString Conversion
  fromByteString,
  castFromByteString,
  toByteString,
  castToByteString,
  toBuilder,
  castToBuilder,
) where

import Control.Monad
import Data.ByteString as S hiding (findIndex)
import Data.ByteString.Builder
import Data.ByteString.Internal
import Data.ByteString.Unsafe as SU
import Data.Massiv.Array.Manifest.Boxed
import Data.Massiv.Array.Manifest.Internal
import Data.Massiv.Array.Manifest.Primitive
import Data.Massiv.Array.Manifest.Storable
import Data.Massiv.Array.Manifest.Unboxed
import Data.Massiv.Array.Mutable
import Data.Massiv.Array.Ops.Fold
import Data.Massiv.Core.Common
import Data.Word (Word8)

-- | /O(n)/ - Convert a strict ByteString into a manifest array. Will return `Nothing` if length
-- doesn't match the total number of elements of new array.
--
-- @since 0.2.1
fromByteString
  :: Load r Ix1 Word8
  => Comp
  -- ^ Computation strategy
  -> ByteString
  -- ^ Strict ByteString to use as a source.
  -> Vector r Word8
fromByteString comp bs = makeArrayLinear comp (SafeSz (S.length bs)) (SU.unsafeIndex bs)
{-# INLINE fromByteString #-}

-- | /O(n)/ - Convert any source array into a strict `ByteString`. In case when the source array is
-- actually storable, no memory copy will occur.
--
-- @since 0.2.1
toByteString ::
     Load r ix Word8
  => Array r ix Word8 -- ^ Source array
  -> ByteString
toByteString = castToByteString . convert
{-# INLINE toByteString #-}

-- | /O(n)/ - Conversion of array monoidally into a ByteString `Builder`.
--
-- @since 0.2.1
toBuilder :: (Index ix, Source r e) => (e -> Builder) -> Array r ix e -> Builder
toBuilder = foldMono
{-# INLINE toBuilder #-}

-- | /O(1)/ - Cast a storable array of `Word8` to ByteString `Builder`.
--
-- @since 0.5.0
castToBuilder :: Index ix => Array S ix Word8 -> Builder
castToBuilder = byteString . castToByteString
{-# INLINE castToBuilder #-}

-- | /O(1)/ - Cast a `S`torable array into a strict `ByteString`
--
-- @since 0.3.0
castToByteString :: Index ix => Array S ix Word8 -> ByteString
castToByteString = (\(fp, len) -> PS fp 0 len) . unsafeArrayToForeignPtr
{-# INLINE castToByteString #-}

-- | /O(1)/ - Cast a strict `ByteString` into a `S`torable array
--
-- @since 0.3.0
castFromByteString :: Comp -> ByteString -> Vector S Word8
castFromByteString comp (PS fp offset len) = unsafeArrayFromForeignPtr comp fp offset (Sz len)
{-# INLINE castFromByteString #-}

-- $boxed_conversion_note
--
-- Important part of all conversions in this section is that the actual boxed
-- `Data.Primitive.Array.Array`, which holds the pointers to values isn't copied around, it is always
-- kept as the same array. Conversion to Massiv boxed array will undergo evaluation during which
-- computation strategies will be respected.

-- | /O(n)/ - Perform a row-major search starting at @0@ for an element. Returns the index
-- of the first occurance of an element or `Nothing` if a predicate could not be satisifed
-- after it was applyied to all elements of the array.
--
-- @since 0.5.5
findIndex :: (Index ix, Manifest r e) => (e -> Bool) -> Array r ix e -> Maybe ix
findIndex f arr = go 0
  where
    !sz = size arr
    !k = totalElem sz
    go !i = do
      guard (i < k)
      if f (unsafeLinearIndex arr i)
        then Just $ fromLinearIndex sz i
        else go (i + 1)
{-# INLINE findIndex #-}

-- | Very similar to @`computeAs` `S`@ except load the source array into memory allocated
-- with @malloc@ on C heap. It can potentially be useful when iteroperating with some C
-- programs.
--
-- @since 0.5.9
mallocCompute
  :: forall r ix e. (Size r, Load r ix e, Storable e) => Array r ix e -> IO (Array S ix e)
mallocCompute arr = do
  let sz = size arr
  marr <- unsafeMallocMArray sz
  computeInto marr arr
  unsafeFreeze (getComp arr) marr
{-# INLINE mallocCompute #-}

-- | Allocate memory on C heap with @malloc@ and copy the source array over.
--
-- @since 0.5.9
mallocCopy :: forall ix e. (Index ix, Storable e) => Array S ix e -> IO (Array S ix e)
mallocCopy arr = do
  let sz = size arr
  marr <- unsafeMallocMArray sz
  unsafeArrayLinearCopy arr 0 marr 0 (SafeSz (totalElem sz))
  unsafeFreeze (getComp arr) marr
{-# INLINE mallocCopy #-}
