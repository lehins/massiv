{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module      : Data.Massiv.Array.Manifest
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Manifest
  (
  -- * Manifest
    Manifest
  , toManifest
  , M
  -- * Boxed
  , B(..)
  , N(..)
  , Uninitialized(..)
  -- ** Conversion
  -- $boxed_conversion_note
  , unwrapArray
  , evalArray
  , unwrapMutableArray
  , evalMutableArray
  , unwrapNormalFormArray
  , evalNormalFormArray
  , unwrapNormalFormMutableArray
  , evalNormalFormMutableArray
  -- * Primitive
  , P(..)
  , Prim
  -- ** Conversion
  , toByteArray
  , fromByteArray
  , fromByteArrayM
  , toMutableByteArray
  , fromMutableByteArray
  , fromMutableByteArrayM
  -- * Storable
  , S(..)
  , Storable
  -- ** Conversion
  , toStorableVector
  , toStorableMVector
  -- ** Direct Pointer Access
  , withPtr
  -- * Unboxed
  , U(..)
  , Unbox
  -- ** Conversion
  , toUnboxedVector
  , toUnboxedMVector
  -- * ByteString Conversion
  , fromByteString
  , castFromByteString
  , toByteString
  , castToByteString
  , toBuilder
  ) where

import Data.ByteString as S
import Data.ByteString.Builder
import Data.ByteString.Internal
import Data.ByteString.Unsafe as SU
import Data.Massiv.Array.Manifest.Boxed
import Data.Massiv.Array.Manifest.Internal
import Data.Massiv.Array.Manifest.Primitive
import Data.Massiv.Array.Manifest.Storable
import Data.Massiv.Array.Manifest.Unboxed
import Data.Massiv.Array.Ops.Fold
import Data.Massiv.Core.Common
import Data.Massiv.Core.Index.Internal (Sz(..))
import Data.Word (Word8)


-- | /O(1)/ - Convert a strict ByteString into a manifest array. Will return `Nothing` if length
-- doesn't match the total number of elements of new array.
--
-- @since 0.2.1
fromByteString ::
     Comp -- ^ Computation strategy
  -> ByteString -- ^ Strict ByteString to use as a source.
  -> Array M Ix1 Word8
fromByteString comp bs = MArray comp (SafeSz (S.length bs)) (SU.unsafeIndex bs)
{-# INLINE fromByteString #-}

-- | /O(n)/ - Convert any source array into a strict `ByteString`. In case when the source array is
-- actually storable, no memory copy will occur.
--
-- @since 0.2.1
toByteString ::
     Load r ix Word8
  => Array r ix Word8 -- ^ Source array
  -> ByteString
toByteString = castToByteString .
#if __GLASGOW_HASKELL__ >= 820
  convert
  {- For ghc-8.0 `covert` results in "internal error: ARR_WORDS object entered!" -}
#else
  compute
#endif
  --fst $ unfoldrN (totalElem (size arr)) (\ !i -> Just (unsafeLinearIndex arr i, i + 1)) 0
{-# INLINE toByteString #-}

-- | /O(n)/ - Conversion of array monoidally into a ByteString Builder.
--
-- @since 0.2.1
toBuilder :: Source r ix e => (e -> Builder) -> Array r ix e -> Builder
toBuilder = foldMono
{-# INLINE toBuilder #-}

-- | /O(1)/ - Cast a `S`torable array into a strict `ByteString`
--
-- @since 0.3.0
castToByteString :: Array S ix Word8 -> ByteString
castToByteString = (\(fp, len) -> PS fp 0 len) . unsafeArrayToForeignPtr
{-# INLINE castToByteString #-}

-- | /O(1)/ - Cast a strict `ByteString` into a `S`torable array
--
-- @since 0.3.0
castFromByteString :: Comp -> ByteString -> Array S Ix1 Word8
castFromByteString comp (PS fp offset len) = unsafeArrayFromForeignPtr comp fp offset (Sz len)
{-# INLINE castFromByteString #-}

-- $boxed_conversion_note
--
-- Important part of all conversions in this section is that the actual boxed
-- `Data.Primitive.Array.Array`, which holds the pointers to values isn't copied around, it is always
-- kept as the same array. Conversion to Massiv boxed array will undergo evaluation during which
-- computation strategies will be respected.
