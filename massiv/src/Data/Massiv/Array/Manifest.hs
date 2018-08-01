{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- |
-- Module      : Data.Massiv.Array.Manifest
-- Copyright   : (c) Alexey Kuleshevich 2018
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
  , toArray
  , fromArray
  , toMutableArray
  , fromMutableArray
  , unwrapNormalFormArray
  , unwrapNormalFormMutableArray
  , fromNormalFormArray
  , toNormalFormArray
  , fromNormalFormMutableArray
  , toNormalFormMutableArray
  -- * Primitive
  , P(..)
  , Prim
  -- ** Conversion
  , toByteArray
  , fromByteArray
  , toMutableByteArray
  , fromMutableByteArray
  -- * Storable
  , S(..)
  , Storable
  -- ** Conversion
  , toStorableVector
  , toStorableMVector
  -- ** Direct Pointer Access
  , withPtr
  , unsafeWithPtr
  -- * Unboxed
  , U(..)
  , Unbox
  -- ** Conversion
  , toUnboxedVector
  , toUnboxedMVector
  -- * ByteString Conversion
  , fromByteString
  , toByteString
  , toBuilder
  ) where

import           Data.ByteString                      as S
import           Data.ByteString.Builder
import           Data.ByteString.Unsafe               as SU
import           Data.Massiv.Array.Manifest.Boxed
import           Data.Massiv.Array.Manifest.Internal
import           Data.Massiv.Array.Manifest.Primitive
import           Data.Massiv.Array.Manifest.Storable
import           Data.Massiv.Array.Manifest.Unboxed
import           Data.Massiv.Array.Ops.Fold
import           Data.Massiv.Core.Common
import           Data.Word                            (Word8)



-- | /O(1)/ - Convert a strict ByteString into a manifest array. Will return `Nothing` if length
-- doesn't match the total number of elements of new array.
--
-- @since 0.2.1
fromByteString ::
     Comp -- ^ Computation strategy
  -> ByteString -- ^ Strict ByteString to use as a source.
  -> Array M Ix1 Word8
fromByteString comp bs = MArray comp (S.length bs) (SU.unsafeIndex bs)
{-# INLINE fromByteString #-}

-- | /O(n)/ - For now only sequenctially convert an array into a strict ByteString
--
-- @since 0.2.1
toByteString ::
  Source r ix Word8
  => Array r ix Word8 -- ^ Source array
  -> ByteString
toByteString arr =
  fst $ unfoldrN (totalElem (size arr)) (\ !i -> Just (unsafeLinearIndex arr i, i + 1)) 0
{-# INLINE toByteString #-}

-- | /O(n)/ - Conversion of array monoidally into a string Builder.
--
-- @since 0.2.1
toBuilder :: Source r ix e => (e -> Builder) -> Array r ix e -> Builder
toBuilder f = foldMono f
{-# INLINE toBuilder #-}


-- $boxed_conversion_note
--
-- Important part of all conversions in this section is that the actual boxed
-- `Data.Primitive.Array.Array` that hold the pointers to values isn't copied around, it is always
-- kept as the same array. Conversion to Massiv boxed array will undergo evaluation during which
-- computation strategies will respected
