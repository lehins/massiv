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
  ) where

import           Data.Massiv.Array.Manifest.Boxed
import           Data.Massiv.Array.Manifest.Internal
import           Data.Massiv.Array.Manifest.Primitive
import           Data.Massiv.Array.Manifest.Storable
import           Data.Massiv.Array.Manifest.Unboxed


-- $boxed_conversion_note
--
-- Important part of all conversions in this section is that the actual boxed
-- `Data.Primitive.Array.Array` that hold the pointers to values isn't copied around, it is always
-- kept as the same array. Conversion to Massiv boxed array will undergo evaluation during which
-- computation strategies will respected
