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
  , toBoxedArray
  , fromBoxedArray
  , toMutableBoxedArray
  , fromMutableBoxedArray
  -- * Primitive
  , P(..)
  , Prim
  , toByteArray
  , fromByteArray
  , toMutableByteArray
  , fromMutableByteArray
  -- * Storable
  , S(..)
  , Storable
  , withPtr
  , unsafeWithPtr
  -- * Unboxed
  , U(..)
  , Unbox
  ) where

import           Data.Massiv.Array.Manifest.Boxed
import           Data.Massiv.Array.Manifest.Internal
import           Data.Massiv.Array.Manifest.Primitive
import           Data.Massiv.Array.Manifest.Storable
import           Data.Massiv.Array.Manifest.Unboxed
