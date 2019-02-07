{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
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
  -- , unsafeGenerateArray
  -- , unsafeGenerateArrayP
  -- * Indexing
    Sz(SafeSz)
  , unsafeIndex
  , unsafeLinearIndex
  , unsafeLinearIndexM
  -- * Manipulations
  , unsafeBackpermute
  , unsafeTraverse
  , unsafeTraverse2
  , unsafeResize
  , unsafeExtract
  -- * Slicing
  , unsafeSlice
  , unsafeOuterSlice
  , unsafeInnerSlice
  -- * Mutable interface
  , unsafeThaw
  , unsafeFreeze
  , unsafeNew
  , unsafeRead
  , unsafeLinearRead
  , unsafeWrite
  , unsafeLinearWrite
  , unsafeLinearSet
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
  ) where

import           Data.Massiv.Array.Delayed.Pull       (D)
import           Data.Massiv.Array.Manifest.Primitive
import           Data.Massiv.Core.Common
import           Data.Massiv.Core.Index.Internal      (Sz (SafeSz))


unsafeBackpermute :: (Load r' ix' e, Source r' ix' e, Index ix) =>
                     Sz ix -> (ix -> ix') -> Array r' ix' e -> Array D ix e
unsafeBackpermute !sz ixF !arr =
  makeArray (getComp arr) sz $ \ !ix -> unsafeIndex arr (ixF ix)
{-# INLINE unsafeBackpermute #-}


unsafeTraverse
  :: (Load r ix' e', Source r ix' e', Index ix)
  => Sz ix
  -> ((ix' -> e') -> ix -> e)
  -> Array r ix' e'
  -> Array D ix e
unsafeTraverse sz f arr1 =
  makeArray (getComp arr1) sz (f (unsafeIndex arr1))
{-# INLINE unsafeTraverse #-}


unsafeTraverse2
  :: (Load r1 ix1 e1, Load r2 ix2 e2, Source r1 ix1 e1, Source r2 ix2 e2, Index ix)
  => Sz ix
  -> ((ix1 -> e1) -> (ix2 -> e2) -> ix -> e)
  -> Array r1 ix1 e1
  -> Array r2 ix2 e2
  -> Array D ix e
unsafeTraverse2 sz f arr1 arr2 =
  makeArray (getComp arr1 <> getComp arr2) sz (f (unsafeIndex arr1) (unsafeIndex arr2))
{-# INLINE unsafeTraverse2 #-}


