{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : Data.Massiv.Array.Ops.Unsafe
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Unsafe
  ( unsafeBackpermute
  , unsafeTraverse
  , unsafeTraverse2
  , Construct(unsafeMakeArray)
  , Source(..)
  , Size(unsafeResize, unsafeExtract)
  , Slice(..)
  , OuterSlice(..)
  , InnerSlice(..)
  , Manifest(..)
  , Mutable(unsafeThaw, unsafeFreeze, unsafeNew, unsafeLinearRead, unsafeLinearWrite)
  , Ragged(unsafeGenerateM)
  ) where

import           Data.Massiv.Array.Delayed.Internal (D)
import           Data.Massiv.Core.Common


unsafeBackpermute :: (Source r' ix' e, Index ix) =>
                     ix -> (ix -> ix') -> Array r' ix' e -> Array D ix e
unsafeBackpermute !sz ixF !arr =
  unsafeMakeArray (getComp arr) sz $ \ !ix -> unsafeIndex arr (ixF ix)
{-# INLINE unsafeBackpermute #-}


unsafeTraverse
  :: (Source r1 ix1 e1, Index ix)
  => ix
  -> ((ix1 -> e1) -> ix -> e)
  -> Array r1 ix1 e1
  -> Array D ix e
unsafeTraverse sz f arr1 =
  unsafeMakeArray (getComp arr1) sz (f (unsafeIndex arr1))
{-# INLINE unsafeTraverse #-}


unsafeTraverse2
  :: (Source r1 ix1 e1, Source r2 ix2 e2, Index ix)
  => ix
  -> ((ix1 -> e1) -> (ix2 -> e2) -> ix -> e)
  -> Array r1 ix1 e1
  -> Array r2 ix2 e2
  -> Array D ix e
unsafeTraverse2 sz f arr1 arr2 =
  unsafeMakeArray (getComp arr1) sz (f (unsafeIndex arr1) (unsafeIndex arr2))
{-# INLINE unsafeTraverse2 #-}
