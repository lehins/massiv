{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Data.Massiv.Core.Index.Stride
-- Copyright   : (c) Alexey Kuleshevich 2018-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
module Data.Massiv.Core.Index.Stride (
  Stride (SafeStride),
  pattern Stride,
  unStride,
  oneStride,
  toLinearIndexStride,
  strideStart,
  strideSize,
) where

import Control.DeepSeq (NFData)
import Data.Massiv.Core.Index.Internal
import System.Random.Stateful (Random, Uniform (..), UniformRange (..))
import GHC.Generics 

-- | Stride provides a way to ignore elements of an array if an index is divisible by a
-- corresponding value in a stride. So, for a @Stride (i :. j)@ only elements with indices will be
-- kept around:
--
-- @
-- ( 0 :. 0) ( 0 :. j) ( 0 :. 2j) ( 0 :. 3j) ...
-- ( i :. 0) ( i :. j) ( i :. 2j) ( i :. 3j) ...
-- (2i :. 0) (2i :. j) (2i :. 2j) (2i :. 3j) ...
-- ...
-- @
--
-- Only positive strides make sense, so `Stride` pattern synonym constructor will prevent a user
-- from creating a stride with negative or zero values, thus promoting safety of the library.
--
-- ====__Examples:__
--
-- * Default and minimal stride of @`Stride` (`pureIndex` 1)@ will have no affect and all elements
--   will kept.
--
-- * If stride is @`Stride` 2@, then every 2nd element (i.e. with index 1, 3, 5, ..) will be skipped
--   and only elemnts with indices divisible by 2 will be kept around.
--
-- * In case of two dimensions, if what you want is to keep all rows divisible by 5, but keep every
--   column intact then you'd use @Stride (5 :. 1)@.
--
-- @since 0.2.1
newtype Stride ix = SafeStride ix deriving (Eq, Ord, NFData, Generic)

-- | A safe bidirectional pattern synonym for `Stride` construction that will make sure stride
-- elements are always positive.
--
-- @since 0.2.1
pattern Stride :: Index ix => ix -> Stride ix
pattern Stride ix <- SafeStride ix
  where
    Stride ix = SafeStride (liftIndex (max 1) ix)

{-# COMPLETE Stride #-}

instance Index ix => Show (Stride ix) where
  showsPrec n (SafeStride ix) = showsPrecWrapped n (("Stride " ++) . showsPrec 1 ix)

instance (UniformRange ix, Index ix) => Uniform (Stride ix) where
  uniformM g = SafeStride <$> uniformRM (pureIndex 1, pureIndex maxBound) g
  {-# INLINE uniformM #-}

instance UniformRange ix => UniformRange (Stride ix) where
  uniformRM (SafeStride l, SafeStride u) g = SafeStride <$> uniformRM (l, u) g
  {-# INLINE uniformRM #-}

instance (UniformRange ix, Index ix) => Random (Stride ix)

-- | Just a helper function for unwrapping `Stride`.
--
-- @since 0.2.1
unStride :: Stride ix -> ix
unStride (SafeStride ix) = ix
{-# INLINE unStride #-}

-- | Adjust starting index according to the stride
--
-- @since 0.2.1
strideStart :: Index ix => Stride ix -> ix -> ix
strideStart (SafeStride stride) ix =
  liftIndex2
    (+)
    ix
    (liftIndex2 mod (liftIndex2 subtract (liftIndex2 mod ix stride) stride) stride)
{-# INLINE strideStart #-}

-- | Adjust size according to the stride.
--
-- @since 0.2.1
strideSize :: Index ix => Stride ix -> Sz ix -> Sz ix
strideSize (SafeStride stride) (SafeSz sz) =
  SafeSz (liftIndex (+ 1) $ liftIndex2 div (liftIndex (subtract 1) sz) stride)
{-# INLINE strideSize #-}

-- | Compute linear index with stride using the original size and index
--
-- @since 0.2.1
toLinearIndexStride
  :: Index ix
  => Stride ix
  -- ^ Stride
  -> Sz ix
  -- ^ Size
  -> ix
  -- ^ Index
  -> Int
toLinearIndexStride (SafeStride stride) sz ix = toLinearIndex sz (liftIndex2 div ix stride)
{-# INLINE toLinearIndexStride #-}

-- | A default stride of @1@, where all elements are kept
--
-- @since 0.2.1
oneStride :: Index ix => Stride ix
oneStride = SafeStride (pureIndex 1)
{-# INLINE oneStride #-}
