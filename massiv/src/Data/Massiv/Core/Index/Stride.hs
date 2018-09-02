{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
-- |
-- Module      : Data.Massiv.Core.Index.Stride
-- Copyright   : (c) Alexey Kuleshevich 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Core.Index.Stride
  ( Stride(SafeStride, Stride)
  , unStride
  , oneStride
  , toLinearIndexStride
  , strideStart
  , strideSize
  ) where

import           Control.DeepSeq
import           Data.Massiv.Core.Index.Class

-- | Stride provides a way to ignore elements of an array if an index is divisible by a
-- corresponding value in a stride. So, for a @Stride (i :. j)@ only elements with indices will be
-- kept around:
--
-- @@@
-- ( 0 :. 0) ( 0 :. j) ( 0 :. 2j) ( 0 :. 3j) ...
-- ( i :. 0) ( i :. j) ( i :. 2j) ( i :. 3j) ...
-- (2i :. 0) (2i :. j) (2i :. 2j) (2i :. 3j) ...
-- ...
-- @@@
--
-- Only positive strides make sense, so `Stride` pattern synonym constructor will prevent a user
-- from creating a stride with negative or zero values, thus promoting safety of the library.
--
-- ====__Examples:__
--
-- * Default and minimal stride of @Stride (`pureIndex` 1)@ will have no affect and all elements
--   will kept.
--
-- * If stride is @Stride 2@, then every 2nd (with index 1, 3, 5, ..) element will be skipped,
--   i.e. only index divisible by 2 becomes relevant.
--
-- * In case of two dimensions, if you want is to keep all rows divisible by 5, but keep every
--   column intact then you'd use @Stride (5 :. 1)@.
--
newtype Stride ix = SafeStride ix deriving (Eq, Ord, NFData)

instance Index ix => Show (Stride ix) where
  show (SafeStride ix) = "Stride (" ++ show ix ++ ")"


-- | A safe pattern sysnonym for `Stride` construction that will make sure stride elements are
-- positive.
pattern Stride :: Index ix => ix -> Stride ix
pattern Stride ix <- SafeStride ix where
        Stride ix = SafeStride (liftIndex (max 1) ix)

unStride :: Stride ix -> ix
unStride (SafeStride ix) = ix
{-# INLINE unStride #-}

-- | Adjust strating index according to the stride
strideStart :: Index ix => Stride ix -> ix -> ix
strideStart (SafeStride stride) ix =
  liftIndex2
    (+)
    ix
    (liftIndex2 mod (liftIndex2 subtract (liftIndex2 mod ix stride) stride) stride)
{-# INLINE strideStart #-}

-- | Adjust size according to the stride.
strideSize :: Index ix => Stride ix -> ix -> ix
strideSize (SafeStride stride) sz = liftIndex (+ 1) $ liftIndex2 div (liftIndex (subtract 1) sz) stride
{-# INLINE strideSize #-}


toLinearIndexStride :: Index ix =>
  Stride ix -- ^ Stride
  -> ix -- ^ Size
  -> ix -- ^ Index
  -> Int
toLinearIndexStride (SafeStride stride) sz ix = toLinearIndex sz (liftIndex2 div ix stride)
{-# INLINE toLinearIndexStride #-}


-- |  A default stride of 1, where nothing is ignored.
oneStride :: Index ix => Stride ix
oneStride = SafeStride (pureIndex 1)
{-# INLINE oneStride #-}


