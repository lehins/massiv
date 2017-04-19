{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
-- |
-- Module      : Data.Array.Massiv.Manifest.Internal
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Manifest.Internal
  ( M
  , Manifest
  , Array(..)
  ) where

import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Common.Shape
import           Data.Foldable


-- | Manifest arrays are backed by actual memory and values are looked up versus
-- computed as it is with delayed arrays. Because of this fact indexing functions
-- `(!)`, `(!?)`, etc. are constrained to manifest arrays only.
class Source r ix e => Manifest r ix e where




-- | Manifest representation
data M

data instance Array M ix e = MArray { mSize :: !ix
                                    , mUnsafeLinearIndex :: Int -> e }

instance Index ix => Massiv M ix where
  size = mSize
  {-# INLINE size #-}

instance Massiv M ix => Foldable (Array M ix) where
  foldr f acc (MArray sz g) =
    loop (totalElem sz - 1) (>= 0) (subtract 1) acc $ \i accI -> f (g i) accI
  {-# INLINE foldr #-}
  foldr' f !acc (MArray sz g) =
    loop (totalElem sz - 1) (>= 0) (subtract 1) acc $ \ !i !accI -> f (g i) accI
  {-# INLINE foldr' #-}
  foldl f acc (MArray sz g) =
    loop 0 (< totalElem sz) (+ 1) acc $ \i accI -> f accI (g i)
  {-# INLINE foldl #-}
  foldl' f !acc (MArray sz g) =
    loop 0 (< totalElem sz) (+ 1) acc $ \ !i !accI -> f accI (g i)
  {-# INLINE foldl' #-}
  length = totalElem . size
  {-# INLINE length #-}



instance Index ix => Source M ix e where
  unsafeLinearIndex = mUnsafeLinearIndex
  {-# INLINE unsafeLinearIndex #-}


instance Index ix => Manifest M ix e


instance Index ix => Shape M ix e where

  unsafeReshape !sz !arr = arr { mSize = sz }
  {-# INLINE unsafeReshape #-}

  unsafeExtract !sIx !newSz !arr =
    MArray newSz $ \ !i ->
      unsafeIndex arr (liftIndex2 (+) (fromLinearIndex newSz i) sIx)
  {-# INLINE unsafeExtract #-}


instance (Index ix, Index (Lower ix)) => Slice M ix e where

  (!?>) !arr !i
    | isSafeIndex m i = Just (MArray szL (\ !k -> unsafeLinearIndex arr (k + kStart)))
    | otherwise = Nothing
    where
      !sz = size arr
      !(m, szL) = unconsDim sz
      !kStart = toLinearIndex sz (consDim i (zeroIndex :: Lower ix))
  {-# INLINE (!?>) #-}

  (<!?) !arr !i
    | isSafeIndex m i = Just (MArray szL (\ !k -> unsafeLinearIndex arr (k * m + kStart)))
    | otherwise = Nothing
    where
      !sz = size arr
      !(szL, m) = unsnocDim sz
      !kStart = toLinearIndex sz (snocDim (zeroIndex :: Lower ix) i)
  {-# INLINE (<!?) #-}
