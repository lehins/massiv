{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
-- Module      : Data.Massiv.Array.Delayed.Interleaved
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Delayed.Interleaved
  ( DI(..)
  , toInterleaved
  , fromInterleaved
  ) where

import Data.Massiv.Array.Delayed.Pull
import Data.Massiv.Core.Common
import Data.Massiv.Core.List (L, showArrayList, showsArrayPrec)


-- | Delayed array that will be loaded in an interleaved fashion during parallel
-- computation.
data DI = DI

newtype instance Array DI ix e = DIArray
  { diArray :: Array D ix e
  } deriving (Eq, Ord, Functor, Applicative, Foldable, Num, Floating, Fractional)

instance (Ragged L ix e, Show e) => Show (Array DI ix e) where
  showsPrec = showsArrayPrec diArray
  showList = showArrayList

instance Index ix => Construct DI ix e where
  makeArray c sz = DIArray . makeArray c sz
  {-# INLINE makeArray #-}

instance Index ix => Resize DI ix where
  unsafeResize sz = DIArray . unsafeResize sz . diArray
  {-# INLINE unsafeResize #-}

instance Index ix => Extract DI ix e where
  unsafeExtract sIx newSz = DIArray . unsafeExtract sIx newSz . diArray
  {-# INLINE unsafeExtract #-}


instance Index ix => Load DI ix e where
  size (DIArray arr) = size arr
  {-# INLINE size #-}
  getComp = dComp . diArray
  {-# INLINE getComp #-}
  setComp c arr = arr { diArray = (diArray arr) { dComp = c } }
  {-# INLINE setComp #-}
  loadArrayM scheduler (DIArray (DArray _ sz f)) uWrite =
    loopM_ 0 (< numWorkers scheduler) (+ 1) $ \ !start ->
      scheduleWork scheduler $
      iterLinearM_ sz start (totalElem sz) (numWorkers scheduler) (<) $ \ !k -> uWrite k . f
  {-# INLINE loadArrayM #-}

instance Index ix => StrideLoad DI ix e where
  loadArrayWithStrideM scheduler stride resultSize arr uWrite =
    let strideIx = unStride stride
        DIArray (DArray _ _ f) = arr
    in loopM_ 0 (< numWorkers scheduler) (+ 1) $ \ !start ->
          scheduleWork scheduler $
          iterLinearM_ resultSize start (totalElem resultSize) (numWorkers scheduler) (<) $
            \ !i ix -> uWrite i (f (liftIndex2 (*) strideIx ix))
  {-# INLINE loadArrayWithStrideM #-}

-- | Convert a source array into an array that, when computed, will have its elemets evaluated out
-- of order (interleaved amongst cores), hence making unbalanced computation better parallelizable.
toInterleaved :: Source r ix e => Array r ix e -> Array DI ix e
toInterleaved = DIArray . delay
{-# INLINE toInterleaved #-}

-- | /O(1)/ - Unwrap the interleved array.
--
-- @since 0.2.1
fromInterleaved :: Array DI ix e -> Array D ix e
fromInterleaved = diArray
{-# INLINE fromInterleaved #-}
