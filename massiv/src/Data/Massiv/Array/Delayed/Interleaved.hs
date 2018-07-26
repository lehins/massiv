{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DeriveGeneric         #-}
-- |
-- Module      : Data.Massiv.Array.Delayed.Interleaved
-- Copyright   : (c) Alexey Kuleshevich 2018
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

import           Data.Massiv.Array.Delayed.Internal
import           Data.Massiv.Core.Common
import           Data.Massiv.Core.Scheduler
import           GHC.Generics (Generic)


-- | Delayed array that will be loaded in an interleaved fasion during parallel
-- computation.
data DI = DI deriving (Generic)

type instance EltRepr DI ix = DI

newtype instance Array DI ix e = DIArray { diArray :: (Array D ix e) } deriving (Generic)

instance Index ix => Construct DI ix e where
  getComp = dComp . diArray
  {-# INLINE getComp #-}

  setComp c arr = arr { diArray = (diArray arr) { dComp = c } }
  {-# INLINE setComp #-}

  unsafeMakeArray c sz = DIArray . unsafeMakeArray c sz
  {-# INLINE unsafeMakeArray #-}

instance Functor (Array DI ix) where
  fmap f (DIArray arr) = DIArray (fmap f arr)

instance Index ix => Size DI ix e where
  size (DIArray arr) = size arr
  {-# INLINE size #-}

  unsafeResize sz = DIArray . unsafeResize sz . diArray
  {-# INLINE unsafeResize #-}

  unsafeExtract sIx newSz = DIArray . unsafeExtract sIx newSz . diArray
  {-# INLINE unsafeExtract #-}


instance Index ix => Load DI ix e where
  loadS (DIArray arr) unsafeRead unsafeWrite = loadS arr unsafeRead unsafeWrite
  {-# INLINE loadS #-}
  loadP wIds (DIArray (DArray _ sz f)) _ unsafeWrite =
    withScheduler_ wIds $ \ scheduler -> do
      let !totalLength = totalElem sz
      loopM_ 0 (< numWorkers scheduler) (+ 1) $ \ !start ->
        scheduleWork scheduler $
        iterLinearM_ sz start totalLength (numWorkers scheduler) (<) $ \ !k !ix ->
          unsafeWrite k $ f ix
  {-# INLINE loadP #-}

-- | Convert a source array into an array that, when computed, will have its elemets evaluated out
-- of order (interleaved amoungs cores), hence making unbalanced computation better parallelizable.
toInterleaved :: Source r ix e => Array r ix e -> Array DI ix e
toInterleaved = DIArray . delay
{-# INLINE toInterleaved #-}

-- | /O(1)/ - Unwrap the interleved array.
--
-- @since 0.2.1
fromInterleaved :: Array DI ix e -> Array D ix e
fromInterleaved = diArray
{-# INLINE fromInterleaved #-}
