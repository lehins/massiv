{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Massiv.Array.Delayed.Interleaved
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Delayed.Interleaved
  ( DI
  , toInterleaved
  ) where

import           Data.Massiv.Array.Delayed.Internal
import           Data.Massiv.Core
import           Data.Massiv.Core.Scheduler


-- | Delayed array that will be loaded in an interleaved fasion during parallel
-- computation.
data DI

data instance Array DI ix e = DIArray { idArray :: !(Array D ix e) }

instance Index ix => Construct DI ix e where
  size (DIArray arr) = size arr

  getComp = dComp . idArray
  {-# INLINE getComp #-}

  setComp c arr = arr { idArray = (idArray arr) { dComp = c } }
  {-# INLINE setComp #-}

  unsafeMakeArray c sz = DIArray . unsafeMakeArray c sz
  {-# INLINE unsafeMakeArray #-}

instance Functor (Array DI ix) where
  fmap f (DIArray arr) = DIArray (fmap f arr)


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


toInterleaved :: Source r ix e => Array r ix e -> Array DI ix e
toInterleaved = DIArray . delay
{-# INLINE toInterleaved #-}
