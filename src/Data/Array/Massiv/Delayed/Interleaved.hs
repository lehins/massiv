{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Array.Massiv.Delayed.Interleaved
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Delayed.Interleaved where

import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Delayed
import           Data.Array.Massiv.Scheduler


-- | Delayed array that will be loaded in an interleaved fasion during parallel
-- computation.
data ID

data instance Array ID ix e = IDArray { idArray :: !(Array D ix e) }

instance Index ix => Massiv ID ix e where
  size (IDArray arr) = size arr

  getComp = dComp . idArray
  {-# INLINE getComp #-}

  setComp arr c = arr { idArray = (idArray arr) { dComp = c } }
  {-# INLINE setComp #-}

  unsafeMakeArray c sz = IDArray . unsafeMakeArray c sz
  {-# INLINE unsafeMakeArray #-}

instance Functor (Array ID ix) where
  fmap f (IDArray arr) = IDArray (fmap f arr)


toInterleaved :: Source r ix e => Array r ix e -> Array ID ix e
toInterleaved = IDArray . delay
{-# INLINE toInterleaved #-}


instance Index ix => Load ID ix e where
  loadS (IDArray arr) unsafeRead unsafeWrite = loadS arr unsafeRead unsafeWrite
  {-# INLINE loadS #-}
  loadP wIds (IDArray (DArray _ sz f)) _ unsafeWrite = do
    scheduler <- makeScheduler wIds
    let !totalLength = totalElem sz
    loopM_ 0 (< numWorkers scheduler) (+ 1) $ \ !start ->
      submitRequest scheduler $
      JobRequest $
      iterLinearM_ sz start totalLength (numWorkers scheduler) (<) $ \ !k !ix ->
        unsafeWrite k $ f ix
    waitTillDone scheduler
  {-# INLINE loadP #-}
