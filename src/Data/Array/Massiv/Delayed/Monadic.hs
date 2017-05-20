{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Array.Massiv.Delayed
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Delayed.Monadic where

import           Control.Monad.Primitive
import           Control.Monad                  (void)
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Common.Shape
import           Data.Array.Massiv.Scheduler
import           Data.Foldable
import Data.Typeable



-- | Delayed representation.
data MD (m :: * -> *)


data instance Array (MD m) ix e = MDArray { mdSize :: !ix
                                          , mdUnsafeIndex :: ix -> m e }



-- | /O(1)/ Conversion from a source array to `D` representation.
delayM :: (Monad m, Source r ix e) => Array r ix e -> Array (MD m) ix e
delayM arr = MDArray (size arr) (return . unsafeIndex arr)
{-# INLINE delayM #-}


instance (Typeable m, Monad m, Index ix) => Massiv (MD m) ix e where
  size = mdSize
  {-# INLINE size #-}

  makeArray sz f = MDArray (liftIndex (max 0) sz) (return . f)
  {-# INLINE makeArray #-}



instance (Typeable m, PrimMonad m, Index ix) => Load (MD m) ix e where
  loadS (MDArray sz f) _ unsafeWrite =
    iterM_ zeroIndex sz 1 (<) $ \ !ix -> do
      v <- f ix
      unsafeWrite (toLinearIndex sz ix) v
  {-# INLINE loadS #-}
  -- loadP (DArray sz f) _ unsafeWrite = do
  --   void $ splitWork sz $ \ !scheduler !chunkLength !totalLength !slackStart -> do
  --     loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
  --       submitRequest scheduler $
  --       JobRequest 0 $
  --       iterLinearM_ sz start (start + chunkLength) 1 (<) $ \ !k !ix -> do
  --         unsafeWrite k $ f ix
  --     submitRequest scheduler $
  --       JobRequest 0 $
  --       iterLinearM_ sz slackStart totalLength 1 (<) $ \ !k !ix -> do
  --         unsafeWrite k (f ix)
  -- {-# INLINE loadP #-}
