{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Data.Array.Massiv.Mutable
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Mutable
  ( Mutable(..)
  , Target(..)
  , loadTargetS
  , loadTargetOnP
  , unsafeRead
  , unsafeWrite
  , sequenceP
  , sequenceOnP
  ) where

import           Control.Monad.Primitive             (PrimMonad (..))
import           Control.Monad.ST                    (runST)
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Common.Ops
import           Data.Array.Massiv.Manifest.Internal
import           Data.Array.Massiv.Scheduler




class Manifest r ix e => Mutable r ix e where
  data MArray s r ix e :: *

  -- | Get the size of a mutable array.
  msize :: MArray s r ix e -> ix

  unsafeThaw :: PrimMonad m =>
                Array r ix e -> m (MArray (PrimState m) r ix e)

  unsafeFreeze :: PrimMonad m =>
                  MArray (PrimState m) r ix e -> m (Array r ix e)

  unsafeNew :: PrimMonad m =>
               ix -> m (MArray (PrimState m) r ix e)

  unsafeLinearRead :: PrimMonad m =>
                      MArray (PrimState m) r ix e -> Int -> m e

  unsafeLinearWrite :: PrimMonad m =>
                       MArray (PrimState m) r ix e -> Int -> e -> m ()


class Mutable r ix e => Target r ix e where

  unsafeTargetRead :: PrimMonad m =>
                      MArray (PrimState m) r ix e -> Int -> m e
  unsafeTargetRead = unsafeLinearRead
  {-# INLINE unsafeTargetRead #-}

  unsafeTargetWrite :: PrimMonad m =>
                       MArray (PrimState m) r ix e -> Int -> e -> m ()
  unsafeTargetWrite = unsafeLinearWrite
  {-# INLINE unsafeTargetWrite #-}


loadTargetS :: (Load r' ix e, Target r ix e) =>
               Array r' ix e -> Array r ix e
loadTargetS !arr =
  runST $ do
    mArr <- unsafeNew (size arr)
    loadS arr (unsafeTargetRead mArr) (unsafeTargetWrite mArr)
    unsafeFreeze mArr
{-# INLINE loadTargetS #-}

loadTargetOnP :: (Load r' ix e, Target r ix e) =>
                 [Int] -> Array r' ix e -> IO (Array r ix e)
loadTargetOnP wIds !arr = do
  mArr <- unsafeNew (size arr)
  loadP wIds arr (unsafeTargetRead mArr) (unsafeTargetWrite mArr)
  unsafeFreeze mArr
{-# INLINE loadTargetOnP #-}


unsafeRead :: (Mutable r ix e, PrimMonad m) =>
               MArray (PrimState m) r ix e -> ix -> m e
unsafeRead !marr !ix = unsafeLinearRead marr (toLinearIndex (msize marr) ix)
{-# INLINE unsafeRead #-}

unsafeWrite :: (Mutable r ix e, PrimMonad m) =>
               MArray (PrimState m) r ix e -> ix -> e -> m ()
unsafeWrite !marr !ix = unsafeLinearWrite marr (toLinearIndex (msize marr) ix)
{-# INLINE unsafeWrite #-}


sequenceOnP :: (Source r1 ix (IO e), Mutable r ix e) =>
               [Int] -> Array r1 ix (IO e) -> IO (Array r ix e)
sequenceOnP wIds !arr = do
  resArrM <- unsafeNew (size arr)
  scheduler <- makeScheduler wIds
  iforM_ arr $ \ !ix action ->
    submitRequest scheduler $ JobRequest (action >>= unsafeWrite resArrM ix)
  waitTillDone scheduler
  unsafeFreeze resArrM
{-# INLINE sequenceOnP #-}


sequenceP :: (Source r1 ix (IO e), Mutable r ix e) => Array r1 ix (IO e) -> IO (Array r ix e)
sequenceP = sequenceOnP []
{-# INLINE sequenceP #-}


