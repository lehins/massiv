{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitForAll #-}

-- |
-- Module      : Data.Massiv.Array.Mutable.Internal
-- Copyright   : (c) Alexey Kuleshevich 2018-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
module Data.Massiv.Array.Mutable.Internal (
  unsafeCreateArray,
  unsafeCreateArray_,
  unsafeCreateArrayS,
  unsafeCopyMArrayST,
) where

import Control.Scheduler
import Data.Massiv.Core.Common

-- | Same as `Data.Massiv.Array.Mutable.createArrayS`, but memory will not be initialized
-- and for unboxed types might contain garbage.
--
-- @since 0.5.0
unsafeCreateArrayS
  :: forall r ix e a m
   . (Manifest r e, Index ix, PrimMonad m)
  => Sz ix
  -- ^ Size of the newly created array
  -> (MArray (PrimState m) r ix e -> m a)
  -- ^ An action that should fill all elements of the brand new mutable array
  -> m (a, Array r ix e)
unsafeCreateArrayS sz action = do
  marr <- unsafeNew sz
  a <- action marr
  arr <- unsafeFreeze Seq marr
  return (a, arr)
{-# INLINE unsafeCreateArrayS #-}

-- | Same as `Data.Massiv.Array.Mutable.createArray`, but memory will not be initialized
-- and for unboxed types might contain garbage.
--
-- @since 0.5.0
unsafeCreateArray
  :: forall r ix e a m b
   . (Manifest r e, Index ix, MonadUnliftIO m)
  => Comp
  -- ^ Computation strategy to use after `MArray` gets frozen and onward.
  -> Sz ix
  -- ^ Size of the newly created array
  -> (Scheduler RealWorld a -> MArray RealWorld r ix e -> m b)
  -- ^ An action that should fill all elements of the brand new mutable array
  -> m ([a], Array r ix e)
unsafeCreateArray comp sz action = do
  marr <- liftIO $ unsafeNew sz
  a <- withScheduler comp (`action` marr)
  arr <- liftIO $ unsafeFreeze comp marr
  return (a, arr)
{-# INLINE unsafeCreateArray #-}

-- | Same as `Data.Massiv.Array.Mutable.createArray_`, but memory will not be initialized
-- and for unboxed types might contain garbage.
--
-- @since 0.5.0
unsafeCreateArray_
  :: forall r ix e a m b
   . (Manifest r e, Index ix, MonadUnliftIO m)
  => Comp
  -- ^ Computation strategy to use after `MArray` gets frozen and onward.
  -> Sz ix
  -- ^ Size of the newly created array
  -> (Scheduler RealWorld a -> MArray RealWorld r ix e -> m b)
  -- ^ An action that should fill all elements of the brand new mutable array
  -> m (Array r ix e)
unsafeCreateArray_ comp sz action = do
  marr <- liftIO $ unsafeNew sz
  withScheduler_ comp (`action` marr)
  arr <- liftIO $ unsafeFreeze comp marr
  return arr
{-# INLINE unsafeCreateArray_ #-}

-- | Make an unsafe copy while using a scheduler
--
-- @since 1.0.4
unsafeCopyMArrayST
  :: forall r ix e s
   . (Manifest r e, Index ix)
  => Scheduler s ()
  -> Array r ix e
  -> MArray s r ix e
  -> ST s ()
unsafeCopyMArrayST scheduler arr marr = do
  let !totalLength = totalElem (size arr)
  splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
    loopA_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
      scheduleWork_ scheduler $ unsafeArrayLinearCopy arr start marr start (SafeSz chunkLength)
    let slackLength = totalLength - slackStart
    when (slackLength > 0) $
      scheduleWork_ scheduler $
        unsafeArrayLinearCopy arr slackStart marr slackStart (SafeSz slackLength)
{-# INLINE unsafeCopyMArrayST #-}
