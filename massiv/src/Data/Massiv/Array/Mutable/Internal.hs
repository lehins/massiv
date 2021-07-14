{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Data.Massiv.Array.Mutable.Internal
-- Copyright   : (c) Alexey Kuleshevich 2018-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Mutable.Internal
  ( unsafeCreateArray
  , unsafeCreateArray_
  , unsafeCreateArrayS
  , unsafeLoadIntoS
  , unsafeLoadIntoM
  ) where

import Control.Scheduler
import Data.Massiv.Core.Common

-- | Same as `Data.Massiv.Array.Mutable.createArrayS`, but memory will not be initialized
-- and for unboxed types might contain garbage.
--
-- @since 0.5.0
unsafeCreateArrayS ::
     forall r ix e a m s. (Mutable r e, Index ix, Primal s m)
  => Sz ix -- ^ Size of the newly created array
  -> (MArray r ix e s -> m a)
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
unsafeCreateArray ::
     forall r ix e a m b. (Mutable r e, Index ix, UnliftPrimal RW m)
  => Comp -- ^ Computation strategy to use after `MArray` gets frozen and onward.
  -> Sz ix -- ^ Size of the newly created array
  -> (Scheduler RW a -> MArray r ix e RW -> m b)
  -- ^ An action that should fill all elements of the brand new mutable array
  -> m ([a], Array r ix e)
unsafeCreateArray comp sz action = do
  marr <- unsafeNew sz
  a <- withScheduler comp (`action` marr)
  arr <- unsafeFreeze comp marr
  return (a, arr)
{-# INLINE unsafeCreateArray #-}

-- | Same as `Data.Massiv.Array.Mutable.createArray_`, but memory will not be initialized
-- and for unboxed types might contain garbage.
--
-- @since 0.5.0
unsafeCreateArray_ ::
     forall r ix e m b. (Mutable r e, Index ix, UnliftPrimal RW m)
  => Comp -- ^ Computation strategy to use after `MArray` gets frozen and onward.
  -> Sz ix -- ^ Size of the newly created array
  -> (Scheduler RW () -> MArray r ix e RW -> m b)
  -- ^ An action that should fill all elements of the brand new mutable array
  -> m (Array r ix e)
unsafeCreateArray_ comp sz action = do
  marr <- unsafeNew sz
  withScheduler_ comp (`action` marr)
  arr <- unsafeFreeze comp marr
  return arr
{-# INLINE unsafeCreateArray_ #-}



-- | Load into a supplied mutable array sequentially. Returned array does not
-- have to be the same
--
-- @since 0.5.7
unsafeLoadIntoS ::
     forall r' r ix e m s. (Load r ix e, Mutable r' e, Primal s m)
  => MVector r' e s
  -> Array r ix e
  -> m (MArray r' ix e s)
unsafeLoadIntoS marr = liftST . unsafeLoadIntoST marr
{-# INLINE unsafeLoadIntoS #-}

-- | Same as `unsafeLoadIntoS`, but respecting computation strategy.
--
-- @since 0.5.7
unsafeLoadIntoM ::
     forall r' r ix e m. (Load r ix e, Mutable r' e, Primal RW m)
  => MVector r' e RW
  -> Array r ix e
  -> m (MArray r' ix e RW)
unsafeLoadIntoM marr = liftIO . unsafeLoadIntoIO marr
{-# INLINE unsafeLoadIntoM #-}
