{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- |
-- Module      : Data.Massiv.Array.Mutable
-- Copyright   : (c) Alexey Kuleshevich 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Mutable
  ( Mutable
  , MArray
  , msize
  , new
  , thaw
  , freeze
  , read
  , read'
  , write
  , write'
  , modify
  , modify'
  , swap
  , swap'
  ) where

import           Prelude                  hiding (read)

import           Control.Monad            (unless)
import           Control.Monad.Primitive  (PrimMonad (..))
import           Data.Massiv.Array.Manifest.Internal
import           Data.Massiv.Array.Unsafe
import           Data.Massiv.Core.Common

-- errorSizeMismatch fName sz1 sz2 =
--   error $ fName ++ ": Size mismatch: " ++ show sz1 ++ " /= " ++ show sz2
-- -- TODO: make sure copy is done in parallel as well as sequentially
-- copy mTargetArr sourceArr = do
--   unless (msize mTargetArr == size sourceArr) $
--     errorSizeMismatch "Data.Massiv.Array.Mutable.copy" (msize mTargetArr) (size sourceArr)
--   mSourdceArray <- unsafeThaw sourceArray
--   -- comp from marr
--   -- TODO: use load
--   imapM_ (unsafeWrite)



-- | Initialize a new mutable array. Negative size will result in an empty array.
new :: (Mutable r ix e, PrimMonad m) => ix -> m (MArray (PrimState m) r ix e)
new sz = unsafeNewZero (liftIndex (max 0) sz)
{-# INLINE new #-}

-- | /O(n)/ - Yield a mutable copy of the immutable array
thaw :: (Mutable r ix e, PrimMonad m) => Array r ix e -> m (MArray (PrimState m) r ix e)
thaw = unsafeThaw . clone
{-# INLINE thaw #-}

-- | /O(n)/ - Yield an immutable copy of the mutable array
freeze :: (Mutable r ix e, PrimMonad m) => Comp -> MArray (PrimState m) r ix e -> m (Array r ix e)
freeze comp marr = unsafeFreeze comp marr >>= (return . clone)
{-# INLINE freeze #-}


-- | /O(1)/ - Lookup an element in the mutable array. Return `Nothing` when index is out of bounds.
read :: (Mutable r ix e, PrimMonad m) =>
        MArray (PrimState m) r ix e -> ix -> m (Maybe e)
read marr ix =
  if isSafeIndex (msize marr) ix
  then Just <$> unsafeRead marr ix
  else return Nothing
{-# INLINE read #-}


-- | /O(1)/ - Same as `read`, but throws an error if index is out of bounds.
read' :: (Mutable r ix e, PrimMonad m) =>
        MArray (PrimState m) r ix e -> ix -> m e
read' marr ix = do
  mval <- read marr ix
  case mval of
    Just e  -> return e
    Nothing -> errorIx "Data.Massiv.Array.Mutable.read'" (msize marr) ix
{-# INLINE read' #-}


-- | /O(1)/ - Write an element into the cell of a mutable array. Returns `False` when index is out
-- of bounds.
write :: (Mutable r ix e, PrimMonad m) =>
        MArray (PrimState m) r ix e -> ix -> e -> m Bool
write marr ix e =
  if isSafeIndex (msize marr) ix
  then unsafeWrite marr ix e >> return True
  else return False
{-# INLINE write #-}


-- | /O(1)/ - Same as `write`, but throws an error if index is out of bounds.
write' :: (Mutable r ix e, PrimMonad m) =>
        MArray (PrimState m) r ix e -> ix -> e -> m ()
write' marr ix e =
  write marr ix e >>= (`unless` errorIx "Data.Massiv.Array.Mutable.write'" (msize marr) ix)
{-# INLINE write' #-}


-- | /O(1)/ - Modify an element in the cell of a mutable array with a supplied function. Returns
-- `False` when index is out of bounds.
modify :: (Mutable r ix e, PrimMonad m) =>
        MArray (PrimState m) r ix e -> (e -> e) -> ix -> m Bool
modify marr f ix =
  if isSafeIndex (msize marr) ix
  then do
    val <- unsafeRead marr ix
    unsafeWrite marr ix $ f val
    return True
  else return False
{-# INLINE modify #-}


-- | /O(1)/ - Same as `modify`, but throws an error if index is out of bounds.
modify' :: (Mutable r ix e, PrimMonad m) =>
        MArray (PrimState m) r ix e -> (e -> e) -> ix -> m ()
modify' marr f ix =
  modify marr f ix >>= (`unless` errorIx "Data.Massiv.Array.Mutable.modify'" (msize marr) ix)
{-# INLINE modify' #-}


-- | /O(1)/ - Swap two elements in a mutable array by supplying their indices. Returns `False` when
-- either one of the indices is out of bounds.
swap :: (Mutable r ix e, PrimMonad m) =>
        MArray (PrimState m) r ix e -> ix -> ix -> m Bool
swap marr ix1 ix2 = do
  let sz = msize marr
  if isSafeIndex sz ix1 && isSafeIndex sz ix2
  then do
    val1 <- unsafeRead marr ix1
    val2 <- unsafeRead marr ix2
    unsafeWrite marr ix1 val2
    unsafeWrite marr ix2 val1
    return True
  else return False
{-# INLINE swap #-}


-- | /O(1)/ - Same as `swap`, but throws an error if index is out of bounds.
swap' :: (Mutable r ix e, PrimMonad m) =>
        MArray (PrimState m) r ix e -> ix -> ix -> m ()
swap' marr ix1 ix2 = do
  success <- swap marr ix1 ix2
  unless success $
    errorIx "Data.Massiv.Array.Mutable.swap'" (msize marr) $
    if isSafeIndex (msize marr) ix1
      then ix2
      else ix1
{-# INLINE swap' #-}

