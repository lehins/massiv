{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Massiv.Array.Manifest.Foreign.Internal
-- Copyright   : (c) Alexey Kuleshevich 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Manifest.Foreign.Internal
  ( F(..)
  , Array(..)
  , fUnsafeLinearIndex
  , MFArray(..)
  , mfUnsafeNew
  , mfUnsafeLinearRead
  , mfUnsafeLinearWrite
  ) where

import           Control.Monad.Primitive
import           Data.Massiv.Array.Manifest.Internal
import           Data.Massiv.Core.Common
import           Foreign.ForeignPtr
import           Foreign.Storable
import           System.IO.Unsafe


-- | Representation for `Storable` elements
data F = F deriving Show

data instance Array F ix e = FArray { fComp   :: !Comp
                                    , fMArray :: MArray RealWorld F ix e
                                    }

type instance EltRepr F ix = M

newtype MFArray e = MFArray { unMFArray :: ForeignPtr e }


fUnsafeLinearIndex :: (Mutable F ix a) => Array F ix a -> Int -> a
fUnsafeLinearIndex (FArray _ ma) i = unsafePerformIO $ unsafeLinearRead ma i
{-# INLINE fUnsafeLinearIndex #-}


mfUnsafeNew :: (PrimMonad m, Storable e, Index ix) => ix -> m (MFArray e)
mfUnsafeNew sz = unsafePrimToPrim $ do
  fp <- mallocForeignPtrArray (totalElem sz)
  return $ MFArray fp
{-# INLINE mfUnsafeNew #-}

mfUnsafeLinearRead :: (PrimMonad m, Storable a) => MFArray a -> Int -> m a
mfUnsafeLinearRead (MFArray fp) i = unsafePrimToPrim $ withForeignPtr fp (`peekElemOff` i)
{-# INLINE mfUnsafeLinearRead #-}

mfUnsafeLinearWrite :: (PrimMonad m, Storable a) => MFArray a -> Int -> a -> m ()
mfUnsafeLinearWrite (MFArray fp) i e =
  unsafePrimToPrim $ withForeignPtr fp (\ptr -> pokeElemOff ptr i e)
{-# INLINE mfUnsafeLinearWrite #-}


