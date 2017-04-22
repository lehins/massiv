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
  , unsafeLinearWrite'
  ) where

import           Control.DeepSeq
import           Control.Monad.Primitive             (PrimMonad (..))
import           Control.Monad.ST                    (runST)
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Manifest.Internal




class Manifest r ix e => Mutable r ix e where
  data MArray s r ix e :: *

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

  computeSeq :: (Massiv r' ix e, Load r' ix) => Array r' ix e -> Array r ix e
  computeSeq !arr = runST $ do
    mArr <- unsafeNew (size arr)
    loadS arr (unsafeLinearRead mArr) (unsafeLinearWrite mArr)
    unsafeFreeze mArr
  {-# INLINE computeSeq #-}

  computePar :: (Massiv r' ix e, Load r' ix) => Array r' ix e -> IO (Array r ix e)
  computePar !arr = do
    mArr <- unsafeNew (size arr)
    loadP arr (unsafeLinearRead mArr) (unsafeLinearWrite mArr)
    unsafeFreeze mArr
  {-# INLINE computePar #-}


unsafeLinearWrite' :: (NFData e, PrimMonad m, Mutable r ix e) =>
                      MArray (PrimState m) r ix e -> Int -> e -> m ()
unsafeLinearWrite' mv i e = e `deepseq` unsafeLinearWrite mv i e
{-# INLINE unsafeLinearWrite' #-}


