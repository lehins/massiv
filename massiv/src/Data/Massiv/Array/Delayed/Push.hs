{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Massiv.Array.Delayed.Push
-- Copyright   : (c) Alexey Kuleshevich 2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Delayed.Push
  ( DL(..)
  , Array(..)
  , toLoadArray
  , makeLoadArray
  , fromStrideLoad
  ) where

import           Data.Massiv.Array.Delayed.Pull
--import           Data.Massiv.Array.Manifest.Boxed
--import           Data.Massiv.Array.Manifest.Internal
import           Data.Massiv.Core.Index.Internal (Sz (SafeSz))
import           Data.Massiv.Core.Common
import           Prelude                             hiding (map, zipWith)


#include "massiv.h"

-- | Delayed load representation. Also known as Push array.
data DL = DL deriving Show


data instance Array DL ix e = DLArray
  { dlComp :: !Comp
  , dlSize :: !(Sz ix)
  , dlLoad :: forall m . Monad m
           => Int
           -> (m () -> m ())
           -> (Int -> e -> m ())
           -> m ()
  }

type instance EltRepr DL ix = DL

instance Index ix => Construct DL ix e where
  setComp c arr = arr {dlComp = c}
  {-# INLINE setComp #-}
  makeArrayLinear comp sz f =
    DLArray comp sz $ \numWorkers scheduleWith dlWrite ->
      splitWith_ numWorkers scheduleWith (SafeSz (totalElem sz)) f dlWrite
  {-# INLINE makeArrayLinear #-}


-- | Specify how an array can be loaded/computed through creation of a `DL` array.
--
-- @since 0.3.0
makeLoadArray ::
     Comp
  -> Sz ix
  -> (forall m. Monad m =>
                  Int -> (m () -> m ()) -> (Int -> e -> m ()) -> m ())
  -> Array DL ix e
makeLoadArray comp sz f = DLArray comp sz f
{-# INLINE makeLoadArray #-}

instance Index ix => Resize Array DL ix where
  unsafeResize !sz arr = arr { dlSize = sz }
  {-# INLINE unsafeResize #-}


-- | Convert any `Load`able array into `DL` representation.
--
-- @since 0.3.0
toLoadArray :: Load r ix e => Array r ix e -> Array DL ix e
toLoadArray arr =
  DLArray (getComp arr) (size arr) $ \numWorkers scheduleWith dlWrite ->
    loadArray numWorkers scheduleWith arr dlWrite
{-# INLINE toLoadArray #-}

-- | Convert an array that can be loaded with stride into `DL` representation.
--
-- @since 0.3.0
fromStrideLoad
  :: StrideLoad r ix e => Stride ix -> Array r ix e -> Array DL ix e
fromStrideLoad stride arr =
  DLArray (getComp arr) newsz $ \numWorkers scheduleWith dlWrite ->
    loadArrayWithStride numWorkers scheduleWith stride newsz arr dlWrite
  where newsz = strideSize stride (size arr)
{-# INLINE fromStrideLoad #-}

instance Index ix => Load DL ix e where
  size = dlSize
  {-# INLINE size #-}
  getComp = dlComp
  {-# INLINE getComp #-}
  loadArray numWorkers scheduleWith DLArray {dlLoad} = dlLoad numWorkers scheduleWith
  {-# INLINE loadArray #-}

instance Functor (Array DL ix) where
  fmap f arr =
    arr
      { dlLoad =
          \numWorkers scheduleWork uWrite ->
            (dlLoad arr) numWorkers scheduleWork (\i e -> uWrite i (f e))
      }
  {-# INLINE fmap #-}

