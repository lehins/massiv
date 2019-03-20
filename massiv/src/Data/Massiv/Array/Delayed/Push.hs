{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
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

import Data.Massiv.Core.Common
import Data.Massiv.Core.Index.Internal (Sz(SafeSz))
import qualified Data.Semigroup as Semigroup
import Prelude hiding (map, zipWith)

#include "massiv.h"

-- | Delayed load representation. Also known as Push array.
data DL = DL deriving Show


data instance Array DL ix e = DLArray
  { dlComp    :: !Comp
  , dlSize    :: !(Sz ix)
  , dlLoad    :: forall m . Monad m
              => Scheduler m ()
              -> Int -- start loading at this linear index
              -> (Int -> e -> m ()) -- linear element writing action
              -> m ()
  }

type instance EltRepr DL ix = DL

instance Index ix => Construct DL ix e where
  setComp c arr = arr {dlComp = c}
  {-# INLINE setComp #-}
  makeArrayLinear comp sz f =
    DLArray comp sz $ \scheduler startAt dlWrite ->
      splitLinearlyWithStartAtM_ scheduler startAt (totalElem sz) (pure . f) dlWrite
  {-# INLINE makeArrayLinear #-}

instance Index ix => Resize DL ix where
  unsafeResize !sz arr = arr { dlSize = sz }
  {-# INLINE unsafeResize #-}

instance Semigroup (Array DL Ix1 e) where
  (<>) (DLArray c1 sz1 load1) (DLArray c2 sz2 load2) =
    DLArray {dlComp = c1 <> c2, dlSize = SafeSz (k + unSz sz2), dlLoad = load}
    where
      !k = unSz sz1
      load :: Monad m => Scheduler m () -> Int -> (Int -> e -> m ()) -> m ()
      load scheduler startAt dlWrite = do
        load1 scheduler startAt dlWrite
        load2 scheduler (startAt + k) dlWrite
      {-# INLINE load #-}
  {-# INLINE (<>) #-}


instance Monoid (Array DL Ix1 e) where
  mempty = makeArray Seq zeroSz (const (throwImpossible Uninitialized))
  {-# INLINE mempty #-}

  mappend = (Semigroup.<>)
  {-# INLINE mappend #-}


-- | Specify how an array can be loaded/computed through creation of a `DL` array.
--
-- @since 0.3.0
makeLoadArray ::
     Comp
  -> Sz ix
  -> (forall m. Monad m => Scheduler m () -> Int -> (Int -> e -> m ()) -> m ())
  -> Array DL ix e
makeLoadArray = DLArray
{-# INLINE makeLoadArray #-}

-- | Convert any `Load`able array into `DL` representation.
--
-- @since 0.3.0
toLoadArray :: Load r ix e => Array r ix e -> Array DL ix e
toLoadArray arr =
  DLArray (getComp arr) (size arr) $ \scheduler startAt dlWrite ->
    loadArrayM scheduler arr (\ !i -> dlWrite (i + startAt))
{-# INLINE toLoadArray #-}

-- | Convert an array that can be loaded with stride into `DL` representation.
--
-- @since 0.3.0
fromStrideLoad
  :: StrideLoad r ix e => Stride ix -> Array r ix e -> Array DL ix e
fromStrideLoad stride arr =
  DLArray (getComp arr) newsz $ \scheduler startAt dlWrite ->
    loadArrayWithStrideM scheduler stride newsz arr (\ !i -> dlWrite (i + startAt))
  where
    newsz = strideSize stride (size arr)
{-# INLINE fromStrideLoad #-}

instance Index ix => Load DL ix e where
  size = dlSize
  {-# INLINE size #-}
  getComp = dlComp
  {-# INLINE getComp #-}
  loadArrayM scheduler DLArray {dlLoad} = dlLoad scheduler 0
  {-# INLINE loadArrayM #-}

instance Functor (Array DL ix) where
  fmap f arr =
    arr
      { dlLoad =
          \scheduler startAt uWrite -> dlLoad arr scheduler startAt (\ !i e -> uWrite i (f e))
      }
  {-# INLINE fmap #-}
