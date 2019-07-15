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
  , makeLoadArrayS
  , makeLoadArray
  , unsafeMakeLoadArray
  , fromStrideLoad
  ) where

import Control.Monad
import Data.Massiv.Core.Common
import Data.Massiv.Core.Index.Internal (Sz(SafeSz))
import Prelude hiding (map, zipWith)
import Control.Scheduler as S (traverse_)
import Data.Foldable as F

#include "massiv.h"

-- | Delayed load representation. Also known as Push array.
data DL = DL deriving Show


data instance Array DL ix e = DLArray
  { dlComp    :: !Comp
  , dlSize    :: !(Sz ix)
  , dlDefault :: !(Maybe e)
  , dlLoad    :: forall m . Monad m
              => Scheduler m ()
              -> Int -- start loading at this linear index
              -> (Int -> e -> m ()) -- linear element writing action
              -> m ()
  }

instance Index ix => Construct DL ix e where
  setComp c arr = arr {dlComp = c}
  {-# INLINE setComp #-}
  makeArrayLinear comp sz f =
    DLArray comp sz Nothing $ \scheduler startAt dlWrite ->
      splitLinearlyWithStartAtM_ scheduler startAt (totalElem sz) (pure . f) dlWrite
  {-# INLINE makeArrayLinear #-}

instance Index ix => Resize DL ix where
  unsafeResize !sz arr = arr { dlSize = sz }
  {-# INLINE unsafeResize #-}

instance Semigroup (Array DL Ix1 e) where
  (<>) = mappendDL
  {-# INLINE (<>) #-}

instance Monoid (Array DL Ix1 e) where
  mempty =
    DLArray
      {dlComp = mempty, dlSize = Sz zeroIndex, dlDefault = Nothing, dlLoad = \_ _ _ -> pure ()}
  {-# INLINE mempty #-}
  mappend = mappendDL
  {-# INLINE mappend #-}
  mconcat [] = mempty
  mconcat [x] = x
  mconcat [x, y] = x <> y
  mconcat xs = mconcatDL xs
  {-# INLINE mconcat #-}

mconcatDL :: forall e . [Array DL Ix1 e] -> Array DL Ix1 e
mconcatDL !arrs =
  DLArray {dlComp = foldMap getComp arrs, dlSize = SafeSz k, dlDefault = Nothing, dlLoad = load}
  where
    !k = F.foldl' (+) 0 (unSz . size <$> arrs)
    load :: Monad m => Scheduler m () -> Int -> (Int -> e -> m ()) -> m ()
    load scheduler startAt dlWrite =
      let loadArr !startAtCur DLArray {dlSize = SafeSz kCur, dlDefault, dlLoad} = do
            let !endAtCur = startAtCur + kCur
            scheduleWork_ scheduler $ do
              S.traverse_
                (\def -> loopM_ startAtCur (< endAtCur) (+ 1) (`dlWrite` def))
                dlDefault
              dlLoad scheduler startAtCur dlWrite
            pure endAtCur
          {-# INLINE loadArr #-}
       in foldM_ loadArr startAt arrs
    {-# INLINE load #-}
{-# INLINE mconcatDL #-}


mappendDL :: forall e . Array DL Ix1 e -> Array DL Ix1 e -> Array DL Ix1 e
mappendDL (DLArray c1 sz1 mDef1 load1) (DLArray c2 sz2 mDef2 load2) =
  DLArray {dlComp = c1 <> c2, dlSize = SafeSz (k1 + k2), dlDefault = Nothing, dlLoad = load}
  where
    !k1 = unSz sz1
    !k2 = unSz sz2
    load :: Monad m => Scheduler m () -> Int -> (Int -> e -> m ()) -> m ()
    load scheduler startAt dlWrite = do
      scheduleWork_ scheduler $ do
        S.traverse_ (\def1 -> loopM_ startAt (< k1) (+ 1) (`dlWrite` def1)) mDef1
        load1 scheduler startAt dlWrite
      scheduleWork_ scheduler $ do
        let startAt2 = startAt + k1
        S.traverse_ (\def2 -> loopM_ startAt2 (< startAt2 + k2) (+ 1) (`dlWrite` def2)) mDef2
        load2 scheduler startAt2 dlWrite
    {-# INLINE load #-}
{-# INLINE mappendDL #-}

-- | Describe how an array should be loaded into memory
--
-- @since 0.3.1
makeLoadArrayS ::
     Index ix =>
     Sz ix
  -- ^ Size of the resulting array
  -> e
  -- ^ Default value to use for all cells that have possibly been ommitted by the writing function
  -> (forall m. Monad m => (ix -> e -> m Bool) -> m ())
  -- ^ Writing function that described which elements to write into the target array.
  -> Array DL ix e
makeLoadArrayS sz defVal writer =
  DLArray Seq sz (Just defVal) $ \_scheduler !startAt uWrite ->
    let safeWrite !ix !e
          | isSafeIndex sz ix = uWrite (startAt + toLinearIndex sz ix) e >> pure True
          | otherwise = pure False
        {-# INLINE safeWrite #-}
     in writer safeWrite
{-# INLINE makeLoadArrayS #-}

-- | Specify how an array can be loaded/computed through creation of a `DL` array.
--
-- @since 0.3.0
makeLoadArray ::
     Comp
  -> Sz ix
  -> (forall m. Monad m => Scheduler m () -> Int -> (Int -> e -> m ()) -> m ())
  -> Array DL ix e
makeLoadArray comp sz = DLArray comp sz Nothing
{-# INLINE makeLoadArray #-}
{-# DEPRECATED makeLoadArray "In favor of equivalent `unsafeMakeLoadArray` and safe `makeLoadArrayS`" #-}

-- | Specify how an array can be loaded/computed through creation of a `DL` array. Unlike
-- `makeLoadArrayS` this function is unsafe since there is no guarantee that all elements will be
-- initialized and in case of parallel scheduler there is a possibility of non-determinism.
--
-- @since 0.3.1
unsafeMakeLoadArray ::
     Comp
  -> Sz ix
  -> Maybe e
  -> (forall m. Monad m => Scheduler m () -> Int -> (Int -> e -> m ()) -> m ())
  -> Array DL ix e
unsafeMakeLoadArray = DLArray
{-# INLINE unsafeMakeLoadArray #-}

-- | Convert any `Load`able array into `DL` representation.
--
-- @since 0.3.0
toLoadArray :: Load r ix e => Array r ix e -> Array DL ix e
toLoadArray arr =
  DLArray (getComp arr) (size arr) Nothing $ \scheduler startAt dlWrite ->
    loadArrayM scheduler arr (\ !i -> dlWrite (i + startAt))
{-# INLINE toLoadArray #-}

-- | Convert an array that can be loaded with stride into `DL` representation.
--
-- @since 0.3.0
fromStrideLoad
  :: StrideLoad r ix e => Stride ix -> Array r ix e -> Array DL ix e
fromStrideLoad stride arr =
  DLArray (getComp arr) newsz Nothing $ \scheduler startAt dlWrite ->
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
  defaultElement = dlDefault
  {-# INLINE defaultElement #-}

instance Functor (Array DL ix) where
  fmap f arr =
    arr
      { dlLoad =
          \scheduler startAt uWrite -> dlLoad arr scheduler startAt (\ !i e -> uWrite i (f e))
      , dlDefault = f <$> dlDefault arr
      }
  {-# INLINE fmap #-}
