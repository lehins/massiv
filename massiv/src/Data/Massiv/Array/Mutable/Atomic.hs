{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Massiv.Array.Mutable.Atomic
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Mutable.Atomic
  ( -- * Atomic element-wise mutation
    atomicReadIntArray
  , atomicWriteIntArray
  , atomicModifyIntArray
  , atomicAddIntArray
  , atomicSubIntArray
  , atomicAndIntArray
  , atomicNandIntArray
  , atomicOrIntArray
  , atomicXorIntArray
  , casIntArray
  ) where

import Control.Monad.Primitive
import Data.Massiv.Array.Manifest.Primitive
import Data.Massiv.Core.Common

-- Atomic operations

-- | Atomically read an `Int` element from the array
--
-- @since 0.3.0
atomicReadIntArray ::
     (Index ix, PrimMonad m) => MArray (PrimState m) P ix Int -> ix -> m (Maybe Int)
atomicReadIntArray marr ix
  | isSafeIndex (msize marr) ix = Just <$> unsafeAtomicReadIntArray marr ix
  | otherwise = pure Nothing
{-# INLINE atomicReadIntArray #-}


-- | Atomically write an `Int` element int the array. Returns `True` if supplied index was correct
-- and write was successfull.
--
-- @since 0.3.0
atomicWriteIntArray ::
     (Index ix, PrimMonad m) => MArray (PrimState m) P ix Int -> ix -> Int -> m Bool
atomicWriteIntArray marr ix f
  | isSafeIndex (msize marr) ix = unsafeAtomicWriteIntArray marr ix f >> pure True
  | otherwise = pure False
{-# INLINE atomicWriteIntArray #-}


-- | Atomically CAS (Compare-and-Swap) an `Int` in the array. Returns the old value.
--
-- @since 0.3.0
casIntArray ::
     (Index ix, PrimMonad m)
  => MArray (PrimState m) P ix Int -- ^ Array to mutate
  -> ix -- ^ Index at which to mutate
  -> Int -- ^ Expected value
  -> Int -- ^ New value
  -> m (Maybe Int)
casIntArray marr ix e n
  | isSafeIndex (msize marr) ix = Just <$> unsafeCasIntArray marr ix e n
  | otherwise = pure Nothing
{-# INLINE casIntArray #-}


-- | Atomically modify an `Int` element of the array. Returns the old value, unless the
-- supplied index was out of bounds.
--
-- @since 0.3.0
atomicModifyIntArray ::
     (Index ix, PrimMonad m) => MArray (PrimState m) P ix Int -> ix -> (Int -> Int) -> m (Maybe Int)
atomicModifyIntArray marr ix f
  | isSafeIndex (msize marr) ix = Just <$> unsafeAtomicModifyIntArray marr ix f
  | otherwise = pure Nothing
{-# INLINE atomicModifyIntArray #-}


-- | Atomically add to an `Int` element in the array. Returns the old value.
--
-- @since 0.3.0
atomicAddIntArray ::
     (Index ix, PrimMonad m) => MArray (PrimState m) P ix Int -> ix -> Int -> m (Maybe Int)
atomicAddIntArray marr ix e
  | isSafeIndex (msize marr) ix = Just <$> unsafeAtomicAddIntArray marr ix e
  | otherwise = pure Nothing
{-# INLINE atomicAddIntArray #-}


-- | Atomically subtract from an `Int` element in the array. Returns the old value.
--
-- @since 0.3.0
atomicSubIntArray ::
     (Index ix, PrimMonad m) => MArray (PrimState m) P ix Int -> ix -> Int -> m (Maybe Int)
atomicSubIntArray marr ix e
  | isSafeIndex (msize marr) ix = Just <$> unsafeAtomicSubIntArray marr ix e
  | otherwise = pure Nothing
{-# INLINE atomicSubIntArray #-}


-- | Atomically AND an `Int` element in the array. Returns the old value.
--
-- @since 0.3.0
atomicAndIntArray ::
     (Index ix, PrimMonad m) => MArray (PrimState m) P ix Int -> ix -> Int -> m (Maybe Int)
atomicAndIntArray marr ix e
  | isSafeIndex (msize marr) ix = Just <$> unsafeAtomicAndIntArray marr ix e
  | otherwise = pure Nothing
{-# INLINE atomicAndIntArray #-}


-- | Atomically NAND an `Int` element in the array. Returns the old value.
--
-- @since 0.3.0
atomicNandIntArray ::
     (Index ix, PrimMonad m) => MArray (PrimState m) P ix Int -> ix -> Int -> m (Maybe Int)
atomicNandIntArray marr ix e
  | isSafeIndex (msize marr) ix = Just <$> unsafeAtomicNandIntArray marr ix e
  | otherwise = pure Nothing
{-# INLINE atomicNandIntArray #-}


-- | Atomically OR an `Int` element in the array. Returns the old value.
--
-- @since 0.3.0
atomicOrIntArray ::
     (Index ix, PrimMonad m) => MArray (PrimState m) P ix Int -> ix -> Int -> m (Maybe Int)
atomicOrIntArray marr ix e
  | isSafeIndex (msize marr) ix = Just <$> unsafeAtomicOrIntArray marr ix e
  | otherwise = pure Nothing
{-# INLINE atomicOrIntArray #-}


-- | Atomically XOR an `Int` element in the array. Returns the old value.
--
-- @since 0.3.0
atomicXorIntArray ::
     (Index ix, PrimMonad m) => MArray (PrimState m) P ix Int -> ix -> Int -> m (Maybe Int)
atomicXorIntArray marr ix e
  | isSafeIndex (msize marr) ix = Just <$> unsafeAtomicXorIntArray marr ix e
  | otherwise = pure Nothing
{-# INLINE atomicXorIntArray #-}
