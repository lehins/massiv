{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Data.Massiv.Vector
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Vector
  ( Vector
  , MVector
  -- * Accessors
  -- *** Size
  , length
  , null
  -- *** Indexing
  , (!)
  , (!?)
  , head'
  , last'
  -- *** Monadic Indexing
  , indexM
  , headM
  , lastM
  -- ** Slicing
  , slice
  , slice'
  , sliceM
  -- *** Init
  , init
  , init'
  , initM
  -- *** Tail
  , tail
  , tail'
  , tailM
  -- *** Take
  , take
  , take'
  , takeM
  -- *** Drop
  , drop
  , drop'
  , dropM
  -- *** SplitAt
  , splitAt
  , splitAt'
  , splitAtM
  -- * Construction
  -- ** Initialization
  , empty
  , singleton
  , replicate
  , generate
  , iterateN
  -- ** Monadic initialization
  , replicateM
  , generateM
  , iterateNM
  -- , create
  -- , createT
  -- ** Unfolding
  , unfoldr
  , unfoldrN
  ) where

import qualified Data.Massiv.Array.Manifest.Vector.Stream as S
import Control.Monad hiding (replicateM)
import Data.Coerce
import Data.Massiv.Core.Common hiding (empty, singleton)
import qualified Data.Massiv.Core.Common as A (empty, singleton)
import Data.Massiv.Array.Delayed.Pull
import Data.Massiv.Array.Delayed.Stream
import qualified Data.Massiv.Array.Ops.Construct as A (replicate)
import Prelude hiding (drop, init, length, null, splitAt, tail, take, replicate)


-- | Type synonym for a single dimension array, or simply a flat vector.
--
-- @since 0.5.0
type Vector r e = Array r Ix1 e


-- | Type synonym for a single dimension mutable array, or simply a flat mutable vector.
--
-- @since 0.5.0
type MVector s r e = MArray s r Ix1 e


-- ========= --
-- Accessors --
-- ========= --


------------------------
-- Length information --
------------------------

-- |
--
-- @since 0.5.0
length :: Load r Ix1 e => Vector r e -> Sz1
length = size
{-# INLINE length #-}


-- |
--
-- @since 0.5.0
null :: Load r Ix1 e => Vector r e -> Bool
null = isEmpty
{-# INLINE null #-}

--------------
-- Indexing --
--------------

-- TODO: Add to vector: headMaybe

-- |
--
-- @since 0.5.0
head' :: Source r Ix1 e => Vector r e -> e
head' = (`evaluate'` 0)
{-# INLINE head' #-}

-- |
--
-- @since 0.5.0
last' :: Source r Ix1 e => Vector r e -> e
last' v = evaluate' v (max 0 (unSz (size v) - 1))
{-# INLINE last' #-}


----------------------
-- Monadic indexing --
----------------------


-- |
--
-- @since 0.5.0
headM :: (Source r Ix1 e, MonadThrow m) => Vector r e -> m e
headM = (`evaluateM` 0)
{-# INLINE headM #-}

-- |
--
-- @since 0.5.0
lastM :: (Source r Ix1 e, MonadThrow m) => Vector r e -> m e
lastM v = evaluateM v (max 0 (unSz (size v) - 1))
{-# INLINE lastM #-}


-- |
--
-- @since 0.5.0
slice :: Source r Ix1 e => Ix1 -> Sz1 -> Vector r e -> Vector r e
slice !i (Sz k) v = unsafeLinearSlice i' newSz v
  where
    !i' = min n (max 0 i)
    !newSz = SafeSz (min (n - i') k)
    Sz n = size v
{-# INLINE slice #-}

-- |
--
-- @since 0.5.0
slice' :: Source r Ix1 e => Ix1 -> Sz1 -> Vector r e -> Vector r e
slice' i k = either throw id . sliceM i k
{-# INLINE slice' #-}

-- |
--
-- @since 0.5.0
sliceM :: (Source r Ix1 e, MonadThrow m) => Ix1 -> Sz1 -> Vector r e -> m (Vector r e)
sliceM i newSz@(Sz k) v
  | i >= 0 && k <= n - i = pure $ unsafeLinearSlice i newSz v
  | otherwise = throwM $ SizeSubregionException sz i newSz
  where
    sz@(Sz n) = size v
{-# INLINE sliceM #-}


-- |
--
-- @since 0.5.0
unsafeSlice :: Source r Ix1 e => Ix1 -> Sz1 -> Vector r e -> Vector r e
unsafeSlice = unsafeLinearSlice
{-# INLINE unsafeSlice #-}

-- |
--
-- @since 0.5.0
init :: Source r Ix1 e => Vector r e -> Vector r e
init v = unsafeLinearSlice 0 (Sz (coerce (size v) - 1)) v
{-# INLINE init #-}

-- |
--
-- @since 0.5.0
init' :: Source r Ix1 e => Vector r e -> Vector r e
init' = either throw id . initM
{-# INLINE init' #-}

-- |
--
-- @since 0.5.0
initM :: (Source r Ix1 e, MonadThrow m) => Vector r e -> m (Vector r e)
initM v = do
  when (null v) $ throwM $ SizeEmptyException $ size v
  pure $ unsafeInit v
{-# INLINE initM #-}


-- |
--
-- @since 0.5.0
unsafeInit :: Source r Ix1 e => Vector r e -> Vector r e
unsafeInit v = unsafeLinearSlice 0 (SafeSz (coerce (size v) - 1)) v
{-# INLINE unsafeInit #-}



-- |
--
-- @since 0.5.0
tail :: Source r Ix1 e => Vector r e -> Vector r e
tail = drop 1
{-# INLINE tail #-}


-- |
--
-- @since 0.5.0
tail' :: Source r Ix1 e => Vector r e -> Vector r e
tail' = either throw id . tailM
{-# INLINE tail' #-}


-- |
--
-- @since 0.5.0
tailM :: (Source r Ix1 e, MonadThrow m) => Vector r e -> m (Vector r e)
tailM v = do
  when (null v) $ throwM $ SizeEmptyException $ size v
  pure $ unsafeTail v
{-# INLINE tailM #-}


-- |
--
-- @since 0.5.0
unsafeTail :: Source r Ix1 e => Vector r e -> Vector r e
unsafeTail = unsafeDrop 1
{-# INLINE unsafeTail #-}



-- |
--
-- @since 0.5.0
take :: Source r Ix1 e => Sz1 -> Vector r e -> Vector r e
take k = fst . splitAt k
{-# INLINE take #-}

-- |
--
-- @since 0.5.0
take' :: Source r Ix1 e => Sz1 -> Vector r e -> Vector r e
take' k = either throw id . takeM k
{-# INLINE take' #-}

-- |
--
-- @since 0.5.0
takeM :: (Source r Ix1 e, MonadThrow m) => Sz1 -> Vector r e -> m (Vector r e)
takeM k v = do
  let sz = size v
  when (k > sz) $ throwM $ SizeSubregionException sz 0 k
  pure $ unsafeTake k v
{-# INLINE takeM #-}

-- |
--
-- @since 0.5.0
unsafeTake :: Source r Ix1 e => Sz1 -> Vector r e -> Vector r e
unsafeTake = unsafeLinearSlice 0
{-# INLINE unsafeTake #-}

-- |
--
-- @since 0.5.0
drop :: Source r Ix1 e => Sz1 -> Vector r e -> Vector r e
drop k = snd . splitAt k
{-# INLINE drop #-}

-- |
--
-- @since 0.5.0
drop' :: Source r Ix1 e => Sz1 -> Vector r e -> Vector r e
drop' k = either throw id . dropM k
{-# INLINE drop' #-}

-- |
--
-- @since 0.5.0
dropM :: (Source r Ix1 e, MonadThrow m) => Sz1 -> Vector r e -> m (Vector r e)
dropM k@(Sz d) v = do
  let sz@(Sz n) = size v
  when (k > sz) $ throwM $ SizeSubregionException sz d (sz - k)
  pure $ unsafeLinearSlice d (SafeSz (n - d)) v
{-# INLINE dropM #-}


-- |
--
-- @since 0.5.0
unsafeDrop :: Source r Ix1 e => Sz1 -> Vector r e -> Vector r e
unsafeDrop (Sz d) v = unsafeLinearSlice d (SafeSz (coerce (size v) - d)) v
{-# INLINE unsafeDrop #-}


-- |
--
-- @since 0.5.0
splitAt :: Source r Ix1 e => Sz1 -> Vector r e -> (Vector r e, Vector r e)
splitAt (Sz k) v = (unsafeTake d v, unsafeDrop d v)
  where
    !n = coerce (size v)
    !d = SafeSz (min k n)
{-# INLINE splitAt #-}

-- |
--
-- @since 0.5.0
splitAt' :: Source r Ix1 e => Sz1 -> Vector r e -> (Vector r e, Vector r e)
splitAt' k = either throw id . splitAtM k
{-# INLINE splitAt' #-}

-- |
--
-- @since 0.5.0
splitAtM :: (Source r Ix1 e, MonadThrow m) => Sz1 -> Vector r e -> m (Vector r e, Vector r e)
splitAtM k v = do
  l <- takeM k v
  pure (l, unsafeDrop k v)
{-# INLINE splitAtM #-}


-- |
--
-- @since 0.5.0
empty :: Construct r Ix1 e => Vector r e
empty = A.empty
{-# INLINE empty #-}

-- |
--
-- @since 0.5.0
singleton :: Construct r Ix1 e => e -> Vector r e
singleton = A.singleton
{-# INLINE singleton #-}

-- |
--
-- @since 0.5.0
generate :: Comp -> Sz1 -> (Ix1 -> e) -> Vector D e
generate = makeArrayLinear
{-# INLINE generate #-}


-- | Replicate the same element
--
-- @since 0.5.0
replicate :: Comp -> Sz1 -> e -> Vector D e
replicate = A.replicate
{-# INLINE replicate #-}

-- |
--
-- @since 0.5.0
iterateN :: Sz1 -> (e -> e) -> e -> Vector DS e
iterateN n f a = fromSteps $ S.iterateN (unSz n) f a
{-# INLINE iterateN #-}


-- |
--
-- @since 0.5.0
replicateM :: Monad m => Sz1 -> m e -> m (Vector DS e)
replicateM n f = fromStepsM $ S.replicateM (unSz n) f
{-# INLINE replicateM #-}


-- |
--
-- @since 0.5.0
generateM :: Monad m => Sz1 -> (Ix1 -> m e) -> m (Vector DS e)
generateM n f = fromStepsM $ S.generateM (unSz n) f
{-# INLINE generateM #-}


-- |
--
-- @since 0.5.0
iterateNM :: Monad m => Sz1 -> (e -> m e) -> e -> m (Vector DS e)
iterateNM n f a = fromStepsM $ S.iterateNM (unSz n) f a
{-# INLINE iterateNM #-}

