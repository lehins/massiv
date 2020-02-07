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
  , length
  , null
  , (!)
  , (!?)
  , head'
  , headM
  , last'
  , lastM
  , slice'
  , sliceM
  , init'
  , initM
  , tail'
  , tailM
  , take'
  , takeM
  ) where

import Control.Monad
import Data.Massiv.Array.Delayed
import Data.Massiv.Array.Ops.Transform
import Data.Massiv.Core.Common
import Prelude hiding (length, null)


-- | Type synonyme for a single dimension array, or simply a flat vector.
--
-- @since 0.5.0
type Vector r e = Array r Ix1 e

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



-- |
--
-- @since 0.5.0
head' :: Source r Ix1 e => Vector r e -> e
head' = (`evaluate'` 0)
{-# INLINE head' #-}


-- |
--
-- @since 0.5.0
headM :: (Source r Ix1 e, MonadThrow m) => Vector r e -> m e
headM = (`evaluateM` 0)
{-# INLINE headM #-}


-- |
--
-- @since 0.5.0
last' :: Source r Ix1 e => Vector r e -> e
last' v = evaluate' v (max 0 (unSz (size v) - 1))
{-# INLINE last' #-}


-- |
--
-- @since 0.5.0
lastM :: (Source r Ix1 e, MonadThrow m) => Vector r e -> m e
lastM v = evaluateM v (max 0 (unSz (size v) - 1))
{-# INLINE lastM #-}


-- |
--
-- @since 0.5.0
slice' :: Extract r Ix1 e => Ix1 -> Sz1 -> Vector r e -> Vector (R r) e
slice' = extract'
{-# INLINE slice' #-}

-- |
--
-- @since 0.5.0
sliceM :: (Extract r Ix1 e, MonadThrow m) => Ix1 -> Sz1 -> Vector r e -> m (Vector (R r) e)
sliceM = extractM
{-# INLINE sliceM #-}


-- |
--
-- @since 0.5.0
init' :: Extract r Ix1 e => Vector r e -> Vector (R r) e
init' = either throw id . initM
{-# INLINE init' #-}

-- |
--
-- @since 0.5.0
initM :: (Extract r Ix1 e, MonadThrow m) => Vector r e -> m (Vector (R r) e)
initM v = do
  when (null v) $ throwM $ SizeEmptyException $ size v
  pure $ unsafeExtract 0 (SafeSz (unSz (size v) - 1)) v
{-# INLINE initM #-}

-- |
--
-- @since 0.5.0
tail' :: Extract r Ix1 e => Vector r e -> Vector (R r) e
tail' = either throw id . tailM
{-# INLINE tail' #-}


-- |
--
-- @since 0.5.0
tailM :: (Extract r Ix1 e, MonadThrow m) => Vector r e -> m (Vector (R r) e)
tailM v = do
  when (null v) $ throwM $ SizeEmptyException $ size v
  pure $ unsafeExtract 1 (SafeSz (unSz (size v) - 1)) v
{-# INLINE tailM #-}


-- TODO: Have functions implemented that don't fail! Eg. take.

-- |
--
-- @since 0.5.0
take' :: Extract r Ix1 e => Sz1 -> Vector r e -> Vector (R r) e
take' k = either throw id . takeM k
{-# INLINE take' #-}

-- |
--
-- @since 0.5.0
takeM :: (Extract r Ix1 e, MonadThrow m) => Sz1 -> Vector r e -> m (Vector (R r) e)
takeM k v = do
  let sz = size v
  when (k > sz) $ throwM $ SizeSubregionException sz 0 k
  pure $ unsafeExtract 0 k v
{-# INLINE takeM #-}



-- TODO: Add to vector: headMaybe
