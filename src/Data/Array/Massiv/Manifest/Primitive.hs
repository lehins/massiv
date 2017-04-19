{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Array.Massiv.Manifest.Primitive
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Manifest.Primitive
  ( P (..)
  , VP.Prim
  , generateM
  , fromVectorPrimitive
  , toVectorPrimitive
  , fromListsPrimitive
  , computePrimitiveS
  , computePrimitiveP
  , mapM
  , imapM
  ) where

import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Manifest.Internal
import           Data.Array.Massiv.Mutable
import           Data.Maybe                          (listToMaybe)
import qualified Data.Vector.Primitive               as VP
import qualified Data.Vector.Primitive.Mutable       as MVP
import           Prelude                             hiding (mapM)
import           System.IO.Unsafe                    (unsafePerformIO)

data P = P

data instance Array P ix e = PArray { pSize :: !ix
                                    , pData :: !(VP.Vector e)
                                    } deriving Eq

instance Index ix => Massiv P ix where
  size = pSize
  {-# INLINE size #-}


instance (Index ix, VP.Prim e) => Source P ix e where
  unsafeLinearIndex (PArray _ v) = VP.unsafeIndex v
  {-# INLINE unsafeLinearIndex #-}


instance (Index ix, VP.Prim e) => Manifest P ix e


instance (Manifest P ix e, VP.Prim e) => Mutable P ix e where
  data MArray s P ix e = MPArray ix (VP.MVector s e)

  unsafeThaw (PArray sz v) = MPArray sz <$> VP.unsafeThaw v
  {-# INLINE unsafeThaw #-}

  unsafeFreeze (MPArray sz v) = PArray sz <$> VP.unsafeFreeze v
  {-# INLINE unsafeFreeze #-}

  unsafeNew sz = MPArray sz <$> MVP.unsafeNew (totalElem sz)
  {-# INLINE unsafeNew #-}

  unsafeLinearRead (MPArray _sz v) i = MVP.unsafeRead v i
  {-# INLINE unsafeLinearRead #-}

  unsafeLinearWrite (MPArray _sz v) i = MVP.unsafeWrite v i
  {-# INLINE unsafeLinearWrite #-}



fromListsPrimitive :: VP.Prim e => [[e]] -> Array P DIM2 e
fromListsPrimitive !ls =
  if all (== n) (map length ls)
    then PArray (m, n) (VP.fromList $ concat ls)
    else error "fromListsVG:Inner lists are of different lengths."
  where -- TODO: check dims
    (m, n) = (length ls, maybe 0 length $ listToMaybe ls)
{-# INLINE fromListsPrimitive #-}


computePrimitiveS :: (Load r' ix, Mutable P ix e) => Array r' ix e -> Array P ix e
computePrimitiveS = computeSeq
{-# INLINE computePrimitiveS #-}


computePrimitiveP :: (Load r' ix, Mutable P ix e) => Array r' ix e -> Array P ix e
computePrimitiveP = unsafePerformIO . computePar
{-# INLINE computePrimitiveP #-}


fromVectorPrimitive :: Index ix => ix -> VP.Vector e -> Array P ix e
fromVectorPrimitive sz v = PArray { pSize = sz, pData = v }
{-# INLINE fromVectorPrimitive #-}


toVectorPrimitive :: Array P ix e -> VP.Vector e
toVectorPrimitive = pData
{-# INLINE toVectorPrimitive #-}


generateM :: (Index ix, VP.Prim a, Monad m) =>
  ix -> (ix -> m a) -> m (Array P ix a)
generateM sz f =
  PArray sz <$> VP.generateM (totalElem sz) (f . fromLinearIndex sz)
{-# INLINE generateM #-}


mapM :: (VP.Prim b, Source r ix a, Monad m) =>
  (a -> m b) -> Array r ix a -> m (Array P ix b)
mapM f arr = do
  let !sz = size arr
  v <- VP.generateM (totalElem sz) (f . unsafeLinearIndex arr)
  return $ PArray sz v
{-# INLINE mapM #-}

imapM :: (VP.Prim b, Source r ix a, Monad m) =>
  (ix -> a -> m b) -> Array r ix a -> m (Array P ix b)
imapM f arr = do
  let !sz = size arr
  v <- VP.generateM (totalElem sz) $ \ !i ->
         let !ix = fromLinearIndex sz i
         in f ix (unsafeIndex arr ix)
  return $ PArray sz v
{-# INLINE imapM #-}

