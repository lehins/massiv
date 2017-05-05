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
  , computePrimitiveS
  , computePrimitiveP
  , mapM
  , imapM
  ) where

import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Common.Shape
import           Data.Array.Massiv.Manifest.Internal
import           Data.Array.Massiv.Mutable
import qualified Data.Vector.Primitive               as VP
import qualified Data.Vector.Primitive.Mutable       as MVP
import           Prelude                             hiding (mapM)
import           System.IO.Unsafe                    (unsafePerformIO)

data P = P

data instance Array P ix e = PArray { pSize :: !ix
                                    , pData :: !(VP.Vector e)
                                    } deriving Eq

instance (VP.Prim e, Index ix) => Massiv P ix e where
  size = pSize
  {-# INLINE size #-}

  makeArray !sz f = PArray sz' $ VP.generate (totalElem sz') (f . fromLinearIndex sz')
    where
      !sz' = liftIndex (max 0) sz
  {-# INLINE makeArray #-}

instance (VP.Prim e, Index ix) => Source P ix e where
  unsafeLinearIndex (PArray _ v) = VP.unsafeIndex v
  {-# INLINE unsafeLinearIndex #-}


instance (VP.Prim e, Index ix) => Shape P ix e where
  type R P = M

  unsafeReshape !sz !arr = arr { pSize = sz }
  {-# INLINE unsafeReshape #-}

  unsafeExtract !sIx !newSz !arr = unsafeExtract sIx newSz (toManifest arr)
  {-# INLINE unsafeExtract #-}


instance (VP.Prim e, Index ix, Index (Lower ix)) => Slice P ix e where

  (!?>) !arr = (toManifest arr !?>)
  {-# INLINE (!?>) #-}

  (<!?) !arr = (toManifest arr <!?)
  {-# INLINE (<!?) #-}

instance (Index ix, VP.Prim e) => Manifest P ix e where

  unsafeLinearIndexM (PArray _ v) = VP.unsafeIndex v
  {-# INLINE unsafeLinearIndexM #-}


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



computePrimitiveS :: (Massiv r ix e, Load r ix, Mutable P ix e) => Array r ix e -> Array P ix e
computePrimitiveS = computeSeq
{-# INLINE computePrimitiveS #-}


computePrimitiveP :: (Massiv r ix e, Load r ix, Mutable P ix e) => Array r ix e -> Array P ix e
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

