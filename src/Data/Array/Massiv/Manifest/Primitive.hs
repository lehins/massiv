{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
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
  -- , generateM
  -- , fromVectorPrimitive
  -- , toVectorPrimitive
  -- , computePrimitiveS
  -- , computePrimitiveP
  -- , mapM
  -- , imapM
  ) where

import           Control.DeepSeq                     (NFData (..), deepseq)
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Common.Shape
import           Data.Array.Massiv.Delayed           (D)
import           Data.Array.Massiv.Manifest.Internal
import           Data.Array.Massiv.Mutable
import qualified Data.Vector.Primitive               as VP
import qualified Data.Vector.Primitive.Mutable       as MVP
import           Prelude                             hiding (mapM)
--import           System.IO.Unsafe                    (unsafePerformIO)

data P = P

data instance Array P ix e = PArray { pComp :: Comp
                                    , pSize :: !ix
                                    , _pData :: !(VP.Vector e)
                                    }

instance (Index ix, NFData e) => NFData (Array P ix e) where
  rnf (PArray c sz v) = c `deepseq` sz `deepseq` v `deepseq` ()
  {-# INLINE rnf #-}


instance (VP.Prim e, Index ix) => Massiv P ix e where
  size = pSize
  {-# INLINE size #-}

  getComp = pComp
  {-# INLINE getComp #-}

  setComp arr c = arr { pComp = c }
  {-# INLINE setComp #-}

  unsafeMakeArray c !sz f = compute (unsafeMakeArray c sz f :: Array D ix e)
  {-# INLINE unsafeMakeArray #-}

instance (VP.Prim e, Index ix) => Source P ix e where
  unsafeLinearIndex (PArray _ _ v) = VP.unsafeIndex v
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

  unsafeLinearIndexM (PArray _ _ v) = VP.unsafeIndex v
  {-# INLINE unsafeLinearIndexM #-}


instance (Index ix, VP.Prim e) => Mutable P ix e where
  data MArray s P ix e = MPArray !ix !(VP.MVector s e)

  msize (MPArray sz _) = sz
  {-# INLINE msize #-}

  unsafeThaw (PArray _ sz v) = MPArray sz <$> VP.unsafeThaw v
  {-# INLINE unsafeThaw #-}

  unsafeFreeze (MPArray sz v) = PArray Seq sz <$> VP.unsafeFreeze v
  {-# INLINE unsafeFreeze #-}

  unsafeNew sz = MPArray sz <$> MVP.unsafeNew (totalElem sz)
  {-# INLINE unsafeNew #-}

  unsafeLinearRead (MPArray _ v) i = MVP.unsafeRead v i
  {-# INLINE unsafeLinearRead #-}

  unsafeLinearWrite (MPArray _ v) i = MVP.unsafeWrite v i
  {-# INLINE unsafeLinearWrite #-}

instance (VP.Prim e, Index ix) => Target P ix e


-- computePrimitiveS :: (Load r ix e, Target P ix e) => Array r ix e -> Array P ix e
-- computePrimitiveS = loadTargetS
-- {-# INLINE computePrimitiveS #-}


-- computePrimitiveP :: (Load r ix e, Target P ix e) => Array r ix e -> Array P ix e
-- computePrimitiveP = unsafePerformIO . loadTargetOnP []
-- {-# INLINE computePrimitiveP #-}


-- fromVectorPrimitive :: Index ix => ix -> VP.Vector e -> Array P ix e
-- fromVectorPrimitive sz v = PArray { pSize = sz, pData = v }
-- {-# INLINE fromVectorPrimitive #-}


-- toVectorPrimitive :: Array P ix e -> VP.Vector e
-- toVectorPrimitive = pData
-- {-# INLINE toVectorPrimitive #-}


-- generateM :: (Index ix, VP.Prim a, Monad m) =>
--   ix -> (ix -> m a) -> m (Array P ix a)
-- generateM sz f =
--   PArray sz <$> VP.generateM (totalElem sz) (f . fromLinearIndex sz)
-- {-# INLINE generateM #-}


-- mapM :: (VP.Prim b, Source r ix a, Monad m) =>
--   (a -> m b) -> Array r ix a -> m (Array P ix b)
-- mapM f arr = do
--   let !sz = size arr
--   v <- VP.generateM (totalElem sz) (f . unsafeLinearIndex arr)
--   return $ PArray sz v
-- {-# INLINE mapM #-}

-- imapM :: (VP.Prim b, Source r ix a, Monad m) =>
--   (ix -> a -> m b) -> Array r ix a -> m (Array P ix b)
-- imapM f arr = do
--   let !sz = size arr
--   v <- VP.generateM (totalElem sz) $ \ !i ->
--          let !ix = fromLinearIndex sz i
--          in f ix (unsafeIndex arr ix)
--   return $ PArray sz v
-- {-# INLINE imapM #-}

