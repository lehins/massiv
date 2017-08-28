{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  , Array(..)
  , VP.Prim
  ) where

import           Control.DeepSeq                     (NFData (..), deepseq)
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Common.Shape
import           Data.Array.Massiv.Delayed           (eq)
import           Data.Array.Massiv.Manifest.Internal
import           Data.Array.Massiv.Mutable
import qualified Data.Vector.Primitive               as VP
import qualified Data.Vector.Primitive.Mutable       as MVP
import           Prelude                             hiding (mapM)

data P = P deriving Show

data instance Array P ix e = PArray { pComp :: !Comp
                                    , pSize :: !ix
                                    , pData :: !(VP.Vector e)
                                    }

instance (Index ix, NFData e) => NFData (Array P ix e) where
  rnf (PArray c sz v) = c `deepseq` sz `deepseq` v `deepseq` ()
  {-# INLINE rnf #-}

instance (VP.Prim e, Eq e, Index ix) => Eq (Array P ix e) where
  (==) = eq (==)
  {-# INLINE (==) #-}


instance (VP.Prim e, Index ix) => Construct P ix e where
  size = pSize
  {-# INLINE size #-}

  getComp = pComp
  {-# INLINE getComp #-}

  setComp c arr = arr { pComp = c }
  {-# INLINE setComp #-}

  unsafeMakeArray Seq !sz f = PArray Seq sz $ VP.generate (totalElem sz) (f . fromLinearIndex sz)
  unsafeMakeArray (ParOn wIds) !sz f = unsafeGenerateArrayP wIds sz f
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

  unsafeFreeze comp (MPArray sz v) = PArray comp sz <$> VP.unsafeFreeze v
  {-# INLINE unsafeFreeze #-}

  unsafeNew sz = MPArray sz <$> MVP.unsafeNew (totalElem sz)
  {-# INLINE unsafeNew #-}

  unsafeLinearRead (MPArray _ v) i = MVP.unsafeRead v i
  {-# INLINE unsafeLinearRead #-}

  unsafeLinearWrite (MPArray _ v) i = MVP.unsafeWrite v i
  {-# INLINE unsafeLinearWrite #-}

instance (VP.Prim e, Index ix) => Target P ix e
