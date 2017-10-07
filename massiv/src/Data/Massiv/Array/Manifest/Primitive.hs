{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Data.Massiv.Array.Manifest.Primitive
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Manifest.Primitive
  ( P(..)
  , Array(..)
  , Prim
  , vectorToByteArray
  ) where

import           Control.DeepSeq                     (NFData (..), deepseq)
import           Control.Monad.ST                    (runST)
import           Data.Massiv.Array.Delayed.Internal  (eq)
import           Data.Massiv.Array.Manifest.Internal
import           Data.Massiv.Array.Mutable
import           Data.Massiv.Array.Ops.Construct
import           Data.Massiv.Core
import           Data.Primitive                      (sizeOf)
import           Data.Primitive.ByteArray
import           Data.Primitive.Types                (Prim)
import qualified Data.Vector.Primitive               as VP
import           GHC.Exts                            (IsList (..))
import           Prelude                             hiding (mapM)

data P = P deriving Show

data instance Array P ix e = PArray { pComp :: !Comp
                                    , pSize :: !ix
                                    , pData :: {-# UNPACK #-} !ByteArray
                                    }

instance (Index ix, NFData e) => NFData (Array P ix e) where
  rnf (PArray c sz a) = c `deepseq` sz `deepseq` a `seq` ()
  {-# INLINE rnf #-}

instance (Prim e, Eq e, Index ix) => Eq (Array P ix e) where
  (==) = eq (==)
  {-# INLINE (==) #-}


instance (Prim e, Index ix) => Construct P ix e where
  size = pSize
  {-# INLINE size #-}

  getComp = pComp
  {-# INLINE getComp #-}

  setComp c arr = arr { pComp = c }
  {-# INLINE setComp #-}

  unsafeMakeArray Seq          !sz f = unsafeGenerateArray sz f
  unsafeMakeArray (ParOn wIds) !sz f = unsafeGenerateArrayP wIds sz f
  {-# INLINE unsafeMakeArray #-}

instance (Prim e, Index ix) => Source P ix e where
  unsafeLinearIndex (PArray _ _ a) = indexByteArray a
  {-# INLINE unsafeLinearIndex #-}


instance (Prim e, Index ix) => Shape P ix e where
  type R P = M

  unsafeReshape !sz !arr = arr { pSize = sz }
  {-# INLINE unsafeReshape #-}

  unsafeExtract !sIx !newSz !arr = unsafeExtract sIx newSz (toManifest arr)
  {-# INLINE unsafeExtract #-}


instance (Prim e, Index ix, Index (Lower ix)) => Slice P ix e where

  (!?>) !arr = (toManifest arr !?>)
  {-# INLINE (!?>) #-}

  (<!?) !arr = (toManifest arr <!?)
  {-# INLINE (<!?) #-}

instance (Index ix, Prim e) => Manifest P ix e where

  unsafeLinearIndexM (PArray _ _ a) = indexByteArray a
  {-# INLINE unsafeLinearIndexM #-}


instance (Index ix, Prim e) => Mutable P ix e where
  data MArray s P ix e = MPArray !ix !(MutableByteArray s)

  msize (MPArray sz _) = sz
  {-# INLINE msize #-}

  unsafeThaw (PArray _ sz a) = MPArray sz <$> unsafeThawByteArray a
  {-# INLINE unsafeThaw #-}

  unsafeFreeze comp (MPArray sz a) = PArray comp sz <$> unsafeFreezeByteArray a
  {-# INLINE unsafeFreeze #-}

  unsafeNew sz = MPArray sz <$> newByteArray (totalElem sz * sizeOf (undefined :: e))
  {-# INLINE unsafeNew #-}

  unsafeLinearRead (MPArray _ a) = readByteArray a
  {-# INLINE unsafeLinearRead #-}

  unsafeLinearWrite (MPArray _ v) = writeByteArray v
  {-# INLINE unsafeLinearWrite #-}


instance Prim e => IsList (Array P Ix1 e) where
  type Item (Array P Ix1 e) = e
  fromList = fromListIx1 Seq
  {-# INLINE fromList #-}
  toList = toListIx1
  {-# INLINE toList #-}


instance Prim e => IsList (Array P Ix2 e) where
  type Item (Array P Ix2 e) = [e]
  fromList = fromListIx2 Seq
  {-# INLINE fromList #-}
  toList = toListIx2
  {-# INLINE toList #-}


vectorToByteArray :: forall e . VP.Prim e => VP.Vector e -> ByteArray
vectorToByteArray (VP.Vector start len arr) =
  if start == 0
    then arr
    else runST $ do
           marr <- newByteArray len
           let elSize = sizeOf (undefined :: e)
           copyByteArray marr 0 arr (start * elSize) (len * elSize)
           unsafeFreezeByteArray marr
{-# INLINE vectorToByteArray #-}
