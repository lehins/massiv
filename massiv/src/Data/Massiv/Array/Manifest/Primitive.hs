{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Massiv.Array.Manifest.Primitive
-- Copyright   : (c) Alexey Kuleshevich 2018
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
import           Data.Massiv.Array.Manifest.List     as A
import           Data.Massiv.Array.Mutable
import           Data.Massiv.Array.Unsafe            (unsafeGenerateArray,
                                                      unsafeGenerateArrayP)
import           Data.Massiv.Core.Common
import           Data.Massiv.Core.List
import           Data.Primitive                      (sizeOf)
import           Data.Primitive.ByteArray
import           Data.Primitive.Types                (Prim)
import qualified Data.Vector.Primitive               as VP
import           GHC.Exts                            as GHC (IsList (..))
import           Prelude                             hiding (mapM)

-- | Representation for `Prim`itive elements
data P = P deriving Show

type instance EltRepr P ix = M

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


instance (Prim e, Index ix) => Size P ix e where
  size = pSize
  {-# INLINE size #-}

  unsafeResize !sz !arr = arr { pSize = sz }
  {-# INLINE unsafeResize #-}

  unsafeExtract !sIx !newSz !arr = unsafeExtract sIx newSz (toManifest arr)
  {-# INLINE unsafeExtract #-}


instance {-# OVERLAPPING #-} Prim e => Slice P Ix1 e where
  unsafeSlice arr i _ _ = Just (unsafeLinearIndex arr i)
  {-# INLINE unsafeSlice #-}


instance ( Prim e
         , Index ix
         , Index (Lower ix)
         , Elt P ix e ~ Elt M ix e
         , Elt M ix e ~ Array M (Lower ix) e
         ) =>
         Slice P ix e where
  unsafeSlice arr = unsafeSlice (toManifest arr)
  {-# INLINE unsafeSlice #-}

instance {-# OVERLAPPING #-} Prim e => OuterSlice P Ix1 e where
  unsafeOuterSlice  = unsafeLinearIndex
  {-# INLINE unsafeOuterSlice #-}

instance ( Prim e
         , Index ix
         , Index (Lower ix)
         , Elt M ix e ~ Array M (Lower ix) e
         , Elt P ix e ~ Array M (Lower ix) e
         ) =>
         OuterSlice P ix e where
  unsafeOuterSlice arr = unsafeOuterSlice (toManifest arr)
  {-# INLINE unsafeOuterSlice #-}


instance {-# OVERLAPPING #-} Prim e => InnerSlice P Ix1 e where
  unsafeInnerSlice arr _ = unsafeLinearIndex arr
  {-# INLINE unsafeInnerSlice #-}

instance ( Prim e
         , Index ix
         , Index (Lower ix)
         , Elt M ix e ~ Array M (Lower ix) e
         , Elt P ix e ~ Array M (Lower ix) e
         ) =>
         InnerSlice P ix e where
  unsafeInnerSlice arr = unsafeInnerSlice (toManifest arr)
  {-# INLINE unsafeInnerSlice #-}

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

  unsafeNewZero sz = do
    let szBytes = totalElem sz * sizeOf (undefined :: e)
    barr <- newByteArray szBytes
    fillByteArray barr 0 szBytes 0
    return $ MPArray sz barr
  {-# INLINE unsafeNewZero #-}

  unsafeLinearRead (MPArray _ a) = readByteArray a
  {-# INLINE unsafeLinearRead #-}

  unsafeLinearWrite (MPArray _ v) = writeByteArray v
  {-# INLINE unsafeLinearWrite #-}


instance ( VP.Prim e
         , IsList (Array L ix e)
         , Nested LN ix e
         , Nested L ix e
         , Ragged L ix e
         ) =>
         IsList (Array P ix e) where
  type Item (Array P ix e) = Item (Array L ix e)
  fromList = A.fromLists' Seq
  {-# INLINE fromList #-}
  toList = GHC.toList . toListArray
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


