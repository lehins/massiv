{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Massiv.Array.Manifest.Primitive
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Manifest.Primitive
  ( P(..)
  , Array(..)
  , Prim
  , toByteArray
  , fromByteArrayM
  , fromByteArray
  , toMutableByteArray
  , fromMutableByteArrayM
  , fromMutableByteArray
  , unsafeAtomicReadIntArray
  , unsafeAtomicWriteIntArray
  , unsafeCasIntArray
  , unsafeAtomicModifyIntArray
  , unsafeAtomicAddIntArray
  , unsafeAtomicSubIntArray
  , unsafeAtomicAndIntArray
  , unsafeAtomicNandIntArray
  , unsafeAtomicOrIntArray
  , unsafeAtomicXorIntArray
  ) where

import Control.DeepSeq (NFData(..), deepseq)
import Control.Monad.Primitive (PrimMonad(primitive), PrimState, primitive_)
import Data.Massiv.Array.Delayed.Pull (eq, ord)
import Data.Massiv.Array.Manifest.Internal
import Data.Massiv.Array.Manifest.List as A
import Data.Massiv.Array.Mutable
import Data.Massiv.Core.Common
import Data.Massiv.Core.List
import Data.Primitive (sizeOf)
import Data.Primitive.ByteArray
import Data.Primitive.Types
import GHC.Base (Int(..))
import GHC.Exts as GHC
import Prelude hiding (mapM)
import System.IO.Unsafe (unsafePerformIO)

#include "massiv.h"

-- | Representation for `Prim`itive elements
data P = P deriving Show

type instance EltRepr P ix = M

data instance Array P ix e = PArray { pComp :: !Comp
                                    , pSize :: !(Sz ix)
                                    , pData :: {-# UNPACK #-} !ByteArray
                                    }

instance (Ragged L ix e, Show e, Prim e) => Show (Array P ix e) where
  showsPrec = showsArrayPrec id
  showList = showArrayList

instance Index ix => NFData (Array P ix e) where
  rnf (PArray c sz a) = c `deepseq` sz `deepseq` a `seq` ()
  {-# INLINE rnf #-}

instance (Prim e, Eq e, Index ix) => Eq (Array P ix e) where
  (==) = eq (==)
  {-# INLINE (==) #-}

instance (Prim e, Ord e, Index ix) => Ord (Array P ix e) where
  compare = ord compare
  {-# INLINE compare #-}

instance (Prim e, Index ix) => Construct P ix e where
  setComp c arr = arr { pComp = c }
  {-# INLINE setComp #-}

  makeArray !comp !sz f = unsafePerformIO $ generateArray comp sz (return . f)
  {-# INLINE makeArray #-}

instance (Prim e, Index ix) => Source P ix e where
  unsafeLinearIndex _pa@(PArray _ _ a) =
    INDEX_CHECK("(Source P ix e).unsafeLinearIndex",
                Sz . elemsBA _pa, indexByteArray) a
  {-# INLINE unsafeLinearIndex #-}


instance Index ix => Resize P ix where
  unsafeResize !sz !arr = arr { pSize = sz }
  {-# INLINE unsafeResize #-}

instance (Prim e, Index ix) => Extract P ix e where
  unsafeExtract !sIx !newSz !arr = unsafeExtract sIx newSz (toManifest arr)
  {-# INLINE unsafeExtract #-}


instance {-# OVERLAPPING #-} Prim e => Slice P Ix1 e where
  unsafeSlice arr i _ _ = pure (unsafeLinearIndex arr i)
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
  unsafeOuterSlice = unsafeLinearIndex
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

  unsafeLinearIndexM _pa@(PArray _ _ a) =
    INDEX_CHECK("(Manifest P ix e).unsafeLinearIndexM",
                Sz . elemsBA _pa, indexByteArray) a
  {-# INLINE unsafeLinearIndexM #-}


instance (Index ix, Prim e) => Mutable P ix e where
  data MArray s P ix e = MPArray !(Sz ix) {-# UNPACK #-} !(MutableByteArray s)

  msize (MPArray sz _) = sz
  {-# INLINE msize #-}

  unsafeThaw (PArray _ sz a) = MPArray sz <$> unsafeThawByteArray a
  {-# INLINE unsafeThaw #-}

  unsafeFreeze comp (MPArray sz a) = PArray comp sz <$> unsafeFreezeByteArray a
  {-# INLINE unsafeFreeze #-}

  unsafeNew sz
    | n <= (maxBound :: Int) `div` eSize = MPArray sz <$> newByteArray (n * eSize)
    | otherwise = error $ "Array size is too big: " ++ show sz
    where !n = totalElem sz
          !eSize = sizeOf (undefined :: e)
  {-# INLINE unsafeNew #-}

  initialize (MPArray sz mba) =
    fillByteArray mba 0 (totalElem sz * sizeOf (undefined :: e)) 0
  {-# INLINE initialize #-}

  unsafeLinearRead _mpa@(MPArray _ ma) =
    INDEX_CHECK("(Mutable P ix e).unsafeLinearRead",
                Sz . elemsMBA _mpa, readByteArray) ma
  {-# INLINE unsafeLinearRead #-}

  unsafeLinearWrite _mpa@(MPArray _ ma) =
    INDEX_CHECK("(Mutable P ix e).unsafeLinearWrite",
                Sz . elemsMBA _mpa, writeByteArray) ma
  {-# INLINE unsafeLinearWrite #-}

  unsafeLinearSet (MPArray _ ma) = setByteArray ma
  {-# INLINE unsafeLinearSet #-}


instance (Prim e, Index ix) => Load P ix e where
  size = pSize
  {-# INLINE size #-}
  getComp = pComp
  {-# INLINE getComp #-}
  loadArrayM !scheduler !arr =
    splitLinearlyWith_ scheduler (elemsCount arr) (unsafeLinearIndex arr)
  {-# INLINE loadArrayM #-}

instance (Prim e, Index ix) => StrideLoad P ix e

instance ( Prim e
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


elemsBA :: forall proxy e . Prim e => proxy e -> ByteArray -> Int
elemsBA _ a = sizeofByteArray a `div` sizeOf (undefined :: e)
{-# INLINE elemsBA #-}


elemsMBA :: forall proxy e s . Prim e => proxy e -> MutableByteArray s -> Int
elemsMBA _ a = sizeofMutableByteArray a `div` sizeOf (undefined :: e)
{-# INLINE elemsMBA #-}


-- | /O(1)/ - Extract the internal `ByteArray`.
--
-- @since 0.2.1
toByteArray :: Array P ix e -> ByteArray
toByteArray = pData
{-# INLINE toByteArray #-}


-- | /O(1)/ - Construct a primitive array from the `ByteArray`. Will return `Nothing` if number of
-- elements doesn't match.
--
-- @since 0.3.0
fromByteArrayM :: (MonadThrow m, Index ix, Prim e) => Comp -> Sz ix -> ByteArray -> m (Array P ix e)
fromByteArrayM comp sz ba =
  guardNumberOfElements sz (Sz (elemsBA arr ba)) >> pure arr
  where
    arr = PArray comp sz ba
{-# INLINE fromByteArrayM #-}

-- | See `fromByteArrayM`.
--
-- @since 0.2.1
fromByteArray :: (Index ix, Prim e) => Comp -> Sz ix -> ByteArray -> Maybe (Array P ix e)
fromByteArray = fromByteArrayM
{-# INLINE fromByteArray #-}
{-# DEPRECATED fromByteArray "In favor of more general `fromByteArrayM`" #-}


-- | /O(1)/ - Extract the internal `MutableByteArray`.
--
-- @since 0.2.1
toMutableByteArray :: MArray s P ix e -> MutableByteArray s
toMutableByteArray (MPArray _ mba) = mba
{-# INLINE toMutableByteArray #-}


-- | /O(1)/ - Construct a primitive mutable array from the `MutableByteArray`. Will return `Nothing`
-- if number of elements doesn't match.
--
-- @since 0.3.0
fromMutableByteArrayM ::
     (MonadThrow m, Index ix, Prim e) => Sz ix -> MutableByteArray s -> m (MArray s P ix e)
fromMutableByteArrayM sz mba =
  guardNumberOfElements sz (Sz (elemsMBA marr mba)) >> pure marr
  where
    marr = MPArray sz mba
{-# INLINE fromMutableByteArrayM #-}

-- | See `fromMutableByteArray`.
--
-- @since 0.2.1
fromMutableByteArray :: (Index ix, Prim e) => Sz ix -> MutableByteArray s -> Maybe (MArray s P ix e)
fromMutableByteArray = fromMutableByteArrayM
{-# INLINE fromMutableByteArray #-}
{-# DEPRECATED fromMutableByteArray "In favor of more general `fromMutableByteArrayM`" #-}


-- | Atomically read an `Int` element from the array
--
-- @since 0.3.0
unsafeAtomicReadIntArray ::
     (Index ix, PrimMonad m) => MArray (PrimState m) P ix Int -> ix -> m Int
unsafeAtomicReadIntArray _mpa@(MPArray sz mba) ix =
  INDEX_CHECK( "unsafeAtomicReadIntArray"
             , Sz . elemsMBA _mpa
             , \(MutableByteArray mba#) (I# i#) ->
                 primitive $ \s# ->
                 case atomicReadIntArray# mba# i# s# of
                   (# s'#, e# #) -> (# s'#, I# e# #))
  mba
  (toLinearIndex sz ix)
{-# INLINE unsafeAtomicReadIntArray #-}

-- | Atomically write an `Int` element int the array
--
-- @since 0.3.0
unsafeAtomicWriteIntArray ::
     (Index ix, PrimMonad m) => MArray (PrimState m) P ix Int -> ix -> Int -> m ()
unsafeAtomicWriteIntArray _mpa@(MPArray sz mba) ix (I# e#) =
  INDEX_CHECK( "unsafeAtomicWriteIntArray"
             , Sz . elemsMBA _mpa
             , \(MutableByteArray mba#) (I# i#) ->
                 primitive_ (atomicWriteIntArray# mba# i# e#))
  mba
  (toLinearIndex sz ix)
{-# INLINE unsafeAtomicWriteIntArray #-}

-- | Atomically CAS an `Int` in the array. Returns the old value.
--
-- @since 0.3.0
unsafeCasIntArray ::
     (Index ix, PrimMonad m) => MArray (PrimState m) P ix Int -> ix -> Int -> Int -> m Int
unsafeCasIntArray _mpa@(MPArray sz mba) ix (I# e#) (I# n#) =
  INDEX_CHECK( "unsafeCasIntArray"
             , Sz . elemsMBA _mpa
             , \(MutableByteArray mba#) (I# i#) ->
                 primitive $ \s# ->
                 case casIntArray# mba# i# e# n# s# of
                   (# s'#, o# #) -> (# s'#, I# o# #))
  mba
  (toLinearIndex sz ix)
{-# INLINE unsafeCasIntArray #-}


-- | Atomically modify an `Int` element of the array. Returns the old value.
--
-- @since 0.3.0
unsafeAtomicModifyIntArray ::
     (Index ix, PrimMonad m) => MArray (PrimState m) P ix Int -> ix -> (Int -> Int) -> m Int
unsafeAtomicModifyIntArray _mpa@(MPArray sz mba) ix f =
  INDEX_CHECK("unsafeAtomicModifyIntArray", Sz . elemsMBA _mpa, atomicModify)
  mba
  (toLinearIndex sz ix)
  where
    atomicModify (MutableByteArray mba#) (I# i#) =
      let go s# o# =
            let !(I# n#) = f (I# o#)
             in case casIntArray# mba# i# o# n# s# of
                  (# s'#, o'# #) ->
                    case o# ==# o'# of
                      0# -> go s# o'#
                      _  -> (# s'#, I# o# #)
       in primitive $ \s# ->
            case atomicReadIntArray# mba# i# s# of
              (# s'#, o# #) -> go s'# o#
    {-# INLINE atomicModify #-}
{-# INLINE unsafeAtomicModifyIntArray #-}


-- | Atomically add to an `Int` element in the array. Returns the old value.
--
-- @since 0.3.0
unsafeAtomicAddIntArray ::
     (Index ix, PrimMonad m) => MArray (PrimState m) P ix Int -> ix -> Int -> m Int
unsafeAtomicAddIntArray _mpa@(MPArray sz mba) ix (I# e#) =
  INDEX_CHECK( "unsafeAtomicAddIntArray"
             , Sz . elemsMBA _mpa
             , \(MutableByteArray mba#) (I# i#) ->
                 primitive $ \s# ->
                 case fetchAddIntArray# mba# i# e# s# of
                   (# s'#, p# #) -> (# s'#, I# p# #))
  mba
  (toLinearIndex sz ix)
{-# INLINE unsafeAtomicAddIntArray #-}


-- | Atomically subtract from an `Int` element in the array. Returns the old value.
--
-- @since 0.3.0
unsafeAtomicSubIntArray ::
     (Index ix, PrimMonad m) => MArray (PrimState m) P ix Int -> ix -> Int -> m Int
unsafeAtomicSubIntArray _mpa@(MPArray sz mba) ix (I# e#) =
  INDEX_CHECK( "unsafeAtomicSubIntArray"
             , Sz . elemsMBA _mpa
             , \(MutableByteArray mba#) (I# i#) ->
                 primitive $ \s# ->
                 case fetchSubIntArray# mba# i# e# s# of
                   (# s'#, p# #) -> (# s'#, I# p# #))
  mba
  (toLinearIndex sz ix)
{-# INLINE unsafeAtomicSubIntArray #-}


-- | Atomically AND an `Int` element in the array. Returns the old value.
--
-- @since 0.3.0
unsafeAtomicAndIntArray ::
     (Index ix, PrimMonad m) => MArray (PrimState m) P ix Int -> ix -> Int -> m Int
unsafeAtomicAndIntArray _mpa@(MPArray sz mba) ix (I# e#) =
  INDEX_CHECK( "unsafeAtomicAndIntArray"
             , Sz . elemsMBA _mpa
             , \(MutableByteArray mba#) (I# i#) ->
                 primitive $ \s# ->
                 case fetchAndIntArray# mba# i# e# s# of
                   (# s'#, p# #) -> (# s'#, I# p# #))
  mba
  (toLinearIndex sz ix)
{-# INLINE unsafeAtomicAndIntArray #-}


-- | Atomically NAND an `Int` element in the array. Returns the old value.
--
-- @since 0.3.0
unsafeAtomicNandIntArray ::
     (Index ix, PrimMonad m) => MArray (PrimState m) P ix Int -> ix -> Int -> m Int
unsafeAtomicNandIntArray _mpa@(MPArray sz mba) ix (I# e#) =
  INDEX_CHECK( "unsafeAtomicNandIntArray"
             , Sz . elemsMBA _mpa
             , \(MutableByteArray mba#) (I# i#) ->
                 primitive $ \s# ->
                 case fetchNandIntArray# mba# i# e# s# of
                   (# s'#, p# #) -> (# s'#, I# p# #))
  mba
  (toLinearIndex sz ix)
{-# INLINE unsafeAtomicNandIntArray #-}


-- | Atomically OR an `Int` element in the array. Returns the old value.
--
-- @since 0.3.0
unsafeAtomicOrIntArray ::
     (Index ix, PrimMonad m) => MArray (PrimState m) P ix Int -> ix -> Int -> m Int
unsafeAtomicOrIntArray _mpa@(MPArray sz mba) ix (I# e#) =
  INDEX_CHECK( "unsafeAtomicOrIntArray"
             , Sz . elemsMBA _mpa
             , \(MutableByteArray mba#) (I# i#) ->
                 primitive $ \s# ->
                 case fetchOrIntArray# mba# i# e# s# of
                   (# s'#, p# #) -> (# s'#, I# p# #))
  mba
  (toLinearIndex sz ix)
{-# INLINE unsafeAtomicOrIntArray #-}


-- | Atomically XOR an `Int` element in the array. Returns the old value.
--
-- @since 0.3.0
unsafeAtomicXorIntArray ::
     (Index ix, PrimMonad m) => MArray (PrimState m) P ix Int -> ix -> Int -> m Int
unsafeAtomicXorIntArray _mpa@(MPArray sz mba) ix (I# e#) =
  INDEX_CHECK( "unsafeAtomicXorIntArray"
             , Sz . elemsMBA _mpa
             , \(MutableByteArray mba#) (I# i#) ->
                 primitive $ \s# ->
                 case fetchXorIntArray# mba# i# e# s# of
                   (# s'#, p# #) -> (# s'#, I# p# #))
  mba
  (toLinearIndex sz ix)
{-# INLINE unsafeAtomicXorIntArray #-}
