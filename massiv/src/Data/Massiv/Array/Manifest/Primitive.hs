{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Massiv.Array.Manifest.Primitive
-- Copyright   : (c) Alexey Kuleshevich 2018-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Manifest.Primitive
  ( P(..)
  , Array(..)
  , Prim
  , toPrimitiveVector
  , toPrimitiveMVector
  , fromPrimitiveVector
  , fromPrimitiveMVector
  , toByteArray
  , toByteArrayM
  , unwrapByteArray
  , unwrapByteArrayOffset
  , unwrapMutableByteArray
  , unwrapMutableByteArrayOffset
  , fromByteArray
  , fromByteArrayM
  , fromByteArrayOffsetM
  , toMutableByteArray
  , toMutableByteArrayM
  , fromMutableByteArrayM
  , fromMutableByteArrayOffsetM
  , fromMutableByteArray
  , shrinkMutableByteArray
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

import Control.Monad
import Control.DeepSeq (NFData(..), deepseq)
import Control.Monad.Primitive (PrimMonad(..), primitive_)
import Data.Massiv.Array.Delayed.Pull -- (eq, ord)
import Data.Massiv.Array.Manifest.Internal
import Data.Massiv.Array.Manifest.List as A
import Data.Massiv.Array.Mutable
import Data.Massiv.Core.Common
import Data.Massiv.Core.Operations
import Data.Massiv.Core.List
import Data.Massiv.Vector.Stream as S (steps, isteps)
import Data.Maybe (fromMaybe)
import Data.Primitive (sizeOf, Prim)
import Data.Primitive.ByteArray
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Primitive.Mutable as MVP
import GHC.Exts as GHC
import Prelude hiding (mapM)
import System.IO.Unsafe (unsafePerformIO)

#include "massiv.h"

-- | Representation for `Prim`itive elements
data P = P deriving Show

data instance Array P ix e = PArray { pComp   :: !Comp
                                    , pSize   :: !(Sz ix)
                                    , pOffset :: {-# UNPACK #-} !Int
                                    , pData   :: {-# UNPACK #-} !ByteArray
                                    }

instance (Ragged L ix e, Show e, Prim e) => Show (Array P ix e) where
  showsPrec = showsArrayPrec id
  showList = showArrayList

instance Index ix => NFData (Array P ix e) where
  rnf (PArray c sz o a) = c `deepseq` sz `deepseq` o `seq` a `seq` ()
  {-# INLINE rnf #-}

instance NFData ix => NFData (MArray s P ix e) where
  rnf (MPArray sz _o _mb) = sz `deepseq` ()
  {-# INLINE rnf #-}

instance (Prim e, Eq e, Index ix) => Eq (Array P ix e) where
  (==) = eqArrays (==)
  {-# INLINE (==) #-}

instance (Prim e, Ord e, Index ix) => Ord (Array P ix e) where
  compare = compareArrays compare
  {-# INLINE compare #-}

instance Strategy P where
  getComp = pComp
  {-# INLINE getComp #-}
  setComp c arr = arr { pComp = c }
  {-# INLINE setComp #-}


instance Index ix => Shape P ix where
  maxLinearSize = Just . SafeSz . elemsCount
  {-# INLINE maxLinearSize #-}

instance Size P where
  size = pSize
  {-# INLINE size #-}

instance Resize P where
  unsafeResize !sz !arr = arr { pSize = sz }
  {-# INLINE unsafeResize #-}

instance Prim e => Source P e where
  unsafeLinearIndex _arr@(PArray _ _ o a) i =
    INDEX_CHECK("(Source P ix e).unsafeLinearIndex",
                SafeSz . elemsBA _arr, indexByteArray) a (i + o)
  {-# INLINE unsafeLinearIndex #-}

  unsafeOuterSlice (PArray c _ o a) szL i =
    PArray c szL (i * totalElem szL + o) a
  {-# INLINE unsafeOuterSlice #-}

  unsafeLinearSlice i k (PArray c _ o a) = PArray c k (i + o) a
  {-# INLINE unsafeLinearSlice #-}

instance Prim e => Manifest P e where

  unsafeLinearIndexM _pa@(PArray _ _sz o a) i =
    INDEX_CHECK("(Manifest P ix e).unsafeLinearIndexM",
                const (Sz (totalElem _sz)), indexByteArray) a (i + o)
  {-# INLINE unsafeLinearIndexM #-}


instance Prim e => Mutable P e where
  data MArray s P ix e = MPArray !(Sz ix) {-# UNPACK #-} !Int {-# UNPACK #-} !(MutableByteArray s)

  msize (MPArray sz _ _) = sz
  {-# INLINE msize #-}

  munsafeResize sz (MPArray _ off marr) = MPArray sz off marr
  {-# INLINE munsafeResize #-}

  unsafeThaw (PArray _ sz o a) = MPArray sz o <$> unsafeThawByteArray a
  {-# INLINE unsafeThaw #-}

  unsafeFreeze comp (MPArray sz o a) = PArray comp sz o <$> unsafeFreezeByteArray a
  {-# INLINE unsafeFreeze #-}

  unsafeNew sz
    | n <= (maxBound :: Int) `div` eSize = MPArray sz 0 <$> newByteArray (n * eSize)
    | otherwise = error $ "Array size is too big: " ++ show sz
    where !n = totalElem sz
          !eSize = sizeOf (undefined :: e)
  {-# INLINE unsafeNew #-}

  initialize (MPArray sz o mba) =
    let k = totalElem sz * sizeOf (undefined :: e)
    in when (k > 0) $ fillByteArray mba o k 0
  {-# INLINE initialize #-}

  unsafeLinearRead _mpa@(MPArray _sz o ma) i =
    INDEX_CHECK("(Mutable P ix e).unsafeLinearRead",
                const (Sz (totalElem _sz)), readByteArray) ma (i + o)
  {-# INLINE unsafeLinearRead #-}

  unsafeLinearWrite _mpa@(MPArray _sz o ma) i =
    INDEX_CHECK("(Mutable P ix e).unsafeLinearWrite",
                const (Sz (totalElem _sz)), writeByteArray) ma (i + o)
  {-# INLINE unsafeLinearWrite #-}

  unsafeLinearSet (MPArray _ o ma) offset (SafeSz sz) = setByteArray ma (offset + o) sz
  {-# INLINE unsafeLinearSet #-}

  unsafeLinearCopy (MPArray _ oFrom maFrom) iFrom (MPArray _ oTo maTo) iTo (Sz k) =
    copyMutableByteArray maTo ((oTo + iTo) * esz) maFrom ((oFrom + iFrom) * esz) (k * esz)
    where esz = sizeOf (undefined :: e)
  {-# INLINE unsafeLinearCopy #-}

  unsafeArrayLinearCopy (PArray _ _ oFrom aFrom) iFrom (MPArray _ oTo maTo) iTo (Sz k) =
    copyByteArray maTo ((oTo + iTo) * esz) aFrom ((oFrom + iFrom) * esz) (k * esz)
    where esz = sizeOf (undefined :: e)
  {-# INLINE unsafeArrayLinearCopy #-}

  unsafeLinearShrink (MPArray _ o ma) sz = do
    shrinkMutableByteArray ma ((o + totalElem sz) * sizeOf (undefined :: e))
    pure $ MPArray sz o ma
  {-# INLINE unsafeLinearShrink #-}

  unsafeLinearGrow (MPArray _ o ma) sz =
    MPArray sz o <$> resizeMutableByteArrayCompat ma ((o + totalElem sz) * sizeOf (undefined :: e))
  {-# INLINE unsafeLinearGrow #-}


instance (Prim e, Index ix) => Load P ix e where
  makeArrayLinear !comp !sz f = unsafePerformIO $ generateArrayLinear comp sz (pure . f)
  {-# INLINE makeArrayLinear #-}

  replicate comp !sz !e = runST (newMArray sz e >>= unsafeFreeze comp)
  {-# INLINE replicate #-}

  loadArrayM !scheduler !arr =
    splitLinearlyWith_ scheduler (elemsCount arr) (unsafeLinearIndex arr)
  {-# INLINE loadArrayM #-}

instance (Prim e, Index ix) => StrideLoad P ix e

instance (Prim e, Index ix) => Stream P ix e where
  toStream = S.steps
  {-# INLINE toStream #-}
  toStreamIx = S.isteps
  {-# INLINE toStreamIx #-}


instance (Prim e, Num e) => FoldNumeric P e where
  unsafeDotProduct = defaultUnsafeDotProduct
  {-# INLINE unsafeDotProduct #-}
  powerSumArray = defaultPowerSumArray
  {-# INLINE powerSumArray #-}
  foldArray = defaultFoldArray
  {-# INLINE foldArray #-}

instance (Prim e, Num e) => Numeric P e where
  unsafeLiftArray = defaultUnsafeLiftArray
  {-# INLINE unsafeLiftArray #-}
  unsafeLiftArray2 = defaultUnsafeLiftArray2
  {-# INLINE unsafeLiftArray2 #-}


instance (Prim e, Floating e) => NumericFloat P e


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


-- | /O(n)/ - Ensure that the size matches the internal `ByteArray`. If not make a copy of
-- the slice and return it as `ByteArray`
--
-- @since 0.2.1
toByteArray :: (Index ix, Prim e) => Array P ix e -> ByteArray
toByteArray arr = fromMaybe (unwrapByteArray $ clone arr) $ toByteArrayM arr
{-# INLINE toByteArray #-}

-- | /O(1)/ - Extract the internal `ByteArray`. This will ignore any possible slicing that
-- has been applied to the array. Use `toByteArray` in order to preserve slicing or
-- `unwrapByteArrayOffset` to get ahold of the offset
--
-- @since 0.5.0
unwrapByteArray :: Array P ix e -> ByteArray
unwrapByteArray = pData
{-# INLINE unwrapByteArray #-}


-- | /O(1)/ - Extract potential linear offset into the underlying `ByteArray`, which can
-- also be extracted with `unwrapByteArray`.
--
-- @since 0.5.9
unwrapByteArrayOffset :: Array P ix e -> Int
unwrapByteArrayOffset = pOffset
{-# INLINE unwrapByteArrayOffset #-}


-- | /O(1)/ - Unwrap Ensure that the size matches the internal `ByteArray`.
--
-- @since 0.5.0
toByteArrayM :: (Prim e, Index ix, MonadThrow m) => Array P ix e -> m ByteArray
toByteArrayM arr@PArray {pSize, pData} = do
  pData <$ guardNumberOfElements pSize (Sz (elemsBA arr pData))
{-# INLINE toByteArrayM #-}


-- | /O(1)/ - Construct a primitive array from the `ByteArray`. Will return `Nothing` if
-- number of elements doesn't match.
--
-- @since 0.3.0
fromByteArrayM :: (MonadThrow m, Index ix, Prim e) => Comp -> Sz ix -> ByteArray -> m (Array P ix e)
fromByteArrayM comp sz = fromByteArrayOffsetM comp sz 0
{-# INLINE fromByteArrayM #-}

-- | /O(1)/ - Construct a primitive array from the `ByteArray`. Will return `Nothing` if
-- number of elements doesn't match.
--
-- @since 0.5.9
fromByteArrayOffsetM ::
     (MonadThrow m, Index ix, Prim e) => Comp -> Sz ix -> Int -> ByteArray -> m (Array P ix e)
fromByteArrayOffsetM comp sz off ba =
  arr <$ guardNumberOfElements sz (SafeSz (elemsBA arr ba - off))
  where
    arr = PArray comp sz off ba
{-# INLINE fromByteArrayOffsetM #-}

-- | /O(1)/ - Construct a flat Array from `ByteArray`
--
-- @since 0.4.0
fromByteArray :: forall e . Prim e => Comp -> ByteArray -> Array P Ix1 e
fromByteArray comp ba = PArray comp (SafeSz (elemsBA (Proxy :: Proxy e) ba)) 0 ba
{-# INLINE fromByteArray #-}


-- | /O(1)/ - Extract the internal `MutableByteArray`. This will discard any possible
-- slicing that has been applied to the array.
--
-- @since 0.5.0
unwrapMutableByteArray :: MArray s P ix e -> MutableByteArray s
unwrapMutableByteArray (MPArray _ _ mba) = mba
{-# INLINE unwrapMutableByteArray #-}

-- | /O(1)/ - Extract the linear offset into underlying `MutableByteArray`, which can aslo
-- be extracted with `unwrapMutableByteArray`.
--
-- @since 0.5.9
unwrapMutableByteArrayOffset :: MArray s P ix e -> Int
unwrapMutableByteArrayOffset (MPArray _ off _) = off
{-# INLINE unwrapMutableByteArrayOffset #-}

-- | /O(n)/ - Try to cast a mutable array to `MutableByteArray`, if sizes do not match make
-- a copy. Returns `True` if an array was converted without a copy, in which case it means
-- that the source at the resulting array are still pointing to the same location in memory.
--
-- @since 0.5.0
toMutableByteArray ::
     forall ix e m. (Prim e, Index ix, PrimMonad m)
  => MArray (PrimState m) P ix e
  -> m (Bool, MutableByteArray (PrimState m))
toMutableByteArray marr@(MPArray sz offset mbas) =
  case toMutableByteArrayM marr of
    Just mba -> pure (True, mba)
    Nothing -> do
      let eSize = sizeOf (undefined :: e)
          szBytes = totalElem sz * eSize
      mbad <- newPinnedByteArray szBytes
      copyMutableByteArray mbad 0 mbas (offset * eSize) szBytes
      pure (False, mbad)
{-# INLINE toMutableByteArray #-}


-- | /O(1)/ - Extract the internal `MutableByteArray`.
--
-- @since 0.2.1
toMutableByteArrayM :: (Index ix, Prim e, MonadThrow m) => MArray s P ix e -> m (MutableByteArray s)
toMutableByteArrayM marr@(MPArray sz _ mba) =
  mba <$ guardNumberOfElements sz (Sz (elemsMBA marr mba))
{-# INLINE toMutableByteArrayM #-}


-- | /O(1)/ - Construct a primitive mutable array from the `MutableByteArray`. Will throw
-- `SizeElementsMismatchException` if number of elements doesn't match.
--
-- @since 0.3.0
fromMutableByteArrayM ::
     (MonadThrow m, Index ix, Prim e) => Sz ix -> MutableByteArray s -> m (MArray s P ix e)
fromMutableByteArrayM sz = fromMutableByteArrayOffsetM sz 0
{-# INLINE fromMutableByteArrayM #-}

-- | /O(1)/ - Construct a primitive mutable array from the `MutableByteArray`. Will throw
-- `SizeElementsMismatchException` if number of elements doesn't match.
--
-- @since 0.5.9
fromMutableByteArrayOffsetM ::
     (MonadThrow m, Index ix, Prim e) => Sz ix -> Ix1 -> MutableByteArray s -> m (MArray s P ix e)
fromMutableByteArrayOffsetM sz off mba =
  marr <$ guardNumberOfElements sz (SafeSz (elemsMBA marr mba - off))
  where
    marr = MPArray sz off mba
{-# INLINE fromMutableByteArrayOffsetM #-}


-- | /O(1)/ - Construct a flat Array from `MutableByteArray`
--
-- @since 0.4.0
fromMutableByteArray :: forall e s . Prim e => MutableByteArray s -> MArray s P Ix1 e
fromMutableByteArray mba = MPArray (SafeSz (elemsMBA (Proxy :: Proxy e) mba)) 0 mba
{-# INLINE fromMutableByteArray #-}




-- | /O(1)/ - Cast a primitive array to a primitive vector.
--
-- @since 0.5.0
toPrimitiveVector :: Index ix => Array P ix e -> VP.Vector e
toPrimitiveVector PArray {pSize, pOffset, pData} = VP.Vector pOffset (totalElem pSize) pData
{-# INLINE toPrimitiveVector #-}


-- | /O(1)/ - Cast a mutable primitive array to a mutable primitive vector.
--
-- @since 0.5.0
toPrimitiveMVector :: Index ix => MArray s P ix e -> MVP.MVector s e
toPrimitiveMVector (MPArray sz offset mba) = MVP.MVector offset (totalElem sz) mba
{-# INLINE toPrimitiveMVector #-}


-- | /O(1)/ - Cast a primitive vector to a primitive array.
--
-- @since 0.5.0
fromPrimitiveVector :: VP.Vector e -> Array P Ix1 e
fromPrimitiveVector (VP.Vector offset len ba) =
  PArray {pComp = Seq, pSize = SafeSz len, pOffset = offset, pData = ba}
{-# INLINE fromPrimitiveVector #-}

-- | /O(1)/ - Cast a mutable primitive vector to a mutable primitive array.
--
-- @since 0.5.0
fromPrimitiveMVector :: MVP.MVector s e -> MArray s P Ix1 e
fromPrimitiveMVector (MVP.MVector offset len mba) = MPArray (SafeSz len) offset mba
{-# INLINE fromPrimitiveMVector #-}

-- | Atomically read an `Int` element from the array
--
-- @since 0.3.0
unsafeAtomicReadIntArray ::
     (Index ix, PrimMonad m) => MArray (PrimState m) P ix Int -> ix -> m Int
unsafeAtomicReadIntArray _mpa@(MPArray sz o mba) ix =
  INDEX_CHECK( "unsafeAtomicReadIntArray"
             , SafeSz . elemsMBA _mpa
             , \(MutableByteArray mba#) (I# i#) ->
                 primitive $ \s# ->
                 case atomicReadIntArray# mba# i# s# of
                   (# s'#, e# #) -> (# s'#, I# e# #))
  mba
  (o + toLinearIndex sz ix)
{-# INLINE unsafeAtomicReadIntArray #-}

-- | Atomically write an `Int` element int the array
--
-- @since 0.3.0
unsafeAtomicWriteIntArray ::
     (Index ix, PrimMonad m) => MArray (PrimState m) P ix Int -> ix -> Int -> m ()
unsafeAtomicWriteIntArray _mpa@(MPArray sz o mba) ix (I# e#) =
  INDEX_CHECK( "unsafeAtomicWriteIntArray"
             , SafeSz . elemsMBA _mpa
             , \(MutableByteArray mba#) (I# i#) ->
                 primitive_ (atomicWriteIntArray# mba# i# e#))
  mba
  (o + toLinearIndex sz ix)
{-# INLINE unsafeAtomicWriteIntArray #-}

-- | Atomically CAS an `Int` in the array. Returns the old value.
--
-- @since 0.3.0
unsafeCasIntArray ::
     (Index ix, PrimMonad m) => MArray (PrimState m) P ix Int -> ix -> Int -> Int -> m Int
unsafeCasIntArray _mpa@(MPArray sz o mba) ix (I# e#) (I# n#) =
  INDEX_CHECK( "unsafeCasIntArray"
             , SafeSz . elemsMBA _mpa
             , \(MutableByteArray mba#) (I# i#) ->
                 primitive $ \s# ->
                 case casIntArray# mba# i# e# n# s# of
                   (# s'#, o# #) -> (# s'#, I# o# #))
  mba
  (o + toLinearIndex sz ix)
{-# INLINE unsafeCasIntArray #-}


-- | Atomically modify an `Int` element of the array. Returns the old value.
--
-- @since 0.3.0
unsafeAtomicModifyIntArray ::
     (Index ix, PrimMonad m) => MArray (PrimState m) P ix Int -> ix -> (Int -> Int) -> m Int
unsafeAtomicModifyIntArray _mpa@(MPArray sz o mba) ix f =
  INDEX_CHECK("unsafeAtomicModifyIntArray", SafeSz . elemsMBA _mpa, atomicModify)
  mba
  (o + toLinearIndex sz ix)
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
unsafeAtomicAddIntArray _mpa@(MPArray sz o mba) ix (I# e#) =
  INDEX_CHECK( "unsafeAtomicAddIntArray"
             , SafeSz . elemsMBA _mpa
             , \(MutableByteArray mba#) (I# i#) ->
                 primitive $ \s# ->
                 case fetchAddIntArray# mba# i# e# s# of
                   (# s'#, p# #) -> (# s'#, I# p# #))
  mba
  (o + toLinearIndex sz ix)
{-# INLINE unsafeAtomicAddIntArray #-}


-- | Atomically subtract from an `Int` element in the array. Returns the old value.
--
-- @since 0.3.0
unsafeAtomicSubIntArray ::
     (Index ix, PrimMonad m) => MArray (PrimState m) P ix Int -> ix -> Int -> m Int
unsafeAtomicSubIntArray _mpa@(MPArray sz o mba) ix (I# e#) =
  INDEX_CHECK( "unsafeAtomicSubIntArray"
             , SafeSz . elemsMBA _mpa
             , \(MutableByteArray mba#) (I# i#) ->
                 primitive $ \s# ->
                 case fetchSubIntArray# mba# i# e# s# of
                   (# s'#, p# #) -> (# s'#, I# p# #))
  mba
  (o + toLinearIndex sz ix)
{-# INLINE unsafeAtomicSubIntArray #-}


-- | Atomically AND an `Int` element in the array. Returns the old value.
--
-- @since 0.3.0
unsafeAtomicAndIntArray ::
     (Index ix, PrimMonad m) => MArray (PrimState m) P ix Int -> ix -> Int -> m Int
unsafeAtomicAndIntArray _mpa@(MPArray sz o mba) ix (I# e#) =
  INDEX_CHECK( "unsafeAtomicAndIntArray"
             , SafeSz . elemsMBA _mpa
             , \(MutableByteArray mba#) (I# i#) ->
                 primitive $ \s# ->
                 case fetchAndIntArray# mba# i# e# s# of
                   (# s'#, p# #) -> (# s'#, I# p# #))
  mba
  (o + toLinearIndex sz ix)
{-# INLINE unsafeAtomicAndIntArray #-}


-- | Atomically NAND an `Int` element in the array. Returns the old value.
--
-- @since 0.3.0
unsafeAtomicNandIntArray ::
     (Index ix, PrimMonad m) => MArray (PrimState m) P ix Int -> ix -> Int -> m Int
unsafeAtomicNandIntArray _mpa@(MPArray sz o mba) ix (I# e#) =
  INDEX_CHECK( "unsafeAtomicNandIntArray"
             , SafeSz . elemsMBA _mpa
             , \(MutableByteArray mba#) (I# i#) ->
                 primitive $ \s# ->
                 case fetchNandIntArray# mba# i# e# s# of
                   (# s'#, p# #) -> (# s'#, I# p# #))
  mba
  (o + toLinearIndex sz ix)
{-# INLINE unsafeAtomicNandIntArray #-}


-- | Atomically OR an `Int` element in the array. Returns the old value.
--
-- @since 0.3.0
unsafeAtomicOrIntArray ::
     (Index ix, PrimMonad m) => MArray (PrimState m) P ix Int -> ix -> Int -> m Int
unsafeAtomicOrIntArray _mpa@(MPArray sz o mba) ix (I# e#) =
  INDEX_CHECK( "unsafeAtomicOrIntArray"
             , SafeSz . elemsMBA _mpa
             , \(MutableByteArray mba#) (I# i#) ->
                 primitive $ \s# ->
                 case fetchOrIntArray# mba# i# e# s# of
                   (# s'#, p# #) -> (# s'#, I# p# #))
  mba
  (o + toLinearIndex sz ix)
{-# INLINE unsafeAtomicOrIntArray #-}


-- | Atomically XOR an `Int` element in the array. Returns the old value.
--
-- @since 0.3.0
unsafeAtomicXorIntArray ::
     (Index ix, PrimMonad m) => MArray (PrimState m) P ix Int -> ix -> Int -> m Int
unsafeAtomicXorIntArray _mpa@(MPArray sz o mba) ix (I# e#) =
  INDEX_CHECK( "unsafeAtomicXorIntArray"
             , SafeSz . elemsMBA _mpa
             , \(MutableByteArray mba#) (I# i#) ->
                 primitive $ \s# ->
                 case fetchXorIntArray# mba# i# e# s# of
                   (# s'#, p# #) -> (# s'#, I# p# #))
  mba
  (o + toLinearIndex sz ix)
{-# INLINE unsafeAtomicXorIntArray #-}


#if !MIN_VERSION_primitive(0,7,1)
shrinkMutableByteArray :: forall m. (PrimMonad m)
  => MutableByteArray (PrimState m)
  -> Int -- ^ new size
  -> m ()
shrinkMutableByteArray (MutableByteArray arr#) (I# n#)
  = primitive_ (shrinkMutableByteArray# arr# n#)
{-# INLINE shrinkMutableByteArray #-}
#endif

resizeMutableByteArrayCompat ::
  PrimMonad m => MutableByteArray (PrimState m) -> Int -> m (MutableByteArray (PrimState m))
#if MIN_VERSION_primitive(0,6,4)
resizeMutableByteArrayCompat = resizeMutableByteArray
#else
resizeMutableByteArrayCompat (MutableByteArray arr#) (I# n#) =
  primitive
    (\s# ->
       case resizeMutableByteArray# arr# n# s# of
         (# s'#, arr'# #) -> (# s'#, MutableByteArray arr'# #))
#endif
{-# INLINE resizeMutableByteArrayCompat #-}
