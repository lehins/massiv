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

import Control.DeepSeq (NFData(..), deepseq)
import Control.Monad.Primitive (PrimMonad(primitive), PrimState, primitive_)
import Control.Monad.ST (runST)
import Data.Massiv.Array.Delayed.Pull (eq, ord, delay)
import Data.Massiv.Array.Manifest.Internal
import Data.Massiv.Array.Manifest.List as A
import Data.Massiv.Array.Manifest.Vector.Stream as S (steps)
import Data.Massiv.Array.Mutable
import Data.Massiv.Core.Common
import Data.Massiv.Core.List
import Data.Massiv.Core.Operations
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

instance (Prim e, Eq e, Index ix) => Eq (Array P ix e) where
  (==) = eq (==)
  {-# INLINE (==) #-}

instance (Prim e, Ord e, Index ix) => Ord (Array P ix e) where
  compare = ord compare
  {-# INLINE compare #-}

instance (Prim e, Index ix) => Construct P ix e where
  makeArrayLinear !comp !sz f = unsafePerformIO $ generateArrayLinear comp sz (pure . f)
  {-# INLINE makeArrayLinear #-}

  makeConstantArray sz e = runST $ unsafeFreeze Seq =<< initializeNew (Just e) sz
  {-# INLINE makeConstantArray #-}


instance (Prim e, Index ix) => Source P ix e where
  unsafeLinearIndex (PArray _ _sz o a) i =
    INDEX_CHECK("(Source P ix e).unsafeLinearIndex",
                const (Sz (totalElem _sz)), indexByteArray a) (i + o)
  {-# INLINE unsafeLinearIndex #-}

  unsafeLinearSlice i k (PArray c _ o a) = PArray c k (o + i) a
  {-# INLINE unsafeLinearSlice #-}


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

  unsafeLinearIndexM _pa@(PArray _ _sz o a) i =
    INDEX_CHECK("(Manifest P ix e).unsafeLinearIndexM",
                const (Sz (totalElem _sz)), indexByteArray a) (i + o)
  {-# INLINE unsafeLinearIndexM #-}


instance (Index ix, Prim e) => Mutable P ix e where
  data MArray s P ix e = MPArray !(Sz ix) {-# UNPACK #-} !Int {-# UNPACK #-} !(MutableByteArray s)

  msize (MPArray sz _ _) = sz
  {-# INLINE msize #-}

  unsafeMutableSlice i k (MPArray _ o ma) = MPArray k (o + i) ma
  {-# INLINE unsafeMutableSlice #-}

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
    fillByteArray mba o (totalElem sz * sizeOf (undefined :: e)) 0
  {-# INLINE initialize #-}

  unsafeLinearRead _mpa@(MPArray _sz o ma) i =
    INDEX_CHECK("(Mutable P ix e).unsafeLinearRead",
                const (Sz (totalElem _sz)), readByteArray ma) (i + o)
  {-# INLINE unsafeLinearRead #-}

  unsafeLinearWrite _mpa@(MPArray _sz o ma) i =
    INDEX_CHECK("(Mutable P ix e).unsafeLinearWrite",
                const (Sz (totalElem _sz)), writeByteArray ma) (i + o)
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
  type R P = M
  size = pSize
  {-# INLINE size #-}
  getComp = pComp
  {-# INLINE getComp #-}
  setComp c arr = arr { pComp = c }
  {-# INLINE setComp #-}
  loadArrayM !scheduler !arr =
    splitLinearlyWith_ scheduler (elemsCount arr) (unsafeLinearIndex arr)
  {-# INLINE loadArrayM #-}

instance (Prim e, Index ix) => StrideLoad P ix e

instance (Prim e, Index ix) => Stream P ix e where
  toStream = S.steps
  {-# INLINE toStream #-}

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


instance (Prim e, Num e) => NumArray P e where
  liftNumArray = liftArray
  {-# INLINE liftNumArray #-}
  unsafeLiftNumArray2 = unsafeLiftArray2
  {-# INLINE unsafeLiftNumArray2 #-}

instance (Prim e, Num e) => ReduceNumArray P e where
  multiplySumArrayS a1 a2 = sumArrayS (multiplicationPointwise (delay a1) (delay a2))
  {-# INLINE multiplySumArrayS #-}
  evenPowerSumArrayS arr = evenPowerSumArrayS (delay arr)
  {-# INLINE evenPowerSumArrayS #-}
  absPowerSumArrayS arr = absPowerSumArrayS (delay arr)
  {-# INLINE absPowerSumArrayS #-}
  absMaxArrayS = maximumArrayS 0 . absPointwise . delay
  {-# INLINE absMaxArrayS #-}

instance (Prim e, Ord e) => ReduceOrdArray P e

instance (Prim e, Floating e) => FloatArray P e

instance RoundFloatArray P Float Float where
  roundPointwise = liftArray roundFloat
  {-# INLINE roundPointwise #-}

instance RoundFloatArray P Double Double where
  roundPointwise = liftArray roundDouble
  {-# INLINE roundPointwise #-}


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
    arr = PArray comp sz 0 ba
{-# INLINE fromByteArrayM #-}

-- | /O(1)/ - Construct a flat Array from `ByteArray`
--
-- @since 0.4.0
fromByteArray :: forall e . Prim e => Comp -> ByteArray -> Array P Ix1 e
fromByteArray comp ba = PArray comp (SafeSz (elemsBA (Proxy :: Proxy e) ba)) 0 ba
{-# INLINE fromByteArray #-}


-- TODO: memmove and shrink if non-zer offset
-- | /O(1)/ - Extract the internal `MutableByteArray`.
--
-- @since 0.2.1
toMutableByteArray :: MArray s P ix e -> MutableByteArray s
toMutableByteArray (MPArray _ _ mba) = mba
{-# INLINE toMutableByteArray #-}


-- | /O(1)/ - Construct a primitive mutable array from the `MutableByteArray`. Will throw
-- `SizeElementsMismatchException` if number of elements doesn't match.
--
-- @since 0.3.0
fromMutableByteArrayM ::
     (MonadThrow m, Index ix, Prim e) => Sz ix -> MutableByteArray s -> m (MArray s P ix e)
fromMutableByteArrayM sz mba =
  guardNumberOfElements sz (Sz (elemsMBA marr mba)) >> pure marr
  where
    marr = MPArray sz 0 mba
{-# INLINE fromMutableByteArrayM #-}

-- | /O(1)/ - Construct a flat Array from `MutableByteArray`
--
-- @since 0.4.0
fromMutableByteArray :: forall e s . Prim e => MutableByteArray s -> MArray s P Ix1 e
fromMutableByteArray mba = MPArray (SafeSz (elemsMBA (Proxy :: Proxy e) mba)) 0 mba
{-# INLINE fromMutableByteArray #-}


-- | Atomically read an `Int` element from the array
--
-- @since 0.3.0
unsafeAtomicReadIntArray ::
     (Index ix, PrimMonad m) => MArray (PrimState m) P ix Int -> ix -> m Int
unsafeAtomicReadIntArray _mpa@(MPArray sz o mba) ix =
  INDEX_CHECK( "unsafeAtomicReadIntArray"
             , Sz . totalElem . size
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
unsafeAtomicWriteIntArray (MPArray sz o mba) ix (I# e#) =
  INDEX_CHECK( "unsafeAtomicWriteIntArray"
             , const (Sz (totalElem sz))
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
unsafeCasIntArray (MPArray sz o mba) ix (I# e#) (I# n#) =
  INDEX_CHECK( "unsafeCasIntArray"
             , const (Sz (totalElem sz))
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
  INDEX_CHECK("unsafeAtomicModifyIntArray", const (Sz (totalElem sz)), atomicModify)
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
unsafeAtomicAddIntArray (MPArray sz o mba) ix (I# e#) =
  INDEX_CHECK( "unsafeAtomicAddIntArray"
             , const (Sz (totalElem sz))
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
unsafeAtomicSubIntArray (MPArray sz o mba) ix (I# e#) =
  INDEX_CHECK( "unsafeAtomicSubIntArray"
             , const (Sz (totalElem sz))
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
unsafeAtomicAndIntArray (MPArray sz o mba) ix (I# e#) =
  INDEX_CHECK( "unsafeAtomicAndIntArray"
             , const (Sz (totalElem sz))
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
unsafeAtomicNandIntArray (MPArray sz o mba) ix (I# e#) =
  INDEX_CHECK( "unsafeAtomicNandIntArray"
             , const (Sz (totalElem sz))
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
             , const (Sz (totalElem sz))
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
unsafeAtomicXorIntArray (MPArray sz o mba) ix (I# e#) =
  INDEX_CHECK( "unsafeAtomicXorIntArray"
             , const (Sz (totalElem sz))
             , \(MutableByteArray mba#) (I# i#) ->
                 primitive $ \s# ->
                 case fetchXorIntArray# mba# i# e# s# of
                   (# s'#, p# #) -> (# s'#, I# p# #))
  mba
  (o + toLinearIndex sz ix)
{-# INLINE unsafeAtomicXorIntArray #-}


shrinkMutableByteArray :: forall m. (PrimMonad m)
  => MutableByteArray (PrimState m)
  -> Int -- ^ new size
  -> m ()
shrinkMutableByteArray (MutableByteArray arr#) (I# n#)
  = primitive_ (shrinkMutableByteArray# arr# n#)
{-# INLINE shrinkMutableByteArray #-}


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
