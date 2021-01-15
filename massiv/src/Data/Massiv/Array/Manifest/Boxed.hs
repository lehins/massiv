{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Massiv.Array.Manifest.Boxed
-- Copyright   : (c) Alexey Kuleshevich 2018-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Manifest.Boxed
  ( B(..)
  , BL(..)
  , BN(..)
  , N
  , pattern N
  , Array(..)
  , wrapLazyArray
  , unwrapLazyArray
  , unwrapNormalForm
  , evalNormalForm
  , unwrapArray
  , evalArray
  , toLazyArray
  , evalLazyArray
  , forceLazyArray
  , unwrapMutableArray
  , unwrapMutableLazyArray
  , evalMutableArray
  , unwrapNormalFormArray
  , evalNormalFormArray
  , unwrapNormalFormMutableArray
  , evalNormalFormMutableArray
  , toBoxedVector
  , toBoxedMVector
  , fromBoxedVector
  , fromBoxedMVector
  , evalBoxedVector
  , evalBoxedMVector
  , evalNormalBoxedVector
  , evalNormalBoxedMVector
  , coerceBoxedArray
  , coerceNormalBoxedArray
  , seqArray
  , deepseqArray
  ) where

import Control.DeepSeq (NFData(..), deepseq)
import Control.Exception
import Control.Monad ((>=>))
import Control.Monad.Primitive
import Control.Monad.ST (runST)
import qualified Data.Foldable as F (Foldable(..))
import Data.Massiv.Array.Delayed.Push (DL)
import Data.Massiv.Array.Delayed.Stream (DS)
import Data.Massiv.Array.Manifest.Internal (M, computeAs, toManifest)
import Data.Massiv.Array.Manifest.List as L
import Data.Massiv.Array.Mutable
import Data.Massiv.Array.Ops.Fold
import Data.Massiv.Array.Ops.Fold.Internal
import Data.Massiv.Array.Ops.Map (traverseA)
import Data.Massiv.Core.Common
import Data.Massiv.Core.List
import Data.Massiv.Core.Operations
import Data.Massiv.Vector.Stream as S (isteps, steps)
import qualified Data.Primitive.Array as A
import qualified Data.Vector as VB
import qualified Data.Vector.Mutable as MVB
import GHC.Exts as GHC
import Prelude hiding (mapM)
import System.IO.Unsafe (unsafePerformIO)

#include "massiv.h"

sizeofArray :: A.Array e -> Int
sizeofMutableArray :: A.MutableArray s e -> Int
#if MIN_VERSION_primitive(0,6,2)
sizeofArray = A.sizeofArray
sizeofMutableArray = A.sizeofMutableArray
#else
sizeofArray (A.Array a#) = I# (sizeofArray# a#)
sizeofMutableArray (A.MutableArray ma#) = I# (sizeofMutableArray# ma#)
#endif

----------------
-- Boxed Lazy --
----------------

-- | Array representation for Boxed elements. This data structure is lazy with respect to
-- its elements, but is strict with respect to the spine.
data BL = BL deriving Show

data instance Array BL ix e = BLArray { blComp   :: !Comp
                                      , blSize   :: !(Sz ix)
                                      , blOffset :: {-# UNPACK #-} !Int
                                      , blData   :: {-# UNPACK #-} !(A.Array e)
                                      }

instance (Ragged L ix e, Show e) => Show (Array BL ix e) where
  showsPrec = showsArrayPrec id
  showList = showArrayList

instance (Ragged L ix e, Show e) => Show (Array DL ix e) where
  showsPrec = showsArrayPrec (computeAs BL)
  showList = showArrayList

instance Show e => Show (Array DS Ix1 e) where
  showsPrec = showsArrayPrec (computeAs BL)
  showList = showArrayList


-- instance (Index ix, NFData e) => NFData (Array BL ix e) where
--   rnf = (`deepseqArray` ())
--   {-# INLINE rnf #-}

instance (Index ix, Eq e) => Eq (Array BL ix e) where
  (==) = eqArrays (==)
  {-# INLINE (==) #-}

instance (Index ix, Ord e) => Ord (Array BL ix e) where
  compare = compareArrays compare
  {-# INLINE compare #-}

instance Index ix => Construct BL ix e where
  setComp c arr = arr { blComp = c }
  {-# INLINE setComp #-}

  makeArrayLinear !comp !sz f = unsafePerformIO $ generateArrayLinear comp sz (pure . f)
  {-# INLINE makeArrayLinear #-}

instance Index ix => Source BL ix e where
  unsafeLinearIndex (BLArray _ _sz o a) i =
    INDEX_CHECK("(Source BL ix e).unsafeLinearIndex",
                SafeSz . sizeofArray, A.indexArray) a (i + o)
  {-# INLINE unsafeLinearIndex #-}

  unsafeLinearSlice i k (BLArray c _ o a) = BLArray c k (o + i) a
  {-# INLINE unsafeLinearSlice #-}


instance Index ix => Resize BL ix where
  unsafeResize !sz !arr = arr { blSize = sz }
  {-# INLINE unsafeResize #-}

instance Index ix => Extract BL ix e where
  unsafeExtract !sIx !newSz !arr = unsafeExtract sIx newSz (toManifest arr)
  {-# INLINE unsafeExtract #-}


instance ( Index ix
         , Index (Lower ix)
         , Elt M ix e ~ Array M (Lower ix) e
         , Elt BL ix e ~ Array M (Lower ix) e
         ) =>
         OuterSlice BL ix e where
  unsafeOuterSlice arr = unsafeOuterSlice (toManifest arr)
  {-# INLINE unsafeOuterSlice #-}

instance ( Index ix
         , Index (Lower ix)
         , Elt M ix e ~ Array M (Lower ix) e
         , Elt BL ix e ~ Array M (Lower ix) e
         ) =>
         InnerSlice BL ix e where
  unsafeInnerSlice arr = unsafeInnerSlice (toManifest arr)
  {-# INLINE unsafeInnerSlice #-}

instance {-# OVERLAPPING #-} Slice BL Ix1 e where
  unsafeSlice arr i _ _ = pure (unsafeLinearIndex arr i)
  {-# INLINE unsafeSlice #-}


instance Index ix => Manifest BL ix e where

  unsafeLinearIndexM (BLArray _ _sz o a) i =
    INDEX_CHECK("(Manifest BL ix e).unsafeLinearIndexM",
                SafeSz . sizeofArray, A.indexArray) a (i + o)
  {-# INLINE unsafeLinearIndexM #-}


instance Index ix => Mutable BL ix e where
  data MArray s BL ix e = MBLArray !(Sz ix) {-# UNPACK #-} !Int {-# UNPACK #-} !(A.MutableArray s e)

  msize (MBLArray sz _ _) = sz
  {-# INLINE msize #-}

  unsafeThaw (BLArray _ sz o a) = MBLArray sz o <$> A.unsafeThawArray a
  {-# INLINE unsafeThaw #-}

  unsafeFreeze comp (MBLArray sz o ma) = BLArray comp sz o <$> A.unsafeFreezeArray ma
  {-# INLINE unsafeFreeze #-}

  unsafeNew sz = MBLArray sz 0 <$> A.newArray (totalElem sz) uninitialized
  {-# INLINE unsafeNew #-}

  initialize _ = return ()
  {-# INLINE initialize #-}

  unsafeLinearRead (MBLArray _ o ma) i =
    INDEX_CHECK("(Mutable BL ix e).unsafeLinearRead",
                SafeSz . sizeofMutableArray, A.readArray) ma (i + o)
  {-# INLINE unsafeLinearRead #-}

  unsafeLinearWrite (MBLArray _sz o ma) i e = e `seq`
    INDEX_CHECK("(Mutable BL ix e).unsafeLinearWrite",
                SafeSz . sizeofMutableArray, A.writeArray) ma (i + o) e
  {-# INLINE unsafeLinearWrite #-}

instance Index ix => Load BL ix e where
  type R BL = M
  size = blSize
  {-# INLINE size #-}
  getComp = blComp
  {-# INLINE getComp #-}
  loadArrayM !scheduler !arr = splitLinearlyWith_ scheduler (elemsCount arr) (unsafeLinearIndex arr)
  {-# INLINE loadArrayM #-}

instance Index ix => StrideLoad BL ix e

instance Index ix => Stream BL ix e where
  toStream = S.steps
  {-# INLINE toStream #-}
  toStreamIx = S.isteps
  {-# INLINE toStreamIx #-}


-- | Row-major sequential folding over a Boxed array.
instance Index ix => Foldable (Array BL ix) where
  fold = fold
  {-# INLINE fold #-}
  foldMap = foldMono
  {-# INLINE foldMap #-}
  foldl = lazyFoldlS
  {-# INLINE foldl #-}
  foldl' = foldlS
  {-# INLINE foldl' #-}
  foldr = foldrFB
  {-# INLINE foldr #-}
  foldr' = foldrS
  {-# INLINE foldr' #-}
  null (BLArray _ sz _ _) = totalElem sz == 0
  {-# INLINE null #-}
  length = totalElem . size
  {-# INLINE length #-}
  toList arr = build (\ c n -> foldrFB c n arr)
  {-# INLINE toList #-}


instance Index ix => Functor (Array BL ix) where
  fmap f arr = makeArrayLinear (blComp arr) (blSize arr) (f . unsafeLinearIndex arr)
  {-# INLINE fmap #-}

instance Index ix => Traversable (Array BL ix) where
  traverse = traverseA
  {-# INLINE traverse #-}

instance ( IsList (Array L ix e)
         , Nested LN ix e
         , Nested L ix e
         , Ragged L ix e
         ) =>
         IsList (Array BL ix e) where
  type Item (Array BL ix e) = Item (Array L ix e)
  fromList = L.fromLists' Seq
  {-# INLINE fromList #-}
  toList = GHC.toList . toListArray
  {-# INLINE toList #-}

instance Num e => FoldNumeric BL e where
  unsafeDotProduct = defaultUnsafeDotProduct
  {-# INLINE unsafeDotProduct #-}
  powerSumArray = defaultPowerSumArray
  {-# INLINE powerSumArray #-}
  foldArray = defaultFoldArray
  {-# INLINE foldArray #-}

instance Num e => Numeric BL e where
  unsafeLiftArray = defaultUnsafeLiftArray
  {-# INLINE unsafeLiftArray #-}
  unsafeLiftArray2 = defaultUnsafeLiftArray2
  {-# INLINE unsafeLiftArray2 #-}



------------------
-- Boxed Strict --
------------------

-- | Array representation for Boxed elements. This structure is element and
-- spine strict, but elements are strict to Weak Head Normal Form (WHNF) only.
data B = B deriving Show

newtype instance Array B ix e = BArray (Array BL ix e)

instance (Ragged L ix e, Show e) => Show (Array B ix e) where
  showsPrec = showsArrayPrec id
  showList = showArrayList

-- instance (Index ix, NFData e) => NFData (Array B ix e) where
--   rnf = (`deepseqArray` ())
--   {-# INLINE rnf #-}

instance (Index ix, Eq e) => Eq (Array B ix e) where
  (==) = eqArrays (==)
  {-# INLINE (==) #-}

instance (Index ix, Ord e) => Ord (Array B ix e) where
  compare = compareArrays compare
  {-# INLINE compare #-}

instance Index ix => Construct B ix e where
  setComp c = coerce (\arr -> arr { blComp = c })
  {-# INLINE setComp #-}

  makeArrayLinear !comp !sz f = unsafePerformIO $ generateArrayLinear comp sz (pure . f)
  {-# INLINE makeArrayLinear #-}

instance Index ix => Source B ix e where
  unsafeLinearIndex arr = unsafeLinearIndex (coerce arr :: Array BL ix e)
  {-# INLINE unsafeLinearIndex #-}

  unsafeLinearSlice i k arr = coerce (unsafeLinearSlice i k (coerce arr :: Array BL ix e))
  {-# INLINE unsafeLinearSlice #-}


instance Index ix => Resize B ix where
  unsafeResize sz = coerce (\arr -> arr { blSize = sz })
  {-# INLINE unsafeResize #-}

instance Index ix => Extract B ix e where
  unsafeExtract !sIx !newSz !arr = unsafeExtract sIx newSz (toManifest arr)
  {-# INLINE unsafeExtract #-}


instance ( Index ix
         , Index (Lower ix)
         , Elt M ix e ~ Array M (Lower ix) e
         , Elt B ix e ~ Array M (Lower ix) e
         ) =>
         OuterSlice B ix e where
  unsafeOuterSlice arr = unsafeOuterSlice (toManifest arr)
  {-# INLINE unsafeOuterSlice #-}

instance ( Index ix
         , Index (Lower ix)
         , Elt M ix e ~ Array M (Lower ix) e
         , Elt B ix e ~ Array M (Lower ix) e
         ) =>
         InnerSlice B ix e where
  unsafeInnerSlice arr = unsafeInnerSlice (toManifest arr)
  {-# INLINE unsafeInnerSlice #-}

instance {-# OVERLAPPING #-} Slice B Ix1 e where
  unsafeSlice arr i _ _ = pure (unsafeLinearIndex arr i)
  {-# INLINE unsafeSlice #-}


instance Index ix => Manifest B ix e where

  unsafeLinearIndexM = coerce unsafeLinearIndexM
  {-# INLINE unsafeLinearIndexM #-}


instance Index ix => Mutable B ix e where
  newtype MArray s B ix e = MBArray (MArray s BL ix e)

  msize = msize . coerce
  {-# INLINE msize #-}

  unsafeThaw arr = MBArray <$> unsafeThaw (coerce arr)
  {-# INLINE unsafeThaw #-}

  unsafeFreeze comp marr = BArray <$> unsafeFreeze comp (coerce marr)
  {-# INLINE unsafeFreeze #-}

  unsafeNew sz = MBArray <$> unsafeNew sz
  {-# INLINE unsafeNew #-}

  initialize _ = return ()
  {-# INLINE initialize #-}

  unsafeLinearRead ma = unsafeLinearRead (coerce ma)
  {-# INLINE unsafeLinearRead #-}

  unsafeLinearWrite ma i e = e `seq` unsafeLinearWrite (coerce ma) i e
  {-# INLINE unsafeLinearWrite #-}

instance Index ix => Load B ix e where
  type R B = M
  size = blSize . coerce
  {-# INLINE size #-}
  getComp = blComp . coerce
  {-# INLINE getComp #-}
  loadArrayM scheduler = coerce (loadArrayM scheduler)
  {-# INLINE loadArrayM #-}

instance Index ix => StrideLoad B ix e

instance Index ix => Stream B ix e where
  toStream = S.steps
  {-# INLINE toStream #-}
  toStreamIx = S.isteps
  {-# INLINE toStreamIx #-}


-- | Row-major sequential folding over a Boxed array.
instance Index ix => Foldable (Array B ix) where
  fold = fold
  {-# INLINE fold #-}
  foldMap = foldMono
  {-# INLINE foldMap #-}
  foldl = lazyFoldlS
  {-# INLINE foldl #-}
  foldl' = foldlS
  {-# INLINE foldl' #-}
  foldr = foldrFB
  {-# INLINE foldr #-}
  foldr' = foldrS
  {-# INLINE foldr' #-}
  null arr = totalElem (size arr) == 0
  {-# INLINE null #-}
  length = totalElem . size
  {-# INLINE length #-}
  toList arr = build (\ c n -> foldrFB c n arr)
  {-# INLINE toList #-}


instance Index ix => Functor (Array B ix) where
  fmap f arr = makeArrayLinear (getComp arr) (size arr) (f . unsafeLinearIndex arr)
  {-# INLINE fmap #-}

instance Index ix => Traversable (Array B ix) where
  traverse = traverseA
  {-# INLINE traverse #-}

instance ( IsList (Array L ix e)
         , Nested LN ix e
         , Nested L ix e
         , Ragged L ix e
         ) =>
         IsList (Array B ix e) where
  type Item (Array B ix e) = Item (Array L ix e)
  fromList = L.fromLists' Seq
  {-# INLINE fromList #-}
  toList = GHC.toList . toListArray
  {-# INLINE toList #-}

instance Num e => FoldNumeric B e where
  unsafeDotProduct = defaultUnsafeDotProduct
  {-# INLINE unsafeDotProduct #-}
  powerSumArray = defaultPowerSumArray
  {-# INLINE powerSumArray #-}
  foldArray = defaultFoldArray
  {-# INLINE foldArray #-}

instance Num e => Numeric B e where
  unsafeLiftArray = defaultUnsafeLiftArray
  {-# INLINE unsafeLiftArray #-}
  unsafeLiftArray2 = defaultUnsafeLiftArray2
  {-# INLINE unsafeLiftArray2 #-}

-----------------------
-- Boxed Normal Form --
-----------------------

-- | Array representation for Boxed elements. This structure is element and
-- spine strict, and elements are always in Normal Form (NF), therefore `NFData`
-- instance is required.
data BN = BN deriving Show

-- | Type and pattern `N` have been added for backwards compatibility and will be replaced
-- in the future in favor of `BN`
type N = BN
pattern N :: N
pattern N = BN
{-# COMPLETE N #-}

newtype instance Array N ix e = NArray { bArray :: Array BL ix e }

instance (Ragged L ix e, Show e, NFData e) => Show (Array N ix e) where
  showsPrec = showsArrayPrec bArray
  showList = showArrayList

-- instance (Index ix, NFData e) => NFData (Array N ix e) where
--   rnf (NArray barr) = barr `deepseqArray` ()
--   {-# INLINE rnf #-}

instance (Index ix, NFData e, Eq e) => Eq (Array N ix e) where
  (==) = eqArrays (==)
  {-# INLINE (==) #-}

instance (Index ix, NFData e, Ord e) => Ord (Array N ix e) where
  compare = compareArrays compare
  {-# INLINE compare #-}


instance (Index ix, NFData e) => Construct N ix e where
  setComp c (NArray arr) = NArray (arr {blComp = c})
  {-# INLINE setComp #-}
  makeArrayLinear !comp !sz f = unsafePerformIO $ generateArrayLinear comp sz (pure . f)
  {-# INLINE makeArrayLinear #-}

instance (Index ix, NFData e) => Source N ix e where
  unsafeLinearIndex (NArray arr) = unsafeLinearIndex arr
  {-# INLINE unsafeLinearIndex #-}
  unsafeLinearSlice i k (NArray a) = NArray $ unsafeLinearSlice i k a
  {-# INLINE unsafeLinearSlice #-}


instance Index ix => Resize N ix where
  unsafeResize !sz = NArray . unsafeResize sz . bArray
  {-# INLINE unsafeResize #-}

instance (Index ix, NFData e) => Extract N ix e where
  unsafeExtract !sIx !newSz !arr = unsafeExtract sIx newSz (toManifest arr)
  {-# INLINE unsafeExtract #-}


instance ( NFData e
         , Index ix
         , Index (Lower ix)
         , Elt M ix e ~ Array M (Lower ix) e
         , Elt N ix e ~ Array M (Lower ix) e
         ) =>
         OuterSlice N ix e where
  unsafeOuterSlice = unsafeOuterSlice . toManifest
  {-# INLINE unsafeOuterSlice #-}

instance ( NFData e
         , Index ix
         , Index (Lower ix)
         , Elt M ix e ~ Array M (Lower ix) e
         , Elt N ix e ~ Array M (Lower ix) e
         ) =>
         InnerSlice N ix e where
  unsafeInnerSlice = unsafeInnerSlice . toManifest
  {-# INLINE unsafeInnerSlice #-}

instance {-# OVERLAPPING #-} NFData e => Slice N Ix1 e where
  unsafeSlice arr i _ _ = pure (unsafeLinearIndex arr i)
  {-# INLINE unsafeSlice #-}


instance (Index ix, NFData e) => Manifest N ix e where

  unsafeLinearIndexM arr = unsafeLinearIndexM (coerce arr)
  {-# INLINE unsafeLinearIndexM #-}


instance (Index ix, NFData e) => Mutable N ix e where
  newtype MArray s N ix e = MNArray (MArray s BL ix e)

  msize = msize . coerce
  {-# INLINE msize #-}

  unsafeThaw arr = MNArray <$> unsafeThaw (coerce arr)
  {-# INLINE unsafeThaw #-}

  unsafeFreeze comp marr = NArray <$> unsafeFreeze comp (coerce marr)
  {-# INLINE unsafeFreeze #-}

  unsafeNew sz = MNArray <$> unsafeNew sz
  {-# INLINE unsafeNew #-}

  initialize _ = return ()
  {-# INLINE initialize #-}

  unsafeLinearRead ma = unsafeLinearRead (coerce ma)
  {-# INLINE unsafeLinearRead #-}

  unsafeLinearWrite ma i e = e `deepseq` unsafeLinearWrite (coerce ma) i e
  {-# INLINE unsafeLinearWrite #-}

instance (Index ix, NFData e) => Load N ix e where
  type R N = M
  size = blSize . coerce
  {-# INLINE size #-}
  getComp = blComp . coerce
  {-# INLINE getComp #-}
  loadArrayM !scheduler !arr = splitLinearlyWith_ scheduler (elemsCount arr) (unsafeLinearIndex arr)
  {-# INLINE loadArrayM #-}

instance (Index ix, NFData e) => StrideLoad N ix e

instance (Index ix, NFData e) => Stream N ix e where
  toStream = toStream . coerce
  {-# INLINE toStream #-}
  toStreamIx = toStreamIx . coerce
  {-# INLINE toStreamIx #-}


instance ( NFData e
         , IsList (Array L ix e)
         , Nested LN ix e
         , Nested L ix e
         , Ragged L ix e
         ) =>
         IsList (Array N ix e) where
  type Item (Array N ix e) = Item (Array L ix e)
  fromList = L.fromLists' Seq
  {-# INLINE fromList #-}
  toList = GHC.toList . toListArray
  {-# INLINE toList #-}

instance (NFData e, Num e) => FoldNumeric N e where
  unsafeDotProduct = defaultUnsafeDotProduct
  {-# INLINE unsafeDotProduct #-}
  powerSumArray = defaultPowerSumArray
  {-# INLINE powerSumArray #-}
  foldArray = defaultFoldArray
  {-# INLINE foldArray #-}

instance (NFData e, Num e) => Numeric N e where
  unsafeLiftArray = defaultUnsafeLiftArray
  {-# INLINE unsafeLiftArray #-}
  unsafeLiftArray2 = defaultUnsafeLiftArray2
  {-# INLINE unsafeLiftArray2 #-}

----------------------
-- Helper functions --
----------------------

uninitialized :: a
uninitialized = throw Uninitialized

---------------------
-- WHNF conversion --
---------------------

-- | /O(1)/ - Unwrap boxed array. This will discard any possible slicing that has been
-- applied to the array.
--
-- @since 0.2.1
unwrapArray :: Array B ix e -> A.Array e
unwrapArray = blData . coerce
{-# INLINE unwrapArray #-}

-- | /O(n)/ - Wrap a boxed array and evaluate all elements to a WHNF.
--
-- @since 0.2.1
evalArray ::
     Comp -- ^ Computation strategy
  -> A.Array e -- ^ Lazy boxed array from @primitive@ package.
  -> Vector B e
evalArray comp a = evalLazyArray $ setComp comp $ wrapLazyArray a
{-# INLINE evalArray #-}


-- | /O(1)/ - Unwrap boxed array. This will discard any possible slicing that has been
-- applied to the array.
--
-- @since 0.6.0
unwrapLazyArray :: Array BL ix e -> A.Array e
unwrapLazyArray = blData
{-# INLINE unwrapLazyArray #-}

-- | /O(1)/ - Wrap a boxed array.
--
-- @since 0.6.0
wrapLazyArray :: A.Array e -> Vector BL e
wrapLazyArray a = BLArray Seq (SafeSz (sizeofArray a)) 0 a
{-# INLINE wrapLazyArray #-}


-- | /O(n)/ - Evaluate all elements of a boxed lazy array to weak head normal form
--
-- @since 0.6.0
toLazyArray :: Array B ix e -> Array BL ix e
toLazyArray = coerce
{-# INLINE toLazyArray #-}

-- | /O(n)/ - Evaluate all elements of a boxed lazy array to weak head normal form
--
-- @since 0.6.0
evalLazyArray :: Index ix => Array BL ix e -> Array B ix e
evalLazyArray arr = arr `seqArray` BArray arr
{-# INLINE evalLazyArray #-}

-- | /O(n)/ - Evaluate all elements of a boxed lazy array to normal form
--
-- @since 0.6.0
forceLazyArray :: (NFData e, Index ix) => Array BL ix e -> Array N ix e
forceLazyArray arr = arr `deepseqArray` NArray arr
{-# INLINE forceLazyArray #-}

-- | /O(1)/ - Unwrap mutable boxed array. This will discard any possible slicing that has been
-- applied to the array.
--
-- @since 0.2.1
unwrapMutableArray :: MArray s B ix e -> A.MutableArray s e
unwrapMutableArray (MBArray (MBLArray _ _ marr)) = marr
{-# INLINE unwrapMutableArray #-}


-- | /O(1)/ - Unwrap mutable boxed lazy array. This will discard any possible slicing that has been
-- applied to the array.
--
-- @since 0.6.0
unwrapMutableLazyArray :: MArray s BL ix e -> A.MutableArray s e
unwrapMutableLazyArray (MBLArray _ _ marr) = marr
{-# INLINE unwrapMutableLazyArray #-}


-- | /O(n)/ - Wrap mutable boxed array and evaluate all elements to WHNF.
--
-- @since 0.2.1
evalMutableArray ::
     PrimMonad m
  => A.MutableArray (PrimState m) e -- ^ Mutable array that will get wrapped
  -> m (MArray (PrimState m) B Ix1 e)
evalMutableArray = fmap MBArray . fromMutableArraySeq seq
{-# INLINE evalMutableArray #-}

-------------------
-- NF conversion --
-------------------

-- | /O(1)/ - Unwrap a fully evaluated boxed array. This will discard any possible slicing
-- that has been applied to the array.
--
-- @since 0.2.1
unwrapNormalFormArray :: Array N ix e -> A.Array e
unwrapNormalFormArray = blData . coerce
{-# INLINE unwrapNormalFormArray #-}

-- | /O(n)/ - Wrap a boxed array and evaluate all elements to a Normal Form (NF).
--
-- @since 0.2.1
evalNormalFormArray ::
     NFData e
  => Comp -- ^ Computation strategy
  -> A.Array e -- ^ Lazy boxed array
  -> Array N Ix1 e
evalNormalFormArray comp = forceLazyArray . setComp comp . wrapLazyArray
{-# INLINE evalNormalFormArray #-}


-- | /O(1)/ - Unwrap a fully evaluated mutable boxed array. This will discard any possible
-- slicing that has been applied to the array.
--
-- @since 0.2.1
unwrapNormalFormMutableArray :: MArray s N ix e -> A.MutableArray s e
unwrapNormalFormMutableArray = unwrapMutableLazyArray . coerce
{-# INLINE unwrapNormalFormMutableArray #-}


-- | /O(n)/ - Wrap mutable boxed array and evaluate all elements to NF.
--
-- @since 0.2.1
evalNormalFormMutableArray ::
     (PrimMonad m, NFData e)
  => A.MutableArray (PrimState m) e
  -> m (MArray (PrimState m) N Ix1 e)
evalNormalFormMutableArray marr = MNArray <$> fromMutableArraySeq deepseq marr
{-# INLINE evalNormalFormMutableArray #-}


-- ----------------------
-- -- Helper functions --
-- ----------------------

fromMutableArraySeq ::
     PrimMonad m
  => (e -> m () -> m a)
  -> A.MutableArray (PrimState m) e
  -> m (MArray (PrimState m) BL Ix1 e)
fromMutableArraySeq with ma = do
  let !sz = sizeofMutableArray ma
  loopM_ 0 (< sz) (+ 1) (A.readArray ma >=> (`with` return ()))
  return $! MBLArray (SafeSz sz) 0 ma
{-# INLINE fromMutableArraySeq #-}


seqArray :: Index ix => Array BL ix a -> t -> t
seqArray !arr t = foldlInternal (flip seq) () (flip seq) () arr `seq` t
{-# INLINE seqArray #-}


deepseqArray :: (NFData a, Index ix) => Array BL ix a -> t -> t
deepseqArray !arr t = foldlInternal (flip deepseq) () (flip seq) () arr `seq` t
{-# INLINE deepseqArray #-}


-- | /O(1)/ - Converts array from `N` to `B` representation.
--
-- @since 0.5.0
unwrapNormalForm :: Array N ix e -> Array B ix e
unwrapNormalForm = coerce
{-# INLINE unwrapNormalForm #-}

-- | /O(n)/ - Compute all elements of a boxed array to NF (normal form)
--
-- @since 0.5.0
evalNormalForm :: (Index ix, NFData e) => Array B ix e -> Array N ix e
evalNormalForm (BArray arr) = arr `deepseqArray` NArray arr
{-# INLINE evalNormalForm #-}

-- | /O(1)/ - Converts a boxed `Array` into a `VB.Vector`.
--
-- @since 0.5.0
toBoxedVector :: Index ix => Array BL ix a -> VB.Vector a
toBoxedVector arr = runST $ VB.unsafeFreeze . toBoxedMVector =<< unsafeThaw arr
{-# INLINE toBoxedVector #-}

-- | /O(1)/ - Converts a boxed `MArray` into a `VMB.MVector`.
--
-- @since 0.5.0
toBoxedMVector :: Index ix => MArray s BL ix a -> MVB.MVector s a
toBoxedMVector (MBLArray sz o marr) = MVB.MVector o (totalElem sz) marr
{-# INLINE toBoxedMVector #-}

-- | /O(n)/ - Convert a boxed vector and evaluate all elements to WHNF. Computation
-- strategy will be respected during evaluation
--
-- @since 0.5.0
evalBoxedVector :: Comp -> VB.Vector a -> Array B Ix1 a
evalBoxedVector comp = evalLazyArray . setComp comp . fromBoxedVector
{-# INLINE evalBoxedVector #-}


-- | /O(n)/ - Convert mutable boxed vector and evaluate all elements to WHNF
-- sequentially. Both keep pointing to the same memory
--
-- @since 0.5.0
evalBoxedMVector :: PrimMonad m => MVB.MVector (PrimState m) a -> m (MArray (PrimState m) B Ix1 a)
evalBoxedMVector (MVB.MVector o k ma) =
  let marr = MBArray (MBLArray (SafeSz k) o ma)
   in marr <$ loopM_ o (< k) (+ 1) (A.readArray ma >=> (`seq` pure ()))
{-# INLINE evalBoxedMVector #-}


-- | /O(1)/ - Cast a boxed vector without touching any elements.
--
-- @since 0.6.0
fromBoxedVector :: VB.Vector a -> Vector BL a
fromBoxedVector v =
  runST $ do
    MVB.MVector o k ma <- VB.unsafeThaw v
    unsafeFreeze Seq $ MBLArray (SafeSz k) o ma
{-# INLINE fromBoxedVector #-}


-- | /O(1)/ - Convert mutable boxed vector to a lazy mutable boxed array. Both keep
-- pointing to the same memory
--
-- @since 0.6.0
fromBoxedMVector :: MVB.MVector s a -> MArray s BL Ix1 a
fromBoxedMVector (MVB.MVector o k ma) = MBLArray (SafeSz k) o ma
{-# INLINE fromBoxedMVector #-}


-- | /O(1)/ - Cast a boxed lazy array. It is unsafe because it can violate the invariant
-- that all elements of `N` array are in NF.
--
-- @since 0.6.0
coerceNormalBoxedArray :: Array BL ix e -> Array N ix e
coerceNormalBoxedArray = coerce
{-# INLINE coerceNormalBoxedArray #-}


-- | /O(1)/ - Cast a boxed lazy array. It is unsafe because it can violate the invariant
-- that all elements of `B` array are in WHNF.
--
-- @since 0.6.0
coerceBoxedArray :: Array BL ix e -> Array B ix e
coerceBoxedArray = coerce
{-# INLINE coerceBoxedArray #-}

-- | /O(n)/ - Convert mutable boxed vector and evaluate all elements to WHNF
-- sequentially. Both keep pointing to the same memory
--
-- @since 0.5.0
evalNormalBoxedMVector ::
     (NFData a, PrimMonad m) => MVB.MVector (PrimState m) a -> m (MArray (PrimState m) N Ix1 a)
evalNormalBoxedMVector (MVB.MVector o k ma) =
  let marr = MNArray (MBLArray (SafeSz k) o ma)
   in marr <$ loopM_ o (< k) (+ 1) (A.readArray ma >=> (`deepseq` pure ()))
{-# INLINE evalNormalBoxedMVector #-}

-- | /O(n)/ - Convert a boxed vector and evaluate all elements to WHNF. Computation
-- strategy will be respected during evaluation
--
-- @since 0.5.0
evalNormalBoxedVector :: NFData a => Comp -> VB.Vector a -> Array N Ix1 a
evalNormalBoxedVector comp v =
  runST $ do
    MVB.MVector o k ma <- VB.unsafeThaw v
    forceLazyArray <$> unsafeFreeze comp (MBLArray (SafeSz k) o ma)
{-# INLINE evalNormalBoxedVector #-}

