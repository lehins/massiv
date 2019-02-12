{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Massiv.Array.Manifest.Boxed
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Manifest.Boxed
  ( B(..)
  , N(..)
  , Array(..)
  , unwrapArray
  , evalArray
  , unwrapMutableArray
  , evalMutableArray
  , unwrapNormalFormArray
  , evalNormalFormArray
  , unwrapNormalFormMutableArray
  , evalNormalFormMutableArray
  , castArrayToVector
  , vectorToArray
  , castVectorToArray
  , seqArray
  , deepseqArray
  ) where

import           Control.DeepSeq                     (NFData (..), deepseq)
import           Control.Exception
import           Control.Monad.Primitive
import           Control.Monad.ST                    (runST)
import qualified Data.Foldable                       as F (Foldable (..))
import           Data.Massiv.Array.Delayed.Pull      (eq, ord)
import           Data.Massiv.Array.Delayed.Push      (DL)
import           Data.Massiv.Array.Manifest.Internal (M, toManifest, computeAs)
import           Data.Massiv.Array.Manifest.List     as L
import           Data.Massiv.Array.Mutable
import           Data.Massiv.Array.Ops.Fold
import           Data.Massiv.Array.Ops.Fold.Internal
import           Data.Massiv.Array.Ops.Map           (traverseA)
import           Data.Massiv.Core.Common
import           Data.Massiv.Core.List
import qualified Data.Primitive.Array                as A
import qualified Data.Vector                         as VB
import qualified Data.Vector.Mutable                 as VB
import           GHC.Base                            (build)
import           GHC.Exts                            as GHC (IsList (..))
import           GHC.Prim
import           GHC.Types
import           Prelude                             hiding (mapM)
import           System.IO.Unsafe                    (unsafePerformIO)

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

------------------
-- Boxed Strict --
------------------

-- | Array representation for Boxed elements. This structure is element and
-- spine strict, but elements are strict to Weak Head Normal Form (WHNF) only.
data B = B deriving Show

type instance EltRepr B ix = M

data instance Array B ix e = BArray { bComp :: !Comp
                                    , bSize :: !(Sz ix)
                                    , bData :: {-# UNPACK #-} !(A.Array e)
                                    }

instance (Ragged L ix e, Show e) => Show (Array B ix e) where
  showsPrec = showsArrayPrec id

instance (Ragged L ix e, Show e) => Show (Array DL ix e) where
  showsPrec = showsArrayPrec (computeAs B)


instance (Index ix, NFData e) => NFData (Array B ix e) where
  rnf = (`deepseqArray` ())
  {-# INLINE rnf #-}

instance (Index ix, Eq e) => Eq (Array B ix e) where
  (==) = eq (==)
  {-# INLINE (==) #-}

instance (Index ix, Ord e) => Ord (Array B ix e) where
  compare = ord compare
  {-# INLINE compare #-}

instance Index ix => Construct B ix e where
  setComp c arr = arr { bComp = c }
  {-# INLINE setComp #-}

  makeArray !comp !sz f = unsafePerformIO $ generateArray comp sz (\ !ix -> return $! f ix)
  {-# INLINE makeArray #-}

instance Index ix => Source B ix e where
  unsafeLinearIndex (BArray _ _ a) =
    INDEX_CHECK("(Source B ix e).unsafeLinearIndex", Sz . sizeofArray, A.indexArray) a
  {-# INLINE unsafeLinearIndex #-}


instance Index ix => Resize Array B ix where
  unsafeResize !sz !arr = arr { bSize = sz }
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

  unsafeLinearIndexM (BArray _ _ a) =
    INDEX_CHECK("(Manifest B ix e).unsafeLinearIndexM", Sz . sizeofArray, A.indexArray) a
  {-# INLINE unsafeLinearIndexM #-}


instance Index ix => Mutable B ix e where
  data MArray s B ix e = MBArray !(Sz ix) {-# UNPACK #-} !(A.MutableArray s e)

  msize (MBArray sz _) = sz
  {-# INLINE msize #-}

  unsafeThaw (BArray _ sz a) = MBArray sz <$> A.unsafeThawArray a
  {-# INLINE unsafeThaw #-}

  unsafeFreeze comp (MBArray sz ma) = BArray comp sz <$> A.unsafeFreezeArray ma
  {-# INLINE unsafeFreeze #-}

  unsafeNew sz = MBArray sz <$> A.newArray (totalElem sz) uninitialized
  {-# INLINE unsafeNew #-}

  initialize _ = return ()
  {-# INLINE initialize #-}

  unsafeLinearRead (MBArray _ ma) =
    INDEX_CHECK("(Mutable B ix e).unsafeLinearRead", Sz . sizeofMutableArray, A.readArray) ma
  {-# INLINE unsafeLinearRead #-}

  unsafeLinearWrite (MBArray _ ma) i e = e `seq`
    INDEX_CHECK("(Mutable B ix e).unsafeLinearWrite", Sz . sizeofMutableArray, A.writeArray) ma i e
  {-# INLINE unsafeLinearWrite #-}

instance Index ix => Load B ix e where
  size = bSize
  {-# INLINE size #-}
  getComp = bComp
  {-# INLINE getComp #-}
  loadArrayM !numWorkers scheduleWork !arr =
    splitLinearlyWith_ numWorkers scheduleWork (elemsCount arr) (unsafeLinearIndex arr)
  {-# INLINE loadArrayM #-}

instance Index ix => StrideLoad B ix e


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
  null (BArray _ sz _) = totalElem sz == 0
  {-# INLINE null #-}
  length = totalElem . size
  {-# INLINE length #-}
  toList arr = build (\ c n -> foldrFB c n arr)
  {-# INLINE toList #-}


instance Index ix => Functor (Array B ix) where
  fmap f arr = makeArrayLinear (bComp arr) (bSize arr) (f . unsafeLinearIndex arr)
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

-----------------------
-- Boxed Normal Form --
-----------------------

-- | Array representation for Boxed elements. This structure is element and
-- spine strict, and elements are always in Normal Form (NF), therefore `NFData`
-- instance is required.
data N = N deriving Show

type instance EltRepr N ix = M

newtype instance Array N ix e = NArray { bArray :: Array B ix e }

instance (Ragged L ix e, Show e, NFData e) => Show (Array N ix e) where
  showsPrec = showsArrayPrec bArray

instance (Index ix, NFData e) => NFData (Array N ix e) where
  rnf (NArray barr) = barr `deepseqArray` ()
  {-# INLINE rnf #-}

instance (Index ix, NFData e, Eq e) => Eq (Array N ix e) where
  (==) = eq (==)
  {-# INLINE (==) #-}

instance (Index ix, NFData e, Ord e) => Ord (Array N ix e) where
  compare = ord compare
  {-# INLINE compare #-}


instance (Index ix, NFData e) => Construct N ix e where
  setComp c (NArray arr) = NArray (arr {bComp = c})
  {-# INLINE setComp #-}
  makeArray !comp !sz f =
    unsafePerformIO $
    generateArray
      comp
      sz
      (\ !ix ->
         let res = f ix
          in res `deepseq` return res)
  {-# INLINE makeArray #-}

instance (Index ix, NFData e) => Source N ix e where
  unsafeLinearIndex (NArray arr) =
    INDEX_CHECK("(Source N ix e).unsafeLinearIndex", Sz . totalElem . size, unsafeLinearIndex) arr
  {-# INLINE unsafeLinearIndex #-}


instance Index ix => Resize Array N ix where
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

  unsafeLinearIndexM (NArray arr) =
    INDEX_CHECK("(Manifest N ix e).unsafeLinearIndexM", Sz . totalElem . size, unsafeLinearIndexM) arr
  {-# INLINE unsafeLinearIndexM #-}


instance (Index ix, NFData e) => Mutable N ix e where
  newtype MArray s N ix e = MNArray { bmArray :: MArray s B ix e }

  msize = msize . bmArray
  {-# INLINE msize #-}

  unsafeThaw (NArray arr) = MNArray <$> unsafeThaw arr
  {-# INLINE unsafeThaw #-}

  unsafeFreeze comp (MNArray marr) = NArray <$> unsafeFreeze comp marr
  {-# INLINE unsafeFreeze #-}

  unsafeNew sz = MNArray <$> unsafeNew sz
  {-# INLINE unsafeNew #-}

  initialize _ = return ()
  {-# INLINE initialize #-}

  unsafeLinearRead (MNArray ma) =
    INDEX_CHECK("(Mutable N ix e).unsafeLinearRead", Sz . totalElem . msize, unsafeLinearRead) ma
  {-# INLINE unsafeLinearRead #-}

  unsafeLinearWrite (MNArray ma) i e = e `deepseq`
    INDEX_CHECK("(Mutable N ix e).unsafeLinearWrite", Sz . totalElem . msize, unsafeLinearWrite) ma i e
  {-# INLINE unsafeLinearWrite #-}

instance (Index ix, NFData e) => Load N ix e where
  size = bSize . bArray
  {-# INLINE size #-}
  getComp = bComp . bArray
  {-# INLINE getComp #-}
  loadArrayM !numWorkers scheduleWork !arr =
    splitLinearlyWith_ numWorkers scheduleWork (elemsCount arr) (unsafeLinearIndex arr)
  {-# INLINE loadArrayM #-}

instance (Index ix, NFData e) => StrideLoad N ix e



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


----------------------
-- Helper functions --
----------------------

uninitialized :: a
uninitialized = throw Uninitialized


-- -- | /O(1)/ - Unwrap a fully evaluated boxed array.
-- --
-- -- @since 0.2.1
-- unwrapNormalFormArray :: Array N ix e -> Array B ix e
-- unwrapNormalFormArray = bArray
-- {-# INLINE unwrapNormalFormArray #-}

-- -- | /O(1)/ - Unwrap a fully evaluated mutable boxed array.
-- --
-- -- @since 0.2.1
-- unwrapNormalFormMutableArray :: MArray s N ix e -> MArray s B ix e
-- unwrapNormalFormMutableArray (MNArray marr) = marr
-- {-# INLINE unwrapNormalFormMutableArray #-}

---------------------
-- WHNF conversion --
---------------------

-- | /O(1)/ - Unwrap boxed array.
--
-- @since 0.2.1
unwrapArray :: Array B ix e -> A.Array e
unwrapArray = bData
{-# INLINE unwrapArray #-}

-- | /O(n)/ - Wrap a boxed array and evaluate all elements to a WHNF.
--
-- @since 0.2.1
evalArray ::
     Comp -- ^ Computation strategy
  -> A.Array e -- ^ Lazy boxed array from @primitive@ package.
  -> Array B Ix1 e
evalArray = fromArraySeq (\a -> a `seqArray` a)
{-# INLINE evalArray #-}

-- | /O(1)/ - Unwrap mutable boxed array.
--
-- @since 0.2.1
unwrapMutableArray :: MArray s B ix e -> A.MutableArray s e
unwrapMutableArray (MBArray _ marr) = marr
{-# INLINE unwrapMutableArray #-}


-- | /O(n)/ - Wrap mutable boxed array and evaluate all elements to WHNF.
--
-- @since 0.2.1
evalMutableArray ::
     PrimMonad m
  => A.MutableArray (PrimState m) e -- ^ Mutable array that will get wrapped
  -> m (MArray (PrimState m) B Ix1 e)
evalMutableArray = fromMutableArraySeq seq
{-# INLINE evalMutableArray #-}

-------------------
-- NF conversion --
-------------------

-- | /O(1)/ - Unwrap a fully evaluated boxed array.
--
-- @since 0.2.1
unwrapNormalFormArray :: Array N ix e -> A.Array e
unwrapNormalFormArray = bData . bArray
{-# INLINE unwrapNormalFormArray #-}

-- | /O(n)/ - Wrap a boxed array and evaluate all elements to a Normal Form (NF).
--
-- @since 0.2.1
evalNormalFormArray ::
     NFData e
  => Comp -- ^ Computation strategy
  -> A.Array e -- ^ Lazy boxed array
  -> Array N Ix1 e
evalNormalFormArray = fromArraySeq (\a -> a `deepseqArray` NArray a)
{-# INLINE evalNormalFormArray #-}


-- | /O(1)/ - Unwrap a fully evaluated mutable boxed array.
--
-- @since 0.2.1
unwrapNormalFormMutableArray :: MArray s N ix e -> A.MutableArray s e
unwrapNormalFormMutableArray (MNArray (MBArray _ marr)) = marr
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


----------------------
-- Helper functions --
----------------------

fromMutableArraySeq ::
     PrimMonad m
  => (e -> m () -> m a)
  -> A.MutableArray (PrimState m) e
  -> m (MArray (PrimState m) B Ix1 e)
fromMutableArraySeq with mbarr = do
  let !sz = sizeofMutableArray mbarr
  loopM_ 0 (< sz) (+ 1) $ \i -> A.readArray mbarr i >>= (`with` return ())
  return $! MBArray (Sz sz) mbarr
{-# INLINE fromMutableArraySeq #-}

fromArraySeq ::
     (Array B Ix1 e -> a)
  -> Comp
  -> A.Array e
  -> a
fromArraySeq with comp barr = with (BArray comp (Sz (sizeofArray barr)) barr)
{-# INLINE fromArraySeq #-}


seqArray :: Index ix => Array B ix a -> t -> t
seqArray !arr t = foldlInternal (flip seq) () (flip seq) () arr `seq` t
{-# INLINE seqArray #-}


deepseqArray :: (NFData a, Index ix) => Array B ix a -> t -> t
deepseqArray !arr t = foldlInternal (flip deepseq) () (flip seq) () arr `seq` t
{-# INLINE deepseqArray #-}


-- | Helper function that converts a boxed `A.Array` into a `VB.Vector`. Supplied total number of
-- elements is assumed to be the same in the array as provided by the size.
castArrayToVector :: A.Array a -> VB.Vector a
castArrayToVector arr = runST $ do
  marr <- A.unsafeThawArray arr
  VB.unsafeFreeze $ VB.MVector 0 (sizeofArray arr) marr
{-# INLINE castArrayToVector #-}

-- | Covert boxed `VB.Vector` into an `A.Array`. Sliced vectors will indure copying.
vectorToArray :: VB.Vector a -> A.Array a
vectorToArray v =
  runST $ do
    VB.MVector start len marr <- VB.unsafeThaw v
    marr' <-
      if start == 0
        then return marr
        else A.cloneMutableArray marr start len
    A.unsafeFreezeArray marr'
{-# INLINE vectorToArray #-}


-- | Cast a Boxed Vector into an Array, but only if it wasn't previously sliced.
castVectorToArray :: VB.Vector a -> Maybe (A.Array a)
castVectorToArray v =
  runST $ do
    VB.MVector start end marr <- VB.unsafeThaw v
    if start == 0 && end == sizeofMutableArray marr
      then Just <$> A.unsafeFreezeArray marr
      else return Nothing
{-# INLINE castVectorToArray #-}
