{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Massiv.Array.Manifest.Unboxed
-- Copyright   : (c) Alexey Kuleshevich 2018-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Manifest.Unboxed
  ( U (..)
  , VU.Unbox
  , Array(..)
  , toUnboxedVector
  , toUnboxedMVector
  , fromUnboxedVector
  , fromUnboxedMVector
  ) where

import Control.DeepSeq (NFData(..), deepseq)
import Data.Massiv.Array.Delayed.Pull (eqArrays, compareArrays)
import Data.Massiv.Array.Manifest.Internal (M, toManifest)
import Data.Massiv.Array.Manifest.List as A
import Data.Massiv.Vector.Stream as S (steps, isteps)
import Data.Massiv.Array.Mutable
import Data.Massiv.Core.Common
import Data.Massiv.Core.List
import Data.Massiv.Core.Operations
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU
import GHC.Exts as GHC (IsList(..))
import Prelude hiding (mapM)
import System.IO.Unsafe (unsafePerformIO)

#include "massiv.h"

-- | Representation for `Unbox`ed elements
data U = U deriving Show

data instance Array U ix e = UArray { uComp :: !Comp
                                    , uSize :: !(Sz ix)
                                    , uData :: !(VU.Vector e)
                                    }

instance (Ragged L ix e, Show e, VU.Unbox e) => Show (Array U ix e) where
  showsPrec = showsArrayPrec id
  showList = showArrayList

instance NFData ix => NFData (Array U ix e) where
  rnf (UArray c sz v) = c `deepseq` sz `deepseq` v `deepseq` ()
  {-# INLINE rnf #-}

instance NFData ix => NFData (MArray s U ix e) where
  rnf (MUArray sz mv) = sz `deepseq` mv `deepseq` ()
  {-# INLINE rnf #-}

instance (VU.Unbox e, Index ix) => Construct U ix e where
  setComp c arr = arr { uComp = c }
  {-# INLINE setComp #-}

  makeArrayLinear !comp !sz f = unsafePerformIO $ generateArrayLinear comp sz (pure . f)
  {-# INLINE makeArrayLinear #-}

  replicate comp !sz !e = runST (newMArray sz e >>= unsafeFreeze comp)
  {-# INLINE replicate #-}


instance (VU.Unbox e, Eq e, Index ix) => Eq (Array U ix e) where
  (==) = eqArrays (==)
  {-# INLINE (==) #-}

instance (VU.Unbox e, Ord e, Index ix) => Ord (Array U ix e) where
  compare = compareArrays compare
  {-# INLINE compare #-}


instance (VU.Unbox e, Index ix) => Source U ix e where
  unsafeLinearIndex (UArray _ _ v) =
    INDEX_CHECK("(Source U ix e).unsafeLinearIndex", Sz . VU.length, VU.unsafeIndex) v
  {-# INLINE unsafeLinearIndex #-}
  unsafeLinearSlice i k (UArray c _ v) = UArray c k $ VU.unsafeSlice i (unSz k) v
  {-# INLINE unsafeLinearSlice #-}


instance Index ix => Resize U ix where
  unsafeResize !sz !arr = arr { uSize = sz }
  {-# INLINE unsafeResize #-}

instance (VU.Unbox e, Index ix) => Extract U ix e where
  unsafeExtract !sIx !newSz !arr = unsafeExtract sIx newSz (toManifest arr)
  {-# INLINE unsafeExtract #-}

instance (VU.Unbox e, Index ix) => Load U ix e where
  type R U = M
  size = uSize
  {-# INLINE size #-}
  getComp = uComp
  {-# INLINE getComp #-}
  loadArrayM !scheduler !arr = splitLinearlyWith_ scheduler (elemsCount arr) (unsafeLinearIndex arr)
  {-# INLINE loadArrayM #-}

instance (VU.Unbox e, Index ix) => StrideLoad U ix e


instance {-# OVERLAPPING #-} VU.Unbox e => Slice U Ix1 e where
  unsafeSlice arr i _ _ = pure (unsafeLinearIndex arr i)
  {-# INLINE unsafeSlice #-}


instance ( VU.Unbox e
         , Index ix
         , Index (Lower ix)
         , Elt U ix e ~ Elt M ix e
         , Elt M ix e ~ Array M (Lower ix) e
         ) =>
         Slice U ix e where
  unsafeSlice arr = unsafeSlice (toManifest arr)
  {-# INLINE unsafeSlice #-}


instance {-# OVERLAPPING #-} VU.Unbox e => OuterSlice U Ix1 e where
  unsafeOuterSlice = unsafeLinearIndex
  {-# INLINE unsafeOuterSlice #-}

instance ( VU.Unbox e
         , Index ix
         , Index (Lower ix)
         , Elt U ix e ~ Elt M ix e
         , Elt M ix e ~ Array M (Lower ix) e
         ) =>
         OuterSlice U ix e where
  unsafeOuterSlice arr = unsafeOuterSlice (toManifest arr)
  {-# INLINE unsafeOuterSlice #-}

instance {-# OVERLAPPING #-} VU.Unbox e => InnerSlice U Ix1 e where
  unsafeInnerSlice arr _ = unsafeLinearIndex arr
  {-# INLINE unsafeInnerSlice #-}

instance ( VU.Unbox e
         , Index ix
         , Index (Lower ix)
         , Elt U ix e ~ Elt M ix e
         , Elt M ix e ~ Array M (Lower ix) e
         ) =>
         InnerSlice U ix e where
  unsafeInnerSlice arr = unsafeInnerSlice (toManifest arr)
  {-# INLINE unsafeInnerSlice #-}

instance (VU.Unbox e, Index ix) => Manifest U ix e where

  unsafeLinearIndexM (UArray _ _ v) =
    INDEX_CHECK("(Manifest U ix e).unsafeLinearIndexM", Sz . VU.length, VU.unsafeIndex) v
  {-# INLINE unsafeLinearIndexM #-}


instance (VU.Unbox e, Index ix) => Mutable U ix e where
  data MArray s U ix e = MUArray !(Sz ix) !(VU.MVector s e)

  msize (MUArray sz _) = sz
  {-# INLINE msize #-}

  unsafeThaw (UArray _ sz v) = MUArray sz <$> VU.unsafeThaw v
  {-# INLINE unsafeThaw #-}

  unsafeFreeze comp (MUArray sz v) = UArray comp sz <$> VU.unsafeFreeze v
  {-# INLINE unsafeFreeze #-}

  unsafeNew sz = MUArray sz <$> MVU.unsafeNew (totalElem sz)
  {-# INLINE unsafeNew #-}

  initialize (MUArray _ marr) = VGM.basicInitialize marr
  {-# INLINE initialize #-}

  unsafeLinearCopy (MUArray _ mvFrom) iFrom (MUArray _ mvTo) iTo (Sz k) =
    MVU.unsafeCopy (MVU.unsafeSlice iTo k mvTo) (MVU.unsafeSlice iFrom k mvFrom)
  {-# INLINE unsafeLinearCopy #-}

  unsafeLinearRead (MUArray _ mv) =
    INDEX_CHECK("(Mutable U ix e).unsafeLinearRead", Sz . MVU.length, MVU.unsafeRead) mv
  {-# INLINE unsafeLinearRead #-}

  unsafeLinearWrite (MUArray _ mv) =
    INDEX_CHECK("(Mutable U ix e).unsafeLinearWrite", Sz . MVU.length, MVU.unsafeWrite) mv
  {-# INLINE unsafeLinearWrite #-}

  unsafeLinearGrow (MUArray _ mv) sz = MUArray sz <$> MVU.unsafeGrow mv (totalElem sz)
  {-# INLINE unsafeLinearGrow #-}


instance (Index ix, VU.Unbox e) => Stream U ix e where
  toStream = S.steps
  {-# INLINE toStream #-}
  toStreamIx = S.isteps
  {-# INLINE toStreamIx #-}


instance ( VU.Unbox e
         , IsList (Array L ix e)
         , Nested LN ix e
         , Nested L ix e
         , Ragged L ix e
         ) =>
         IsList (Array U ix e) where
  type Item (Array U ix e) = Item (Array L ix e)
  fromList = A.fromLists' Seq
  {-# INLINE fromList #-}
  toList = GHC.toList . toListArray
  {-# INLINE toList #-}


instance (VU.Unbox e, Num e) => FoldNumeric U e where
  unsafeDotProduct = defaultUnsafeDotProduct
  {-# INLINE unsafeDotProduct #-}
  powerSumArray = defaultPowerSumArray
  {-# INLINE powerSumArray #-}
  foldArray = defaultFoldArray
  {-# INLINE foldArray #-}

instance (VU.Unbox e, Num e) => Numeric U e where
  unsafeLiftArray = defaultUnsafeLiftArray
  {-# INLINE unsafeLiftArray #-}
  unsafeLiftArray2 = defaultUnsafeLiftArray2
  {-# INLINE unsafeLiftArray2 #-}


-- | /O(1)/ - Unwrap unboxed array and pull out the underlying unboxed vector.
--
-- @since 0.2.1
toUnboxedVector :: Array U ix e -> VU.Vector e
toUnboxedVector = uData
{-# INLINE toUnboxedVector #-}


-- | /O(1)/ - Unwrap unboxed mutable array and pull out the underlying unboxed mutable vector.
--
-- @since 0.2.1
toUnboxedMVector :: MArray s U ix e -> VU.MVector s e
toUnboxedMVector (MUArray _ mv) = mv
{-# INLINE toUnboxedMVector #-}



-- | /O(1)/ - Wrap an unboxed vector and produce an unboxed flat array.
--
-- @since 0.6.0
fromUnboxedVector :: VU.Unbox e => Comp -> VU.Vector e -> Array U Ix1 e
fromUnboxedVector comp v = UArray comp (SafeSz (VU.length v)) v
{-# INLINE fromUnboxedVector #-}


-- | /O(1)/ - Wrap an unboxed mutable vector and produce a mutable unboxed flat array.
--
-- @since 0.5.0
fromUnboxedMVector :: VU.Unbox e => VU.MVector s e -> MArray s U Ix1 e
fromUnboxedMVector mv = MUArray (SafeSz (MVU.length mv)) mv
{-# INLINE fromUnboxedMVector #-}
