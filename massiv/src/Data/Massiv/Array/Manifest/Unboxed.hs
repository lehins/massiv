{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Data.Massiv.Array.Manifest.Unboxed
-- Copyright   : (c) Alexey Kuleshevich 2018-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
module Data.Massiv.Array.Manifest.Unboxed (
  U (..),
  Unbox,
  Array (..),
  MArray (..),
  toUnboxedVector,
  toUnboxedMVector,
  fromUnboxedVector,
  fromUnboxedMVector,
) where

import Control.DeepSeq (NFData (..), deepseq)
import Control.Monad.Primitive (stToPrim)
import Data.Massiv.Array.Delayed.Pull (D, compareArrays, eqArrays)
import Data.Massiv.Array.Manifest.Internal
import Data.Massiv.Array.Manifest.List as A
import Data.Massiv.Array.Mutable
import Data.Massiv.Core.Common
import Data.Massiv.Core.List
import Data.Massiv.Core.Operations
import Data.Massiv.Vector.Stream as S (isteps, steps)
import qualified Data.Vector.Generic.Mutable as VGM
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU
import GHC.Exts as GHC (IsList (..))
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (mapM)

-- | Representation for `Unbox`ed elements
data U = U deriving (Show)

data instance Array U ix e = UArray
  { uComp :: !Comp
  , uSize :: !(Sz ix)
  , uData :: !(VU.Vector e)
  }
data instance MArray s U ix e = MUArray !(Sz ix) !(VU.MVector s e)

instance (Ragged L ix e, Show e, Unbox e) => Show (Array U ix e) where
  showsPrec = showsArrayPrec id
  showList = showArrayList

instance NFData ix => NFData (Array U ix e) where
  rnf (UArray c sz v) = c `deepseq` sz `deepseq` v `deepseq` ()
  {-# INLINE rnf #-}

instance NFData ix => NFData (MArray s U ix e) where
  rnf (MUArray sz mv) = sz `deepseq` mv `deepseq` ()
  {-# INLINE rnf #-}

instance Strategy U where
  getComp = uComp
  {-# INLINE getComp #-}
  setComp c arr = arr{uComp = c}
  {-# INLINE setComp #-}
  repr = U

instance (Unbox e, Eq e, Index ix) => Eq (Array U ix e) where
  (==) = eqArrays (==)
  {-# INLINE (==) #-}

instance (Unbox e, Ord e, Index ix) => Ord (Array U ix e) where
  compare = compareArrays compare
  {-# INLINE compare #-}

instance Unbox e => Source U e where
  unsafeLinearIndex (UArray _ _ v) =
    indexAssert "U.unsafeLinearIndex" (SafeSz . VU.length) VU.unsafeIndex v
  {-# INLINE unsafeLinearIndex #-}

  unsafeOuterSlice (UArray c _ v) szL i =
    let k = totalElem szL
     in UArray c szL $ VU.unsafeSlice (i * k) k v
  {-# INLINE unsafeOuterSlice #-}

  unsafeLinearSlice i k (UArray c _ v) = UArray c k $ VU.unsafeSlice i (unSz k) v
  {-# INLINE unsafeLinearSlice #-}

instance Index ix => Shape U ix where
  maxLinearSize = Just . SafeSz . elemsCount
  {-# INLINE maxLinearSize #-}

instance Size U where
  size = uSize
  {-# INLINE size #-}
  unsafeResize !sz !arr = arr{uSize = sz}
  {-# INLINE unsafeResize #-}

instance (Unbox e, Index ix) => Load U ix e where
  makeArray comp sz f = compute (makeArray comp sz f :: Array D ix e)
  {-# INLINE makeArray #-}

  makeArrayLinear !comp !sz f = unsafePerformIO $ generateArrayLinear comp sz (pure . f)
  {-# INLINE makeArrayLinear #-}

  replicate comp !sz !e = runST (newMArray sz e >>= unsafeFreeze comp)
  {-# INLINE replicate #-}

  iterArrayLinearST_ !scheduler !arr =
    splitLinearlyWith_ scheduler (elemsCount arr) (unsafeLinearIndex arr)
  {-# INLINE iterArrayLinearST_ #-}

instance (Unbox e, Index ix) => StrideLoad U ix e

instance Unbox e => Manifest U e where
  unsafeLinearIndexM (UArray _ _ v) =
    indexAssert "S.unsafeLinearIndexM" (SafeSz . VU.length) VU.unsafeIndex v
  {-# INLINE unsafeLinearIndexM #-}

  sizeOfMArray (MUArray sz _) = sz
  {-# INLINE sizeOfMArray #-}

  unsafeResizeMArray sz (MUArray _ mv) = MUArray sz mv
  {-# INLINE unsafeResizeMArray #-}

  unsafeLinearSliceMArray i k (MUArray _ mv) = MUArray k $ MVU.unsafeSlice i (unSz k) mv
  {-# INLINE unsafeLinearSliceMArray #-}

  unsafeThaw (UArray _ sz v) = MUArray sz <$> VU.unsafeThaw v
  {-# INLINE unsafeThaw #-}

  unsafeFreeze comp (MUArray sz v) = UArray comp sz <$> VU.unsafeFreeze v
  {-# INLINE unsafeFreeze #-}

  unsafeNew sz = MUArray sz <$> MVU.unsafeNew (totalElem sz)
  {-# INLINE unsafeNew #-}

  initialize (MUArray _ marr) = stToPrim (VGM.basicInitialize marr)
  {-# INLINE initialize #-}

  unsafeLinearCopy (MUArray _ mvFrom) iFrom (MUArray _ mvTo) iTo (Sz k) =
    MVU.unsafeCopy (MVU.unsafeSlice iTo k mvTo) (MVU.unsafeSlice iFrom k mvFrom)
  {-# INLINE unsafeLinearCopy #-}

  unsafeLinearRead (MUArray _ mv) =
    indexAssert "U.unsafeLinearRead" (Sz . MVU.length) MVU.unsafeRead mv
  {-# INLINE unsafeLinearRead #-}

  unsafeLinearWrite (MUArray _ mv) =
    indexAssert "U.unsafeLinearWrite" (Sz . MVU.length) MVU.unsafeWrite mv
  {-# INLINE unsafeLinearWrite #-}

  unsafeLinearGrow (MUArray _ mv) sz = MUArray sz <$> MVU.unsafeGrow mv (totalElem sz)
  {-# INLINE unsafeLinearGrow #-}

instance (Index ix, Unbox e) => Stream U ix e where
  toStream = S.steps
  {-# INLINE toStream #-}
  toStreamIx = S.isteps
  {-# INLINE toStreamIx #-}

instance (Unbox e, IsList (Array L ix e), Ragged L ix e) => IsList (Array U ix e) where
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
fromUnboxedVector :: VU.Unbox e => Comp -> VU.Vector e -> Vector U e
fromUnboxedVector comp v = UArray comp (SafeSz (VU.length v)) v
{-# INLINE fromUnboxedVector #-}

-- | /O(1)/ - Wrap an unboxed mutable vector and produce a mutable unboxed flat array.
--
-- @since 0.5.0
fromUnboxedMVector :: Unbox e => VU.MVector s e -> MVector s U e
fromUnboxedMVector mv = MUArray (SafeSz (MVU.length mv)) mv
{-# INLINE fromUnboxedMVector #-}
