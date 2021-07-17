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
-- Module      : Data.Massiv.Array.Manifest.Primal
-- Copyright   : (c) Alexey Kuleshevich 2018-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Manifest.Primal
  ( P(..)
  , Array(..)
  ) where

import Primal.Monad (Primal(..), primal_)
import Primal.Eval (NFData(..), deepseq)
import Primal.Array.Unboxed
import Data.Massiv.Array.Delayed.Pull (eqArrays, compareArrays)
import Data.Massiv.Array.Manifest.Internal
import Data.Massiv.Array.Manifest.List as A
import Data.Massiv.Array.Mutable
import Data.Massiv.Core.Common as Massiv
import Data.Massiv.Core.Operations
import Data.Massiv.Core.List
import Data.Maybe (fromMaybe)

-- | Representation for `Prim`itive elements
data P = P deriving Show

data instance Array P ix e = PArray { pComp   :: !Comp
                                    , pSize   :: !(Sz ix)
                                    , pOffset :: {-# UNPACK #-} !Int
                                    , pData   :: {-# UNPACK #-} !(UArray e)
                                    }

-- instance (Ragged L ix e, Show e, Prim e) => Show (Array P ix e) where
--   showsPrec = showsArrayPrec id
--   showList = showArrayList

-- instance Index ix => NFData (Array P ix e) where
--   rnf (PArray c sz o a) = c `deepseq` sz `deepseq` o `seq` a `seq` ()
--   {-# INLINE rnf #-}

-- instance NFData ix => NFData (MArray P ix e s) where
--   rnf (MPArray sz _o _mb) = sz `deepseq` ()
--   {-# INLINE rnf #-}

-- instance (Prim e, Eq e, Index ix) => Eq (Array P ix e) where
--   (==) = eqArrays (==)
--   {-# INLINE (==) #-}

-- instance (Prim e, Ord e, Index ix) => Ord (Array P ix e) where
--   compare = compareArrays compare
--   {-# INLINE compare #-}

instance Strategy P where
  getComp = pComp
  {-# INLINE getComp #-}
  setComp c arr = arr { pComp = c }
  {-# INLINE setComp #-}


-- instance Index ix => Shape P ix where
--   maxLinearSize = Just . SafeSz . elemsCount
--   {-# INLINE maxLinearSize #-}

instance Massiv.Size P where
  size = pSize
  {-# INLINE size #-}

instance Resize P where
  unsafeResize !sz !arr = arr { pSize = sz }
  {-# INLINE unsafeResize #-}

instance Unbox e => Source P e where
  unsafeLinearIndex (PArray _ _ o a) i = indexUArray a (i + o)
  {-# INLINE unsafeLinearIndex #-}

  unsafeOuterSlice (PArray c _ o a) szL i = PArray c szL (i * totalElem szL + o) a
  {-# INLINE unsafeOuterSlice #-}

  unsafeLinearSlice i k (PArray c _ o a) = PArray c k (i + o) a
  {-# INLINE unsafeLinearSlice #-}

instance Unbox e => Manifest P e where
  unsafeLinearIndexM _pa@(PArray _ _sz o a) i = indexUArray a (i + o)
  {-# INLINE unsafeLinearIndexM #-}


data instance MArray P ix e s =
  MPArray !(Sz ix) {-# UNPACK #-} !Int {-# UNPACK #-} !(UMArray e s)

instance Unbox e => Mutable P e where
  msize (MPArray sz _ _) = sz
  {-# INLINE msize #-}

  munsafeResize sz (MPArray _ off marr) = MPArray sz off marr
  {-# INLINE munsafeResize #-}

  unsafeThaw (PArray _ sz o a) = MPArray sz o <$> thawUArray a
  {-# INLINE unsafeThaw #-}

  unsafeFreeze comp (MPArray sz o a) = PArray comp sz o <$> freezeUMArray a
  {-# INLINE unsafeFreeze #-}

  unsafeNew sz = MPArray sz 0 <$> newRawUMArray (Size (totalElem sz))
  {-# INLINE unsafeNew #-}

  initialize (MPArray sz o mba) = setUMArray mba o (Size (totalElem sz)) 0
  {-# INLINE initialize #-}

--   unsafeLinearRead _mpa@(MPArray _sz o ma) i = liftST $
--     INDEX_CHECK("(Mutable P ix e).unsafeLinearRead",
--                 const (Sz (totalElem _sz)), readByteArray) ma (i + o)
--   {-# INLINE unsafeLinearRead #-}

--   unsafeLinearWrite _mpa@(MPArray _sz o ma) i = liftST .
--     INDEX_CHECK("(Mutable P ix e).unsafeLinearWrite",
--                 const (Sz (totalElem _sz)), writeByteArray) ma (i + o)
--   {-# INLINE unsafeLinearWrite #-}

--   unsafeLinearSet (MPArray _ o ma) offset (SafeSz sz) = liftST . setByteArray ma (offset + o) sz
--   {-# INLINE unsafeLinearSet #-}

--   unsafeLinearCopy (MPArray _ oFrom maFrom) iFrom (MPArray _ oTo maTo) iTo (Sz k) =
--     liftST $ copyMutableByteArray maTo ((oTo + iTo) * esz) maFrom ((oFrom + iFrom) * esz) (k * esz)
--     where
--       esz = sizeOf (undefined :: e)
--   {-# INLINE unsafeLinearCopy #-}

--   unsafeArrayLinearCopy (PArray _ _ oFrom aFrom) iFrom (MPArray _ oTo maTo) iTo (Sz k) =
--     liftST $ copyByteArray maTo ((oTo + iTo) * esz) aFrom ((oFrom + iFrom) * esz) (k * esz)
--     where
--       esz = sizeOf (undefined :: e)
--   {-# INLINE unsafeArrayLinearCopy #-}

--   unsafeLinearShrink (MPArray _ o ma) sz = do
--     liftST $ shrinkMutableByteArray ma ((o + totalElem sz) * sizeOf (undefined :: e))
--     pure $ MPArray sz o ma
--   {-# INLINE unsafeLinearShrink #-}

--   unsafeLinearGrow (MPArray _ o ma) sz =
--     MPArray sz o <$>
--     liftST (resizeMutableByteArrayCompat ma ((o + totalElem sz) * sizeOf (undefined :: e)))
--   {-# INLINE unsafeLinearGrow #-}


-- instance (Prim e, Index ix) => Load P ix e where
--   makeArrayLinear !comp !sz f = unsafePerformIO $ generateArrayLinear comp sz (pure . f)
--   {-# INLINE makeArrayLinear #-}

--   replicate comp !sz !e = runST (newMArray sz e >>= unsafeFreeze comp)
--   {-# INLINE replicate #-}

--   loadArrayWithST !scheduler !arr =
--     splitLinearlyWith_ scheduler (elemsCount arr) (unsafeLinearIndex arr)
--   {-# INLINE loadArrayWithST #-}

-- instance (Prim e, Index ix) => StrideLoad P ix e

-- instance (Prim e, Index ix) => Stream P ix e where
--   toStream = S.steps
--   {-# INLINE toStream #-}
--   toStreamIx = S.isteps
--   {-# INLINE toStreamIx #-}


-- instance (Prim e, Num e) => FoldNumeric P e where
--   unsafeDotProduct = defaultUnsafeDotProduct
--   {-# INLINE unsafeDotProduct #-}
--   powerSumArray = defaultPowerSumArray
--   {-# INLINE powerSumArray #-}
--   foldArray = defaultFoldArray
--   {-# INLINE foldArray #-}

-- instance (Prim e, Num e) => Numeric P e where
--   unsafeLiftArray = defaultUnsafeLiftArray
--   {-# INLINE unsafeLiftArray #-}
--   unsafeLiftArray2 = defaultUnsafeLiftArray2
--   {-# INLINE unsafeLiftArray2 #-}


-- instance (Prim e, Floating e) => NumericFloat P e


-- instance (Prim e, IsList (Array L ix e), Ragged L ix e) => IsList (Array P ix e) where
--   type Item (Array P ix e) = Item (Array L ix e)
--   fromList = A.fromLists' Seq
--   {-# INLINE fromList #-}
--   toList = GHC.toList . toListArray
--   {-# INLINE toList #-}
