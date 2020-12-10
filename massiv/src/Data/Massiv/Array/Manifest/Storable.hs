{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Massiv.Array.Manifest.Storable
-- Copyright   : (c) Alexey Kuleshevich 2018-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Manifest.Storable
  ( S (..)
  , Array(..)
  , Storable
  , toStorableVector
  , toStorableMVector
  , fromStorableVector
  , fromStorableMVector
  , withPtr
  , unsafeWithPtr
  , unsafeMallocMArray
  , unsafeArrayToForeignPtr
  , unsafeMArrayToForeignPtr
  , unsafeArrayFromForeignPtr
  , unsafeArrayFromForeignPtr0
  , unsafeMArrayFromForeignPtr
  , unsafeMArrayFromForeignPtr0
  ) where

import Control.DeepSeq (NFData(..), deepseq)
import Control.Exception
import Control.Monad.IO.Unlift
import Control.Monad.Primitive (unsafePrimToPrim)
import Data.Massiv.Array.Delayed.Pull (compareArrays, eqArrays)
import Data.Massiv.Array.Manifest.Internal
import Data.Massiv.Array.Manifest.List as A
import Data.Massiv.Array.Manifest.Primitive (shrinkMutableByteArray)
import Data.Massiv.Array.Mutable
import Data.Massiv.Core.Common
import Data.Massiv.Core.List
import Data.Massiv.Core.Operations
import Data.Massiv.Vector.Stream as S (isteps, steps)
import Data.Primitive.ByteArray (MutableByteArray(..))
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as MVS
import Foreign.ForeignPtr (newForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array (advancePtr, copyArray)
import Foreign.Ptr
import Foreign.Storable
import GHC.Exts as GHC (IsList(..))
import GHC.ForeignPtr (ForeignPtr(..), ForeignPtrContents(..))
import Prelude hiding (mapM)
import System.IO.Unsafe (unsafePerformIO)

#include "massiv.h"

-- | Representation for `Storable` elements
data S = S deriving Show

data instance Array S ix e = SArray { sComp :: !Comp
                                    , sSize :: !(Sz ix)
                                    , sData :: !(VS.Vector e)
                                    }

instance (Ragged L ix e, Show e, Storable e) => Show (Array S ix e) where
  showsPrec = showsArrayPrec id
  showList = showArrayList

instance NFData ix => NFData (Array S ix e) where
  rnf (SArray c sz v) = c `deepseq` sz `deepseq` v `deepseq` ()
  {-# INLINE rnf #-}

instance NFData ix => NFData (MArray s S ix e) where
  rnf (MSArray sz mv) = sz `deepseq` mv `deepseq` ()
  {-# INLINE rnf #-}

instance (Storable e, Eq e, Index ix) => Eq (Array S ix e) where
  (==) = eqArrays (==)
  {-# INLINE (==) #-}

instance (Storable e, Ord e, Index ix) => Ord (Array S ix e) where
  compare = compareArrays compare
  {-# INLINE compare #-}

instance Strategy S where
  getComp = sComp
  {-# INLINE getComp #-}
  setComp c arr = arr { sComp = c }
  {-# INLINE setComp #-}

instance (Storable e, Index ix) => Construct S ix e where
  makeArrayLinear !comp !sz f = unsafePerformIO $ generateArrayLinear comp sz (pure . f)
  {-# INLINE makeArrayLinear #-}

  replicate comp !sz !e = runST (newMArray sz e >>= unsafeFreeze comp)
  {-# INLINE replicate #-}

instance VS.Storable e => Source S e where
  unsafeLinearIndex (SArray _ _ v) =
    INDEX_CHECK("(Source S ix e).unsafeLinearIndex", Sz . VS.length, VS.unsafeIndex) v
  {-# INLINE unsafeLinearIndex #-}
  unsafeLinearSlice i k (SArray c _ v) = SArray c k $ VS.unsafeSlice i (unSz k) v
  {-# INLINE unsafeLinearSlice #-}

instance Index ix => Shape S ix where
  maxLinearSize = Just . SafeSz . elemsCount
  {-# INLINE maxLinearSize #-}

instance Size S where
  size = sSize
  {-# INLINE size #-}

instance Resize S where
  unsafeResize !sz !arr = arr { sSize = sz }
  {-# INLINE unsafeResize #-}

instance (Storable e, Index ix) => Extract S ix e where
  unsafeExtract !sIx !newSz !arr = unsafeExtract sIx newSz (toManifest arr)
  {-# INLINE unsafeExtract #-}



instance (Storable e, Elt S ix e ~ Elt M ix e, Slice M ix e) => Slice S ix e where
  unsafeSlice = unsafeSlice . toManifest
  {-# INLINE unsafeSlice #-}

instance (Storable e, Elt S ix e ~ Elt M ix e, OuterSlice M ix e) => OuterSlice S ix e where
  unsafeOuterSlice = unsafeOuterSlice . toManifest
  {-# INLINE unsafeOuterSlice #-}

instance (Storable e, Elt S ix e ~ Elt M ix e, InnerSlice M ix e) => InnerSlice S ix e where
  unsafeInnerSlice = unsafeInnerSlice . toManifest
  {-# INLINE unsafeInnerSlice #-}


instance Storable e => Manifest S e where

  unsafeLinearIndexM (SArray _ _ v) =
    INDEX_CHECK("(Manifest S ix e).unsafeLinearIndexM", Sz . VS.length, VS.unsafeIndex) v
  {-# INLINE unsafeLinearIndexM #-}


instance Storable e => Mutable S e where
  data MArray s S ix e = MSArray !(Sz ix) !(VS.MVector s e)

  msize (MSArray sz _) = sz
  {-# INLINE msize #-}

  munsafeResize sz (MSArray _ mvec) = MSArray sz mvec
  {-# INLINE munsafeResize #-}

  unsafeThaw (SArray _ sz v) = MSArray sz <$> VS.unsafeThaw v
  {-# INLINE unsafeThaw #-}

  unsafeFreeze comp (MSArray sz v) = SArray comp sz <$> VS.unsafeFreeze v
  {-# INLINE unsafeFreeze #-}

  unsafeNew sz = MSArray sz <$> MVS.unsafeNew (totalElem sz)
  {-# INLINE unsafeNew #-}

  initialize (MSArray _ marr) = VGM.basicInitialize marr
  {-# INLINE initialize #-}

  unsafeLinearRead (MSArray _ mv) =
    INDEX_CHECK("(Mutable S ix e).unsafeLinearRead", Sz . MVS.length, MVS.unsafeRead) mv
  {-# INLINE unsafeLinearRead #-}

  unsafeLinearWrite (MSArray _ mv) =
    INDEX_CHECK("(Mutable S ix e).unsafeLinearWrite", Sz . MVS.length, MVS.unsafeWrite) mv
  {-# INLINE unsafeLinearWrite #-}

  unsafeLinearSet (MSArray _ mv) i k = VGM.basicSet (MVS.unsafeSlice i (unSz k) mv)
  {-# INLINE unsafeLinearSet #-}

  unsafeLinearCopy marrFrom iFrom marrTo iTo (Sz k) = do
    let MSArray _ (MVS.MVector _ fpFrom) = marrFrom
        MSArray _ (MVS.MVector _ fpTo) = marrTo
    unsafePrimToPrim $
      withForeignPtr fpFrom $ \ ptrFrom ->
        withForeignPtr fpTo $ \ ptrTo -> do
          let ptrFrom' = advancePtr ptrFrom iFrom
              ptrTo' = advancePtr ptrTo iTo
          copyArray ptrTo' ptrFrom' k
  {-# INLINE unsafeLinearCopy #-}

  unsafeArrayLinearCopy arrFrom iFrom marrTo iTo sz = do
    marrFrom <- unsafeThaw arrFrom
    unsafeLinearCopy marrFrom iFrom marrTo iTo sz
  {-# INLINE unsafeArrayLinearCopy #-}

  unsafeLinearShrink marr@(MSArray _ mv@(MVS.MVector _ (ForeignPtr _ fpc))) sz = do
    let shrinkMBA :: MutableByteArray RealWorld -> IO ()
        shrinkMBA mba = shrinkMutableByteArray mba (totalElem sz * sizeOf (undefined :: e))
        {-# INLINE shrinkMBA #-}
    case fpc of
      MallocPtr mba# _ -> do
        unsafePrimToPrim $ shrinkMBA (MutableByteArray mba#)
        pure $ MSArray sz mv
      PlainPtr mba# -> do
        unsafePrimToPrim $ shrinkMBA (MutableByteArray mba#)
        pure $ MSArray sz mv
      _ -> unsafeDefaultLinearShrink marr sz
  {-# INLINE unsafeLinearShrink #-}

  unsafeLinearGrow (MSArray oldSz mv) sz =
    MSArray sz <$> MVS.unsafeGrow mv (totalElem sz - totalElem oldSz)
  {-# INLINE unsafeLinearGrow #-}


instance (Index ix, Storable e) => Load S ix e where
  type R S = M
  loadArrayM !scheduler !arr = splitLinearlyWith_ scheduler (elemsCount arr) (unsafeLinearIndex arr)
  {-# INLINE loadArrayM #-}

instance (Index ix, Storable e) => StrideLoad S ix e

instance (Index ix, Storable e) => Stream S ix e where
  toStream = S.steps
  {-# INLINE toStream #-}
  toStreamIx = S.isteps
  {-# INLINE toStreamIx #-}


instance (Storable e, Num e) => FoldNumeric S e where
  unsafeDotProduct = defaultUnsafeDotProduct
  {-# INLINE unsafeDotProduct #-}
  powerSumArray = defaultPowerSumArray
  {-# INLINE powerSumArray #-}
  foldArray = defaultFoldArray
  {-# INLINE foldArray #-}

instance (Storable e, Num e) => Numeric S e where
  unsafeLiftArray = defaultUnsafeLiftArray
  {-# INLINE unsafeLiftArray #-}
  unsafeLiftArray2 = defaultUnsafeLiftArray2
  {-# INLINE unsafeLiftArray2 #-}

instance (Storable e, Floating e) => NumericFloat S e


instance ( Storable e
         , IsList (Array L ix e)
         , Nested LN ix e
         , Nested L ix e
         , Ragged L ix e
         ) =>
         IsList (Array S ix e) where
  type Item (Array S ix e) = Item (Array L ix e)
  fromList = A.fromLists' Seq
  {-# INLINE fromList #-}
  toList = GHC.toList . toListArray
  {-# INLINE toList #-}

-- | A pointer to the beginning of the storable array. It is unsafe since, if mutated, it can break
-- referential transparency.
--
-- @since 0.1.3
unsafeWithPtr :: (MonadUnliftIO m, Storable a) => Array S ix a -> (Ptr a -> m b) -> m b
unsafeWithPtr arr f = withRunInIO $ \run -> VS.unsafeWith (sData arr) (run . f)
{-# INLINE unsafeWithPtr #-}


-- | A pointer to the beginning of the mutable array.
--
-- @since 0.1.3
withPtr :: (MonadUnliftIO m, Storable a) => MArray RealWorld S ix a -> (Ptr a -> m b) -> m b
withPtr (MSArray _ mv) f = withRunInIO $ \run -> MVS.unsafeWith mv (run . f)
{-# INLINE withPtr #-}


-- | /O(1)/ - Unwrap storable array and pull out the underlying storable vector.
--
-- @since 0.2.1
toStorableVector :: Array S ix e -> VS.Vector e
toStorableVector = sData
{-# INLINE toStorableVector #-}


-- | /O(1)/ - Unwrap storable mutable array and pull out the underlying storable mutable vector.
--
-- @since 0.2.1
toStorableMVector :: MArray s S ix e -> VS.MVector s e
toStorableMVector (MSArray _ mv) = mv
{-# INLINE toStorableMVector #-}

-- | /O(1)/ - Cast a storable vector to a storable array.
--
-- @since 0.5.0
fromStorableVector :: Storable e => Comp -> VS.Vector e -> Array S Ix1 e
fromStorableVector comp v = SArray {sComp = comp, sSize = SafeSz (VS.length v), sData = v}
{-# INLINE fromStorableVector #-}

-- | /O(1)/ - Cast a mutable storable vector to a mutable storable array.
--
-- @since 0.5.0
fromStorableMVector :: MVS.MVector s e -> MArray s S Ix1 e
fromStorableMVector mv@(MVS.MVector len _) = MSArray (SafeSz len) mv
{-# INLINE fromStorableMVector #-}


-- | /O(1)/ - Yield the underlying `ForeignPtr` together with its length.
--
-- @since 0.3.0
unsafeArrayToForeignPtr :: Storable e => Array S ix e -> (ForeignPtr e, Int)
unsafeArrayToForeignPtr = VS.unsafeToForeignPtr0 . toStorableVector
{-# INLINE unsafeArrayToForeignPtr #-}

-- | /O(1)/ - Yield the underlying `ForeignPtr` together with its length.
--
-- @since 0.3.0
unsafeMArrayToForeignPtr :: Storable e => MArray s S ix e -> (ForeignPtr e, Int)
unsafeMArrayToForeignPtr = MVS.unsafeToForeignPtr0 . toStorableMVector
{-# INLINE unsafeMArrayToForeignPtr #-}

-- | /O(1)/ - Wrap a `ForeignPtr` and it's size into a pure storable array.
--
-- @since 0.3.0
unsafeArrayFromForeignPtr0 :: Storable e => Comp -> ForeignPtr e -> Sz1 -> Array S Ix1 e
unsafeArrayFromForeignPtr0 comp ptr sz =
  SArray {sComp = comp, sSize = sz, sData = VS.unsafeFromForeignPtr0 ptr (unSz sz)}
{-# INLINE unsafeArrayFromForeignPtr0 #-}

-- | /O(1)/ - Wrap a `ForeignPtr`, an offset and it's size into a pure storable array.
--
-- @since 0.3.0
unsafeArrayFromForeignPtr :: Storable e => Comp -> ForeignPtr e -> Int -> Sz1 -> Array S Ix1 e
unsafeArrayFromForeignPtr comp ptr offset sz =
  SArray {sComp = comp, sSize = sz, sData = VS.unsafeFromForeignPtr ptr offset (unSz sz)}
{-# INLINE unsafeArrayFromForeignPtr #-}


-- | /O(1)/ - Wrap a `ForeignPtr` and it's size into a mutable storable array. It is still safe to
-- modify the pointer, unless the array gets frozen prior to modification.
--
-- @since 0.3.0
unsafeMArrayFromForeignPtr0 :: Storable e => ForeignPtr e -> Sz1 -> MArray s S Ix1 e
unsafeMArrayFromForeignPtr0 fp sz =
  MSArray sz (MVS.unsafeFromForeignPtr0 fp (unSz sz))
{-# INLINE unsafeMArrayFromForeignPtr0 #-}


-- | /O(1)/ - Wrap a `ForeignPtr`, an offset and it's size into a mutable storable array. It is
-- still safe to modify the pointer, unless the array gets frozen prior to modification.
--
-- @since 0.3.0
unsafeMArrayFromForeignPtr :: Storable e => ForeignPtr e -> Int -> Sz1 -> MArray s S Ix1 e
unsafeMArrayFromForeignPtr fp offset sz =
  MSArray sz (MVS.unsafeFromForeignPtr fp offset (unSz sz))
{-# INLINE unsafeMArrayFromForeignPtr #-}


-- | Allocate memory using @malloc@ on C heap, instead of on Haskell heap. Memory is left
-- uninitialized
--
-- @since 0.5.9
unsafeMallocMArray ::
     forall ix e m. (Index ix, Storable e, MonadIO m)
  => Sz ix
  -> m (MArray RealWorld S ix e)
unsafeMallocMArray sz = liftIO $ do
  let n = totalElem sz
  foreignPtr <- mask_ $ do
    ptr <- mallocBytes (sizeOf (undefined :: e) * n)
    newForeignPtr finalizerFree ptr
  pure $ MSArray sz (MVS.unsafeFromForeignPtr0 foreignPtr n)
{-# INLINE unsafeMallocMArray #-}
