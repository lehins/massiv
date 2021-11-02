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
-- Copyright   : (c) Alexey Kuleshevich 2018-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Manifest.Storable
  ( S (..)
  , Array(..)
  , MArray(..)
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
import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Primitive
import Data.Massiv.Array.Delayed.Pull (D, compareArrays, eqArrays)
import Data.Massiv.Array.Manifest.Internal
import Data.Massiv.Array.Manifest.List as A
import Data.Massiv.Array.Mutable
import Data.Massiv.Core.Common
import Data.Massiv.Core.List
import Data.Massiv.Core.Operations
import Data.Massiv.Vector.Stream as S (isteps, steps)
import Data.Primitive.ByteArray
import Data.Primitive.Ptr (setPtr)
import Data.Word
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array (advancePtr, copyArray)
import Foreign.Ptr
import Foreign.Storable
import GHC.Exts as GHC
import GHC.ForeignPtr
import Prelude hiding (mapM)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce

import qualified Data.Vector.Generic.Mutable as MVG
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as MVS

#include "massiv.h"

-- | Representation for `Storable` elements
data S = S deriving Show

data instance Array S ix e = SArray { sComp   :: !Comp
                                    , sSize   :: !(Sz ix)
                                    , sData   :: {-# UNPACK #-} !(ForeignPtr e)
                                    }

data instance MArray s S ix e = MSArray !(Sz ix) {-# UNPACK #-} !(ForeignPtr e)

instance (Ragged L ix e, Show e, Storable e) => Show (Array S ix e) where
  showsPrec = showsArrayPrec id
  showList = showArrayList

instance NFData ix => NFData (Array S ix e) where
  rnf (SArray c sz _v) = c `deepseq` sz `deepseq` ()
  {-# INLINE rnf #-}

instance NFData ix => NFData (MArray s S ix e) where
  rnf (MSArray sz _mv) = sz `deepseq` ()
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
  repr = S

plusFp :: ForeignPtr a -> Int -> ForeignPtr b
plusFp (ForeignPtr addr c) (I# d) = ForeignPtr (plusAddr# addr d) c

advanceForeignPtr :: forall e . Storable e => ForeignPtr e -> Int -> ForeignPtr e
advanceForeignPtr fp i = plusFp fp (i * sizeOf (undefined :: e))
{-# INLINE advanceForeignPtr #-}

indexForeignPtr :: Storable e => ForeignPtr e -> Int -> e
indexForeignPtr fp i = unsafeInlineIO $ unsafeWithForeignPtr fp $ \p -> peekElemOff p i
{-# INLINE indexForeignPtr #-}

instance Storable e => Source S e where
  unsafeLinearIndex (SArray _ _sz fp) =
    INDEX_CHECK("(Source S ix e).unsafeLinearIndex", const (toLinearSz _sz), indexForeignPtr) fp
  {-# INLINE unsafeLinearIndex #-}

  unsafeOuterSlice (SArray c _ fp) szL i =
    let k = totalElem szL
    in SArray c szL $ advanceForeignPtr fp (i * k)
  {-# INLINE unsafeOuterSlice #-}

  unsafeLinearSlice i k (SArray c _ fp) =
    SArray c k $ advanceForeignPtr fp i
  {-# INLINE unsafeLinearSlice #-}

instance Index ix => Shape S ix where
  maxLinearSize = Just . SafeSz . elemsCount
  {-# INLINE maxLinearSize #-}

instance Size S where
  size = sSize
  {-# INLINE size #-}
  unsafeResize !sz !arr = arr { sSize = sz }
  {-# INLINE unsafeResize #-}


instance Storable e => Manifest S e where

  unsafeLinearIndexM (SArray _ _sz fp) =
    INDEX_CHECK("(Source S ix e).unsafeLinearIndex", const (toLinearSz _sz), indexForeignPtr) fp
  {-# INLINE unsafeLinearIndexM #-}

  sizeOfMArray (MSArray sz _) = sz
  {-# INLINE sizeOfMArray #-}

  unsafeResizeMArray sz (MSArray _ fp) = MSArray sz fp
  {-# INLINE unsafeResizeMArray #-}

  unsafeLinearSliceMArray i k (MSArray _ fp) = MSArray k $ advanceForeignPtr fp i
  {-# INLINE unsafeLinearSliceMArray #-}

  unsafeThaw (SArray _ sz fp) = pure $ MSArray sz fp
  {-# INLINE unsafeThaw #-}

  unsafeFreeze comp (MSArray sz v) = pure $ SArray comp sz v
  {-# INLINE unsafeFreeze #-}

  unsafeNew sz = do
    let !n = totalElem sz
        dummy = undefined :: e
        !eSize = sizeOf dummy
    when (n > (maxBound :: Int) `div` eSize) $ error $ "Array size is too big: " ++ show sz
    unsafeIOToPrim $ do
      fp <- mallocPlainForeignPtrAlignedBytes (n * sizeOf dummy) (alignment dummy)
      pure $ MSArray sz fp
  {-# INLINE unsafeNew #-}

  initialize (MSArray sz fp) =
    unsafeIOToPrim $
      unsafeWithForeignPtr fp $ \p ->
        setPtr (castPtr p) (totalElem sz * sizeOf (undefined :: e)) (0 :: Word8)
  {-# INLINE initialize #-}

  unsafeLinearRead (MSArray _sz fp) o = unsafeIOToPrim $
    INDEX_CHECK("(Manifest S ix e).unsafeLinearRead", const (toLinearSz _sz), (\_ _ -> unsafeWithForeignPtr fp (`peekElemOff` o))) fp o
  {-# INLINE unsafeLinearRead #-}

  unsafeLinearWrite (MSArray _sz fp) o e = unsafeIOToPrim $
    INDEX_CHECK("(Manifest S ix e).unsafeLinearWrite", const (toLinearSz _sz), (\_ _ -> unsafeWithForeignPtr fp (\p -> pokeElemOff p o e))) fp o
  {-# INLINE unsafeLinearWrite #-}

  unsafeLinearSet (MSArray _ fp) i k e =
    stToPrim (MVG.basicSet (MVS.unsafeFromForeignPtr0 (advanceForeignPtr fp i) (unSz k)) e)
  {-# INLINE unsafeLinearSet #-}

  unsafeLinearCopy (MSArray _ fpFrom) iFrom (MSArray _ fpTo) iTo (Sz k) = do
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

  unsafeLinearShrink marr@(MSArray _ fp@(ForeignPtr _ fpc)) sz = do
    let shrinkMBA :: MutableByteArray RealWorld -> IO ()
        shrinkMBA mba = shrinkMutableByteArray mba (totalElem sz * sizeOf (undefined :: e))
        {-# INLINE shrinkMBA #-}
    case fpc of
      MallocPtr mba# _ -> do
        unsafePrimToPrim $ shrinkMBA (MutableByteArray mba#)
        pure $ MSArray sz fp
      PlainPtr mba# -> do
        unsafePrimToPrim $ shrinkMBA (MutableByteArray mba#)
        pure $ MSArray sz fp
      _ -> unsafeDefaultLinearShrink marr sz
  {-# INLINE unsafeLinearShrink #-}

instance (Index ix, Storable e) => Load S ix e where
  makeArray comp sz f = compute (makeArray comp sz f :: Array D ix e)
  {-# INLINE makeArray #-}

  makeArrayLinear !comp !sz f = unsafePerformIO $ generateArrayLinear comp sz (pure . f)
  {-# INLINE makeArrayLinear #-}

  replicate comp !sz !e = runST (newMArray sz e >>= unsafeFreeze comp)
  {-# INLINE replicate #-}

  iterArrayLinearST_ !scheduler !arr =
    splitLinearlyWith_ scheduler (elemsCount arr) (unsafeLinearIndex arr)
  {-# INLINE iterArrayLinearST_ #-}

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


instance (Storable e, IsList (Array L ix e), Ragged L ix e) => IsList (Array S ix e) where
  type Item (Array S ix e) = Item (Array L ix e)
  fromList = A.fromLists' Seq
  {-# INLINE fromList #-}
  toList = GHC.toList . toListArray
  {-# INLINE toList #-}

-- | A pointer to the beginning of the storable array. It is unsafe since, if mutated, it can break
-- referential transparency.
--
-- @since 0.1.3
unsafeWithPtr :: MonadUnliftIO m => Array S ix e -> (Ptr e -> m b) -> m b
unsafeWithPtr arr f = withRunInIO $ \run -> unsafeWithForeignPtr (sData arr) (run . f)
{-# INLINE unsafeWithPtr #-}


-- | A pointer to the beginning of the mutable array.
--
-- @since 0.1.3
withPtr :: MonadUnliftIO m => MArray RealWorld S ix e -> (Ptr e -> m b) -> m b
withPtr (MSArray _ fp) f = withRunInIO $ \run -> unsafeWithForeignPtr fp (run . f)
{-# INLINE withPtr #-}


-- | /O(1)/ - Unwrap storable array and pull out the underlying storable vector.
--
-- @since 0.2.1
toStorableVector :: Index ix => Array S ix e -> VS.Vector e
toStorableVector arr =
  unsafeCoerce $ -- this hack is needed to workaround the redundant Storable constraint
                 -- see haskell/vector#394
  VS.unsafeFromForeignPtr0 (castForeignPtr (sData arr) :: ForeignPtr Word) (totalElem (sSize arr))
{-# INLINE toStorableVector #-}


-- | /O(1)/ - Unwrap storable mutable array and pull out the underlying storable mutable vector.
--
-- @since 0.2.1
toStorableMVector :: Index ix => MArray s S ix e -> VS.MVector s e
toStorableMVector (MSArray sz fp) = MVS.MVector (totalElem sz) fp
{-# INLINE toStorableMVector #-}

-- | /O(1)/ - Cast a storable vector to a storable array.
--
-- @since 0.5.0
fromStorableVector :: Comp -> VS.Vector e -> Vector S e
fromStorableVector comp v =
  -- unasfeCoerce hack below is needed to workaround the redundant Storable
  -- constraint see haskell/vector#394
  case VS.unsafeToForeignPtr0 (unsafeCoerce v :: VS.Vector Word) of
    (fp, k) -> SArray {sComp = comp, sSize = SafeSz k, sData = castForeignPtr fp}
{-# INLINE fromStorableVector #-}

-- | /O(1)/ - Cast a mutable storable vector to a mutable storable array.
--
-- @since 0.5.0
fromStorableMVector :: MVS.MVector s e -> MVector s S e
fromStorableMVector (MVS.MVector n fp) = MSArray (SafeSz n) fp
{-# INLINE fromStorableMVector #-}


-- | /O(1)/ - Yield the underlying `ForeignPtr` together with its length.
--
-- @since 0.3.0
unsafeArrayToForeignPtr :: Index ix => Array S ix e -> (ForeignPtr e, Int)
unsafeArrayToForeignPtr (SArray _ sz fp) = (fp, totalElem sz)
{-# INLINE unsafeArrayToForeignPtr #-}

-- | /O(1)/ - Yield the underlying `ForeignPtr` together with its length.
--
-- @since 0.3.0
unsafeMArrayToForeignPtr :: Index ix => MArray s S ix e -> (ForeignPtr e, Int)
unsafeMArrayToForeignPtr (MSArray sz fp) = (fp, totalElem sz)
{-# INLINE unsafeMArrayToForeignPtr #-}

-- | /O(1)/ - Wrap a `ForeignPtr` and it's size into a pure storable array.
--
-- @since 0.3.0
unsafeArrayFromForeignPtr0 :: Comp -> ForeignPtr e -> Sz1 -> Vector S e
unsafeArrayFromForeignPtr0 comp fp sz = SArray {sComp = comp, sSize = sz, sData = fp}
{-# INLINE unsafeArrayFromForeignPtr0 #-}

-- | /O(1)/ - Wrap a `ForeignPtr`, an offset and it's size into a pure storable array.
--
-- @since 0.3.0
unsafeArrayFromForeignPtr :: Storable e => Comp -> ForeignPtr e -> Int -> Sz1 -> Array S Ix1 e
unsafeArrayFromForeignPtr comp ptr offset sz =
  SArray {sComp = comp, sSize = sz, sData = advanceForeignPtr ptr offset}
{-# INLINE unsafeArrayFromForeignPtr #-}


-- | /O(1)/ - Wrap a `ForeignPtr` and it's size into a mutable storable array. It is still safe to
-- modify the pointer, unless the array gets frozen prior to modification.
--
-- @since 0.3.0
unsafeMArrayFromForeignPtr0 :: ForeignPtr e -> Sz1 -> MArray s S Ix1 e
unsafeMArrayFromForeignPtr0 fp sz = MSArray sz fp
{-# INLINE unsafeMArrayFromForeignPtr0 #-}


-- | /O(1)/ - Wrap a `ForeignPtr`, an offset and it's size into a mutable storable array. It is
-- still safe to modify the pointer, unless the array gets frozen prior to modification.
--
-- @since 0.3.0
unsafeMArrayFromForeignPtr :: Storable e => ForeignPtr e -> Int -> Sz1 -> MArray s S Ix1 e
unsafeMArrayFromForeignPtr fp offset sz = MSArray sz (advanceForeignPtr fp offset)
{-# INLINE unsafeMArrayFromForeignPtr #-}


-- | Allocate memory using @malloc@ on C heap, instead of on Haskell heap. Memory is left
-- uninitialized
--
-- @since 0.5.9
unsafeMallocMArray ::
     forall ix e m. (Index ix, Storable e, PrimMonad m)
  => Sz ix
  -> m (MArray (PrimState m) S ix e)
unsafeMallocMArray sz = unsafePrimToPrim $ do
  let n = totalElem sz
  foreignPtr <- mask_ $ do
    ptr <- mallocBytes (sizeOf (undefined :: e) * n)
    newForeignPtr finalizerFree ptr
  pure $ MSArray sz foreignPtr
{-# INLINE unsafeMallocMArray #-}


#if !MIN_VERSION_base(4,15,0)
-- | A compatibility wrapper for 'GHC.ForeignPtr.unsafeWithForeignPtr' provided
-- by GHC 9.0.1 and later.
--
-- Only to be used when the continuation is known not to
-- unconditionally diverge lest unsoundness can result.
unsafeWithForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
unsafeWithForeignPtr = withForeignPtr
#endif
