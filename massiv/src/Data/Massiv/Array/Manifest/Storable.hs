{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Massiv.Array.Manifest.Storable
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Manifest.Storable
  ( S (..)
  , Array(..)
  , VS.Storable
  , toStorableVector
  , toStorableMVector
  , withPtr
  , unsafeWithPtr
  ) where

import           Control.DeepSeq                     (NFData (..), deepseq)
import           Control.Monad.IO.Unlift
import           Data.Massiv.Array.Delayed.Pull      (eq, ord)
import           Data.Massiv.Array.Manifest.Internal
import           Data.Massiv.Array.Manifest.List     as A
import           Data.Massiv.Array.Mutable
import           Data.Massiv.Core.Common
import           Data.Massiv.Core.List
import qualified Data.Vector.Generic.Mutable         as VGM
import qualified Data.Vector.Storable                as VS
import qualified Data.Vector.Storable.Mutable        as MVS
import           Foreign.Ptr
import           GHC.Exts                            as GHC (IsList (..))
import           Prelude                             hiding (mapM)
import           System.IO.Unsafe                    (unsafePerformIO)

#include "massiv.h"

-- | Representation for `Storable` elements
data S = S deriving Show

type instance EltRepr S ix = M

data instance Array S ix e = SArray { sComp :: !Comp
                                    , sSize :: !(Sz ix)
                                    , sData :: !(VS.Vector e)
                                    }

instance (Ragged L ix e, Show e, VS.Storable e) => Show (Array S ix e) where
  showsPrec = showsArrayPrec id
  showList = showArrayList

instance NFData ix => NFData (Array S ix e) where
  rnf (SArray c sz v) = c `deepseq` sz `deepseq` v `deepseq` ()
  {-# INLINE rnf #-}

instance (VS.Storable e, Eq e, Index ix) => Eq (Array S ix e) where
  (==) = eq (==)
  {-# INLINE (==) #-}

instance (VS.Storable e, Ord e, Index ix) => Ord (Array S ix e) where
  compare = ord compare
  {-# INLINE compare #-}

instance (VS.Storable e, Index ix) => Construct S ix e where
  setComp c arr = arr { sComp = c }
  {-# INLINE setComp #-}

  makeArray !comp !sz f = unsafePerformIO $ generateArray comp sz (return . f)
  {-# INLINE makeArray #-}


instance (VS.Storable e, Index ix) => Source S ix e where
  unsafeLinearIndex (SArray _ _ v) =
    INDEX_CHECK("(Source S ix e).unsafeLinearIndex", Sz . VS.length, VS.unsafeIndex) v
  {-# INLINE unsafeLinearIndex #-}

instance Index ix => Resize Array S ix where
  unsafeResize !sz !arr = arr { sSize = sz }
  {-# INLINE unsafeResize #-}

instance (VS.Storable e, Index ix) => Extract S ix e where
  unsafeExtract !sIx !newSz !arr = unsafeExtract sIx newSz (toManifest arr)
  {-# INLINE unsafeExtract #-}



instance ( VS.Storable e
         , Index ix
         , Index (Lower ix)
         , Elt M ix e ~ Array M (Lower ix) e
         , Elt S ix e ~ Array M (Lower ix) e
         ) =>
         OuterSlice S ix e where
  unsafeOuterSlice arr = unsafeOuterSlice (toManifest arr)
  {-# INLINE unsafeOuterSlice #-}

instance ( VS.Storable e
         , Index ix
         , Index (Lower ix)
         , Elt M ix e ~ Array M (Lower ix) e
         , Elt S ix e ~ Array M (Lower ix) e
         ) =>
         InnerSlice S ix e where
  unsafeInnerSlice arr = unsafeInnerSlice (toManifest arr)
  {-# INLINE unsafeInnerSlice #-}

instance {-# OVERLAPPING #-} VS.Storable e => Slice S Ix1 e where
  unsafeSlice arr i _ _ = pure (unsafeLinearIndex arr i)
  {-# INLINE unsafeSlice #-}


instance (Index ix, VS.Storable e) => Manifest S ix e where

  unsafeLinearIndexM (SArray _ _ v) =
    INDEX_CHECK("(Manifest S ix e).unsafeLinearIndexM", Sz . VS.length, VS.unsafeIndex) v
  {-# INLINE unsafeLinearIndexM #-}


instance (Index ix, VS.Storable e) => Mutable S ix e where
  data MArray s S ix e = MSArray !(Sz ix) !(VS.MVector s e)

  msize (MSArray sz _) = sz
  {-# INLINE msize #-}

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


instance (Index ix, VS.Storable e) => Load S ix e where
  size = sSize
  {-# INLINE size #-}
  getComp = sComp
  {-# INLINE getComp #-}
  loadArrayM !numWorkers scheduleWork !arr =
    splitLinearlyWith_ numWorkers scheduleWork (elemsCount arr) (unsafeLinearIndex arr)
  {-# INLINE loadArrayM #-}

instance (Index ix, VS.Storable e) => StrideLoad S ix e


instance ( VS.Storable e
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
unsafeWithPtr :: (MonadUnliftIO m, VS.Storable a) => Array S ix a -> (Ptr a -> m b) -> m b
unsafeWithPtr arr f = withRunInIO $ \run -> VS.unsafeWith (sData arr) (run . f)
{-# INLINE unsafeWithPtr #-}


-- | A pointer to the beginning of the mutable array.
--
-- @since 0.1.3
withPtr :: (MonadUnliftIO m, VS.Storable a) => MArray RealWorld S ix a -> (Ptr a -> m b) -> m b
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
