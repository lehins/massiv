{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DeriveGeneric         #-}
-- |
-- Module      : Data.Massiv.Array.Manifest.Storable
-- Copyright   : (c) Alexey Kuleshevich 2018
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
import           Data.Massiv.Array.Delayed.Internal  (eq, ord)
import           Data.Massiv.Array.Manifest.Internal
import           Data.Massiv.Array.Manifest.List     as A
import           Data.Massiv.Array.Mutable
import           Data.Massiv.Array.Unsafe            (unsafeGenerateArray,
                                                      unsafeGenerateArrayP)
import           Data.Massiv.Core.Common
import           Data.Massiv.Core.List
import qualified Data.Vector.Storable                as VS
import qualified Data.Vector.Storable.Mutable        as MVS
import           Foreign.Ptr
import           GHC.Exts                            as GHC (IsList (..))
import           GHC.Generics (Generic)
import           Prelude                             hiding (mapM)

#include "massiv.h"

-- | Representation for `Storable` elements
data S = S deriving (Show, Generic)

type instance EltRepr S ix = M

data instance Array S ix e = SArray { sComp :: !Comp
                                    , sSize :: !ix
                                    , sData :: !(VS.Vector e)
                                    } deriving (Generic)


instance (Index ix, NFData e) => NFData (Array S ix e) where
  rnf (SArray c sz v) = c `deepseq` sz `deepseq` v `deepseq` ()

instance (VS.Storable e, Eq e, Index ix) => Eq (Array S ix e) where
  (==) = eq (==)
  {-# INLINE (==) #-}

instance (VS.Storable e, Ord e, Index ix) => Ord (Array S ix e) where
  compare = ord compare
  {-# INLINE compare #-}

instance (VS.Storable e, Index ix) => Construct S ix e where
  getComp = sComp
  {-# INLINE getComp #-}

  setComp c arr = arr { sComp = c }
  {-# INLINE setComp #-}

  unsafeMakeArray Seq          !sz f = unsafeGenerateArray sz f
  unsafeMakeArray (ParOn wIds) !sz f = unsafeGenerateArrayP wIds sz f
  {-# INLINE unsafeMakeArray #-}


instance (VS.Storable e, Index ix) => Source S ix e where
  unsafeLinearIndex (SArray _ _ v) =
    INDEX_CHECK("(Source S ix e).unsafeLinearIndex", VS.length, VS.unsafeIndex) v
  {-# INLINE unsafeLinearIndex #-}


instance (VS.Storable e, Index ix) => Size S ix e where
  size = sSize
  {-# INLINE size #-}

  unsafeResize !sz !arr = arr { sSize = sz }
  {-# INLINE unsafeResize #-}

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


instance (Index ix, VS.Storable e) => Manifest S ix e where

  unsafeLinearIndexM (SArray _ _ v) =
    INDEX_CHECK("(Manifest S ix e).unsafeLinearIndexM", VS.length, VS.unsafeIndex) v
  {-# INLINE unsafeLinearIndexM #-}


instance (Index ix, VS.Storable e) => Mutable S ix e where
  data MArray s S ix e = MSArray !ix !(VS.MVector s e)

  msize (MSArray sz _) = sz
  {-# INLINE msize #-}

  unsafeThaw (SArray _ sz v) = MSArray sz <$> VS.unsafeThaw v
  {-# INLINE unsafeThaw #-}

  unsafeFreeze comp (MSArray sz v) = SArray comp sz <$> VS.unsafeFreeze v
  {-# INLINE unsafeFreeze #-}

  unsafeNew sz = MSArray sz <$> MVS.unsafeNew (totalElem sz)
  {-# INLINE unsafeNew #-}

  unsafeNewZero sz = MSArray sz <$> MVS.new (totalElem sz)
  {-# INLINE unsafeNewZero #-}

  unsafeLinearRead (MSArray _ mv) =
    INDEX_CHECK("(Mutable S ix e).unsafeLinearRead", MVS.length, MVS.unsafeRead) mv
  {-# INLINE unsafeLinearRead #-}

  unsafeLinearWrite (MSArray _ mv) =
    INDEX_CHECK("(Mutable S ix e).unsafeLinearWrite", MVS.length, MVS.unsafeWrite) mv
  {-# INLINE unsafeLinearWrite #-}


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

-- | A pointer to the beginning of the storable array. It is unsafe since it can break referential
-- transparency.
--
-- @since 0.1.3
unsafeWithPtr :: VS.Storable a => Array S ix a -> (Ptr a -> IO b) -> IO b
unsafeWithPtr arr = VS.unsafeWith (sData arr)



-- | A pointer to the beginning of the mutable array.
--
-- @since 0.1.3
withPtr :: (Index ix, VS.Storable a) => MArray RealWorld S ix a -> (Ptr a -> IO b) -> IO b
withPtr (MSArray _ mv) = MVS.unsafeWith mv


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
