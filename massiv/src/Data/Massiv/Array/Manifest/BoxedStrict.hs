{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Massiv.Array.Manifest.Boxed
-- Copyright   : (c) Alexey Kuleshevich 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Manifest.BoxedStrict
  ( B (..)
  , Array(..)
  ) where

import           Control.DeepSeq                     (NFData (..))
import qualified Data.Foldable                       as F (Foldable (..))
import           Data.Massiv.Array.Delayed.Internal  (eq, ord)
import           Data.Massiv.Array.Manifest.BoxedNF  (deepseqArray,
                                                      deepseqArrayP)
import           Data.Massiv.Array.Unsafe            (unsafeGenerateArray,
                                                      unsafeGenerateArrayP)
import           Data.Massiv.Array.Manifest.Internal
import           Data.Massiv.Array.Manifest.List     as A
import           Data.Massiv.Array.Mutable
import           Data.Massiv.Array.Ops.Fold.Internal
import           Data.Massiv.Core.Common
import           Data.Massiv.Core.List
import qualified Data.Primitive.Array                as A
import           GHC.Base                            (build)
import           GHC.Exts                            as GHC (IsList (..))
import           Prelude                             hiding (mapM)

#include "massiv.h"


-- | Array representation for Boxed elements. This structure is element and
-- spine strict, but elements are strict to Weak Head Normal Form (WHNF) only.
data B = B deriving Show

type instance EltRepr B ix = M

data instance Array B ix e = BArray { bComp :: !Comp
                                    , bSize :: !ix
                                    , bData :: {-# UNPACK #-} !(A.Array e)
                                    }

instance (Index ix, NFData e) => NFData (Array B ix e) where
  rnf (BArray comp sz arr) =
    case comp of
      Seq        -> deepseqArray sz arr ()
      ParOn wIds -> deepseqArrayP wIds sz arr ()

instance (Index ix, Eq e) => Eq (Array B ix e) where
  (==) = eq (==)
  {-# INLINE (==) #-}

instance (Index ix, Ord e) => Ord (Array B ix e) where
  compare = ord compare
  {-# INLINE compare #-}

instance Index ix => Construct B ix e where
  getComp = bComp
  {-# INLINE getComp #-}

  setComp c arr = arr { bComp = c }
  {-# INLINE setComp #-}

  unsafeMakeArray Seq          !sz f = unsafeGenerateArray sz f
  unsafeMakeArray (ParOn wIds) !sz f = unsafeGenerateArrayP wIds sz f
  {-# INLINE unsafeMakeArray #-}

instance Index ix => Source B ix e where
  unsafeLinearIndex (BArray _ _ a) =
    INDEX_CHECK("(Source B ix e).unsafeLinearIndex", A.sizeofArray, A.indexArray) a
  {-# INLINE unsafeLinearIndex #-}


instance Index ix => Size B ix e where
  size = bSize
  {-# INLINE size #-}

  unsafeResize !sz !arr = arr { bSize = sz }
  {-# INLINE unsafeResize #-}

  unsafeExtract !sIx !newSz !arr = unsafeExtract sIx newSz (toManifest arr)
  {-# INLINE unsafeExtract #-}


instance ( NFData e
         , Index ix
         , Index (Lower ix)
         , Elt M ix e ~ Array M (Lower ix) e
         , Elt B ix e ~ Array M (Lower ix) e
         ) =>
         OuterSlice B ix e where
  unsafeOuterSlice arr = unsafeOuterSlice (toManifest arr)
  {-# INLINE unsafeOuterSlice #-}

instance ( NFData e
         , Index ix
         , Index (Lower ix)
         , Elt M ix e ~ Array M (Lower ix) e
         , Elt B ix e ~ Array M (Lower ix) e
         ) =>
         InnerSlice B ix e where
  unsafeInnerSlice arr = unsafeInnerSlice (toManifest arr)
  {-# INLINE unsafeInnerSlice #-}


instance Index ix => Manifest B ix e where

  unsafeLinearIndexM (BArray _ _ a) =
    INDEX_CHECK("(Manifest B ix e).unsafeLinearIndexM", A.sizeofArray, A.indexArray) a
  {-# INLINE unsafeLinearIndexM #-}


uninitialized :: a
uninitialized = error "Data.Array.Massiv.Manifest.BoxedStrict: uninitialized element"


instance Index ix => Mutable B ix e where
  data MArray s B ix e = MBArray !ix {-# UNPACK #-} !(A.MutableArray s e)

  msize (MBArray sz _) = sz
  {-# INLINE msize #-}

  unsafeThaw (BArray _ sz a) = MBArray sz <$> A.unsafeThawArray a
  {-# INLINE unsafeThaw #-}

  unsafeFreeze comp (MBArray sz ma) = BArray comp sz <$> A.unsafeFreezeArray ma
  {-# INLINE unsafeFreeze #-}

  unsafeNew sz = MBArray sz <$> A.newArray (totalElem sz) uninitialized
  {-# INLINE unsafeNew #-}

  unsafeNewZero = unsafeNew
  {-# INLINE unsafeNewZero #-}

  unsafeLinearRead (MBArray _ ma) =
    INDEX_CHECK("(Mutable B ix e).unsafeLinearRead", A.sizeofMutableArray, A.readArray) ma
  {-# INLINE unsafeLinearRead #-}

  unsafeLinearWrite (MBArray _ ma) i e = e `seq`
    INDEX_CHECK("(Mutable B ix e).unsafeLinearWrite", A.sizeofMutableArray, A.writeArray) ma i e
  {-# INLINE unsafeLinearWrite #-}


-- | Row-major sequential folding over a Boxed array.
instance Index ix => Foldable (Array B ix) where
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
  sum = F.foldl' (+) 0
  {-# INLINE sum #-}
  product = F.foldl' (*) 1
  {-# INLINE product #-}
  length = totalElem . size
  {-# INLINE length #-}
  toList arr = build (\ c n -> foldrFB c n arr)
  {-# INLINE toList #-}


instance ( IsList (Array L ix e)
         , Nested LN ix e
         , Nested L ix e
         , Ragged L ix e
         ) =>
         IsList (Array B ix e) where
  type Item (Array B ix e) = Item (Array L ix e)
  fromList = A.fromLists' Seq
  {-# INLINE fromList #-}
  toList = GHC.toList . toListArray
  {-# INLINE toList #-}
