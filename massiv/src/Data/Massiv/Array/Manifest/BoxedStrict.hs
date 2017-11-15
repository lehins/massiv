{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Massiv.Array.Manifest.Boxed
-- Copyright   : (c) Alexey Kuleshevich 2017
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
import           Data.Massiv.Array.Delayed.Internal  (eq)
import           Data.Massiv.Array.Manifest.BoxedNF  (deepseqArray,
                                                      deepseqArrayP)
import           Data.Massiv.Array.Manifest.Internal
import           Data.Massiv.Array.Mutable
import           Data.Massiv.Array.Ops.Fold
import           Data.Massiv.Core.List
import           Data.Massiv.Core
import qualified Data.Primitive.Array                as A
import           GHC.Base                            (build)
import           GHC.Exts                            (IsList (..))
import           Prelude                             hiding (mapM)


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

instance Index ix => Construct B ix e where
  size = bSize
  {-# INLINE size #-}

  getComp = bComp
  {-# INLINE getComp #-}

  setComp c arr = arr { bComp = c }
  {-# INLINE setComp #-}

  unsafeMakeArray Seq          !sz f = unsafeGenerateArray sz f
  unsafeMakeArray (ParOn wIds) !sz f = unsafeGenerateArrayP wIds sz f
  {-# INLINE unsafeMakeArray #-}

instance Index ix => Source B ix e where
  unsafeLinearIndex (BArray _ _ a) = A.indexArray a
  {-# INLINE unsafeLinearIndex #-}


instance Index ix => Size B ix e where
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

  unsafeLinearIndexM (BArray _ _ a) = A.indexArray a
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

  unsafeLinearRead (MBArray _ ma) i = A.readArray ma i
  {-# INLINE unsafeLinearRead #-}

  unsafeLinearWrite (MBArray _ ma) i e = e `seq` A.writeArray ma i e
  {-# INLINE unsafeLinearWrite #-}


-- | Row-major folding over a Boxed array.
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


instance (IsList (Array L ix e), Load L ix e, Construct L ix e) =>
         IsList (Array B ix e) where
  type Item (Array B ix e) = Item (Array L ix e)
  fromList xs = compute (fromList xs :: Array L ix e)
  {-# INLINE fromList #-}
  toList = toList . toListArray
  {-# INLINE toList #-}
