{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Data.Array.Massiv.Manifest.Boxed
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Manifest.BoxedStrict
  ( B (..)
  , Array(..)
  ) where

import           Control.DeepSeq                     (NFData (..))
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Common.Shape
import           Data.Array.Massiv.Delayed (eq)
import           Data.Array.Massiv.Manifest.BoxedNF  (deepseqArray,
                                                      deepseqArrayP)
import           Data.Array.Massiv.Manifest.Internal
import           Data.Array.Massiv.Mutable
import           Data.Array.Massiv.Ops.Fold
import           Data.Foldable                       (Foldable (..))
import qualified Data.Primitive.Array                as A
import           GHC.Base                            (build)
import           Prelude                             hiding (mapM)

-- | Array representation for Boxed elements. This structure is element and
-- spine strict, but elements are strict to Weak Head Normal Form (WHNF) only.
data B = B deriving Show

data instance Array B ix e = BArray { bComp :: !Comp
                                    , bSize :: !ix
                                    , bData :: {-# UNPACK #-} !(A.Array e)
                                    }

instance (Index ix, NFData e) => NFData (Array B ix e) where
  rnf (BArray comp sz arr) = -- comp `deepseq` sz `deepseq` a `seq` ()
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


instance Index ix => Shape B ix e where
  type R B = M

  unsafeReshape !sz !arr = arr { bSize = sz }
  {-# INLINE unsafeReshape #-}

  unsafeExtract !sIx !newSz !arr = unsafeExtract sIx newSz (toManifest arr)
  {-# INLINE unsafeExtract #-}


instance (Index ix, Index (Lower ix)) => Slice B ix e where

  (!?>) !arr = (toManifest arr !?>)
  {-# INLINE (!?>) #-}

  (<!?) !arr = (toManifest arr <!?)
  {-# INLINE (<!?) #-}


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
  sum = foldl' (+) 0
  {-# INLINE sum #-}
  product = foldl' (*) 1
  {-# INLINE product #-}
  length = totalElem . size
  {-# INLINE length #-}
  toList arr = build (\ c n -> foldrFB c n arr)
  {-# INLINE toList #-}

