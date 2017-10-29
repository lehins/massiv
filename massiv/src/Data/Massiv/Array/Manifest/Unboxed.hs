{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Massiv.Array.Manifest.Unboxed
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Manifest.Unboxed
  ( U (..)
  , VU.Unbox
  , Array(..)
  ) where

import           Control.DeepSeq                     (NFData (..), deepseq)
import           Data.Massiv.Array.Delayed.Internal  (eq)
import           Data.Massiv.Array.Manifest.Internal (M, toManifest)
import           Data.Massiv.Array.Mutable
import           Data.Massiv.Core
import           Data.Massiv.Core.List
import qualified Data.Vector.Unboxed                 as VU
import qualified Data.Vector.Unboxed.Mutable         as MVU
import           GHC.Exts                            (IsList (..))
import           Prelude                             hiding (mapM)

data U = U deriving Show

type instance EltRepr U ix = M

data instance Array U ix e = UArray { uComp :: !Comp
                                    , uSize :: !ix
                                    , uData :: !(VU.Vector e)
                                    }


instance (Index ix, NFData e) => NFData (Array U ix e) where
  rnf (UArray c sz v) = c `deepseq` sz `deepseq` v `deepseq` ()


instance (VU.Unbox e, Index ix) => Construct U ix e where
  size = uSize
  {-# INLINE size #-}

  getComp = uComp
  {-# INLINE getComp #-}

  setComp c arr = arr { uComp = c }
  {-# INLINE setComp #-}

  unsafeMakeArray Seq          !sz f = unsafeGenerateArray sz f
  unsafeMakeArray (ParOn wIds) !sz f = unsafeGenerateArrayP wIds sz f
  {-# INLINE unsafeMakeArray #-}


instance (VU.Unbox e, Eq e, Index ix) => Eq (Array U ix e) where
  (==) = eq (==)
  {-# INLINE (==) #-}


instance (VU.Unbox e, Index ix) => Source U ix e where
  unsafeLinearIndex (UArray _ _ v) = VU.unsafeIndex v
  {-# INLINE unsafeLinearIndex #-}


instance (VU.Unbox e, Index ix) => Shape U ix e where
  unsafeReshape !sz !arr = arr { uSize = sz }
  {-# INLINE unsafeReshape #-}

  unsafeExtract !sIx !newSz !arr = unsafeExtract sIx newSz (toManifest arr)
  {-# INLINE unsafeExtract #-}


instance {-# OVERLAPPING #-} VU.Unbox e => Slice U Ix1 e where
  unsafeSlice arr i _ _ = Just (unsafeLinearIndex arr i)
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
  unsafeOuterSlice arr _ = unsafeLinearIndex arr
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

  unsafeLinearIndexM (UArray _ _ v) = VU.unsafeIndex v
  {-# INLINE unsafeLinearIndexM #-}

instance (VU.Unbox e, Index ix) => Mutable U ix e where
  data MArray s U ix e = MUArray ix (VU.MVector s e)

  msize (MUArray sz _) = sz
  {-# INLINE msize #-}

  unsafeThaw (UArray _ sz v) = MUArray sz <$> VU.unsafeThaw v
  {-# INLINE unsafeThaw #-}

  unsafeFreeze comp (MUArray sz v) = UArray comp sz <$> VU.unsafeFreeze v
  {-# INLINE unsafeFreeze #-}

  unsafeNew sz = MUArray sz <$> MVU.unsafeNew (totalElem sz)
  {-# INLINE unsafeNew #-}

  unsafeLinearRead (MUArray _ v) i = MVU.unsafeRead v i
  {-# INLINE unsafeLinearRead #-}

  unsafeLinearWrite (MUArray _ v) i = MVU.unsafeWrite v i
  {-# INLINE unsafeLinearWrite #-}


instance (VU.Unbox e, IsList (Array L ix e), Load L ix e, Construct L ix e) =>
         IsList (Array U ix e) where
  type Item (Array U ix e) = Item (Array L ix e)
  fromList xs = compute (fromList xs :: Array L ix e)
  {-# INLINE fromList #-}
  toList = toList . toListArray
  {-# INLINE toList #-}
