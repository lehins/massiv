{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Massiv.Array.Manifest.Storable
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Manifest.Storable
  ( S (..)
  , Array(..)
  , VS.Storable
  ) where

import           Control.DeepSeq                     (NFData (..), deepseq)
import           Data.Massiv.Array.Delayed.Internal  (eq)
import           Data.Massiv.Array.Manifest.Internal
import           Data.Massiv.Array.Mutable
import           Data.Massiv.Core
import           Data.Massiv.Core.List
import qualified Data.Vector.Storable                as VS
import qualified Data.Vector.Storable.Mutable        as MVS
import           GHC.Exts                            (IsList (..))
import           Prelude                             hiding (mapM)

data S = S deriving Show

type instance EltRepr S ix = M

data instance Array S ix e = SArray { sComp :: !Comp
                                    , sSize :: !ix
                                    , sData :: !(VS.Vector e)
                                    }

instance (Index ix, NFData e) => NFData (Array S ix e) where
  rnf (SArray c sz v) = c `deepseq` sz `deepseq` v `deepseq` ()

instance (VS.Storable e, Eq e, Index ix) => Eq (Array S ix e) where
  (==) = eq (==)
  {-# INLINE (==) #-}

instance (VS.Storable e, Index ix) => Construct S ix e where
  size = sSize
  {-# INLINE size #-}

  getComp = sComp
  {-# INLINE getComp #-}

  setComp c arr = arr { sComp = c }
  {-# INLINE setComp #-}

  unsafeMakeArray Seq          !sz f = unsafeGenerateArray sz f
  unsafeMakeArray (ParOn wIds) !sz f = unsafeGenerateArrayP wIds sz f
  {-# INLINE unsafeMakeArray #-}


instance (VS.Storable e, Index ix) => Source S ix e where
  unsafeLinearIndex (SArray _ _ v) = VS.unsafeIndex v
  {-# INLINE unsafeLinearIndex #-}


instance (VS.Storable e, Index ix) => Size S ix e where
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

  unsafeLinearIndexM (SArray _ _ v) = VS.unsafeIndex v
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

  unsafeLinearRead (MSArray _ v) i = MVS.unsafeRead v i
  {-# INLINE unsafeLinearRead #-}

  unsafeLinearWrite (MSArray _ v) i = MVS.unsafeWrite v i
  {-# INLINE unsafeLinearWrite #-}


instance (VS.Storable e, IsList (Array L ix e), Load L ix e, Construct L ix e) =>
         IsList (Array S ix e) where
  type Item (Array S ix e) = Item (Array L ix e)
  fromList xs = compute (fromList xs :: Array L ix e)
  {-# INLINE fromList #-}
  toList = toList . toListArray
  {-# INLINE toList #-}
