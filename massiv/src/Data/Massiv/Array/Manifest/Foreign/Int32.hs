{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Massiv.Array.Manifest.Foreign.Word8
-- Copyright   : (c) Alexey Kuleshevich 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Manifest.Foreign.Word8 where

import           Control.DeepSeq                             (NFData (..),
                                                              deepseq)
import           Control.Monad.Primitive
import           Data.Int
import           Data.Massiv.Array.Delayed.Internal          (eq, ord)
import           Data.Massiv.Array.Manifest.Foreign.Internal
import           Data.Massiv.Array.Manifest.Internal
import           Data.Massiv.Array.Manifest.List             as A
import           Data.Massiv.Array.Mutable
import           Data.Massiv.Array.Unsafe                    (unsafeGenerateArray,
                                                              unsafeGenerateArrayP)
import           Data.Massiv.Core.Common
import           Data.Massiv.Core.List
import qualified Data.Vector.Storable                        as VS
import qualified Data.Vector.Storable.Mutable                as MVS
import           Foreign.ForeignPtr
import           Foreign.Storable
import           GHC.Exts                                    as GHC (IsList (..))
import           Prelude                                     hiding (mapM)
import           System.IO.Unsafe

-- instance (Index ix, NFData e) => NFData (Array S ix e) where
--   rnf (SArray c sz v) = c `deepseq` sz `deepseq` v `deepseq` ()

-- instance (VS.Storable e, Eq e, Index ix) => Eq (Array S ix e) where
--   (==) = eq (==)
--   {-# INLINE (==) #-}

-- instance (VS.Storable e, Ord e, Index ix) => Ord (Array S ix e) where
--   compare = ord compare
--   {-# INLINE compare #-}

instance (Index ix, Mutable F ix e) => Construct F ix e where
  getComp = fComp
  {-# INLINE getComp #-}

  setComp c arr = arr { fComp = c }
  {-# INLINE setComp #-}

  unsafeMakeArray Seq          !sz f = unsafeGenerateArray sz f
  unsafeMakeArray (ParOn wIds) !sz f = unsafeGenerateArrayP wIds sz f
  {-# INLINE unsafeMakeArray #-}


instance Index ix => Source F ix Int32 where
  unsafeLinearIndex (FArray _ _ fp) i = unsafePerformIO $ withForeignPtr fp (`peekElemOff` i)
  {-# INLINE unsafeLinearIndex #-}


instance Index ix => Size F ix Int32 where
  size = fSize
  {-# INLINE size #-}

  unsafeResize !sz !arr = arr { fSize = sz }
  {-# INLINE unsafeResize #-}

  unsafeExtract !sIx !newSz !arr = unsafeExtract sIx newSz (toManifest arr)
  {-# INLINE unsafeExtract #-}



-- instance ( VS.Storable e
--          , Index ix
--          , Index (Lower ix)
--          , Elt M ix e ~ Array M (Lower ix) e
--          , Elt S ix e ~ Array M (Lower ix) e
--          ) =>
--          OuterSlice S ix e where
--   unsafeOuterSlice arr = unsafeOuterSlice (toManifest arr)
--   {-# INLINE unsafeOuterSlice #-}

-- instance ( VS.Storable e
--          , Index ix
--          , Index (Lower ix)
--          , Elt M ix e ~ Array M (Lower ix) e
--          , Elt S ix e ~ Array M (Lower ix) e
--          ) =>
--          InnerSlice S ix e where
--   unsafeInnerSlice arr = unsafeInnerSlice (toManifest arr)
--   {-# INLINE unsafeInnerSlice #-}


instance Index ix => Manifest F ix Int32 where
  unsafeLinearIndexM (FArray _ _ fp) i =
    unsafePerformIO $ withForeignPtr fp (`peekElemOff` i)
  {-# INLINE unsafeLinearIndexM #-}


instance Index ix => Mutable F ix Int32 where
  data MArray s F ix Int32 = MFArrayInt32 !ix !(ForeignPtr Int32)

  msize (MFArrayInt32 sz _) = sz
  {-# INLINE msize #-}

  unsafeThaw (FArray _ sz fp) = return $ MFArrayInt32 sz fp
  {-# INLINE unsafeThaw #-}

  unsafeFreeze comp (MFArrayInt32 sz fp) = return $ FArray comp sz fp
  {-# INLINE unsafeFreeze #-}

  unsafeNew sz = unsafePrimToPrim $ do
    fp <- mallocForeignPtrArray (totalElem sz)
    return $ MFArrayInt32 sz fp
  {-# INLINE unsafeNew #-}

  unsafeNewZero = unsafeNew
  {-# INLINE unsafeNewZero #-}

  unsafeLinearRead (MFArrayInt32 _ fp) i =
    unsafePrimToPrim $ withForeignPtr fp (`peekElemOff` i)
  {-# INLINE unsafeLinearRead #-}

  unsafeLinearWrite (MFArrayInt32 _ fp) i e =
    unsafePrimToPrim $ withForeignPtr fp (\ptr -> pokeElemOff ptr i e)
  {-# INLINE unsafeLinearWrite #-}


-- plusArrayInt32 (MFArrayInt32 _ fp1) (MFArrayInt32 _ fp2) = do


-- instance ( VS.Storable e
--          , IsList (Array L ix e)
--          , Nested LN ix e
--          , Nested L ix e
--          , Ragged L ix e
--          ) =>
--          IsList (Array S ix e) where
--   type Item (Array S ix e) = Item (Array L ix e)
--   fromList = A.fromLists' Seq
--   {-# INLINE fromList #-}
--   toList = GHC.toList . toListArray
--   {-# INLINE toList #-}
