{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Data.Array.Massiv.Manifest.Storable
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Manifest.Storable
  ( S (..)
  , Array(..)
  , VS.Storable
  -- , generateM
  -- , fromVectorStorable
  -- , toVectorStorable
  -- , computeStorableS
  -- , computeStorableP
  -- , mapM
  -- , imapM
  ) where

import           Control.DeepSeq                     (NFData (..), deepseq)
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Common.Shape
import           Data.Array.Massiv.Manifest.Internal
import           Data.Array.Massiv.Mutable
import qualified Data.Vector.Storable                as VS
import qualified Data.Vector.Storable.Mutable        as MVS
import           Prelude                             hiding (mapM)
-- import           System.IO.Unsafe                    (unsafePerformIO)

data S = S

data instance Array S ix e = SArray { sComp :: Comp
                                    , sSize :: !ix
                                    , sData :: !(VS.Vector e)
                                    } deriving Eq


instance (Index ix, NFData e) => NFData (Array S ix e) where
  rnf (SArray c sz v) = c `deepseq` sz `deepseq` v `deepseq` ()


instance (VS.Storable e, Index ix) => Massiv S ix e where
  size = sSize
  {-# INLINE size #-}

  getComp = sComp
  {-# INLINE getComp #-}

  setComp c arr = arr { sComp = c }
  {-# INLINE setComp #-}

  unsafeMakeArray c !sz f = SArray c sz $ VS.generate (totalElem sz) (f . fromLinearIndex sz)
  {-# INLINE unsafeMakeArray #-}


instance (VS.Storable e, Index ix) => Source S ix e where
  unsafeLinearIndex (SArray _ _ v) = VS.unsafeIndex v
  {-# INLINE unsafeLinearIndex #-}


instance (VS.Storable e, Index ix) => Shape S ix e where
  type R S = M

  unsafeReshape !sz !arr = arr { sSize = sz }
  {-# INLINE unsafeReshape #-}

  unsafeExtract !sIx !newSz !arr = unsafeExtract sIx newSz (toManifest arr)
  {-# INLINE unsafeExtract #-}


instance (VS.Storable e, Index ix, Index (Lower ix)) => Slice S ix e where

  (!?>) !arr = (toManifest arr !?>)
  {-# INLINE (!?>) #-}

  (<!?) !arr = (toManifest arr <!?)
  {-# INLINE (<!?) #-}


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


instance (Index ix, VS.Storable e) => Target S ix e


-- computeStorableS :: (Load r ix e, Target S ix e) => Array r ix e -> Array S ix e
-- computeStorableS = loadTargetS
-- {-# INLINE computeStorableS #-}


-- computeStorableP :: (Load r ix e, Target S ix e) => Array r ix e -> Array S ix e
-- computeStorableP = unsafePerformIO . loadTargetOnP []
-- {-# INLINE computeStorableP #-}


-- fromVectorStorable :: Index ix => ix -> VS.Vector e -> Array S ix e
-- fromVectorStorable sz v = SArray { sSize = sz, sData = v }
-- {-# INLINE fromVectorStorable #-}


-- toVectorStorable :: Array S ix e -> VS.Vector e
-- toVectorStorable = sData
-- {-# INLINE toVectorStorable #-}


-- generateM :: (Index ix, VS.Storable a, Monad m) =>
--   ix -> (ix -> m a) -> m (Array S ix a)
-- generateM sz f =
--   SArray sz <$> VS.generateM (totalElem sz) (f . fromLinearIndex sz)
-- {-# INLINE generateM #-}


-- mapM :: (VS.Storable b, Source r ix a, Monad m) =>
--   (a -> m b) -> Array r ix a -> m (Array S ix b)
-- mapM f arr = do
--   let !sz = size arr
--   v <- VS.generateM (totalElem sz) (f . unsafeLinearIndex arr)
--   return $ SArray sz v
-- {-# INLINE mapM #-}

-- imapM :: (VS.Storable b, Source r ix a, Monad m) =>
--   (ix -> a -> m b) -> Array r ix a -> m (Array S ix b)
-- imapM f arr = do
--   let !sz = size arr
--   v <- VS.generateM (totalElem sz) $ \ !i ->
--          let !ix = fromLinearIndex sz i
--          in f ix (unsafeIndex arr ix)
--   return $ SArray sz v
-- {-# INLINE imapM #-}

