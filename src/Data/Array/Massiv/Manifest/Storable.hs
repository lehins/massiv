{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
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
  , VS.Storable
  , generateM
  , fromVectorStorable
  , toVectorStorable
  , computeStorableS
  , computeStorableP
  , mapM
  , imapM
  ) where

import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Manifest.Internal
import           Data.Array.Massiv.Mutable
import qualified Data.Vector.Storable                as VS
import qualified Data.Vector.Storable.Mutable        as MVS
import           Prelude                             hiding (mapM)
import           System.IO.Unsafe                    (unsafePerformIO)

data S = S

data instance Array S ix e = SArray { sSize :: !ix
                                    , sData :: !(VS.Vector e)
                                    } deriving Eq

instance (VS.Storable e, Index ix) => Massiv S ix e where
  size = sSize
  {-# INLINE size #-}

  makeArray !sz f = SArray sz' $ VS.generate (totalElem sz') (f . fromLinearIndex sz')
    where
      !sz' = liftIndex (max 0) sz
  {-# INLINE makeArray #-}


instance (Index ix, VS.Storable e) => Source S ix e where
  unsafeLinearIndex (SArray _ v) = VS.unsafeIndex v
  {-# INLINE unsafeLinearIndex #-}


instance (Index ix, VS.Storable e) => Manifest S ix e


instance (Manifest S ix e, VS.Storable e) => Mutable S ix e where
  data MArray s S ix e = MSArray ix (VS.MVector s e)

  unsafeThaw (SArray sz v) = MSArray sz <$> VS.unsafeThaw v
  {-# INLINE unsafeThaw #-}

  unsafeFreeze (MSArray sz v) = SArray sz <$> VS.unsafeFreeze v
  {-# INLINE unsafeFreeze #-}

  unsafeNew sz = MSArray sz <$> MVS.unsafeNew (totalElem sz)
  {-# INLINE unsafeNew #-}

  unsafeLinearRead (MSArray _sz v) i = MVS.unsafeRead v i
  {-# INLINE unsafeLinearRead #-}

  unsafeLinearWrite (MSArray _sz v) i = MVS.unsafeWrite v i
  {-# INLINE unsafeLinearWrite #-}



computeStorableS :: (Massiv r ix e, Load r ix, Mutable S ix e) => Array r ix e -> Array S ix e
computeStorableS = computeSeq
{-# INLINE computeStorableS #-}


computeStorableP :: (Massiv r ix e, Load r ix, Mutable S ix e) => Array r ix e -> Array S ix e
computeStorableP = unsafePerformIO . computePar
{-# INLINE computeStorableP #-}


fromVectorStorable :: Index ix => ix -> VS.Vector e -> Array S ix e
fromVectorStorable sz v = SArray { sSize = sz, sData = v }
{-# INLINE fromVectorStorable #-}


toVectorStorable :: Array S ix e -> VS.Vector e
toVectorStorable = sData
{-# INLINE toVectorStorable #-}


generateM :: (Index ix, VS.Storable a, Monad m) =>
  ix -> (ix -> m a) -> m (Array S ix a)
generateM sz f =
  SArray sz <$> VS.generateM (totalElem sz) (f . fromLinearIndex sz)
{-# INLINE generateM #-}


mapM :: (VS.Storable b, Source r ix a, Monad m) =>
  (a -> m b) -> Array r ix a -> m (Array S ix b)
mapM f arr = do
  let !sz = size arr
  v <- VS.generateM (totalElem sz) (f . unsafeLinearIndex arr)
  return $ SArray sz v
{-# INLINE mapM #-}

imapM :: (VS.Storable b, Source r ix a, Monad m) =>
  (ix -> a -> m b) -> Array r ix a -> m (Array S ix b)
imapM f arr = do
  let !sz = size arr
  v <- VS.generateM (totalElem sz) $ \ !i ->
         let !ix = fromLinearIndex sz i
         in f ix (unsafeIndex arr ix)
  return $ SArray sz v
{-# INLINE imapM #-}

