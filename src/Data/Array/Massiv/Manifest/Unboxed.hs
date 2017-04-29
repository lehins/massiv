{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Array.Massiv.Manifest.Unboxed
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Manifest.Unboxed
  ( U (..)
  , VU.Unbox
  , generateM
  , fromVectorUnboxed
  , toVectorUnboxed
  , computeUnboxedS
  , computeUnboxedP
  , mapM
  , imapM
  ) where

import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Common.Shape
import           Data.Array.Massiv.Manifest.Internal
import           Data.Array.Massiv.Mutable
import qualified Data.Vector.Unboxed                 as VU
import qualified Data.Vector.Unboxed.Mutable         as MVU
import           Prelude                             hiding (mapM)
import           System.IO.Unsafe                    (unsafePerformIO)

data U = U

data instance Array U ix e = UArray { uSize :: !ix
                                    , uData :: !(VU.Vector e)
                                    } deriving Eq

instance (VU.Unbox e, Index ix) => Massiv U ix e where
  size = uSize
  {-# INLINE size #-}

  makeArray !sz f = UArray sz' $ VU.generate (totalElem sz') (f . fromLinearIndex sz')
    where
      !sz' = liftIndex (max 0) sz
  {-# INLINE makeArray #-}

instance (VU.Unbox e, Index ix) => Source U ix e where
  unsafeLinearIndex (UArray _ v) = VU.unsafeIndex v
  {-# INLINE unsafeLinearIndex #-}


instance (VU.Unbox e, Index ix) => Shape U ix e where
  type R U = M

  unsafeReshape !sz !arr = arr { uSize = sz }
  {-# INLINE unsafeReshape #-}

  unsafeExtract !sIx !newSz !arr = unsafeExtract sIx newSz (toManifest arr)
  {-# INLINE unsafeExtract #-}


instance (VU.Unbox e, Index ix, Index (Lower ix)) => Slice U ix e where

  (!?>) !arr = (toManifest arr !?>)
  {-# INLINE (!?>) #-}

  (<!?) !arr = (toManifest arr <!?)
  {-# INLINE (<!?) #-}

instance (Index ix, VU.Unbox e) => Manifest U ix e


instance (Manifest U ix e, VU.Unbox e) => Mutable U ix e where
  data MArray s U ix e = MUArray ix (VU.MVector s e)

  unsafeThaw (UArray sz v) = MUArray sz <$> VU.unsafeThaw v
  {-# INLINE unsafeThaw #-}

  unsafeFreeze (MUArray sz v) = UArray sz <$> VU.unsafeFreeze v
  {-# INLINE unsafeFreeze #-}

  unsafeNew sz = MUArray sz <$> MVU.unsafeNew (totalElem sz)
  {-# INLINE unsafeNew #-}

  unsafeLinearRead (MUArray _sz v) i = MVU.unsafeRead v i
  {-# INLINE unsafeLinearRead #-}

  unsafeLinearWrite (MUArray _sz v) i = MVU.unsafeWrite v i
  {-# INLINE unsafeLinearWrite #-}


computeUnboxedS :: (Massiv r ix e, Load r ix, Mutable U ix e) => Array r ix e -> Array U ix e
computeUnboxedS = computeSeq
{-# INLINE computeUnboxedS #-}


computeUnboxedP :: (Massiv r ix e, Load r ix, Mutable U ix e) => Array r ix e -> Array U ix e
computeUnboxedP = unsafePerformIO . computePar
{-# INLINE computeUnboxedP #-}


fromVectorUnboxed :: Index ix => ix -> VU.Vector e -> Array U ix e
fromVectorUnboxed sz v = UArray { uSize = sz, uData = v }
{-# INLINE fromVectorUnboxed #-}


toVectorUnboxed :: Array U ix e -> VU.Vector e
toVectorUnboxed = uData
{-# INLINE toVectorUnboxed #-}


generateM :: (Index ix, VU.Unbox a, Monad m) =>
  ix -> (ix -> m a) -> m (Array U ix a)
generateM sz f =
  UArray sz <$> VU.generateM (totalElem sz) (f . fromLinearIndex sz)
{-# INLINE generateM #-}


mapM :: (VU.Unbox b, Source r ix a, Monad m) =>
  (a -> m b) -> Array r ix a -> m (Array U ix b)
mapM f arr = do
  let !sz = size arr
  v <- VU.generateM (totalElem sz) (f . unsafeLinearIndex arr)
  return $ UArray sz v
{-# INLINE mapM #-}

imapM :: (VU.Unbox b, Source r ix a, Monad m) =>
  (ix -> a -> m b) -> Array r ix a -> m (Array U ix b)
imapM f arr = do
  let !sz = size arr
  v <- VU.generateM (totalElem sz) $ \ !i ->
         let !ix = fromLinearIndex sz i
         in f ix (unsafeIndex arr ix)
  return $ UArray sz v
{-# INLINE imapM #-}

