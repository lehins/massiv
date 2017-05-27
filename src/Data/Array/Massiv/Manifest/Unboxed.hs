{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
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
  -- , generateM
  , fromVectorUnboxed
  , toVectorUnboxed
  , computeUnboxedS
  -- , computeUnboxedP
  -- , mapM
  -- , imapM
  -- , fromUnboxedArray
  ) where

import           Control.DeepSeq                     (NFData (..), deepseq)
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Common.Shape
import           Data.Array.Massiv.Delayed           (D)
import           Data.Array.Massiv.Manifest.Internal
import           Data.Array.Massiv.Mutable
import qualified Data.Vector.Unboxed                 as VU
import qualified Data.Vector.Unboxed.Mutable         as MVU
import           Prelude                             hiding (mapM)
-- import           System.IO.Unsafe                    (unsafePerformIO)

data U = U

data instance Array U ix e = UArray { uComp :: !Comp
                                    , uSize :: !ix
                                    , uData :: !(VU.Vector e)
                                    } deriving Eq


instance (Index ix, NFData e) => NFData (Array U ix e) where
  rnf (UArray c sz v) = c `deepseq` sz `deepseq` v `deepseq` ()


instance (VU.Unbox e, Index ix) => Massiv U ix e where
  size = uSize
  {-# INLINE size #-}

  getComp = uComp
  {-# INLINE getComp #-}

  setComp arr c = arr { uComp = c }
  {-# INLINE setComp #-}

  unsafeMakeArray !c !sz f = compute (unsafeMakeArray c sz f :: Array D ix e)
  {-# INLINE unsafeMakeArray #-}


instance (VU.Unbox e, Index ix) => Source U ix e where
  unsafeLinearIndex (UArray _ _ v) = VU.unsafeIndex v
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

instance (VU.Unbox e, Index ix) => Manifest U ix e where

  unsafeLinearIndexM (UArray _ _ v) = VU.unsafeIndex v
  {-# INLINE unsafeLinearIndexM #-}

instance (VU.Unbox e, Index ix) => Mutable U ix e where
  data MArray s U ix e = MUArray !Comp !ix !(VU.MVector s e)

  msize (MUArray _ sz _) = sz
  {-# INLINE msize #-}

  unsafeThaw (UArray c sz v) = MUArray c sz <$> VU.unsafeThaw v
  {-# INLINE unsafeThaw #-}

  unsafeFreeze (MUArray c sz v) = UArray c sz <$> VU.unsafeFreeze v
  {-# INLINE unsafeFreeze #-}

  unsafeNew !c !sz = MUArray c sz <$> MVU.unsafeNew (totalElem sz)
  {-# INLINE unsafeNew #-}

  unsafeLinearRead (MUArray _ _ v) i = MVU.unsafeRead v i
  {-# INLINE unsafeLinearRead #-}

  unsafeLinearWrite (MUArray _ _ v) i = MVU.unsafeWrite v i
  {-# INLINE unsafeLinearWrite #-}


instance (Index ix, VU.Unbox e) => Target U ix e


computeUnboxedS :: (Load r ix e, Target U ix e) => Array r ix e -> Array U ix e
computeUnboxedS = loadTargetS
{-# INLINE computeUnboxedS #-}


-- computeUnboxedP :: (Load r ix e, Target U ix e) => Array r ix e -> Array U ix e
-- computeUnboxedP = unsafePerformIO . loadTargetOnP []
-- {-# INLINE computeUnboxedP #-}


fromVectorUnboxed :: Index ix => ix -> VU.Vector e -> Array U ix e
fromVectorUnboxed sz v = UArray { uComp = Seq, uSize = sz, uData = v }
{-# INLINE fromVectorUnboxed #-}


toVectorUnboxed :: Array U ix e -> VU.Vector e
toVectorUnboxed = uData
{-# INLINE toVectorUnboxed #-}


-- fromUnboxedArray :: VU.Unbox e => Array U ix e -> Array M ix e
-- fromUnboxedArray (UArray sz v) = MArray sz (VU.unsafeIndex v)
-- {-# INLINE fromUnboxedArray #-}


-- generateM :: (Index ix, VU.Unbox a, Monad m) =>
--   ix -> (ix -> m a) -> m (Array U ix a)
-- generateM sz f =
--   UArray sz <$> VU.generateM (totalElem sz) (f . fromLinearIndex sz)
-- {-# INLINE generateM #-}


-- mapM :: (VU.Unbox b, Source r ix a, Monad m) =>
--   (a -> m b) -> Array r ix a -> m (Array U ix b)
-- mapM f arr = do
--   let !sz = size arr
--   v <- VU.generateM (totalElem sz) (f . unsafeLinearIndex arr)
--   return $ UArray sz v
-- {-# INLINE mapM #-}

-- imapM :: (VU.Unbox b, Source r ix a, Monad m) =>
--   (ix -> a -> m b) -> Array r ix a -> m (Array U ix b)
-- imapM f arr = do
--   let !sz = size arr
--   v <- VU.generateM (totalElem sz) $ \ !i ->
--          let !ix = fromLinearIndex sz i
--          in f ix (unsafeIndex arr ix)
--   return $ UArray sz v
-- {-# INLINE imapM #-}

