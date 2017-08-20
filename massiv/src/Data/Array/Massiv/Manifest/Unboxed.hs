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
  , Array(..)
  ) where

import           Control.DeepSeq                     (NFData (..), deepseq)
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Common.Shape
import           Data.Array.Massiv.Delayed
import           Data.Array.Massiv.Manifest.Internal
import           Data.Array.Massiv.Mutable
import qualified Data.Vector.Unboxed                 as VU

import qualified Data.Vector.Unboxed.Mutable         as MVU
import           Prelude                             hiding (mapM)

-- import           Control.Monad.ST                    (runST)
-- import           Data.Array.Massiv.Scheduler
-- import           System.IO.Unsafe                    (unsafePerformIO)

data U = U deriving Show

data instance Array U ix e = UArray { uComp :: Comp
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

  unsafeMakeArray Seq !sz f = UArray Seq sz $ VU.generate (totalElem sz) (f . fromLinearIndex sz)
  unsafeMakeArray (ParOn wIds) !sz f = unsafeGenerateArrayP wIds sz f
  {-# INLINE unsafeMakeArray #-}


instance (VU.Unbox e, Eq e, Index ix) => Eq (Array U ix e) where
  (==) = eq (==)
  {-# INLINE (==) #-}


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


instance (Index ix, VU.Unbox e) => Target U ix e


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

