{-# LANGUAGE BangPatterns #-}
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
module Data.Array.Massiv.Manifest.Unboxed where

import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Compute
import           Data.Array.Massiv.Manifest
import           Data.Maybe                  (listToMaybe)
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as MVU
import System.IO.Unsafe (unsafePerformIO)

data U = U

data instance Array U ix e = UArray { uSize :: !ix
                                    , uData :: !(VU.Vector e)
                                    } deriving Eq

instance Index ix => Massiv U ix where
  size = uSize
  {-# INLINE size #-}


instance (Index ix, VU.Unbox e) => Source U ix e where
  unsafeLinearIndex (UArray _ v) = VU.unsafeIndex v
  {-# INLINE unsafeLinearIndex #-}


instance VU.Unbox e => Manifest U DIM1 e where
  type Elt U DIM1 e = e
  (!?) (UArray _ v) = (v VU.!?)
  {-# INLINE (!?) #-}


instance VU.Unbox e => Manifest U DIM2 e where
  (!?) = maybeLowerIndex
  {-# INLINE (!?) #-}


instance VU.Unbox e => Manifest U DIM3 e where
  (!?) = maybeLowerIndex
  {-# INLINE (!?) #-}


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



fromListsUnboxed :: VU.Unbox e => [[e]] -> Array M DIM2 e
fromListsUnboxed !ls =
  if all (== n) (map length ls)
    then MArray (m, n) $ VU.unsafeIndex (VU.fromList $ concat ls)
    else error "fromListsVG:Inner lists are of different lengths."
  where -- TODO: check dims
    (m, n) = (length ls, maybe 0 length $ listToMaybe ls)
{-# INLINE fromListsUnboxed #-}


computeUnboxedS :: (Load r' ix, Mutable U ix e) => Array r' ix e -> Array U ix e
computeUnboxedS = computeS
{-# INLINE computeUnboxedS #-}


computeUnboxedP :: (Load r' ix, Mutable U ix e) => Array r' ix e -> IO (Array U ix e)
computeUnboxedP = computeP
{-# INLINE computeUnboxedP #-}


unsafeComputeUnboxedP :: (Load r' ix, Mutable U ix e) => Array r' ix e -> Array U ix e
unsafeComputeUnboxedP = unsafePerformIO . computeP
{-# INLINE unsafeComputeUnboxedP #-}


toVectorUnboxed :: Array U ix e -> VU.Vector e
toVectorUnboxed (UArray _ v) = v
{-# INLINE toVectorUnboxed #-}
