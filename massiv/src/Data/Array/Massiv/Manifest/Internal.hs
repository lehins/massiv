{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
-- |
-- Module      : Data.Array.Massiv.Manifest.Internal
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Manifest.Internal
  ( M
  , Manifest(..)
  , Array(..)
  -- , (!!>)
  , makeBoxedVector
  , toManifest
  ) where

import           Control.Monad                  (void)
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Common.Shape
import           Data.Array.Massiv.Delayed
import           Data.Array.Massiv.Ops.Fold     as M
import           Data.Array.Massiv.Scheduler
import           Data.Foldable                  (Foldable (..))
import qualified Data.Vector                    as V
import           GHC.Base                       (build)

-- | Manifest arrays are backed by actual memory and values are looked up versus
-- computed as it is with delayed arrays. Because of this fact indexing functions
-- `(!)`, `(!?)`, etc. are constrained to manifest arrays only.
class Source r ix e => Manifest r ix e where

  unsafeLinearIndexM :: Array r ix e -> Int -> e


-- | Manifest representation
data M

data instance Array M ix e = MArray { mComp :: Comp
                                    , mSize :: !ix
                                    , mUnsafeLinearIndex :: Int -> e }


instance Index ix => Construct M ix e where
  size = mSize
  {-# INLINE size #-}

  getComp = mComp
  {-# INLINE getComp #-}

  setComp c arr = arr { mComp = c }
  {-# INLINE setComp #-}

  unsafeMakeArray c !sz f = MArray c sz (V.unsafeIndex (makeBoxedVector sz f))
  {-# INLINE unsafeMakeArray #-}


makeBoxedVector :: Index ix => ix -> (ix -> a) -> V.Vector a
makeBoxedVector !sz f = V.generate (totalElem sz) (f . fromLinearIndex sz)
{-# INLINE makeBoxedVector #-}


-- | _O(1)_ Conversion of manifest arrays to `M` representation.
toManifest :: Manifest r ix e => Array r ix e -> Array M ix e
toManifest !arr = MArray (getComp arr) (size arr) (unsafeLinearIndexM arr) where
{-# INLINE toManifest #-}


-- | Row-major folding over a Manifest array.
instance Index ix => Foldable (Array M ix) where
  foldl = lazyFoldlS
  {-# INLINE foldl #-}
  foldl' = foldlS
  {-# INLINE foldl' #-}
  foldr = foldrFB
  {-# INLINE foldr #-}
  foldr' = foldrS
  {-# INLINE foldr' #-}
  null (MArray _ sz _) = totalElem sz == 0
  {-# INLINE null #-}
  sum = foldl' (+) 0
  {-# INLINE sum #-}
  product = foldl' (*) 1
  {-# INLINE product #-}
  length = totalElem . size
  {-# INLINE length #-}
  toList arr = build (\ c n -> foldrFB c n arr)
  {-# INLINE toList #-}



instance Index ix => Source M ix e where
  unsafeLinearIndex = mUnsafeLinearIndex
  {-# INLINE unsafeLinearIndex #-}


instance Index ix => Manifest M ix e where

  unsafeLinearIndexM = mUnsafeLinearIndex
  {-# INLINE unsafeLinearIndexM #-}


instance Index ix => Shape M ix e where

  unsafeReshape !sz !arr = arr { mSize = sz }
  {-# INLINE unsafeReshape #-}

  unsafeExtract !sIx !newSz !arr =
    MArray (getComp arr) newSz $ \ i ->
      unsafeIndex arr (liftIndex2 (+) (fromLinearIndex newSz i) sIx)
  {-# INLINE unsafeExtract #-}


instance (Index ix, Index (Lower ix)) =>
         Slice M ix e where
  (!?>) !arr !i
    | isSafeIndex m i =
      Just (MArray (getComp arr) szL (unsafeLinearIndex arr . (+ kStart)))
    | otherwise = Nothing
    where
      !sz = size arr
      !(m, szL) = unconsDim sz
      !kStart = toLinearIndex sz (consDim i (zeroIndex :: Lower ix))
  {-# INLINE (!?>) #-}
  (<!?) !arr !i
    | isSafeIndex m i =
      Just
        (MArray (getComp arr) szL (\k -> unsafeLinearIndex arr (k * m + kStart)))
    | otherwise = Nothing
    where
      !sz = size arr
      !(szL, m) = unsnocDim sz
      !kStart = toLinearIndex sz (snocDim (zeroIndex :: Lower ix) i)
  {-# INLINE (<!?) #-}


instance Index ix => Load M ix e where
  loadS (MArray _ sz f) _ unsafeWrite =
    iterM_ 0 (totalElem sz) 1 (<) $ \ !i ->
      unsafeWrite i (f i)
  {-# INLINE loadS #-}
  loadP wIds (MArray _ sz f) _ unsafeWrite = do
    void $ splitWork wIds (totalElem sz) $ \ !scheduler !chunkLength !totalLength !slackStart -> do
      loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
        submitRequest scheduler $
        JobRequest $
        iterM_ start (start + chunkLength) 1 (<) $ \ !i ->
          unsafeWrite i (f i)
      submitRequest scheduler $
        JobRequest $
        iterM_ slackStart totalLength 1 (<) $ \ !i ->
          unsafeWrite i (f i)
  {-# INLINE loadP #-}
