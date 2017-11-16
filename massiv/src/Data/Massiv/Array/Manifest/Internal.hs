{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
-- |
-- Module      : Data.Massiv.Array.Manifest.Internal
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Manifest.Internal
  ( M
  , Manifest(..)
  , Array(..)
  , makeBoxedVector
  , toManifest
  ) where

import           Data.Foldable                      (Foldable (..))
import           Data.Massiv.Array.Delayed.Internal
import           Data.Massiv.Array.Ops.Fold         as M
import           Data.Massiv.Core
import           Data.Massiv.Core.Scheduler
import qualified Data.Vector                        as V
import           GHC.Base                           (build)


-- | General Manifest representation
data M

data instance Array M ix e = MArray { mComp :: !Comp
                                    , mSize :: !ix
                                    , mUnsafeLinearIndex :: Int -> e }
type instance EltRepr M ix = M

instance Index ix => Construct M ix e where
  getComp = mComp
  {-# INLINE getComp #-}

  setComp c arr = arr { mComp = c }
  {-# INLINE setComp #-}

  unsafeMakeArray !c !sz f = MArray c sz (V.unsafeIndex (makeBoxedVector sz f))
  {-# INLINE unsafeMakeArray #-}


makeBoxedVector :: Index ix => ix -> (ix -> a) -> V.Vector a
makeBoxedVector !sz f = V.generate (totalElem sz) (f . fromLinearIndex sz)
{-# INLINE makeBoxedVector #-}


-- | _O(1)_ Conversion of `Manifest` arrays to `M` representation.
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


instance Index ix => Size M ix e where
  size = mSize
  {-# INLINE size #-}

  unsafeResize !sz !arr = arr { mSize = sz }
  {-# INLINE unsafeResize #-}

  unsafeExtract !sIx !newSz !arr =
    MArray (getComp arr) newSz $ \ i ->
      unsafeIndex arr (liftIndex2 (+) (fromLinearIndex newSz i) sIx)
  {-# INLINE unsafeExtract #-}



instance {-# OVERLAPPING #-} Slice M Ix1 e where
  unsafeSlice arr i _ _ = Just (unsafeLinearIndex arr i)
  {-# INLINE unsafeSlice #-}

instance ( Index ix
         , Index (Lower ix)
         , Elt M ix e ~ Array M (Lower ix) e
         ) =>
         Slice M ix e where
  unsafeSlice arr start cutSz dim = do
    newSz <- dropDim cutSz dim
    return $ unsafeResize newSz (unsafeExtract start cutSz arr)
  {-# INLINE unsafeSlice #-}

instance {-# OVERLAPPING #-} OuterSlice M Ix1 e where
  unsafeOuterSlice !arr _ = unsafeIndex arr
  {-# INLINE unsafeOuterSlice #-}

instance (Elt M ix e ~ Array M (Lower ix) e, Index ix, Index (Lower ix)) => OuterSlice M ix e where
  unsafeOuterSlice !arr !(_, szL) !i =
    MArray (getComp arr) szL (unsafeLinearIndex arr . (+ kStart))
    where
      !kStart = toLinearIndex (size arr) (consDim i (zeroIndex :: Lower ix))
  {-# INLINE unsafeOuterSlice #-}

instance {-# OVERLAPPING #-} InnerSlice M Ix1 e where
  unsafeInnerSlice !arr _ = unsafeIndex arr
  {-# INLINE unsafeInnerSlice #-}

instance (Elt M ix e ~ Array M (Lower ix) e, Index ix, Index (Lower ix)) => InnerSlice M ix e where
  unsafeInnerSlice !arr !(szL, m) !i =
    MArray (getComp arr) szL (\k -> unsafeLinearIndex arr (k * m + kStart))
    where
      !kStart = toLinearIndex (size arr) (snocDim (zeroIndex :: Lower ix) i)
  {-# INLINE unsafeInnerSlice #-}


instance Index ix => Load M ix e where
  loadS (MArray _ sz f) _ unsafeWrite =
    iterM_ 0 (totalElem sz) 1 (<) $ \ !i ->
      unsafeWrite i (f i)
  {-# INLINE loadS #-}
  loadP wIds (MArray _ sz f) _ unsafeWrite = do
    divideWork_ wIds (totalElem sz) $ \ !scheduler !chunkLength !totalLength !slackStart -> do
      loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
        scheduleWork scheduler $
        iterM_ start (start + chunkLength) 1 (<) $ \ !i ->
          unsafeWrite i (f i)
      scheduleWork scheduler $
        iterM_ slackStart totalLength 1 (<) $ \ !i ->
          unsafeWrite i (f i)
  {-# INLINE loadP #-}
