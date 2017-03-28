{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Data.Array.Massiv.Manifest.Unboxed
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Manifest.Unboxed where

import qualified Data.Vector.Generic             as VG
import qualified Data.Vector.Unboxed             as VU
import Data.Maybe (listToMaybe)
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Manifest
import           Data.Array.Massiv.Manifest.Loading
import           Control.DeepSeq                 (NFData, deepseq)



unboxed :: V VU.Vector
unboxed = V


computeUnboxedS, computeUnboxedP :: (VU.Unbox e, Load r ix) => Array r ix e -> Array M ix e
computeUnboxedS !arr =
  vector `deepseq` MArray (size arr) (VU.unsafeIndex vector)
  where
    !vector = computeVG Sequential unboxed arr
{-# INLINE computeUnboxedS #-}
computeUnboxedP !arr =
  vector `deepseq` MArray (size arr) (VU.unsafeIndex vector)
  where
    !vector = computeVG Parallel unboxed arr
{-# INLINE computeUnboxedP #-}

computeUnboxedIO :: (Load r ix, VU.Unbox e)
                  => Array r ix e -> IO (Array M ix e)
computeUnboxedIO !arr = do
  vector <- loadVectorParallelIO unboxed arr
  return (vector `deepseq` MArray (size arr) (VU.unsafeIndex vector))
{-# INLINE computeUnboxedIO #-}


toVectorUnboxed :: (Source r ix, VU.Unbox e) => Array r ix e -> VU.Vector e
toVectorUnboxed !arr = VU.generate (totalElem (size arr)) (unsafeLinearIndex arr)
{-# INLINE toVectorUnboxed #-}

toVectorUnboxedS :: (Load r ix, VU.Unbox e) => Array r ix e -> VU.Vector e
toVectorUnboxedS = computeVG Sequential unboxed
{-# INLINE toVectorUnboxedS #-}

toVectorUnboxedP :: (Load r ix, VU.Unbox e) => Array r ix e -> VU.Vector e
toVectorUnboxedP = computeVG Parallel unboxed
{-# INLINE toVectorUnboxedP #-}

fromListsUnboxed :: VU.Unbox e => [[e]] -> Array M DIM2 e
fromListsUnboxed !ls =
  if all (== n) (map length ls)
    then MArray (m, n) $ VU.unsafeIndex (VU.fromList $ concat ls)
    else error "fromListsVG:Inner lists are of different lengths."
  where -- TODO: check dims
    (m, n) = (length ls, maybe 0 length $ listToMaybe ls)
{-# INLINE fromListsUnboxed #-}


imapMaybe
  :: (Iterator RowMajor ix, Source r ix, VG.Vector VU.Vector b)
  => (ix -> e -> Maybe b) -> Array r ix e -> Array M DIM1 b
imapMaybe = imapMaybeS unboxed
{-# INLINE imapMaybe #-}


mapMaybe
  :: (Foldable (Array r ix), Source r ix, VG.Vector VU.Vector b)
  => (e -> Maybe b) -> Array r ix e -> Array M DIM1 b
mapMaybe = mapMaybeS unboxed
{-# INLINE mapMaybe #-}



ifilter
  :: (Iterator RowMajor ix, Source r ix, VG.Vector VU.Vector e)
  => (ix -> e -> Bool) -> Array r ix e -> Array M DIM1 e
ifilter f = imapMaybeS unboxed (\ !i !v -> if f i v then Just v else Nothing)
{-# INLINE ifilter #-}


ifilterIx
  :: (Iterator RowMajor ix, Source r ix, VU.Unbox e, VU.Unbox ix)
  => (ix -> e -> Bool) -> Array r ix e -> (Array M DIM1 ix, Array M DIM1 e)
ifilterIx f !arr =
  (MArray vLen (VU.unsafeIndex vectorIx), MArray vLen (VU.unsafeIndex vectorVal))
  where
    !vLen = VU.length vectorIx
    !(vectorIx, vectorVal) = imapMaybeIxS (\ !i !v -> if f i v then Just v else Nothing) arr
{-# INLINE ifilterIx #-}


filterIx
  :: (Iterator RowMajor ix, Source r ix, VU.Unbox e, VU.Unbox ix)
  => (e -> Bool) -> Array r ix e -> (Array M DIM1 ix, Array M DIM1 e)
filterIx f = ifilterIx $ \ _ v -> f v
{-# INLINE filterIx #-}



filter
  :: (Foldable (Array r ix), Source r ix, VG.Vector VU.Vector e)
  => (e -> Bool) -> Array r ix e -> Array M DIM1 e
filter f = mapMaybeS unboxed (\ !v -> if f v then Just v else Nothing)
{-# INLINE filter #-}
