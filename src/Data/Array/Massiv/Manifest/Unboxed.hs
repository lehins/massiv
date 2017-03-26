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



unboxed :: V VU.Vector
unboxed = V


computeUnboxedS, computeUnboxedP
  :: (Load r ix, VG.Vector VU.Vector e)
  => Array r ix e -> Array M ix e

computeUnboxedS = compute Sequential unboxed
{-# INLINE computeUnboxedS #-}

computeUnboxedP = compute Parallel unboxed
{-# INLINE computeUnboxedP #-}

computeUnboxedPIO
  :: (Load r ix, VG.Vector VU.Vector e)
  => Array r ix e -> IO (Array M ix e)
computeUnboxedPIO = computeP unboxed
{-# INLINE computeUnboxedPIO #-}


fromListsUnboxed :: VU.Unbox e => [[e]] -> Array M DIM2 e
fromListsUnboxed !ls =
  if all (== n) (map length ls)
    then MArray (m, n) $ VU.unsafeIndex (VU.fromList $ concat ls)
    else error "fromListsVG:Inner lists are of different lengths."
  where -- TODO: check dims
    (m, n) = (length ls, maybe 0 length $ listToMaybe ls)
{-# INLINE fromListsUnboxed #-}
