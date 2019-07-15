{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Massiv.Core.Operations
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
module Data.Massiv.Core.Operations
  ( Numeric(..)
  , FloatingPoint(..)
  ) where

import Data.Massiv.Core.Common
import Data.Massiv.Array.Ops.Fold.Internal


class Num e => Numeric r e where

  sumArray :: proxy r -> Array (R r) Ix1 e -> e
  default sumArray :: Source (R r) Ix1 e => proxy r -> Array (R r) Ix1 e -> e
  sumArray _ = foldlS (+) 0
  {-# INLINE sumArray #-}

  productArray :: proxy r -> Array (R r) Ix1 e -> e
  default productArray :: Source (R r) Ix1 e => proxy r -> Array (R r) Ix1 e -> e
  productArray _ = foldlS (*) 1
  {-# INLINE productArray #-}

  -- | Raise each element in the array to some non-negative power and sum the results
  powerSumArray :: proxy r -> Array (R r) Ix1 e -> Int -> e

  dotProduct :: proxy r -> Array (R r) Ix1 e -> Array (R r) Ix1 e -> e

  plusElementArray :: Array r ix e -> e -> Array (R r) ix e

  minusElementArray :: Array r ix e -> e -> Array (R r) ix e

  multiplyElementArray :: Array r ix e -> e -> Array (R r) ix e


  absPointwise :: Array r ix e -> Array (R r) ix e

  additionPointwise :: Array r ix e -> Array r ix e -> Array (R r) ix e

  multiplicationPointwise :: Array r ix e -> Array r ix e -> Array (R r) ix e

  -- | Raise each element of the array to the power
  powerPointwise :: Array r ix e -> Int -> Array (R r) ix e

class FloatingPoint r e where

  divideElementArray :: Array r ix e -> e -> Array (R r) ix e

  divisionPointwise :: Array r ix e -> Array r ix e -> Array (R r) ix e

  recipPointwise :: Array r ix e -> Array (R r) ix e
  sqrtPointwise :: Array r ix e -> Array (R r) ix e

  floorPointwise :: Integral a => Array r ix e -> Array (R r) ix a
  ceilingPointwise :: Integral a => Array r ix e -> Array (R r) ix a

