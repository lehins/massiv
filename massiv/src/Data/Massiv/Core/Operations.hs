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
  , NumericFloat(..)
  ) where

import Data.Massiv.Core.Common
import Data.Massiv.Array.Ops.Fold.Internal


class (R r ~ r, Num e) => Numeric r e where

  sumArray :: Array r Ix1 e -> e
  default sumArray :: Source r Ix1 e => Array r Ix1 e -> e
  sumArray = foldlS (+) 0
  {-# INLINE sumArray #-}

  productArray :: Array r Ix1 e -> e
  default productArray :: Source r Ix1 e => Array r Ix1 e -> e
  productArray = foldlS (*) 1
  {-# INLINE productArray #-}

  -- | Raise each element in the array to some non-negative power and sum the results
  powerSumArray :: Array r Ix1 e -> Int -> e

  unsafeDotProduct :: Array r Ix1 e -> Array r Ix1 e -> e

  plusScalar :: Index ix => Array r ix e -> e -> Array r ix e

  minusScalar :: Index ix => Array r ix e -> e -> Array r ix e

  multiplyScalar :: Index ix => Array r ix e -> e -> Array r ix e


  absPointwise :: Index ix => Array r ix e -> Array r ix e

  additionPointwise :: Index ix => Array r ix e -> Array r ix e -> Array r ix e

  subtractionPointwise :: Index ix => Array r ix e -> Array r ix e -> Array r ix e

  multiplicationPointwise :: Index ix => Array r ix e -> Array r ix e -> Array r ix e

  -- | Raise each element of the array to the power
  powerPointwise :: Index ix => Array r ix e -> Int -> Array r ix e


class NumericFloat r e where

  divideScalar :: Index ix => Array r ix e -> e -> Array r ix e

  divisionPointwise :: Index ix => Array r ix e -> Array r ix e -> Array r ix e

  recipPointwise :: Index ix => Array r ix e -> Array r ix e
  sqrtPointwise :: Index ix => Array r ix e -> Array r ix e

  floorPointwise :: (Index ix, Integral a) => Array r ix e -> Array r ix a
  ceilingPointwise :: (Index ix, Integral a) => Array r ix e -> Array r ix a


-- class Equality r e where

--   unsafeEq :: Index ix => Array r ix e -> Array r ix e -> Bool

--   unsafeEqPointwise :: Index ix => Array r ix e -> Array r ix e -> Array r ix Bool


-- class Relation r e where

--   unsafePointwiseLT :: Array r ix e -> Array r ix e -> Array r ix Bool
--   unsafePointwiseLTE :: Array r ix e -> Array r ix e -> Array r ix Bool

--   unsafePointwiseGT :: Array r ix e -> Array r ix e -> Array r ix Bool
--   unsafePointwiseGTE :: Array r ix e -> Array r ix e -> Array r ix Bool

--   unsafePointwiseMin :: Array r ix e -> Array r ix e -> Array r ix e
--   unsafePointwiseMax :: Array r ix e -> Array r ix e -> Array r ix e

--   unsafeMinimum :: Array r ix e -> e

--   unsafeMaximum :: Array r ix e -> e


