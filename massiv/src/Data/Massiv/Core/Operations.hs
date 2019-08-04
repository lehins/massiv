{-# LANGUAGE CPP #-}
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
  ( NumArray(..)
  , FloatArray(..)
  , ReduceNumArray(..)
  , ReduceOrdArray(..)
  , RoundFloatArray(..)
  , roundDouble
  , roundFloat
  ) where

#include "MachDeps.h"

import Data.Massiv.Core.Common
import Data.Massiv.Array.Ops.Fold.Internal
--import GHC.Float.RealFracMethods


-- | Pointwise operations on arrays with numers
--
-- @since 0.4.1
class ReduceNumArray r e => NumArray r e where

  {-# MINIMAL liftNumArray, unsafeLiftNumArray2 #-}

  -- | Add a scalar froto each element in the array. Respect computation strategy.
  --
  -- @since 0.4.0
  plusScalar :: Index ix => Array r ix e -> e -> Array r ix e
  plusScalar arr e = liftNumArray (+ e) arr
  {-# INLINE plusScalar #-}

  -- | Subtract a scalar from each element in the array. Respect computation strategy.
  --
  -- @since 0.4.0
  minusScalar :: Index ix => Array r ix e -> e -> Array r ix e
  minusScalar arr e = liftNumArray (subtract e) arr
  {-# INLINE minusScalar #-}

  -- | Multiply each element in the array. Respect computation strategy.
  --
  -- @since 0.4.0
  multiplyScalar :: Index ix => Array r ix e -> e -> Array r ix e
  multiplyScalar arr e = liftNumArray (* e) arr
  {-# INLINE multiplyScalar #-}

  -- | Raise each element of the array to some positive power. Respect computation strategy.
  --
  -- @since 0.4.0
  powerScalar :: Index ix => Array r ix e -> Int -> Array r ix e
  powerScalar arr pow = liftNumArray (^ pow) arr
  {-# INLINE powerScalar #-}

  -- | Compute absolute value of each element in the array. Respect computation strategy.
  --
  -- @since 0.4.0
  absPointwise :: Index ix => Array r ix e -> Array r ix e
  absPointwise = liftNumArray abs
  {-# INLINE absPointwise #-}

  -- | Subtract two arrays pointwise.  Assume both arrays are of the same size. Respect
  -- computation strategy.
  --
  -- @since 0.4.0
  additionPointwise :: Index ix => Array r ix e -> Array r ix e -> Array r ix e
  additionPointwise = unsafeLiftNumArray2 (+)
  {-# INLINE additionPointwise #-}

  -- | Subtract one array from another pointwise. Assume both arrays are of the same size.
  -- Respect computation strategy.
  --
  -- @since 0.4.0
  subtractionPointwise :: Index ix => Array r ix e -> Array r ix e -> Array r ix e
  subtractionPointwise = unsafeLiftNumArray2 (-)
  {-# INLINE subtractionPointwise #-}

  -- | Multiply two arrays pointwise. Assume both arrays are of the same size.  Respect
  -- computation strategy.
  --
  -- @since 0.4.0
  multiplicationPointwise :: Index ix => Array r ix e -> Array r ix e -> Array r ix e
  multiplicationPointwise = unsafeLiftNumArray2 (*)
  {-# INLINE multiplicationPointwise #-}

  -- | Apply a function to each element of the array without changing the element
  -- type. Respect computation strategy.
  --
  -- @since 0.4.1
  liftNumArray :: Index ix => (e -> e) -> Array r ix e -> Array r ix e

  -- | Apply a binary function to elements of two arrays pointwise. Type of arrays and
  -- elements does not change. Assumes both arrays are of the same size, therefore it is
  -- unsafe. Respect computation strategy.
  --
  -- @since 0.4.0
  unsafeLiftNumArray2 :: Index ix => (e -> e -> e) -> Array r ix e -> Array r ix e -> Array r ix e

-- | Recucing operations on arrays with numbers
--
-- @since 0.4.1
class Num e => ReduceNumArray r e where

  -- | Compute the sum of all elements in the array sequentially.
  --
  -- @since 0.4.1
  sumArrayS :: Index ix => Array r ix e -> e
  default sumArrayS :: Source r ix e => Array r ix e -> e
  sumArrayS = foldlS (+) 0
  {-# INLINE sumArrayS #-}

  -- | Compute the product of all elements in the array sequentially.
  --
  -- @since 0.4.1
  productArrayS :: Index ix => Array r ix e -> e
  default productArrayS :: Source r ix e => Array r ix e -> e
  productArrayS = foldlS (*) 1
  {-# INLINE productArrayS #-}

  -- | Sequentially multiply two arrays pointwise and then sum the results (dot product)
  --
  -- @since 0.4.1
  multiplySumArrayS :: Index ix => Array r ix e -> Array r ix e -> e

  -- | Raise each element in the array to an even positive power and sum the results
  --
  -- > prop $ \arr pow -> even pow ==> evenPowerSumArrayS arr pow === absPowerSumArrayS arr pow
  --
  -- @since 0.4.1
  evenPowerSumArrayS :: Index ix => Array r ix e -> Int -> e

  -- | Raise absolute value of each element in the array to some positive power and sum
  -- the results
  --
  -- @since 0.4.1
  absPowerSumArrayS :: Index ix => Array r ix e -> Int -> e

  -- | Get the maximum aboslute valuein the array.
  --
  -- @since 0.4.1
  absMaxArrayS :: (Ord e, Index ix) => Array r ix e -> e

class Ord e => ReduceOrdArray r e where

  maximumArrayS :: Index ix => e -> Array r ix e -> e
  default maximumArrayS :: Source r ix e => e -> Array r ix e -> e
  maximumArrayS = foldlS max
  {-# INLINE maximumArrayS #-}

  minimumArrayS :: Index ix => e -> Array r ix e -> e
  default minimumArrayS :: Source r ix e => e -> Array r ix e -> e
  minimumArrayS = foldlS min
  {-# INLINE minimumArrayS #-}

-- | Operations on arrays with floating point numers
--
-- @since 0.4.1
class (NumArray r e, Floating e) => FloatArray r e where
  divideScalar :: Index ix => Array r ix e -> e -> Array r ix e
  divideScalar arr e = liftNumArray (/ e) arr
  {-# INLINE divideScalar #-}

  recipMultiplyScalar :: Index ix => Array r ix e -> e -> Array r ix e
  recipMultiplyScalar arr e = liftNumArray (e /) arr
  {-# INLINE recipMultiplyScalar #-}

  divisionPointwise :: Index ix => Array r ix e -> Array r ix e -> Array r ix e
  divisionPointwise = unsafeLiftNumArray2 (/)
  {-# INLINE divisionPointwise #-}

  recipPointwise :: Index ix => Array r ix e -> Array r ix e
  recipPointwise = liftNumArray recip
  {-# INLINE recipPointwise #-}

  sqrtPointwise :: Index ix => Array r ix e -> Array r ix e
  sqrtPointwise = liftNumArray sqrt
  {-# INLINE sqrtPointwise #-}


class RoundFloatArray r e a where

  -- truncatePointwise :: Index ix => Array r ix e -> Array r ix a

  roundPointwise :: Index ix => Array r ix e -> Array r ix a

  -- ceilingPointwise :: Index ix => Array r ix e -> Array r ix a

  -- floorPointwise :: Index ix => Array r ix e -> Array r ix a


foreign import ccall unsafe "rintDouble" roundDouble :: Double -> Double

foreign import ccall unsafe "rintFloat" roundFloat :: Float -> Float


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


