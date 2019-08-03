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
class Num e => NumArray r e where

  {-# MINIMAL liftArray, unsafeLiftArray2 #-}

  -- | Add a scalar froto each element in the array. Respect computation strategy.
  --
  -- @since 0.4.0
  plusScalar :: Index ix => Array r ix e -> e -> Array r ix e
  plusScalar arr e = liftArray (+ e) arr
  {-# INLINE plusScalar #-}

  -- | Subtract a scalar from each element in the array. Respect computation strategy.
  --
  -- @since 0.4.0
  minusScalar :: Index ix => Array r ix e -> e -> Array r ix e
  minusScalar arr e = liftArray (subtract e) arr
  {-# INLINE minusScalar #-}

  -- | Multiply each element in the array. Respect computation strategy.
  --
  -- @since 0.4.0
  multiplyScalar :: Index ix => Array r ix e -> e -> Array r ix e
  multiplyScalar arr e = liftArray (* e) arr
  {-# INLINE multiplyScalar #-}

  -- | Compute absolute value of each element in the array. Respect computation strategy.
  --
  -- @since 0.4.0
  absPointwise :: Index ix => Array r ix e -> Array r ix e
  absPointwise = liftArray abs
  {-# INLINE absPointwise #-}

  -- | Subtract two arrays pointwise.  Assume both arrays are of the same size. Respect
  -- computation strategy.
  --
  -- @since 0.4.0
  additionPointwise :: Index ix => Array r ix e -> Array r ix e -> Array r ix e
  additionPointwise = unsafeLiftArray2 (+)
  {-# INLINE additionPointwise #-}

  -- | Subtract one array from another pointwise. Assume both arrays are of the same size.
  -- Respect computation strategy.
  --
  -- @since 0.4.0
  subtractionPointwise :: Index ix => Array r ix e -> Array r ix e -> Array r ix e
  subtractionPointwise = unsafeLiftArray2 (-)
  {-# INLINE subtractionPointwise #-}

  -- | Multiply two arrays pointwise. Assume both arrays are of the same size.  Respect
  -- computation strategy.
  --
  -- @since 0.4.0
  multiplicationPointwise :: Index ix => Array r ix e -> Array r ix e -> Array r ix e
  multiplicationPointwise = unsafeLiftArray2 (*)
  {-# INLINE multiplicationPointwise #-}

  -- | Raise each element of the array to the power. Respect computation strategy.
  --
  -- @since 0.4.0
  powerPointwise :: Index ix => Array r ix e -> Int -> Array r ix e
  powerPointwise arr pow = liftArray (^ pow) arr
  {-# INLINE powerPointwise #-}


  -- | Apply a function to each element of the array without changing the element
  -- type. Respect computation strategy.
  --
  -- @since 0.4.0
  liftArray :: Index ix => (e -> e) -> Array r ix e -> Array r ix e

  -- | Apply a binary function to elements of two arrays pointwise. Type of arrays and
  -- elements does not change. Assumes both arrays are of the same size, therefore it is
  -- unsafe. Respect computation strategy.
  --
  -- @since 0.4.0
  unsafeLiftArray2 :: Index ix => (e -> e -> e) -> Array r ix e -> Array r ix e -> Array r ix e

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


-- | Operations on arrays with floating point numers
--
-- @since 0.4.1
class (NumArray r e, Floating e) => FloatArray r e where
  divideScalar :: Index ix => Array r ix e -> e -> Array r ix e
  divideScalar arr e = liftArray (/ e) arr
  {-# INLINE divideScalar #-}

  recipMultiplyScalar :: Index ix => Array r ix e -> e -> Array r ix e
  recipMultiplyScalar arr e = liftArray (e /) arr
  {-# INLINE recipMultiplyScalar #-}

  divisionPointwise :: Index ix => Array r ix e -> Array r ix e -> Array r ix e
  divisionPointwise = unsafeLiftArray2 (/)
  {-# INLINE divisionPointwise #-}

  recipPointwise :: Index ix => Array r ix e -> Array r ix e
  recipPointwise = liftArray recip
  {-# INLINE recipPointwise #-}

  sqrtPointwise :: Index ix => Array r ix e -> Array r ix e
  sqrtPointwise = liftArray sqrt
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


