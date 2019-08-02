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
  , ReduceNumeric(..)
  , NumericFloat(..)
  ) where

import Data.Massiv.Core.Common
import Data.Massiv.Array.Ops.Fold.Internal


class Num e => ReduceNumeric r e where

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

class Num e => Numeric r e where

  {-# MINIMAL unsafeLiftArray, unsafeLiftArray2 #-}

  -- | Add a scalar froto each element in the array. Respect computation strategy.
  --
  -- @since 0.4.0
  plusScalar :: Index ix => Array r ix e -> e -> Array r ix e
  plusScalar arr e = unsafeLiftArray (+ e) arr
  {-# INLINE plusScalar #-}

  -- | Subtract a scalar from each element in the array. Respect computation strategy.
  --
  -- @since 0.4.0
  minusScalar :: Index ix => Array r ix e -> e -> Array r ix e
  minusScalar arr e = unsafeLiftArray (subtract e) arr
  {-# INLINE minusScalar #-}

  -- | Multiply each element in the array. Respect computation strategy.
  --
  -- @since 0.4.0
  multiplyScalar :: Index ix => Array r ix e -> e -> Array r ix e
  multiplyScalar arr e = unsafeLiftArray (* e) arr
  {-# INLINE multiplyScalar #-}

  -- | Compute absolute value of each element in the array. Respect computation strategy.
  --
  -- @since 0.4.0
  absPointwise :: Index ix => Array r ix e -> Array r ix e
  absPointwise = unsafeLiftArray abs
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
  powerPointwise arr pow = unsafeLiftArray (^ pow) arr
  {-# INLINE powerPointwise #-}


  -- | Apply a function to each element of the array. Respect computation strategy.
  --
  -- @since 0.4.0
  unsafeLiftArray :: Index ix => (e -> e) -> Array r ix e -> Array r ix e

  -- | Apply a binary function to elements of two arrays pointwise. Assume both arrays are
  -- of the same size. Respect computation strategy.
  --
  -- @since 0.4.0
  unsafeLiftArray2 :: Index ix => (e -> e -> e) -> Array r ix e -> Array r ix e -> Array r ix e



class (Numeric r e, Floating e) => NumericFloat r e where
  divideScalar :: Index ix => Array r ix e -> e -> Array r ix e
  divideScalar arr e = unsafeLiftArray (/ e) arr
  {-# INLINE divideScalar #-}

  recipMultiplyScalar :: Index ix => Array r ix e -> e -> Array r ix e
  recipMultiplyScalar arr e = unsafeLiftArray (e /) arr
  {-# INLINE recipMultiplyScalar #-}

  divisionPointwise :: Index ix => Array r ix e -> Array r ix e -> Array r ix e
  divisionPointwise = unsafeLiftArray2 (/)
  {-# INLINE divisionPointwise #-}

  recipPointwise :: Index ix => Array r ix e -> Array r ix e
  recipPointwise = unsafeLiftArray recip
  {-# INLINE recipPointwise #-}

  sqrtPointwise :: Index ix => Array r ix e -> Array r ix e
  sqrtPointwise = unsafeLiftArray sqrt
  {-# INLINE sqrtPointwise #-}

class NumericRound r e a where

  truncatePointwise :: Index ix => Array r ix e -> Array r ix a

  roundPointwise :: Index ix => Array r ix e -> Array r ix a
  -- roundPointwise = unsafeLiftArray round
  -- {-# INLINE roundPointwise #-}

  ceilingPointwise :: Index ix => Array r ix e -> Array r ix a
  -- ceilingPointwise = unsafeLiftArray ceiling
  -- {-# INLINE ceilingPointwise #-}

  floorPointwise :: (RealFrac e, Index ix) => Array r ix e -> Array r ix a
  -- floorPointwise = unsafeLiftArray floor
  -- {-# INLINE floorPointwise #-}


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


