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


class Num e => Numeric r e where

  {-# MINIMAL foldArray, unsafeLiftArray, unsafeLiftArray2 #-}

  -- | Compute sum of all elements in the array
  --
  -- @since 0.5.6
  sumArray :: Index ix => Array r ix e -> e
  sumArray = foldArray (+) 0
  {-# INLINE sumArray #-}

  -- | Compute product of all elements in the array
  --
  -- @since 0.5.6
  productArray :: Index ix => Array r ix e -> e
  productArray = foldArray (*) 1
  {-# INLINE productArray #-}

  -- | Raise each element in the array to some non-negative power and sum the results
  --
  -- @since 0.5.7
  powerSumArray :: Index ix => Array r ix e -> Int -> e
  powerSumArray arr p = sumArray $ unsafeLiftArray (^ p) arr
  {-# INLINE powerSumArray #-}

  -- | Compute dot product without any extraneous checks
  --
  -- @since 0.5.6
  unsafeDotProduct :: Index ix => Array r ix e -> Array r ix e -> e
  unsafeDotProduct v1 v2 = sumArray $ unsafeLiftArray2 (*) v1 v2
  {-# INLINE unsafeDotProduct #-}


  plusScalar :: Index ix => Array r ix e -> e -> Array r ix e
  plusScalar arr e = unsafeLiftArray (+ e) arr
  {-# INLINE plusScalar #-}

  minusScalar :: Index ix => Array r ix e -> e -> Array r ix e
  minusScalar arr e = unsafeLiftArray (subtract e) arr
  {-# INLINE minusScalar #-}

  scalarMinus :: Index ix => e -> Array r ix e -> Array r ix e
  scalarMinus e arr = unsafeLiftArray (e -) arr
  {-# INLINE scalarMinus #-}

  multiplyScalar :: Index ix => Array r ix e -> e -> Array r ix e
  multiplyScalar arr e = unsafeLiftArray (* e) arr
  {-# INLINE multiplyScalar #-}

  absPointwise :: Index ix => Array r ix e -> Array r ix e
  absPointwise = unsafeLiftArray abs
  {-# INLINE absPointwise #-}

  additionPointwise :: Index ix => Array r ix e -> Array r ix e -> Array r ix e
  additionPointwise = unsafeLiftArray2 (+)
  {-# INLINE additionPointwise #-}

  subtractionPointwise :: Index ix => Array r ix e -> Array r ix e -> Array r ix e
  subtractionPointwise = unsafeLiftArray2 (-)
  {-# INLINE subtractionPointwise #-}

  multiplicationPointwise :: Index ix => Array r ix e -> Array r ix e -> Array r ix e
  multiplicationPointwise = unsafeLiftArray2 (*)
  {-# INLINE multiplicationPointwise #-}

  -- TODO:
  --  - rename to powerScalar
  --  - add? powerPointwise :: Array r ix e -> Array r ix Int -> Array r ix e
  -- | Raise each element of the array to the power
  powerPointwise :: Index ix => Array r ix e -> Int -> Array r ix e
  powerPointwise arr pow = unsafeLiftArray (^ pow) arr
  {-# INLINE powerPointwise #-}

  -- | Fold over an array
  --
  -- @since 0.5.6
  foldArray :: Index ix => (e -> e -> e) -> e -> Array r ix e -> e

  unsafeLiftArray :: Index ix => (e -> e) -> Array r ix e -> Array r ix e

  unsafeLiftArray2 :: Index ix => (e -> e -> e) -> Array r ix e -> Array r ix e -> Array r ix e



class (Numeric r e, Floating e) => NumericFloat r e where

  divideScalar :: Index ix => Array r ix e -> e -> Array r ix e
  divideScalar arr e = unsafeLiftArray (/ e) arr
  {-# INLINE divideScalar #-}

  scalarDivide :: Index ix => e -> Array r ix e -> Array r ix e
  scalarDivide e arr = unsafeLiftArray (e /) arr
  {-# INLINE scalarDivide #-}

  divisionPointwise :: Index ix => Array r ix e -> Array r ix e -> Array r ix e
  divisionPointwise = unsafeLiftArray2 (/)
  {-# INLINE divisionPointwise #-}

  recipPointwise :: Index ix => Array r ix e -> Array r ix e
  recipPointwise = unsafeLiftArray recip
  {-# INLINE recipPointwise #-}

  sqrtPointwise :: Index ix => Array r ix e -> Array r ix e
  sqrtPointwise = unsafeLiftArray sqrt
  {-# INLINE sqrtPointwise #-}

  -- floorPointwise :: (Index ix, Integral a) => Array r ix e -> Array r ix a
  -- floorPointwise = unsafeLiftArray floor
  -- {-# INLINE floorPointwise #-}

  -- ceilingPointwise :: (Index ix, Integral a) => Array r ix e -> Array r ix a
  -- ceilingPointwise = unsafeLiftArray ceiling
  -- {-# INLINE ceilingPointwise #-}


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


