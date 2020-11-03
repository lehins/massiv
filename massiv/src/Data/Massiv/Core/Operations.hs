{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Massiv.Core.Operations
-- Copyright   : (c) Alexey Kuleshevich 2019-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
module Data.Massiv.Core.Operations
  ( FoldNumeric(..)
  , defaultPowerSumArray
  , defaultUnsafeDotProduct
  , defaultFoldArray
  , Numeric(..)
  , defaultUnsafeLiftArray
  , defaultUnsafeLiftArray2
  , NumericFloat(..)
  ) where

import Data.Massiv.Core.Common



class (Size r, Num e) => FoldNumeric r e where

  {-# MINIMAL foldArray, powerSumArray, unsafeDotProduct #-}

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

  -- | Compute dot product without any extraneous checks
  --
  -- @since 0.5.6
  unsafeDotProduct :: Index ix => Array r ix e -> Array r ix e -> e

  -- | Fold over an array
  --
  -- @since 0.5.6
  foldArray :: Index ix => (e -> e -> e) -> e -> Array r ix e -> e


defaultUnsafeDotProduct ::
     (Num e, Index ix, Source r e) => Array r ix e -> Array r ix e -> e
defaultUnsafeDotProduct a1 a2 = go 0 0
  where
    !len = totalElem (size a1)
    go !acc i
      | i < len = go (acc + unsafeLinearIndex a1 i * unsafeLinearIndex a2 i) (i + 1)
      | otherwise = acc
{-# INLINE defaultUnsafeDotProduct #-}

defaultPowerSumArray :: (Index ix, Source r e, Num e) => Array r ix e -> Int -> e
defaultPowerSumArray arr p = go 0 0
  where
    !len = totalElem (size arr)
    go !acc i
      | i < len = go (acc + unsafeLinearIndex arr i ^ p) (i + 1)
      | otherwise = acc
{-# INLINE defaultPowerSumArray #-}

defaultFoldArray :: (Index ix, Source r e) => (e -> e -> e) -> e -> Array r ix e -> e
defaultFoldArray f !initAcc arr = go initAcc 0
  where
    !len = totalElem (size arr)
    go !acc i
      | i < len = go (f acc (unsafeLinearIndex arr i)) (i + 1)
      | otherwise = acc
{-# INLINE defaultFoldArray #-}

class FoldNumeric r e => Numeric r e where

  {-# MINIMAL unsafeLiftArray, unsafeLiftArray2 #-}

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

  unsafeLiftArray :: Index ix => (e -> e) -> Array r ix e -> Array r ix e

  unsafeLiftArray2 :: Index ix => (e -> e -> e) -> Array r ix e -> Array r ix e -> Array r ix e


defaultUnsafeLiftArray ::
     (Construct r ix e, Source r e) => (e -> e) -> Array r ix e -> Array r ix e
defaultUnsafeLiftArray f arr = makeArrayLinear (getComp arr) (size arr) (f . unsafeLinearIndex arr)
{-# INLINE defaultUnsafeLiftArray #-}


defaultUnsafeLiftArray2 ::
     (Construct r ix e, Source r e)
  => (e -> e -> e)
  -> Array r ix e
  -> Array r ix e
  -> Array r ix e
defaultUnsafeLiftArray2 f a1 a2 =
  makeArrayLinear
    (getComp a1 <> getComp a2)
    (SafeSz (liftIndex2 min (unSz (size a1)) (unSz (size a2)))) $ \ !i ->
    f (unsafeLinearIndex a1 i) (unsafeLinearIndex a2 i)
{-# INLINE defaultUnsafeLiftArray2 #-}


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


