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
-- import Data.Massiv.Array.Ops.Fold.Internal


class Num e => Numeric r e where

  {-# MINIMAL unsafeLiftArray, unsafeLiftArray2 #-}

  -- sumArray :: Array r Ix1 e -> e
  -- default sumArray :: Source r Ix1 e => Array r Ix1 e -> e
  -- sumArray = foldlS (+) 0
  -- {-# INLINE sumArray #-}

  -- productArray :: Array r Ix1 e -> e
  -- default productArray :: Source r Ix1 e => Array r Ix1 e -> e
  -- productArray = foldlS (*) 1
  -- {-# INLINE productArray #-}

  -- -- | Raise each element in the array to some non-negative power and sum the results
  -- powerSumArray :: Array r Ix1 e -> Int -> e

  -- unsafeDotProduct :: Array r Ix1 e -> Array r Ix1 e -> e

  plusScalar :: Index ix => Array r ix e -> e -> Array r ix e
  plusScalar arr e = unsafeLiftArray (+ e) arr
  {-# INLINE plusScalar #-}

  minusScalar :: Index ix => Array r ix e -> e -> Array r ix e
  minusScalar arr e = unsafeLiftArray (subtract e) arr
  {-# INLINE minusScalar #-}

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

  -- | Raise each element of the array to the power
  powerPointwise :: Index ix => Array r ix e -> Int -> Array r ix e
  powerPointwise arr pow = unsafeLiftArray (^ pow) arr
  {-# INLINE powerPointwise #-}


  unsafeLiftArray :: Index ix => (a -> e) -> Array r ix a -> Array r ix e

  unsafeLiftArray2 :: Index ix => (a -> b -> e) -> Array r ix a -> Array r ix b -> Array r ix e



class (Numeric r e, Floating e) => NumericFloat r e where

  divideScalar :: Index ix => Array r ix e -> e -> Array r ix e
  divideScalar arr e = unsafeLiftArray (/ e) arr
  {-# INLINE divideScalar #-}

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


