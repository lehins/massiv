{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Massiv.Array.Numeric
  ( -- * Spec for safe Mutable instance
    prop_MatrixMatrixMultiply
  , mutableNumericSpec
  ) where

import Data.Massiv.Array as A
import Test.Massiv.Utils as T


--naiveMatrixMatrixMultiply :: Array P Ix2 Double -> Array P Ix2 Double -> Array P Ix2 Double
naiveMatrixMatrixMultiply ::
     (Num e, Source (R r1) Ix1 e, Source (R r2) Ix1 e, OuterSlice r1 Ix2 e, InnerSlice r2 Ix2 e)
  => Array r1 Ix2 e
  -> Array r2 Ix2 e
  -> Array D Ix2 e
naiveMatrixMatrixMultiply arr1 arr2
  | n1 /= m2 =
    error $
    "(|*|): Inner array dimensions must agree, but received: " ++
    show (size arr1) ++ " and " ++ show (size arr2)
  | otherwise =
    makeArrayR D Seq (Sz (m1 :. n2)) $ \(i :. j) ->
      A.foldlS (+) 0 (A.zipWith (*) (arr1 !> i) (arr2 <! j))
  where
    Sz2 m1 n1 = size arr1
    Sz2 m2 n2 = size arr2
{-# INLINE naiveMatrixMatrixMultiply #-}


prop_MatrixMatrixMultiply ::
     forall r e. (Numeric r e, Mutable r Ix2 e, Eq e, Show e)
  => Fun e e
  -> Matrix r e
  -> Property
prop_MatrixMatrixMultiply f arr = (arr !><! arr') === naiveMatrixMatrixMultiply (delay arr) arr'
  where
    arr' = A.transpose (A.map (applyFun f) arr)

mutableNumericSpec ::
     forall r e.
     ( Numeric r e
     , Mutable r Ix2 e
     , Eq e
     , Show e
     , Function e
     , CoArbitrary e
     , Arbitrary e
     , Arbitrary (Array r Ix2 e)
     , Show (Array r Ix2 e)
     )
  => Spec
mutableNumericSpec =
  describe "Numerc Operations" $ do
    prop "MatrixMatrixMultiply" $ prop_MatrixMatrixMultiply @r @e
