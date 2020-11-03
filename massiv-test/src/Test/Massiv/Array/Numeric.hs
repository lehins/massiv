{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Massiv.Array.Numeric
  ( -- * Spec for safe Mutable instance
    prop_MatrixMatrixMultiply
  , mutableNumericSpec
  , mutableNumericFloatSpec
  ) where

import Data.Massiv.Array as A
import Test.Massiv.Utils as T
import Test.Massiv.Core.Common ()


naiveMatrixMatrixMultiply ::
     (Num e, Source (R r1) e, Source (R r2) e, OuterSlice r1 Ix2 e, InnerSlice r2 Ix2 e)
  => Array r1 Ix2 e
  -> Array r2 Ix2 e
  -> Array D Ix2 e
naiveMatrixMatrixMultiply arr1 arr2
  | n1 /= m2 =
    error $
    "(|*|): Inner array dimensions must agree, but received: " ++
    show (size arr1) ++ " and " ++ show (size arr2)
  | isEmpty arr1 || isEmpty arr2 = empty
  | otherwise =
    makeArrayR D Seq (Sz (m1 :. n2)) $ \(i :. j) ->
      A.foldlS (+) 0 (A.zipWith (*) (arr1 !> i) (arr2 <! j))
  where
    Sz2 m1 n1 = size arr1
    Sz2 m2 n2 = size arr2
{-# INLINE naiveMatrixMatrixMultiply #-}


prop_MatrixMatrixMultiply ::
     forall r e. (Numeric r e, Mutable r e, Eq (Matrix r e), Show (Matrix r e))
  => Fun e e
  -> Matrix r e
  -> Property
prop_MatrixMatrixMultiply f arr = expectProp $ do
  let arr' = A.transpose (A.map (applyFun f) arr)
  arr !><! compute arr' `shouldBe` compute (naiveMatrixMatrixMultiply (delay arr) arr')
  arr !><! compute (transpose arr) `shouldBe`
    compute (naiveMatrixMatrixMultiply (delay arr) (transpose arr))
  let Sz2 m n = size arr
  when (m /= n) $
    arr .><. arr `shouldThrow` (== SizeMismatchException (size arr) (Sz2 m n))

prop_MatrixVectorMultiply ::
     forall r e.
     ( Numeric r e
     , InnerSlice r Ix2 e
     , Mutable r e
     , Source (R r) e
     , Construct r Ix1 e
     , Eq e
     , Show e
     )
  => Fun Int e
  -> Matrix r e
  -> Property
prop_MatrixVectorMultiply f arr =
  expectProp $ do
    let Sz2 _ n = size arr
        v = makeArray Seq (Sz n) (applyFun f)
    arr !>< v `shouldBe` flatten (naiveMatrixMatrixMultiply (delay arr) (resize' (Sz2 n 1) v))
    arr .>< makeArray Seq (Sz (n + 1)) (applyFun f) `shouldThrow`
      (== SizeMismatchException (size arr) (Sz2 (n + 1) 1))

prop_VectorMatrixMultiply ::
     forall r e.
     ( Numeric r e
     , OuterSlice r Ix2 e
     , Construct r Ix1 e
     , Source (R r) e
     , Mutable r e
     , Show (Vector r e)
     , Eq (Vector r e)
     )
  => Fun Int e
  -> Matrix r e
  -> Property
prop_VectorMatrixMultiply f arr =
  expectProp $ do
    let Sz2 m _ = size arr
        v = makeArray Seq (Sz m) (applyFun f)
    v ><! arr `shouldBe`
      compute (flatten (naiveMatrixMatrixMultiply (resize' (Sz2 1 m) v) (delay arr)))
    multiplyVectorByMatrix v arr `shouldReturn` compute (v ><! arr)
    makeArray Seq (Sz (m + 1)) (applyFun f) ><. arr `shouldThrow`
      (== SizeMismatchException (Sz2 1 (m + 1)) (size arr))

prop_DotProduct ::
     forall r e. (Numeric r e, Mutable r e, Eq e, Show e, Construct r Ix1 e)
  => Fun e e
  -> Vector r e
  -> Property
prop_DotProduct f v =
  expectProp $ do
    let v' = A.map (applyFun f) v
    v !.! compute v' `shouldBe` A.sum (A.zipWith (*) v v')
    dotM v (makeArray Seq (size v + 1) (const 0)) `shouldThrow`
      (== SizeMismatchException (size v) (size v + 1))

prop_Norm ::
     forall r e. (NumericFloat r e, Mutable r e, RealFloat e, Show e)
  => e
  -> Vector r e
  -> Property
prop_Norm eps v = epsilonEq eps (sqrt (v !.! v)) (normL2 v)



prop_Plus ::
     forall r e.
     (Numeric r e, Mutable r e, Show (Matrix r e), Eq (Matrix r e))
  => Fun e e
  -> Matrix r e
  -> e
  -> Property
prop_Plus f arr e = expectProp $ do
  arr .+ e `shouldBe` compute (A.map (+ e) arr)
  e +. arr `shouldBe` arr .+ e
  let arr' = compute (A.map (applyFun f) arr)
  arr !+! arr' `shouldBe` compute (A.zipWith (+) arr arr')
  let Sz2 m n = size arr
  when (m /= n) $
    arr .+. compute (transpose arr) `shouldThrow` (== SizeMismatchException (size arr) (Sz2 n m))

prop_Minus ::
     forall r e.
     (Numeric r e, Mutable r e, Show (Array r Ix2 e), Eq (Array r Ix2 e))
  => Fun e e
  -> Matrix r e
  -> e
  -> Property
prop_Minus f arr e = expectProp $ do
  arr .- e `shouldBe` compute (A.map (subtract e) arr)
  e -. arr `shouldBe` negateA (arr .- e)
  let arr' = compute (A.map (applyFun f) arr)
  arr !-! arr' `shouldBe` compute (A.zipWith (-) arr arr')
  let Sz2 m n = size arr
  when (m /= n) $
    arr .-. compute (transpose arr) `shouldThrow` (== SizeMismatchException (size arr) (Sz2 n m))

prop_Times ::
     forall r e.
     (Numeric r e, Mutable r e, Show (Matrix r e), Eq (Matrix r e))
  => Fun e e
  -> Matrix r e
  -> e
  -> Property
prop_Times f arr e = expectProp $ do
  arr .* e `shouldBe` compute (A.map (* e) arr)
  e *. arr `shouldBe` arr .* e
  let arr' = compute (A.map (applyFun f) arr)
  arr !*! arr' `shouldBe` compute (A.zipWith (*) arr arr')
  let Sz2 m n = size arr
  when (m /= n) $
    arr .*. compute (transpose arr) `shouldThrow` (== SizeMismatchException (size arr) (Sz2 n m))

prop_Divide ::
     forall r e.
     ( NumericFloat r e
     , Mutable r e
     , Show e
     , RealFloat e
     , Show (Matrix r e)
     , Eq (Matrix r e)
     )
  => e -- ^ Epsilon
  -> Fun e e
  -> Matrix r e
  -> e
  -> Property
prop_Divide eps f arr e = e /= 0 ==> expectProp $ do
  arr ./ e `shouldBe` compute (A.map (/ e) arr)
  epsilonFoldableExpect eps (delay (e /. arr)) (delay (e *. recipA arr))
  let arr' = compute (A.map (applyFun f) arr)
  unless (A.or (A.zipWith (\x y -> x == 0 && y == 0) arr arr')) $
    arr !/! arr' `shouldBe` compute (A.zipWith (/) arr arr')
  let Sz2 m n = size arr
  when (m /= n) $
    arr ./. compute (transpose arr) `shouldThrow` (== SizeMismatchException (size arr) (Sz2 n m))

prop_Floating ::
     forall r e. (RealFloat e, Source r e, NumericFloat r e, Show e)
  => e
  -> Matrix r e
  -> Property
prop_Floating eps arr = expectProp $ do
  epsilonFoldableExpect eps (delay (absA arr)) (A.map abs arr)
  epsilonFoldableExpect eps (delay (signumA arr)) (A.map signum arr)
  epsilonFoldableExpect eps (delay (recipA arr)) (A.map recip arr)
  epsilonFoldableExpect eps (delay (expA arr)) (A.map exp arr)
  epsilonFoldableExpect eps (delay (sqrtA arr)) (A.map sqrt arr)
  epsilonFoldableExpect eps (delay (logA arr)) (A.map log arr)
  epsilonFoldableExpect eps (delay (sinA arr)) (A.map sin arr)
  epsilonFoldableExpect eps (delay (cosA arr)) (A.map cos arr)
  epsilonFoldableExpect eps (delay (tanA arr)) (A.map tan arr)
  epsilonFoldableExpect eps (delay (asinA arr)) (A.map asin arr)
  epsilonFoldableExpect eps (delay (acosA arr)) (A.map acos arr)
  epsilonFoldableExpect eps (delay (atanA arr)) (A.map atan arr)
  epsilonFoldableExpect eps (delay (sinhA arr)) (A.map sinh arr)
  epsilonFoldableExpect eps (delay (coshA arr)) (A.map cosh arr)
  epsilonFoldableExpect eps (delay (tanhA arr)) (A.map tanh arr)
  epsilonFoldableExpect eps (delay (asinhA arr)) (A.map asinh arr)
  epsilonFoldableExpect eps (delay (acoshA arr)) (A.map acosh arr)
  epsilonFoldableExpect eps (delay (atanhA arr)) (A.map atanh arr)

prop_Floating2 ::
     forall r e. (RealFloat e, Mutable r e, NumericFloat r e, Show e)
  => e
  -> Matrix r e
  -> Fun e e
  -> Property
prop_Floating2 eps arr1 f = expectProp $ do
  let arr2 = compute (A.map (applyFun f) arr1)
  epsilonFoldableExpect eps (delay (logBaseA arr1 arr2)) (A.zipWith logBase arr1 arr2)
  epsilonFoldableExpect eps (delay (arr1 .** arr2)) (A.zipWith (**) arr1 arr2)
  res <- atan2A arr1 arr2
  epsilonFoldableExpect eps (delay res) (A.zipWith atan2 arr1 arr2)


mutableNumericSpec ::
     forall r e.
     ( Numeric r e
     , Mutable r e
     , Construct r Ix1 e
     , Construct r Ix2 e
     , InnerSlice r Ix2 e
     , OuterSlice r Ix2 e
     , Source (R r) e
     , Eq e
     , Show e
     , Function e
     , CoArbitrary e
     , Arbitrary e
     , Arbitrary (Matrix r e)
     , Arbitrary (Vector r e)
     , Show (Matrix r e)
     , Eq (Matrix r e)
     , Show (Vector r e)
     , Eq (Vector r e)
     )
  => Spec
mutableNumericSpec =
  describe "Numeric Operations" $ do
    prop "Plus" $ prop_Plus @r @e
    prop "Minus" $ prop_Minus @r @e
    prop "Times" $ prop_Times @r @e
    prop "DotProduct" $ prop_DotProduct @r @e
    prop "Power" $ \(arr :: Array r Ix2 e) (NonNegative p) -> expectProp $
      arr .^ p `shouldBe` compute (A.map (^ p) arr)
    prop "MatrixMatrixMultiply" $ prop_MatrixMatrixMultiply @r @e
    prop "MatrixVectorMultiply" $ prop_MatrixVectorMultiply @r @e
    prop "VectorMatrixMultiply" $ prop_VectorMatrixMultiply @r @e
    prop "Identity" $ \ n -> expectProp $ do
      computeIO (identityMatrix (Sz n)) `shouldReturn`
        makeArray @r Seq (Sz2 n n) (\ (i :. j) -> if i == j then 1 else 0 :: e)
    prop "LowerTriangular" $ \ comp n f -> expectProp $ do
      computeIO (lowerTriangular comp (Sz n) (applyFun f . fromIx2)) `shouldReturn`
        makeArray @r Seq (Sz2 n n) (\ (i :. j) -> if i >= j then applyFun f (i, j) else 0 :: e)
    prop "UpperTriangular" $ \ comp n f -> expectProp $ do
      computeIO (upperTriangular comp (Sz n) (applyFun f . fromIx2)) `shouldReturn`
        makeArray @r Seq (Sz2 n n) (\ (i :. j) -> if i <= j then applyFun f (i, j) else 0 :: e)

mutableNumericFloatSpec ::
     forall r.
     ( NumericFloat r Float
     , Mutable r Float
     , Arbitrary (Vector r Float)
     , Arbitrary (Matrix r Float)
     , Show (Vector r Float)
     , Show (Matrix r Float)
     , Eq (Matrix r Float)
     , NumericFloat r Double
     , Mutable r Double
     , Arbitrary (Vector r Double)
     , Arbitrary (Matrix r Double)
     , Show (Vector r Double)
     , Show (Matrix r Double)
     , Eq (Matrix r Double)
     )
  => Spec
mutableNumericFloatSpec = do
  let ef = 1e-6 :: Float
      ed = 1e-12 :: Double
  describe "NumericFloat Operations" $ do
    describe "Float" $ do
      prop "Divide" $ prop_Divide @r ef
      prop "Floating" $ prop_Floating @r ef
      prop "Floating2" $ prop_Floating2 @r ef
      prop "Norm" $ prop_Norm @r ef
      prop "Power" $ prop_Power @r ef
    describe "Double" $ do
      prop "Divide" $ prop_Divide @r ed
      prop "Floating" $ prop_Floating @r ed
      prop "Floating2" $ prop_Floating2 @r ed
      prop "Norm" $ prop_Norm @r ed
      prop "Power" $ prop_Power @r ed

prop_Power ::
     (Numeric r e, Source r e, RealFloat e, Show e) => e -> Matrix r e -> Int -> Property
prop_Power eps arr p = expectProp $
  epsilonFoldableExpect eps (delay (arr .^^ p)) (A.map (^^ p) arr)
