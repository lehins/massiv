{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Massiv.Array.Numeric
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Numeric
  ( -- * Numeric
    -- ** Pointwise addition
    (.+)
  , (+.)
  , (.+.)
  , (!+!)
  -- ** Pointwise subtraction
  , (.-)
  , (-.)
  , (.-.)
  , (!-!)
  -- ** Pointwise multiplication
  , (.*)
  , (*.)
  , (.*.)
  , (!*!)
  , (.^)
  -- ** Matrix multiplication
  , (.><)
  , (!><)
  , (><.)
  , (><!)
  , (.><.)
  , (!><!)
  , (#>)
  , (|*|)
  , multiplyTransposed
  -- ** Simple matrices
  , identityMatrix
  , lowerTriangular
  , upperTriangular
  , negateA
  , absA
  , signumA
  , fromIntegerA
  -- * Integral
  , quotA
  , remA
  , divA
  , modA
  , quotRemA
  , divModA
  -- * Fractional
  , (./)
  , (/.)
  , (./.)
  , (!/!)
  , (.^^)
  , recipA
  , fromRationalA
  -- * Floating
  , piA
  , expA
  , logA
  , sqrtA
  , (.**)
  , logBaseA
  , sinA
  , cosA
  , tanA
  , asinA
  , acosA
  , atanA
  , sinhA
  , coshA
  , tanhA
  , asinhA
  , acoshA
  , atanhA
  -- * RealFrac
  , truncateA
  , roundA
  , ceilingA
  , floorA
  -- * RealFloat
  , atan2A
  ) where
import Data.Massiv.Array.Manifest
import Data.Massiv.Array.Delayed.Pull
import Data.Massiv.Array.Delayed.Push
import Data.Massiv.Array.Manifest.Internal
import Data.Massiv.Array.Ops.Fold as A
import Data.Massiv.Array.Ops.Map as A
import Data.Massiv.Array.Ops.Transform as A
import Data.Massiv.Array.Ops.Construct
import Data.Massiv.Core
import Data.Massiv.Core.Common
import Data.Massiv.Core.Operations
import Prelude as P


infixr 8  .^, .^^
infixl 7  !*!, .*., .*, *., !/!, ./., ./, /., `quotA`, `remA`, `divA`, `modA`
infixl 6  !+!, .+., .+, +., !-!, .-., .-, -.

liftArray2Matching
  :: (Source r1 ix a, Source r2 ix b)
  => (a -> b -> e) -> Array r1 ix a -> Array r2 ix b -> Array D ix e
liftArray2Matching f !arr1 !arr2
  | sz1 == sz2 =
    makeArray
      (getComp arr1 <> getComp arr2)
      sz1
      (\ !ix -> f (unsafeIndex arr1 ix) (unsafeIndex arr2 ix))
  | otherwise = throw $ SizeMismatchException (size arr1) (size arr2)
  where
    sz1 = size arr1
    sz2 = size arr2
{-# INLINE liftArray2Matching #-}

liftArray2M ::
     (Load r ix e, Numeric r e, MonadThrow m)
  => (e -> e -> e)
  -> Array r ix e
  -> Array r ix e
  -> m (Array r ix e)
liftArray2M f a1 a2
  | size a1 == size a2 = pure $ unsafeLiftArray2 f a1 a2
  | otherwise = throwM $ SizeMismatchException (size a1) (size a2)
{-# INLINE liftArray2M #-}


liftNumericArray2M ::
     (Load r ix e, MonadThrow m)
  => (Array r ix e -> Array r ix e -> Array r ix e)
  -> Array r ix e
  -> Array r ix e
  -> m (Array r ix e)
liftNumericArray2M f a1 a2
  | size a1 == size a2 = pure $ f a1 a2
  | otherwise = throwM $ SizeMismatchException (size a1) (size a2)
{-# INLINE liftNumericArray2M #-}


-- | Add two arrays together pointwise. Same as `!+!` but produces monadic computation
-- that allows for handling failure.
--
-- /__Throws Exceptions__/: `SizeMismatchException` when array sizes do not match.
--
-- @since 0.4.0
(.+.) ::
     (Load r ix e, Numeric r e, MonadThrow m) => Array r ix e -> Array r ix e -> m (Array r ix e)
(.+.) = liftNumericArray2M additionPointwise
{-# INLINE (.+.) #-}

-- | Add two arrays together pointwise. Prefer to use monadic version of this function
-- `.+.` whenever possible, because it is better to avoid partial functions.
--
-- [Partial] Mismatched array sizes will result in an impure exception being thrown.
--
-- ====__Example__
--
-- >>> let a1 = Ix1 0 ... 10
-- >>> let a2 = Ix1 20 ... 30
-- >>> a1 !+! a2
-- Array D Seq (Sz1 11)
--   [ 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40 ]
--
-- @since 0.5.6
(!+!) :: (Load r ix e, Numeric r e) => Array r ix e -> Array r ix e -> Array r ix e
(!+!) a1 a2 = throwEither $ liftNumericArray2M additionPointwise a1 a2
{-# INLINE (!+!) #-}

-- | Add a scalar to each element of the array. Array is on the left.
--
-- @since 0.1.0
(.+) :: (Index ix, Numeric r e) => Array r ix e -> e -> Array r ix e
(.+) = plusScalar
{-# INLINE (.+) #-}

-- | Add a scalar to each element of the array. Array is on the right.
--
-- @since 0.4.0
(+.) :: (Index ix, Numeric r e) => e -> Array r ix e -> Array r ix e
(+.) = flip plusScalar
{-# INLINE (+.) #-}

-- | Subtract two arrays pointwise. Same as `!-!` but produces monadic computation that
-- allows for handling failure.
--
-- /__Throws Exceptions__/: `SizeMismatchException` when array sizes do not match.
--
-- @since 0.4.0
(.-.) ::
     (Load r ix e, Numeric r e, MonadThrow m) => Array r ix e -> Array r ix e -> m (Array r ix e)
(.-.) = liftNumericArray2M subtractionPointwise
{-# INLINE (.-.) #-}


-- | Subtract one array from another pointwise. Prefer to use monadic version of this
-- function `.-.` whenever possible, because it is better to avoid partial functions.
--
-- [Partial] Mismatched array sizes will result in an impure exception being thrown.
--
-- ====__Example__
--
-- >>> let a1 = Ix1 0 ... 10
-- >>> let a2 = Ix1 20 ... 30
-- >>> a1 !-! a2
-- Array D Seq (Sz1 11)
--   [ -20, -20, -20, -20, -20, -20, -20, -20, -20, -20, -20 ]
--
-- @since 0.5.6
(!-!) :: (Load r ix e, Numeric r e) => Array r ix e -> Array r ix e -> Array r ix e
(!-!) a1 a2 = throwEither $ liftNumericArray2M subtractionPointwise a1 a2
{-# INLINE (!-!) #-}

-- | Subtract a scalar from each element of the array. Array is on the left.
--
-- @since 0.4.0
(.-) :: (Index ix, Numeric r e) => Array r ix e -> e -> Array r ix e
(.-) = minusScalar
{-# INLINE (.-) #-}

-- | Subtract a scalar from each element of the array. Array is on the right.
--
-- @since 0.4.0
(-.) :: (Index ix, Numeric r e) => e -> Array r ix e -> Array r ix e
(-.) = flip minusScalar
{-# INLINE (-.) #-}


-- | Multiply two arrays together pointwise. Same as `!*!` but produces monadic
-- computation that allows for handling failure.
--
-- /__Throws Exceptions__/: `SizeMismatchException` when array sizes do not match.
--
-- @since 0.4.0
(.*.) ::
     (Load r ix e, Numeric r e, MonadThrow m) => Array r ix e -> Array r ix e -> m (Array r ix e)
(.*.) = liftNumericArray2M multiplicationPointwise
{-# INLINE (.*.) #-}


-- | Multiplication of two arrays pointwise. Prefer to use monadic version of this
-- function `.*.` whenever possible, because it is better to avoid partial functions.
--
-- [Partial] Mismatched array sizes will result in an impure exception being thrown.
--
-- ====__Example__
--
-- >>> let a1 = Ix1 0 ... 10
-- >>> let a2 = Ix1 20 ... 30
-- >>> a1 !*! a2
-- Array D Seq (Sz1 11)
--   [ 0, 21, 44, 69, 96, 125, 156, 189, 224, 261, 300 ]
--
-- @since 0.5.6
(!*!) :: (Load r ix e, Numeric r e) => Array r ix e -> Array r ix e -> Array r ix e
(!*!) a1 a2 = throwEither $ liftNumericArray2M multiplicationPointwise a1 a2
{-# INLINE (!*!) #-}


-- | Multiply each element of the array by a scalar value. Scalar is on the right.
--
-- ====__Example__
--
-- >>> let arr = Ix1 20 ..: 25
-- >>> arr
-- Array D Seq (Sz1 5)
--   [ 20, 21, 22, 23, 24 ]
-- >>> arr .* 10
-- Array D Seq (Sz1 5)
--   [ 200, 210, 220, 230, 240 ]
--
-- @since 0.4.0
(.*) :: (Index ix, Numeric r e) => Array r ix e -> e -> Array r ix e
(.*) = multiplyScalar
{-# INLINE (.*) #-}


-- | Multiply each element of the array by a scalar value. Scalar is on the left.
--
-- ====__Example__
--
-- >>> let arr = Ix1 20 ..: 25
-- >>> arr
-- Array D Seq (Sz1 5)
--   [ 20, 21, 22, 23, 24 ]
-- >>> 10 *. arr
-- Array D Seq (Sz1 5)
--   [ 200, 210, 220, 230, 240 ]
--
-- @since 0.4.0
(*.) :: (Index ix, Numeric r e) => e -> Array r ix e -> Array r ix e
(*.) = flip multiplyScalar
{-# INLINE (*.) #-}


-- | Raise each element of the array to a power.
--
-- ====__Example__
--
-- >>> let arr = Ix1 20 ..: 25
-- >>> arr
-- Array D Seq (Sz1 5)
--   [ 20, 21, 22, 23, 24 ]
-- >>> arr .^ 3
-- Array D Seq (Sz1 5)
--   [ 8000, 9261, 10648, 12167, 13824 ]
--
-- @since 0.4.0
(.^) :: (Index ix, Numeric r e) => Array r ix e -> Int -> Array r ix e
(.^) = powerPointwise
{-# INLINE (.^) #-}

-- | Matrix-vector product
--
-- Inner dimensions must agree, otherwise `SizeMismatchException`
--
-- @since 0.5.2
(#>) :: (MonadThrow m, Num e, Source (R r) Ix1 e, Manifest r' Ix1 e, OuterSlice r Ix2 e) =>
        Array r Ix2 e -- ^ Matrix
     -> Array r' Ix1 e -- ^ Vector
     -> m (Array D Ix1 e)
mm #> v
  | mCols /= n = throwM $ SizeMismatchException (size mm) (Sz2 n 1)
  | otherwise = pure $ makeArray (getComp mm <> getComp v) (Sz1 mRows) $ \i ->
      A.foldlS (+) 0 (A.zipWith (*) (unsafeOuterSlice mm i) v)
  where
    Sz2 mRows mCols = size mm
    Sz1 n = size v
{-# INLINE (#>) #-}
{-# DEPRECATED (#>) "In favor of (`.><`)" #-}


-- | Multiply a matrix by a column vector. Same as `!><` but produces monadic
-- computation that allows for handling failure.
--
-- /__Throws Exceptions__/: `SizeMismatchException` when inner dimensions of arrays do not match.
--
-- @since 0.5.6
(.><) :: (MonadThrow m, Numeric r e, Source r Ix1 e, Source r Ix2 e) =>
         Matrix r e -- ^ Matrix
      -> Vector r e -- ^ Column vector (Used many times, so make sure it is computed)
      -> m (Vector D e)
(.><) mm v
  | mCols /= n = throwM $ SizeMismatchException (size mm) (Sz2 n 1)
  | otherwise = pure $ makeArray (getComp mm <> getComp v) (Sz1 mRows) $ \i ->
      unsafeDotProduct (unsafeLinearSlice i sz mm) v
  where
    Sz2 mRows mCols = size mm
    sz@(Sz1 n) = size v
{-# INLINE (.><) #-}


-- | Multiply a matrix by a column vector
--
-- [Partial] Throws impure exception when inner dimensions do not agree
--
-- @since 0.5.6
(!><) ::
     (Numeric r e, Source r Ix1 e, Source r Ix2 e)
  => Matrix r e -- ^ Matrix
  -> Vector r e -- ^ Column vector (Used many times, so make sure it is computed)
  -> Vector D e
(!><) mm v = throwEither (mm .>< v)
{-# INLINE (!><) #-}


-- | Multiply a row vector by a matrix. Same as `><!` but produces monadic computation
-- that allows for handling failure.
--
-- /__Throws Exceptions__/: `SizeMismatchException` when inner dimensions of arrays do not match.
--
-- @since 0.5.6
(><.) :: (MonadThrow m, Numeric r e, Mutable r Ix1 e, Mutable r Ix2 e) =>
         Vector r e -- ^ Row vector
      -> Matrix r e -- ^ Matrix
      -> m (Vector D e)
(><.) v mm
  | mRows /= n = throwM $ SizeMismatchException (Sz2 1 n) (size mm)
  | otherwise = pure $ makeArray (getComp mm <> getComp v) (Sz1 mCols) $ \i ->
      unsafeDotProduct (unsafeLinearSlice i sz mm') v
  where
    Sz2 mRows mCols = size mm
    mm' = compute (transpose mm)
    sz@(Sz1 n) = size v
{-# INLINE (><.) #-}


-- | Multiply a row vector by a matrix.
--
-- [Partial] Throws impure exception when inner dimensions do not agree
--
-- @since 0.5.6
(><!) ::
     (Numeric r e, Mutable r Ix1 e, Mutable r Ix2 e)
  => Vector r e -- ^ Row vector
  -> Matrix r e -- ^ Matrix
  -> Vector D e
(><!) v mm = throwEither (v ><. mm)
{-# INLINE (><!) #-}



-- | Multiply two matrices together.
--
-- [Partial] Inner dimension must agree
--
-- ====__Examples__
--
-- >>> a1 = makeArrayR P Seq (Sz2 5 6) $ \(i :. j) -> i + j
-- >>> a2 = makeArrayR D Seq (Sz2 6 5) $ \(i :. j) -> i - j
-- >>> a1 !><! a2
-- Array P Seq (Sz (5 :. 5))
--   [ [ 55, 40, 25, 10, -5 ]
--   , [ 70, 49, 28, 7, -14 ]
--   , [ 85, 58, 31, 4, -23 ]
--   , [ 100, 67, 34, 1, -32 ]
--   , [ 115, 76, 37, -2, -41 ]
--   ]
--
-- @since 0.5.6
(!><!) :: (Numeric r e, Mutable r Ix2 e, Source r' Ix2 e) => Matrix r e -> Matrix r' e -> Matrix D e
(!><!) a1 a2 = throwEither (a1 `multiplyMatrices` a2)
{-# INLINE (!><!) #-}

-- | Matrix multiplication. Same as `!><!` but produces monadic computation that allows
-- for handling failure.
--
-- /__Throws Exceptions__/: `SizeMismatchException` when inner dimensions of arrays do not match.
--
-- @since 0.5.6
(.><.) ::
     (Numeric r e, Mutable r Ix2 e, Source r' Ix2 e, MonadThrow m)
  => Matrix r e
  -> Matrix r' e
  -> m (Matrix D e)
(.><.) = multiplyMatrices
{-# INLINE (.><.) #-}



multiplyMatrices ::
     forall r r' e m. (Numeric r e, Mutable r Ix2 e, Source r' Ix2 e, MonadThrow m)
  => Matrix r e
  -> Matrix r' e
  -> m (Matrix D e)
multiplyMatrices arr1 arr2 = multiplyMatricesTransposed arr1 arr2'
  where
    arr2' :: Array r Ix2 e
    arr2' = compute $ transpose arr2
{-# INLINE [1] multiplyMatrices #-}

{-# RULES
"multiplyMatricesTransposed" [~1] forall arr1 arr2 . multiplyMatrices arr1 (transpose arr2) =
    multiplyMatricesTransposedFused arr1 (convert arr2)
 #-}

-- | Computes the matrix-matrix product where second matrix is transposed (i.e. M x N')
--
-- > m1 .><. transpose m2 == multiplyMatricesTransposed m1 m2
multiplyMatricesTransposed ::
     (Numeric r e, Manifest r Ix2 e, MonadThrow m)
  => Matrix r e
  -> Matrix r  e
  -> m (Matrix D e)
multiplyMatricesTransposed arr1 arr2
  | n1 /= m2 = throwM $ SizeMismatchException (size arr1) (size arr2)
  | otherwise =
    pure $
    DArray (getComp arr1 <> getComp arr2) (SafeSz (m1 :. n2)) $ \(i :. j) ->
      unsafeDotProduct (unsafeLinearSlice i n arr1) (unsafeLinearSlice j n arr2)
  where
    n = SafeSz n1
    SafeSz (m1 :. n1) = size arr1
    SafeSz (n2 :. m2) = size arr2
{-# INLINE multiplyMatricesTransposed #-}

multiplyMatricesTransposedFused ::
     (Manifest r Ix2 e, Numeric r e, MonadThrow m) => Matrix r e -> Matrix r e -> m (Matrix D e)
multiplyMatricesTransposedFused arr1 arr2 = multiplyMatricesTransposed arr1 arr2
{-# INLINE multiplyMatricesTransposedFused #-}



--- Below is outdated

-- | Matrix multiplication
--
-- /__Throws Exceptions__/: `SizeMismatchException` when inner dimensions of arrays do not match.
--
-- @since 0.4.0
(|*|) ::
     (Mutable r Ix2 e, Source r' Ix2 e, OuterSlice r Ix2 e, Source (R r) Ix1 e, Num e, MonadThrow m)
  => Array r Ix2 e
  -> Array r' Ix2 e
  -> m (Array r Ix2 e)
(|*|) a1 a2 = compute <$> multArrs a1 a2
{-# INLINE [1] (|*|) #-}
{-# DEPRECATED (|*|) "In favor of `.><.`" #-}

{-# RULES
"multDoubleTranspose" [~1] forall arr1 arr2 . arr1 |*| transpose arr2 =
    multiplyTransposedFused arr1 (convert arr2)
 #-}


multiplyTransposedFused ::
     ( Mutable r Ix2 e
     , OuterSlice r Ix2 e
     , Source (R r) Ix1 e
     , Num e
     , MonadThrow m
     )
  => Array r Ix2 e
  -> Array r Ix2 e
  -> m (Array r Ix2 e)
multiplyTransposedFused arr1 arr2 = compute <$> multiplyTransposed arr1 arr2
{-# INLINE multiplyTransposedFused #-}


multArrs :: forall r r' e m.
            ( Mutable r Ix2 e
            , Source r' Ix2 e
            , OuterSlice r Ix2 e
            , Source (R r) Ix1 e
            , Num e
            , MonadThrow m
            )
         => Array r Ix2 e -> Array r' Ix2 e -> m (Array D Ix2 e)
multArrs arr1 arr2 = multiplyTransposed arr1 arr2'
  where
    arr2' :: Array r Ix2 e
    arr2' = compute $ transpose arr2
{-# INLINE multArrs #-}

-- | Computes the matrix-matrix transposed product (i.e. A * A')
multiplyTransposed ::
     ( Manifest r Ix2 e
     , OuterSlice r Ix2 e
     , Source (R r) Ix1 e
     , Num e
     , MonadThrow m
     )
  => Array r Ix2 e
  -> Array r Ix2 e
  -> m (Array D Ix2 e)
multiplyTransposed arr1 arr2
  | n1 /= m2 = throwM $ SizeMismatchException (size arr1) (size arr2)
  | otherwise =
    pure $
    DArray (getComp arr1 <> getComp arr2) (SafeSz (m1 :. n2)) $ \(i :. j) ->
      A.foldlS (+) 0 (A.zipWith (*) (unsafeOuterSlice arr1 i) (unsafeOuterSlice arr2 j))
  where
    SafeSz (m1 :. n1) = size arr1
    SafeSz (n2 :. m2) = size arr2
{-# INLINE multiplyTransposed #-}

-- | Create an indentity matrix.
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Array
-- >>> identityMatrix 5
-- Array DL Seq (Sz (5 :. 5))
--   [ [ 1, 0, 0, 0, 0 ]
--   , [ 0, 1, 0, 0, 0 ]
--   , [ 0, 0, 1, 0, 0 ]
--   , [ 0, 0, 0, 1, 0 ]
--   , [ 0, 0, 0, 0, 1 ]
--   ]
--
-- @since 0.3.6
identityMatrix :: Num e => Sz1 -> Matrix DL e
identityMatrix (Sz n) =
  makeLoadArrayS (Sz2 n n) 0 $ \ w -> loopM_ 0 (< n) (+1) $ \ i -> w (i :. i) 1
{-# INLINE identityMatrix #-}

-- | Create a lower triangular (L in LU decomposition) matrix of size @NxN@
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Array
-- >>> lowerTriangular Seq 5 (\(i :. j) -> i + j)
-- Array DL Seq (Sz (5 :. 5))
--   [ [ 0, 0, 0, 0, 0 ]
--   , [ 1, 2, 0, 0, 0 ]
--   , [ 2, 3, 4, 0, 0 ]
--   , [ 3, 4, 5, 6, 0 ]
--   , [ 4, 5, 6, 7, 8 ]
--   ]
--
-- @since 0.5.2
lowerTriangular :: Num e => Comp -> Sz1 -> (Ix2 -> e) -> Matrix DL e
lowerTriangular comp (Sz1 n) f =
  let sz = Sz2 n n
   in unsafeMakeLoadArrayAdjusted comp sz (Just 0) $ \scheduler wr ->
        forM_ (0 ..: n) $ \i ->
          scheduleWork scheduler $
          forM_ (0 ... i) $ \j ->
            let ix = i :. j
             in wr (toLinearIndex sz ix) (f ix)
{-# INLINE lowerTriangular #-}

-- | Create an upper triangular (U in LU decomposition) matrix of size @NxN@
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Array
-- >>> upperTriangular Par 5 (\(i :. j) -> i + j)
-- Array DL Par (Sz (5 :. 5))
--   [ [ 0, 1, 2, 3, 4 ]
--   , [ 0, 2, 3, 4, 5 ]
--   , [ 0, 0, 4, 5, 6 ]
--   , [ 0, 0, 0, 6, 7 ]
--   , [ 0, 0, 0, 0, 8 ]
--   ]
--
-- @since 0.5.2
upperTriangular :: Num e => Comp -> Sz1 -> (Ix2 -> e) -> Matrix DL e
upperTriangular comp (Sz1 n) f =
  let sz = Sz2 n n
   in unsafeMakeLoadArrayAdjusted comp sz (Just 0) $ \scheduler wr ->
        forM_ (0 ..: n) $ \i ->
          scheduleWork scheduler $
          forM_ (i ..: n) $ \j ->
            let ix = i :. j
             in wr (toLinearIndex sz ix) (f ix)
{-# INLINE upperTriangular #-}

-- | Negate each element of the array
--
-- @since 0.4.0
negateA :: (Index ix, Numeric r e) => Array r ix e -> Array r ix e
negateA = unsafeLiftArray negate
{-# INLINE negateA #-}

-- | Apply `abs` to each element of the array
--
-- @since 0.4.0
absA :: (Index ix, Numeric r e) => Array r ix e -> Array r ix e
absA = absPointwise
{-# INLINE absA #-}

-- | Apply `signum` to each element of the array
--
-- @since 0.4.0
signumA :: (Index ix, Numeric r e) => Array r ix e -> Array r ix e
signumA = unsafeLiftArray signum
{-# INLINE signumA #-}

-- | Create a singleton array from an `Integer`
fromIntegerA :: (Index ix, Num e) => Integer -> Array D ix e
fromIntegerA = singleton . fromInteger
{-# INLINE fromIntegerA #-}
{-# DEPRECATED fromIntegerA "This almost never a desired behavior. Use `Data.Massiv.Array.Ops.Construct.replicate` instead" #-}

-- | Divide each element of one array by another pointwise. Same as `!/!` but produces
-- monadic computation that allows for handling failure.
--
-- /__Throws Exceptions__/: `SizeMismatchException` when array sizes do not match.
--
-- @since 0.4.0
(./.) ::
     (Load r ix e, NumericFloat r e, MonadThrow m)
  => Array r ix e
  -> Array r ix e
  -> m (Array r ix e)
(./.) = liftNumericArray2M divisionPointwise
{-# INLINE (./.) #-}


-- | Divide two arrays pointwise. Prefer to use monadic version of this function `./.`
-- whenever possible, because it is better to avoid partial functions.
--
-- [Partial] Mismatched array sizes will result in an impure exception being thrown.
--
-- ====__Example__
--
-- >>> let arr1 = fromIntegral <$> (Ix1 20 ..: 25) :: Array D Ix1 Float
-- >>> let arr2 = fromIntegral <$> (Ix1 100 ..: 105) :: Array D Ix1 Float
-- >>> arr1 !/! arr2
-- Array D Seq (Sz1 5)
--   [ 0.2, 0.20792079, 0.21568628, 0.22330096, 0.23076923 ]
--
-- @since 0.5.6
(!/!) :: (Load r ix e, NumericFloat r e) => Array r ix e -> Array r ix e -> Array r ix e
(!/!) a1 a2 = throwEither $ liftNumericArray2M divisionPointwise a1 a2
{-# INLINE (!/!) #-}

-- | Divide a scalar value by each element of the array.
--
-- > e /. arr == e *. recipA arr
--
-- ====__Example__
--
-- >>> let arr = fromIntegral <$> (Ix1 20 ..: 25) :: Array D Ix1 Float
-- >>> arr
-- Array D Seq (Sz1 5)
--   [ 20.0, 21.0, 22.0, 23.0, 24.0 ]
-- >>> 100 /. arr
-- Array D Seq (Sz1 5)
--   [ 5.0, 4.7619047, 4.5454545, 4.347826, 4.1666665 ]
--
-- @since 0.5.6
(/.) ::(Index ix,  NumericFloat r e) => e -> Array r ix e -> Array r ix e
(/.) = scalarDivide
{-# INLINE (/.) #-}

-- | Divide each element of the array by a scalar value.
--
-- ====__Example__
--
-- >>> let arr = fromIntegral <$> (Ix1 20 ..: 25) :: Array D Ix1 Float
-- >>> arr
-- Array D Seq (Sz1 5)
--   [ 20.0, 21.0, 22.0, 23.0, 24.0 ]
-- >>> arr ./ 100
-- Array D Seq (Sz1 5)
--   [ 0.2, 0.21, 0.22, 0.23, 0.24 ]
--
-- @since 0.4.0
(./) ::(Index ix,  NumericFloat r e) => Array r ix e -> e -> Array r ix e
(./) = divideScalar
{-# INLINE (./) #-}

(.^^)
  :: (Index ix, Numeric r e, Fractional e, Integral b)
  => Array r ix e -> b -> Array r ix e
(.^^) arr n = unsafeLiftArray (^^ n) arr
{-# INLINE (.^^) #-}

-- | Apply reciprical to each element of the array.
--
-- > recipA arr == 1 /. arr
--
recipA :: (Index ix, NumericFloat r e) => Array r ix e -> Array r ix e
recipA = recipPointwise
{-# INLINE recipA #-}


fromRationalA
  :: (Index ix, Fractional e)
  => Rational -> Array D ix e
fromRationalA = singleton . fromRational
{-# INLINE fromRationalA #-}

piA
  :: (Index ix, Floating e)
  => Array D ix e
piA = singleton pi
{-# INLINE piA #-}

expA :: (Index ix, NumericFloat r e) => Array r ix e -> Array r ix e
expA = unsafeLiftArray exp
{-# INLINE expA #-}

sqrtA :: (Index ix, NumericFloat r e) => Array r ix e -> Array r ix e
sqrtA = unsafeLiftArray sqrt
{-# INLINE sqrtA #-}

logA :: (Index ix, NumericFloat r e) => Array r ix e -> Array r ix e
logA = unsafeLiftArray log
{-# INLINE logA #-}

logBaseA
  :: (Source r1 ix e, Source r2 ix e, Floating e)
  => Array r1 ix e -> Array r2 ix e -> Array D ix e
logBaseA = liftArray2Matching logBase
{-# INLINE logBaseA #-}

(.**)
  :: (Source r1 ix e, Source r2 ix e, Floating e)
  => Array r1 ix e -> Array r2 ix e -> Array D ix e
(.**) = liftArray2Matching (**)
{-# INLINE (.**) #-}



sinA :: (Index ix, NumericFloat r e) => Array r ix e -> Array r ix e
sinA = unsafeLiftArray sin
{-# INLINE sinA #-}

cosA :: (Index ix, NumericFloat r e) => Array r ix e -> Array r ix e
cosA = unsafeLiftArray cos
{-# INLINE cosA #-}

tanA :: (Index ix, NumericFloat r e) => Array r ix e -> Array r ix e
tanA = unsafeLiftArray tan
{-# INLINE tanA #-}

asinA :: (Index ix, NumericFloat r e) => Array r ix e -> Array r ix e
asinA = unsafeLiftArray asin
{-# INLINE asinA #-}

atanA :: (Index ix, NumericFloat r e) => Array r ix e -> Array r ix e
atanA = unsafeLiftArray atan
{-# INLINE atanA #-}

acosA :: (Index ix, NumericFloat r e) => Array r ix e -> Array r ix e
acosA = unsafeLiftArray acos
{-# INLINE acosA #-}

sinhA :: (Index ix, NumericFloat r e) => Array r ix e -> Array r ix e
sinhA = unsafeLiftArray sinh
{-# INLINE sinhA #-}

tanhA :: (Index ix, NumericFloat r e) => Array r ix e -> Array r ix e
tanhA = unsafeLiftArray tanh
{-# INLINE tanhA #-}

coshA :: (Index ix, NumericFloat r e) => Array r ix e -> Array r ix e
coshA = unsafeLiftArray cosh
{-# INLINE coshA #-}

asinhA :: (Index ix, NumericFloat r e) => Array r ix e -> Array r ix e
asinhA = unsafeLiftArray asinh
{-# INLINE asinhA #-}

acoshA :: (Index ix, NumericFloat r e) => Array r ix e -> Array r ix e
acoshA = unsafeLiftArray acosh
{-# INLINE acoshA #-}

atanhA :: (Index ix, NumericFloat r e) => Array r ix e -> Array r ix e
atanhA = unsafeLiftArray atanh
{-# INLINE atanhA #-}


quotA
  :: (Source r1 ix e, Source r2 ix e, Integral e)
  => Array r1 ix e -> Array r2 ix e -> Array D ix e
quotA = liftArray2Matching quot
{-# INLINE quotA #-}


remA
  :: (Source r1 ix e, Source r2 ix e, Integral e)
  => Array r1 ix e -> Array r2 ix e -> Array D ix e
remA = liftArray2Matching rem
{-# INLINE remA #-}

divA
  :: (Source r1 ix e, Source r2 ix e, Integral e)
  => Array r1 ix e -> Array r2 ix e -> Array D ix e
divA = liftArray2Matching div
{-# INLINE divA #-}

modA
  :: (Source r1 ix e, Source r2 ix e, Integral e)
  => Array r1 ix e -> Array r2 ix e -> Array D ix e
modA = liftArray2Matching mod
{-# INLINE modA #-}



quotRemA
  :: (Source r1 ix e, Source r2 ix e, Integral e)
  => Array r1 ix e -> Array r2 ix e -> (Array D ix e, Array D ix e)
quotRemA arr1 = A.unzip . liftArray2Matching (quotRem) arr1
{-# INLINE quotRemA #-}


divModA
  :: (Source r1 ix e, Source r2 ix e, Integral e)
  => Array r1 ix e -> Array r2 ix e -> (Array D ix e, Array D ix e)
divModA arr1 = A.unzip . liftArray2Matching (divMod) arr1
{-# INLINE divModA #-}



truncateA
  :: (Source r ix a, RealFrac a, Integral e)
  => Array r ix a -> Array D ix e
truncateA = A.map truncate
{-# INLINE truncateA #-}


roundA :: (Source r ix a, RealFrac a, Integral e) => Array r ix a -> Array D ix e
roundA = A.map round
{-# INLINE roundA #-}


ceilingA :: (Source r ix a, RealFrac a, Integral e) => Array r ix a -> Array D ix e
ceilingA = A.map ceiling
{-# INLINE ceilingA #-}


floorA :: (Source r ix a, RealFrac a, Integral e) => Array r ix a -> Array D ix e
floorA = A.map floor
{-# INLINE floorA #-}

atan2A ::
     (Load r ix e, Numeric r e, RealFloat e, MonadThrow m)
  => Array r ix e
  -> Array r ix e
  -> m (Array r ix e)
atan2A = liftArray2M atan2
{-# INLINE atan2A #-}
