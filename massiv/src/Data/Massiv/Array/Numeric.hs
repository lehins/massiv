{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
  ( -- * Num
    (.+.)
  , (.+)
  , (+.)
  , (.-.)
  , (.-)
  , (-.)
  , (.*.)
  , (.*)
  , (*.)
  , (.^)
  , (|*|)
  , multiplyTransposed
  , dotProductM
  , identityMatrix
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
  , (./.)
  , (./)
  , (/.)
  , (.^^)
  , recipA
  , fromRationalA
  -- * Floating
  , expA
  , logA
  , sqrtA
  , (.**)
  , (.**.)
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
  -- * Norm
  , normL0
  , normL1
  , normL2
  , normL3
  , normL4
  , normLp
  , normLpi
  , normLpq
  , normLpqi
  , normLinf
  ) where

import Data.Massiv.Array.Delayed.Pull
import Data.Massiv.Array.Delayed.Push
import Data.Massiv.Array.Manifest.Internal
import Data.Massiv.Array.Ops.Fold.Internal (foldlInternal, splitReduce2Internal, splitReduceInternal)
import Data.Massiv.Array.Ops.Map as A
import Data.Massiv.Array.Ops.Transform as A
import Data.Massiv.Core
import Data.Massiv.Core.Common
import Data.Massiv.Core.Index.Internal (Sz(SafeSz))
import Data.Massiv.Core.Operations
import Data.Ratio
import Prelude as P


infixr 8  .^, .^^, .**, .**.
infixl 7  .*., .*, *., ./., ./, /., `quotA`, `remA`, `divA`, `modA`
infixl 6  .+., .+, +., .-., .-, -.


liftNumArray2M ::
     (Load r ix e, NumArray r e, MonadThrow m)
  => (e -> e -> e)
  -> Array r ix e
  -> Array r ix e
  -> m (Array r ix e)
liftNumArray2M f a1 a2
  | size a1 == size a2 = pure $ unsafeLiftNumArray2 f a1 a2
  | otherwise = throwM $ SizeMismatchException (size a1) (size a2)
{-# INLINE liftNumArray2M #-}


applyArray2M ::
     (Load r ix e, MonadThrow m)
  => (Array r ix e -> Array r ix e -> Array r ix e)
  -> Array r ix e
  -> Array r ix e
  -> m (Array r ix e)
applyArray2M f a1 a2
  | size a1 == size a2 = pure $ f a1 a2
  | otherwise = throwM $ SizeMismatchException (size a1) (size a2)
{-# INLINE applyArray2M #-}


-- | Add two arrays together pointwise. Throws `SizeMismatchException` if arrays sizes do
-- not match.
--
-- @since 0.4.0
(.+.) ::
     (Load r ix e, NumArray r e, MonadThrow m) => Array r ix e -> Array r ix e -> m (Array r ix e)
(.+.) = applyArray2M additionPointwise
{-# INLINE (.+.) #-}

-- | Add a scalar to each element of the array. Array is on the left.
--
-- @since 0.1.0
(.+) :: (Index ix, NumArray r e) => Array r ix e -> e -> Array r ix e
(.+) = plusScalar
{-# INLINE (.+) #-}

-- | Add a scalar to each element of the array. Array is on the right.
--
-- @since 0.4.0
(+.) :: (Index ix, NumArray r e) => e -> Array r ix e -> Array r ix e
(+.) = flip plusScalar
{-# INLINE (+.) #-}

-- | Subtract two arrays pointwise. Throws `SizeMismatchException` if arrays sizes do not
-- match.
--
-- @since 0.4.0
(.-.) ::
     (Load r ix e, NumArray r e, MonadThrow m) => Array r ix e -> Array r ix e -> m (Array r ix e)
(.-.) = applyArray2M subtractionPointwise
{-# INLINE (.-.) #-}


-- | Subtract a scalar from each element of the array. Array is on the left.
--
-- @since 0.1.0
(.-) :: (Index ix, NumArray r e) => Array r ix e -> e -> Array r ix e
(.-) = minusScalar
{-# INLINE (.-) #-}

-- | Subtract a scalar from each element of the array. Array is on the right.
--
-- @since 0.4.0
(-.) :: (Index ix, NumArray r e) => e -> Array r ix e -> Array r ix e
(-.) = flip minusScalar
{-# INLINE (-.) #-}


-- | Multiply two arrays together pointwise. [Hadamard
-- product](https://en.wikipedia.org/wiki/Hadamard_product_(matrices)). Throws
-- `SizeMismatchException` when both arrays to not have the same size.
--
-- @since 0.4.0
(.*.) ::
     (Load r ix e, NumArray r e, MonadThrow m) => Array r ix e -> Array r ix e -> m (Array r ix e)
(.*.) = applyArray2M multiplicationPointwise
{-# INLINE (.*.) #-}

-- | Multiply by a scalar each element of the array. Array is on the left.
--
-- @since 0.4.0
(.*) :: (Index ix, NumArray r e) => Array r ix e -> e -> Array r ix e
(.*) = multiplyScalar
{-# INLINE (.*) #-}

-- | Multiply by a scalar each element of the array. Array is on the right.
--
-- @since 0.4.0
(*.) :: (Index ix, NumArray r e) => e -> Array r ix e -> Array r ix e
(*.) = flip multiplyScalar
{-# INLINE (*.) #-}


-- | Raise each element in the array to some non-negative power. Throws `NegativeValue` on
-- negative powers.
--
-- @since 0.4.1
(.^) ::
     (Construct r ix e, Load r ix e, NumArray r e, MonadThrow m)
  => Array r ix e
  -> Int
  -> m (Array r ix e)
(.^) arr p
  | p < 0 = throwM $ NegativeValue p
  | p == 0 = pure $ setComp (getComp arr) $ makeConstantArray (size arr) 1
  | otherwise = pure $ powerScalar arr p
{-# INLINE (.^) #-}

-- | Perform matrix multiplication. First array must have exactly the same number of rows
-- as the second array columns, otherwise `SizeMismatchException`.
--
-- @since 0.4.0
(|*|) ::
     (Mutable r Ix2 e, Source r' Ix2 e, ReduceNumArray r e, MonadThrow m)
  => Matrix r e -- ^ First array
  -> Matrix r' e -- ^ Second array
  -> m (Matrix r e)
(|*|) a1 a2 = compute <$> multArrs a1 a2
{-# INLINE [1] (|*|) #-}

{-# RULES
"multDoubleTranspose" [~1] forall arr1 arr2 . arr1 |*| transpose arr2 =
    multiplyTransposedFused arr1 (convert arr2)
 #-}

multiplyTransposedFused ::
     (Mutable r Ix2 e, ReduceNumArray r e, MonadThrow m)
  => Matrix r e
  -> Matrix r e
  -> m (Matrix r e)
multiplyTransposedFused arr1 arr2 = compute <$> multiplyTransposed arr1 arr2
{-# INLINE multiplyTransposedFused #-}


multArrs ::
     forall r r' e m. (Mutable r Ix2 e, Source r' Ix2 e, ReduceNumArray r e, MonadThrow m)
  => Matrix r e
  -> Matrix r' e
  -> m (Matrix D e)
multArrs arr1 arr2 = multiplyTransposed arr1 arr2'
  where
    arr2' :: Matrix r e
    arr2' = compute $ transpose arr2
{-# INLINE multArrs #-}

-- | Perform matrix multiplication, where the second array is in a transformed form. Inner
-- dimensions must agree, otherwise `SizeMismatchException`.
--
-- @since 0.4.0
multiplyTransposed ::
     (Manifest r Ix2 e, ReduceNumArray r e, MonadThrow m)
  => Matrix r e
  -> Matrix r e
  -> m (Matrix D e)
multiplyTransposed arr1 arr2
  | n1 /= m2 = throwM $ SizeMismatchException (size arr1) (size arr2)
  | otherwise =
    pure $
    DArray (getComp arr1 <> getComp arr2) (SafeSz (m1 :. n2)) $ \(i :. j) ->
      multiplySumArrayS
        (unsafeLinearSlice (i * n1) (SafeSz n1) arr1)
        (unsafeLinearSlice (j * n1) (SafeSz n1) arr2)
  where
    SafeSz (m1 :. n1) = size arr1
    SafeSz (n2 :. m2) = size arr2
{-# INLINE multiplyTransposed #-}


dotProductM ::
     (Source r ix e, ReduceNumArray r e, MonadThrow f) => Array r ix e -> Array r ix e -> f e
dotProductM arr1 arr2
  | size arr1 == size arr2 = pure $ splitReduce2Internal multiplySumArrayS (+) 0 arr1 arr2
  | otherwise = throwM $ SizeMismatchException (size arr1) (size arr2)
{-# INLINE dotProductM #-}



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
identityMatrix :: Sz1 -> Matrix DL Int
identityMatrix (Sz n) = makeLoadArrayS (Sz2 n n) 0 $ \ w -> loopM_ 0 (< n) (+1) $ \ i -> w (i :. i) 1
{-# INLINE identityMatrix #-}


negateA :: (Index ix, NumArray r e) => Array r ix e -> Array r ix e
negateA = (0 -.)
{-# INLINE negateA #-}

absA :: (Index ix, NumArray r e) => Array r ix e -> Array r ix e
absA = absPointwise
{-# INLINE absA #-}

signumA :: (Index ix, NumArray r e) => Array r ix e -> Array r ix e
signumA = liftNumArray signum
{-# INLINE signumA #-}

fromIntegerA :: (Index ix, Num e) => Integer -> Array D ix e
fromIntegerA = singleton . fromInteger
{-# INLINE fromIntegerA #-}

(./.) ::
     (Load r ix e, FloatArray r e, MonadThrow m)
  => Array r ix e
  -> Array r ix e
  -> m (Array r ix e)
(./.) = applyArray2M divisionPointwise
{-# INLINE (./.) #-}

(./) ::(Index ix,  FloatArray r e) => Array r ix e -> e -> Array r ix e
(./) = divideScalar
{-# INLINE (./) #-}

(/.) ::(Index ix,  FloatArray r e) => e -> Array r ix e -> Array r ix e
(/.) = flip recipMultiplyScalar
{-# INLINE (/.) #-}

(.^^)
  :: (Index ix, NumArray r e, Fractional e, Integral b)
  => Array r ix e -> b -> Array r ix e
(.^^) arr n = liftNumArray (^^ n) arr
{-# INLINE (.^^) #-}

recipA :: (Index ix, FloatArray r e) => Array r ix e -> Array r ix e
recipA = recipPointwise
{-# INLINE recipA #-}


fromRationalA
  :: (Index ix, Fractional e)
  => Rational -> Array D ix e
fromRationalA = singleton . fromRational
{-# INLINE fromRationalA #-}

expA :: (Index ix, FloatArray r e) => Array r ix e -> Array r ix e
expA = liftNumArray exp
{-# INLINE expA #-}

sqrtA :: (Index ix, FloatArray r e) => Array r ix e -> Array r ix e
sqrtA = sqrtPointwise
{-# INLINE sqrtA #-}

logA :: (Index ix, FloatArray r e) => Array r ix e -> Array r ix e
logA = liftNumArray log
{-# INLINE logA #-}

logBaseA
  :: (Source r1 ix e, Source r2 ix e, Floating e, MonadThrow m)
  => Array r1 ix e -> Array r2 ix e -> m (Array D ix e)
logBaseA = liftArray2M logBase
{-# INLINE logBaseA #-}

(.**) :: (Source r ix e, Floating e) => Array r ix e -> e -> Array D ix e
(.**) arr p = liftArray (** p) arr
{-# INLINE (.**) #-}


(.**.)
  :: (Source r1 ix e, Source r2 ix e, Floating e, MonadThrow m)
  => Array r1 ix e -> Array r2 ix e -> m (Array D ix e)
(.**.) = liftArray2M (**)
{-# INLINE (.**.) #-}



sinA :: (Index ix, FloatArray r e) => Array r ix e -> Array r ix e
sinA = liftNumArray sin
{-# INLINE sinA #-}

cosA :: (Index ix, FloatArray r e) => Array r ix e -> Array r ix e
cosA = liftNumArray cos
{-# INLINE cosA #-}

tanA :: (Index ix, FloatArray r e) => Array r ix e -> Array r ix e
tanA = liftNumArray cos
{-# INLINE tanA #-}

asinA :: (Index ix, FloatArray r e) => Array r ix e -> Array r ix e
asinA = liftNumArray asin
{-# INLINE asinA #-}

atanA :: (Index ix, FloatArray r e) => Array r ix e -> Array r ix e
atanA = liftNumArray atan
{-# INLINE atanA #-}

acosA :: (Index ix, FloatArray r e) => Array r ix e -> Array r ix e
acosA = liftNumArray acos
{-# INLINE acosA #-}

sinhA :: (Index ix, FloatArray r e) => Array r ix e -> Array r ix e
sinhA = liftNumArray sinh
{-# INLINE sinhA #-}

tanhA :: (Index ix, FloatArray r e) => Array r ix e -> Array r ix e
tanhA = liftNumArray cos
{-# INLINE tanhA #-}

coshA :: (Index ix, FloatArray r e) => Array r ix e -> Array r ix e
coshA = liftNumArray cosh
{-# INLINE coshA #-}

asinhA :: (Index ix, FloatArray r e) => Array r ix e -> Array r ix e
asinhA = liftNumArray asinh
{-# INLINE asinhA #-}

acoshA :: (Index ix, FloatArray r e) => Array r ix e -> Array r ix e
acoshA = liftNumArray acosh
{-# INLINE acoshA #-}

atanhA :: (Index ix, FloatArray r e) => Array r ix e -> Array r ix e
atanhA = liftNumArray atanh
{-# INLINE atanhA #-}


quotA
  :: (Source r1 ix e, Source r2 ix e, Integral e, MonadThrow m)
  => Array r1 ix e -> Array r2 ix e -> m (Array D ix e)
quotA = liftArray2M quot
{-# INLINE quotA #-}


remA
  :: (Source r1 ix e, Source r2 ix e, Integral e, MonadThrow m)
  => Array r1 ix e -> Array r2 ix e -> m (Array D ix e)
remA = liftArray2M rem
{-# INLINE remA #-}

divA
  :: (Source r1 ix e, Source r2 ix e, Integral e, MonadThrow m)
  => Array r1 ix e -> Array r2 ix e -> m (Array D ix e)
divA = liftArray2M div
{-# INLINE divA #-}

modA
  :: (Source r1 ix e, Source r2 ix e, Integral e, MonadThrow m)
  => Array r1 ix e -> Array r2 ix e -> m (Array D ix e)
modA = liftArray2M mod
{-# INLINE modA #-}



quotRemA
  :: (Source r1 ix e, Source r2 ix e, Integral e, MonadThrow m)
  => Array r1 ix e -> Array r2 ix e -> m (Array D ix e, Array D ix e)
quotRemA arr1 arr2 = A.unzip <$> liftArray2M @D quotRem arr1 arr2
{-# INLINE quotRemA #-}


divModA
  :: (Source r1 ix e, Source r2 ix e, Integral e, MonadThrow m)
  => Array r1 ix e -> Array r2 ix e -> m (Array D ix e, Array D ix e)
divModA arr1 arr2 = A.unzip <$> liftArray2M @D divMod arr1 arr2
{-# INLINE divModA #-}



truncateA ::
     (Source r ix a, RealFrac a, Integral e) => Array r ix a -> Array D ix e
truncateA = A.map truncate
{-# INLINE truncateA #-}


roundA ::
     (Source r ix a, RealFrac a, Integral e) => Array r ix a -> Array D ix e
roundA = A.map round
{-# INLINE roundA #-}


ceilingA ::
     (Source r ix a, RealFrac a, Integral e) => Array r ix a -> Array D ix e
ceilingA = A.map ceiling
{-# INLINE ceilingA #-}


floorA ::
     (Source r ix a, RealFrac a, Integral e) => Array r ix a -> Array D ix e
floorA = A.map floor
{-# INLINE floorA #-}

atan2A ::
     (Load r ix e, NumArray r e, RealFloat e, MonadThrow m)
  => Array r ix e
  -> Array r ix e
  -> m (Array r ix e)
atan2A = liftNumArray2M atan2
{-# INLINE atan2A #-}

normL0 :: (Source r ix e, Eq e, Num e) => Array r ix e -> Int
normL0 = foldlInternal (\acc e -> if e == 0 then acc + 1 else acc) 0 (+) 0
{-# INLINE normL0 #-}

normL1 :: (Source r ix e, ReduceNumArray r e) => Array r ix e -> e
normL1 = splitReduceInternal (`absPowerSumArrayS` 1) (+) 0
{-# INLINE normL1 #-}

normL2 :: (Source r ix e, ReduceNumArray r e, Floating e) => Array r ix e -> e
normL2 = sqrt . splitReduceInternal (`evenPowerSumArrayS` 2) (+) 0
{-# INLINE normL2 #-}

normL3 :: (Source r ix e, ReduceNumArray r e, Floating e) => Array r ix e -> e
normL3 arr = splitReduceInternal (`absPowerSumArrayS` 3) (+) 0 arr ** (1 / fromIntegral (3 :: Int))
{-# INLINE normL3 #-}

normL4 :: (Source r ix e, ReduceNumArray r e, Floating e) => Array r ix e -> e
normL4 = sqrt . sqrt . splitReduceInternal (`evenPowerSumArrayS` 4) (+) 0
{-# INLINE normL4 #-}


-- | Same as `normLp`, except it only accepts an integral power, hence is more efficient.
--
-- @since 0.4.1
normLpi ::
     (Source r ix e, ReduceNumArray r e, Floating e, Eq e, MonadThrow m)
  => Int
  -> Array r ix e
  -> m e
normLpi n arr
  | n < 0 = throwM $ NegativeValue n
  | n == 0 = pure $ fromIntegral $ normL0 arr
  | even n = pure $ splitReduceInternal (`evenPowerSumArrayS` n) (+) 0 arr ** (1 / fromIntegral n)
  | otherwise = pure $ splitReduceInternal (`absPowerSumArrayS` n) (+) 0 arr ** (1 / fromIntegral n)
{-# INLINE normLpi #-}


-- | Compute L/p/-norm
--
-- @since 0.4.1
normLp ::
     (Source r ix e, NumArray r e, Floating e, MonadThrow m)
  => Rational -- ^ @p@ - a real value, where @p >= 1@
  -> Array r ix e
  -> m e
normLp p arr
  | p < 1 = throwM $ InvalidLowerBound p True 1
  | otherwise =
    let p' = fromRational p
        rp' = fromRational (recip p)
        arr' =
          if even (numerator p)
            then powerScalar arr (fromInteger (numerator p)) .** fromRational (1 % denominator p)
            else A.map abs arr .** p'
     in pure (splitReduceInternal sumArrayS (+) 0 arr' ** rp')
{-# INLINE normLp #-}



-- | Infinity norm.
--
-- @since 0.4.1
normLinf :: (Source r ix e, ReduceNumArray r e, Ord e) => Array r ix e -> e
normLinf = splitReduceInternal absMaxArrayS (\acc -> max acc . abs) 0
{-# INLINE normLinf #-}


-- | Compute L/p,q/-norm of a matrix
--
-- @since 0.4.1
normLpq ::
     forall r e m.
     ( Mutable r Ix2 e
     , Mutable r Ix1 e
     , NumArray r e
     , Floating e
     , MonadThrow m
     )
  => Rational -- ^ @p@ - a real value, where @p >= 1@
  -> Rational -- ^ @q@ - a real value, where @q >= 1@
  -> Matrix r e
  -> m e
normLpq p q arr
  | p < 1 = throwM $ InvalidLowerBound p True 1
  | q < 1 = throwM $ InvalidLowerBound q True 1
  | isEmpty arr = pure 0
  | otherwise =
    let arr' = compute (transpose arr) :: Matrix r e
        Sz2 m n = size arr
        vec =
          makeArray (getComp arr) (SafeSz n) $ \j ->
            either throw id $ normLp p (setComp Seq (unsafeLinearSlice (j * m) (SafeSz m) arr'))
     in normLp q (vec :: Vector r e)
{-# INLINE normLpq #-}



-- | Compute L/p,q/-norm of a matrix. For efficiency, both @p@ and @q@ are integers,
--
-- @since 0.4.1
normLpqi ::
     forall r e m.
     ( Mutable r Ix2 e
     , Mutable r Ix1 e
     , ReduceNumArray r e
     , Floating e
     , Eq e
     , MonadThrow m
     )
  => Int -- ^ @p@ - a real value, where @p >= 1@
  -> Int -- ^ @q@ - a real value, where @q >= 1@
  -> Matrix r e
  -> m e
normLpqi p q arr
  | p < 1 = throwM $ InvalidLowerBound p True 1
  | q < 1 = throwM $ InvalidLowerBound q True 1
  | isEmpty arr = pure 0
  | otherwise =
    let arr' = compute (transpose arr) :: Matrix r e
        Sz2 m n = size arr
        vec =
          makeArray (getComp arr) (SafeSz n) $ \j ->
            either throw id $ normLpi p (setComp Seq (unsafeLinearSlice (j * m) (SafeSz m) arr'))
     in normLpi q (vec :: Vector r e)
{-# INLINE normLpqi #-}
