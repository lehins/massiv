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
  -- * Norm
  , normL1
  , normL2
  , normL3
  , normL4
  , normLn
  ) where

import Data.Massiv.Array.Delayed.Pull
import Data.Massiv.Array.Delayed.Push
import Data.Massiv.Array.Manifest.Internal
import Data.Massiv.Array.Ops.Fold.Internal (splitReduce2Internal,
                                            splitReduceInternal)
import Data.Massiv.Array.Ops.Map as A
import Data.Massiv.Array.Ops.Transform as A
import Data.Massiv.Core
import Data.Massiv.Core.Common
import Data.Massiv.Core.Index.Internal (Sz(SafeSz))
import Data.Massiv.Core.Operations
import Prelude as P


infixr 8  .^, .^^
infixl 7  .*., .*, *., ./., ./, /., `quotA`, `remA`, `divA`, `modA`
infixl 6  .+., .+, +., .-., .-, -.

liftArray2Matching
  :: (Source r1 ix a, Source r2 ix b)
  => (a -> b -> e) -> Array r1 ix a -> Array r2 ix b -> Array D ix e
liftArray2Matching f !arr1 !arr2
  | sz1 == sz2 =
    makeArray
      (getComp arr1 <> getComp arr2)
      sz1
      (\ !ix -> f (unsafeIndex arr1 ix) (unsafeIndex arr2 ix))
  | otherwise = throw $ SizeMismatchException sz1 sz2
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


-- | Add two arrays together pointwise. Throws `SizeMismatchException` if arrays sizes do
-- not match.
--
-- @since 0.4.0
(.+.) ::
     (Load r ix e, Numeric r e, MonadThrow m) => Array r ix e -> Array r ix e -> m (Array r ix e)
(.+.) = liftNumericArray2M additionPointwise
{-# INLINE (.+.) #-}

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

-- | Subtract two arrays pointwise. Throws `SizeMismatchException` if arrays sizes do not
-- match.
--
-- @since 0.4.0
(.-.) ::
     (Load r ix e, Numeric r e, MonadThrow m) => Array r ix e -> Array r ix e -> m (Array r ix e)
(.-.) = liftNumericArray2M subtractionPointwise
{-# INLINE (.-.) #-}


-- | Subtract a scalar from each element of the array. Array is on the left.
--
-- @since 0.1.0
(.-) :: (Index ix, Numeric r e) => Array r ix e -> e -> Array r ix e
(.-) = minusScalar
{-# INLINE (.-) #-}

-- | Subtract a scalar from each element of the array. Array is on the right.
--
-- @since 0.4.0
(-.) :: (Index ix, Numeric r e) => e -> Array r ix e -> Array r ix e
(-.) = flip minusScalar
{-# INLINE (-.) #-}


-- | Hadamard product. Multiply two arrays together pointwise.
--
-- @since 0.4.0
(.*.) ::
     (Load r ix e, Numeric r e, MonadThrow m) => Array r ix e -> Array r ix e -> m (Array r ix e)
(.*.) = liftNumericArray2M multiplicationPointwise
{-# INLINE (.*.) #-}

(.*) :: (Index ix, Numeric r e) => Array r ix e -> e -> Array r ix e
(.*) = multiplyScalar
{-# INLINE (.*) #-}

(*.) :: (Index ix, Numeric r e) => e -> Array r ix e -> Array r ix e
(*.) = flip multiplyScalar
{-# INLINE (*.) #-}

(.^) :: (Index ix, Numeric r e) => Array r ix e -> Int -> Array r ix e
(.^) = powerPointwise
{-# INLINE (.^) #-}

-- | Perform matrix multiplication. Inner dimensions must agree, otherwise `SizeMismatchException`.
(|*|) ::
     (Mutable r Ix2 e, Source r' Ix2 e, ReduceNumeric r e, MonadThrow m)
  => Array r Ix2 e
  -> Array r' Ix2 e
  -> m (Array r Ix2 e)
(|*|) a1 a2 = compute <$> multArrs a1 a2
{-# INLINE [1] (|*|) #-}

{-# RULES
"multDoubleTranspose" [~1] forall arr1 arr2 . arr1 |*| transpose arr2 =
    multiplyTransposedFused arr1 (convert arr2)
 #-}

multiplyTransposedFused ::
     (Mutable r Ix2 e, ReduceNumeric r e, MonadThrow m)
  => Array r Ix2 e
  -> Array r Ix2 e
  -> m (Array r Ix2 e)
multiplyTransposedFused arr1 arr2 = compute <$> multiplyTransposed arr1 arr2
{-# INLINE multiplyTransposedFused #-}


multArrs ::
     forall r r' e m. (Mutable r Ix2 e, Source r' Ix2 e, ReduceNumeric r e, MonadThrow m)
  => Array r Ix2 e
  -> Array r' Ix2 e
  -> m (Array D Ix2 e)
multArrs arr1 arr2 = multiplyTransposed arr1 arr2'
  where
    arr2' :: Array r Ix2 e
    arr2' = compute $ transpose arr2
{-# INLINE multArrs #-}

multiplyTransposed ::
     (Manifest r Ix2 e, ReduceNumeric r e, MonadThrow m)
  => Array r Ix2 e
  -> Array r Ix2 e
  -> m (Array D Ix2 e)
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


dotProductM :: (Source r ix e, ReduceNumeric r e, MonadThrow f) => Array r ix e -> Array r ix e -> f e
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
identityMatrix :: Sz1 -> Array DL Ix2 Int
identityMatrix (Sz n) = makeLoadArrayS (Sz2 n n) 0 $ \ w -> loopM_ 0 (< n) (+1) $ \ i -> w (i :. i) 1
{-# INLINE identityMatrix #-}


negateA :: (Index ix, Numeric r e) => Array r ix e -> Array r ix e
negateA = unsafeLiftArray negate
{-# INLINE negateA #-}

absA :: (Index ix, Numeric r e) => Array r ix e -> Array r ix e
absA = absPointwise
{-# INLINE absA #-}

signumA :: (Index ix, Numeric r e) => Array r ix e -> Array r ix e
signumA = unsafeLiftArray signum
{-# INLINE signumA #-}

fromIntegerA :: (Index ix, Num e) => Integer -> Array D ix e
fromIntegerA = singleton . fromInteger
{-# INLINE fromIntegerA #-}

(./.) ::
     (Load r ix e, NumericFloat r e, MonadThrow m)
  => Array r ix e
  -> Array r ix e
  -> m (Array r ix e)
(./.) = liftNumericArray2M divisionPointwise
{-# INLINE (./.) #-}

(./) ::(Index ix,  NumericFloat r e) => Array r ix e -> e -> Array r ix e
(./) = divideScalar
{-# INLINE (./) #-}

(/.) ::(Index ix,  NumericFloat r e) => e -> Array r ix e -> Array r ix e
(/.) = flip recipMultiplyScalar
{-# INLINE (/.) #-}

(.^^)
  :: (Index ix, Numeric r e, Fractional e, Integral b)
  => Array r ix e -> b -> Array r ix e
(.^^) arr n = unsafeLiftArray (^^ n) arr
{-# INLINE (.^^) #-}

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
tanA = unsafeLiftArray cos
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
tanhA = unsafeLiftArray cos
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
quotRemA arr1 = A.unzip . liftArray2Matching quotRem arr1
{-# INLINE quotRemA #-}


divModA
  :: (Source r1 ix e, Source r2 ix e, Integral e)
  => Array r1 ix e -> Array r2 ix e -> (Array D ix e, Array D ix e)
divModA arr1 = A.unzip . liftArray2Matching divMod arr1
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
     (Load r ix e, Numeric r e, RealFloat e, MonadThrow m)
  => Array r ix e
  -> Array r ix e
  -> m (Array r ix e)
atan2A = liftArray2M atan2
{-# INLINE atan2A #-}

-- normL0 :: (Source r ix e, Numeric r e) => Array r ix e -> e
-- normL0 = -- count non-zero
-- {-# INLINE normL0 #-}

normL1 :: (Source r ix e, ReduceNumeric r e) => Array r ix e -> e
normL1 = splitReduceInternal (`absPowerSumArrayS` 1) (+) 0
{-# INLINE normL1 #-}

normL2 :: (Source r ix e, ReduceNumeric r e, Floating e) => Array r ix e -> e
normL2 = sqrt . splitReduceInternal (`evenPowerSumArrayS` 2) (+) 0
{-# INLINE normL2 #-}

normL3 :: (Source r ix e, ReduceNumeric r e, Floating e) => Array r ix e -> e
normL3 arr = splitReduceInternal (`absPowerSumArrayS` 3) (+) 0 arr ** (1 / fromIntegral (3 :: Int))
{-# INLINE normL3 #-}

normL4 :: (Source r ix e, ReduceNumeric r e, Floating e) => Array r ix e -> e
normL4 = sqrt . sqrt . splitReduceInternal (`evenPowerSumArrayS` 4) (+) 0
{-# INLINE normL4 #-}

normLn :: (Source r ix e, ReduceNumeric r e, Floating e, MonadThrow m) => Int -> Array r ix e -> m e
normLn n arr
  | n < 0 = throwM $ NegativeValue n
 --  | n == 0 = pure $ normL0 arr
  | otherwise = pure $ splitReduceInternal (`absPowerSumArrayS` n) (+) 0 arr ** (1 / fromIntegral n)
{-# INLINE normLn #-}

-- normLinfinity :: (Source r ix e, Numeric r e) => Array r ix e -> e
-- normLinfinity = absMaximum
-- {-# INLINE normLinfinity #-}
