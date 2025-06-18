{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Massiv.Array.Numeric
-- Copyright   : (c) Alexey Kuleshevich 2018-2025
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
module Data.Massiv.Array.Numeric (
  -- * Numeric
  Numeric,
  NumericFloat,
  liftNumArray2M,

  -- ** Pointwise addition
  (.+),
  (+.),
  (.+.),
  (!+!),
  sumArraysM,
  sumArrays',

  -- ** Pointwise subtraction
  (.-),
  (-.),
  (.-.),
  (!-!),

  -- ** Pointwise multiplication
  (.*),
  (*.),
  (.*.),
  (!*!),
  (.^),
  productArraysM,
  productArrays',

  -- ** Dot product
  (!.!),
  dotM,

  -- ** Matrix multiplication
  (.><),
  (!><),
  multiplyMatrixByVector,
  (><.),
  (><!),
  multiplyVectorByMatrix,
  (.><.),
  (!><!),
  multiplyMatrices,
  multiplyMatricesTransposed,

  -- * Norms
  normL2,

  -- * Simple matrices
  identityMatrix,
  lowerTriangular,
  upperTriangular,
  negateA,
  absA,
  signumA,

  -- * Integral
  quotA,
  remA,
  divA,
  modA,
  quotRemA,
  divModA,

  -- * Fractional
  (./),
  (/.),
  (./.),
  (!/!),
  (.^^),
  recipA,

  -- * Floating
  expA,
  logA,
  sqrtA,
  (.**),
  logBaseA,
  sinA,
  cosA,
  tanA,
  asinA,
  acosA,
  atanA,
  sinhA,
  coshA,
  tanhA,
  asinhA,
  acoshA,
  atanhA,

  -- * RealFrac
  truncateA,
  roundA,
  ceilingA,
  floorA,

  -- * RealFloat
  atan2A,
) where

import Control.Monad (when)
import Control.Scheduler
import qualified Data.Foldable as F
import Data.Function
import Data.Massiv.Array.Delayed.Pull
import Data.Massiv.Array.Delayed.Push
import Data.Massiv.Array.Manifest.Internal
import Data.Massiv.Array.Ops.Construct
import Data.Massiv.Array.Ops.Map as A
import Data.Massiv.Core
import Data.Massiv.Core.Common as A
import Data.Massiv.Core.Operations
import System.IO.Unsafe
import Prelude as P

infixr 8 .^, .^^

infixl 7 !*!, .*., .*, *., !/!, ./., ./, /., `quotA`, `remA`, `divA`, `modA`

infixl 6 !+!, .+., .+, +., !-!, .-., .-, -.

-- | Similar to `liftArray2M`, except it can be applied only to representations
-- with `Numeric` instance and result representation stays the same.
--
-- @since 1.0.0
liftNumArray2M
  :: (Index ix, Numeric r e, MonadThrow m)
  => (e -> e -> e)
  -> Array r ix e
  -> Array r ix e
  -> m (Array r ix e)
liftNumArray2M f a1 a2
  | size a1 == size a2 = pure $ unsafeLiftArray2 f a1 a2
  | isZeroSz sz1 && isZeroSz sz2 = pure $ unsafeResize zeroSz a1
  | otherwise = throwM $ SizeMismatchException sz1 sz2
  where
    !sz1 = size a1
    !sz2 = size a2
{-# INLINE liftNumArray2M #-}

applyExactSize2M
  :: (Index ix, Size r, MonadThrow m)
  => (Array r ix e -> Array r ix e -> Array r ix e)
  -> Array r ix e
  -> Array r ix e
  -> m (Array r ix e)
applyExactSize2M f a1 a2
  | size a1 == size a2 = pure $! f a1 a2
  | isZeroSz sz1 && isZeroSz sz2 = pure $! unsafeResize zeroSz a1
  | otherwise = throwM $! SizeMismatchException sz1 sz2
  where
    !sz1 = size a1
    !sz2 = size a2
{-# INLINE applyExactSize2M #-}

-- | Add two arrays together pointwise. Same as `!+!` but produces monadic computation
-- that allows for handling failure.
--
-- /__Throws Exception__/: `SizeMismatchException` when array sizes do not match.
--
-- @since 0.4.0
(.+.) :: (Index ix, Numeric r e, MonadThrow m) => Array r ix e -> Array r ix e -> m (Array r ix e)
(.+.) = applyExactSize2M additionPointwise
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
(!+!) :: (HasCallStack, Index ix, Numeric r e) => Array r ix e -> Array r ix e -> Array r ix e
(!+!) a1 a2 = throwEither (a1 .+. a2)
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
-- /__Throws Exception__/: `SizeMismatchException` when array sizes do not match.
--
-- @since 0.4.0
(.-.)
  :: (Index ix, Numeric r e, MonadThrow m) => Array r ix e -> Array r ix e -> m (Array r ix e)
(.-.) = applyExactSize2M subtractionPointwise
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
(!-!) :: (Index ix, Numeric r e) => Array r ix e -> Array r ix e -> Array r ix e
(!-!) a1 a2 = throwEither (a1 .-. a2)
{-# INLINE (!-!) #-}

-- | Subtract a scalar from each element of the array. Array is on the left.
--
-- @since 0.4.0
(.-) :: (Index ix, Numeric r e) => Array r ix e -> e -> Array r ix e
(.-) = minusScalar
{-# INLINE (.-) #-}

-- | Subtract each element of the array from a scalar. Array is on the right.
--
-- @since 0.5.6
(-.) :: (Index ix, Numeric r e) => e -> Array r ix e -> Array r ix e
(-.) = scalarMinus
{-# INLINE (-.) #-}

-- | Multiply two arrays together pointwise. Same as `!*!` but produces monadic
-- computation that allows for handling failure.
--
-- /__Throws Exception__/: `SizeMismatchException` when array sizes do not match.
--
-- @since 0.4.0
(.*.)
  :: (Index ix, Numeric r e, MonadThrow m) => Array r ix e -> Array r ix e -> m (Array r ix e)
(.*.) = applyExactSize2M multiplicationPointwise
{-# INLINE (.*.) #-}

-- | Multiplication of two arrays pointwise,
-- i.e. <https://en.wikipedia.org/wiki/Hadamard_product_(matrices) Hadamard product>.
-- Prefer to use monadic version of this function `.*.` whenever possible,
-- because it is better to avoid partial functions.
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
(!*!) :: (Index ix, Numeric r e) => Array r ix e -> Array r ix e -> Array r ix e
(!*!) a1 a2 = throwEither (a1 .*. a2)
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

-- | Dot product of two vectors.
--
-- [Partial] Throws an impure exception when lengths of vectors do not match
--
-- @since 0.5.6
(!.!) :: (Numeric r e, Source r e) => Vector r e -> Vector r e -> e
(!.!) v1 v2 = throwEither $ dotM v1 v2
{-# INLINE (!.!) #-}

-- | Dot product of two vectors.
--
-- /__Throws Exception__/: `SizeMismatchException` when lengths of vectors do not match
--
-- @since 0.5.6
dotM :: (FoldNumeric r e, Source r e, MonadThrow m) => Vector r e -> Vector r e -> m e
dotM v1 v2
  | size v1 /= size v2 = throwM $ SizeMismatchException (size v1) (size v2)
  | comp == Seq = pure $! unsafeDotProduct v1 v2
  | otherwise = pure $! unsafePerformIO $ unsafeDotProductIO v1 v2
  where
    comp = getComp v1 <> getComp v2
{-# INLINE dotM #-}

unsafeDotProductIO
  :: (MonadUnliftIO m, Index ix, FoldNumeric r b, Source r b)
  => Array r ix b
  -> Array r ix b
  -> m b
unsafeDotProductIO v1 v2 = do
  results <-
    withScheduler comp $ \scheduler ->
      splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> liftIO $ do
        let n = SafeSz chunkLength
        loopA_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
          scheduleWork scheduler $
            pure $!
              unsafeDotProduct (unsafeLinearSlice start n v1) (unsafeLinearSlice start n v2)
        when (slackStart < totalLength) $ do
          let k = SafeSz (totalLength - slackStart)
          scheduleWork scheduler $
            pure $!
              unsafeDotProduct (unsafeLinearSlice slackStart k v1) (unsafeLinearSlice slackStart k v2)
  pure $! F.foldl' (+) 0 results
  where
    totalLength = totalElem (size v1)
    comp = getComp v1 <> getComp v2
{-# INLINE unsafeDotProductIO #-}

-- | Compute L2 norm of an array.
--
-- @since 0.5.6
normL2 :: (FoldNumeric r e, Source r e, Index ix, Floating e) => Array r ix e -> e
normL2 v
  | getComp v == Seq = sqrt $! powerSumArray v 2
  | otherwise = sqrt $! unsafePerformIO $ powerSumArrayIO v 2
{-# INLINE normL2 #-}

powerSumArrayIO
  :: (MonadUnliftIO m, Index ix, FoldNumeric r b, Source r b)
  => Array r ix b
  -> Int
  -> m b
powerSumArrayIO v p = do
  results <-
    withScheduler (getComp v) $ \scheduler ->
      splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> liftIO $ do
        let n = SafeSz chunkLength
        loopA_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
          scheduleWork scheduler $ pure $! powerSumArray (unsafeLinearSlice start n v) p
        when (slackStart < totalLength) $ do
          let k = SafeSz (totalLength - slackStart)
          scheduleWork scheduler $ pure $! powerSumArray (unsafeLinearSlice slackStart k v) p
  pure $! F.foldl' (+) 0 results
  where
    totalLength = totalElem (size v)
{-# INLINE powerSumArrayIO #-}

-- | Multiply a matrix by a column vector. Same as `!><` but produces monadic
-- computation that allows for handling failure.
--
-- /__Throws Exception__/: `SizeMismatchException` when inner dimensions of arrays do not match.
--
-- @since 0.5.6
(.><)
  :: (MonadThrow m, FoldNumeric r e, Source r e)
  => Matrix r e
  -- ^ Matrix
  -> Vector r e
  -- ^ Column vector (Used many times, so make sure it is computed)
  -> m (Vector D e)
(.><) mm v
  | mCols /= n = throwM $ SizeMismatchException (size mm) (Sz2 n 1)
  | mRows == 0 || mCols == 0 = pure $ setComp comp empty
  | otherwise = pure $ makeArray comp (Sz1 mRows) $ \i ->
      unsafeDotProduct (unsafeLinearSlice (i * n) sz mm) v
  where
    comp = getComp mm <> getComp v
    Sz2 mRows mCols = size mm
    sz@(Sz1 n) = size v
{-# INLINE (.><) #-}

-- | Multiply matrix by a column vector. Same as `.><` but returns computed version of a vector
--
-- /__Throws Exception__/: `SizeMismatchException` when inner dimensions of arrays do not match.
--
-- @since 0.5.7
multiplyMatrixByVector
  :: (MonadThrow m, Numeric r e, Manifest r e)
  => Matrix r e
  -- ^ Matrix
  -> Vector r e
  -- ^ Column vector (Used many times, so make sure it is computed)
  -> m (Vector r e)
multiplyMatrixByVector mm v = compute <$> mm .>< v
{-# INLINE multiplyMatrixByVector #-}

-- | Multiply a matrix by a column vector
--
-- [Partial] Throws impure exception when inner dimensions do not agree
--
-- @since 0.5.6
(!><)
  :: (Numeric r e, Source r e)
  => Matrix r e
  -- ^ Matrix
  -> Vector r e
  -- ^ Column vector (Used many times, so make sure it is computed)
  -> Vector D e
(!><) mm v = throwEither (mm .>< v)
{-# INLINE (!><) #-}

-- | Multiply a row vector by a matrix. Same as `><!` but produces monadic computation
-- that allows for handling failure.
--
-- /__Throws Exception__/: `SizeMismatchException` when inner dimensions of arrays do not match.
--
-- @since 0.5.6
(><.)
  :: (MonadThrow m, Numeric r e, Manifest r e)
  => Vector r e
  -- ^ Row vector
  -> Matrix r e
  -- ^ Matrix
  -> m (Vector r e)
(><.) = multiplyVectorByMatrix
{-# INLINE (><.) #-}

-- | Multiply a row vector by a matrix. Same as `><.` but returns computed vector instead of
-- a delayed one.
--
-- /__Throws Exception__/: `SizeMismatchException` when inner dimensions of arrays do not match.
--
-- @since 0.5.7
multiplyVectorByMatrix
  :: (MonadThrow m, Numeric r e, Manifest r e)
  => Vector r e
  -- ^ Row vector
  -> Matrix r e
  -- ^ Matrix
  -> m (Vector r e)
multiplyVectorByMatrix v mm
  | mRows /= n = throwM $ SizeMismatchException (Sz2 1 n) (size mm)
  | mRows == 0 || mCols == 0 = pure $ runST (unsafeFreeze comp =<< unsafeNew zeroSz)
  | otherwise =
      pure $!
        unsafePerformIO $ do
          mv <- newMArray (Sz mCols) 0
          withMassivScheduler_ comp $ \scheduler -> do
            let loopCols x ivto =
                  fix $ \go im iv ->
                    when (iv < ivto) $ do
                      _ <- unsafeLinearModify mv (\a -> pure $ a + unsafeLinearIndex mm im * x) iv
                      go (im + 1) (iv + 1)
                loopRows i0 from to =
                  flip fix i0 $ \go i ->
                    when (i < mRows) $ do
                      loopCols (unsafeLinearIndex v i) to (i * mCols + from) from
                      go (i + 1)
            splitLinearlyM_ scheduler mCols (loopRows 0)
          unsafeFreeze comp mv
  where
    comp = getComp mm <> getComp v
    Sz2 mRows mCols = size mm
    Sz1 n = size v
{-# INLINE multiplyVectorByMatrix #-}

-- | Multiply a row vector by a matrix.
--
-- [Partial] Throws impure exception when inner dimensions do not agree
--
-- @since 0.5.6
(><!)
  :: (Numeric r e, Manifest r e)
  => Vector r e
  -- ^ Row vector (Used many times, so make sure it is computed)
  -> Matrix r e
  -- ^ Matrix
  -> Vector r e
(><!) v mm = throwEither (v ><. mm)
{-# INLINE (><!) #-}

-- | Multiply two matrices together.
--
-- [Partial] Inner dimension must agree
--
-- ====__Examples__
--
-- >>> import Data.Massiv.Array
-- >>> a1 = makeArrayR P Seq (Sz2 5 6) $ \(i :. j) -> i + j
-- >>> a2 = makeArrayR P Seq (Sz2 6 5) $ \(i :. j) -> i - j
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
(!><!) :: (Numeric r e, Manifest r e) => Matrix r e -> Matrix r e -> Matrix r e
(!><!) a1 a2 = throwEither (a1 `multiplyMatrices` a2)
{-# INLINE (!><!) #-}

-- | Matrix multiplication. Same as `!><!` but produces monadic computation that allows
-- for handling failure.
--
-- /__Throws Exception__/: `SizeMismatchException` when inner dimensions of arrays do not match.
--
-- @since 0.5.6
(.><.) :: (Numeric r e, Manifest r e, MonadThrow m) => Matrix r e -> Matrix r e -> m (Matrix r e)
(.><.) = multiplyMatrices
{-# INLINE (.><.) #-}

-- | Synonym for `.><.`
--
-- @since 0.5.6
multiplyMatrices
  :: (Numeric r e, Manifest r e, MonadThrow m) => Matrix r e -> Matrix r e -> m (Matrix r e)
multiplyMatrices arrA arrB
  -- mA == 1 = -- TODO: call multiplyVectorByMatrix
  -- nA == 1 = -- TODO: call multiplyMatrixByVector
  | nA /= mB = throwM $ SizeMismatchException (size arrA) (size arrB)
  | isEmpty arrA || isEmpty arrB = pure $ runST (unsafeFreeze comp =<< unsafeNew zeroSz)
  | otherwise = pure $! unsafePerformIO $ do
      marrC <- newMArray (SafeSz (mA :. nB)) 0
      withScheduler_ comp $ \scheduler -> do
        let withC00 iA jB f =
              let !ixC00 = iA * nB + jB
               in f ixC00 =<< unsafeLinearRead marrC ixC00
            withC01 ixC00 f =
              let !ixC01 = ixC00 + 1
               in f ixC01 =<< unsafeLinearRead marrC ixC01
            withC10 ixC00 f =
              let !ixC10 = ixC00 + nB
               in f ixC10 =<< unsafeLinearRead marrC ixC10
            withC11 ixC01 f =
              let !ixC11 = ixC01 + nB
               in f ixC11 =<< unsafeLinearRead marrC ixC11
            withB00 iB jB f =
              let !ixB00 = iB * nB + jB
               in f ixB00 $! unsafeLinearIndex arrB ixB00
            withB00B10 iB jB f =
              withB00 iB jB $ \ixB00 b00 ->
                let !ixB10 = ixB00 + nB
                 in f ixB00 b00 ixB10 $! unsafeLinearIndex arrB ixB10
            withA00 iA jA f =
              let !ixA00 = iA * nA + jA
               in f ixA00 $! unsafeLinearIndex arrA ixA00
            withA00A10 iA jA f =
              withA00 iA jA $ \ixA00 a00 ->
                let !ixA10 = ixA00 + nA
                 in f ixA00 a00 ixA10 $! unsafeLinearIndex arrA ixA10
        let loopColsB_UnRowBColA_UnRowA a00 a01 a10 a11 iA iB jB
              | jB < n2B = do
                  withB00B10 iB jB $ \ixB00 b00 ixB10 b10 -> do
                    let !b01 = unsafeLinearIndex arrB (ixB00 + 1)
                        !b11 = unsafeLinearIndex arrB (ixB10 + 1)
                    withC00 iA jB $ \ixC00 c00 -> do
                      unsafeLinearWrite marrC ixC00 (c00 + a00 * b00 + a01 * b10)
                      withC01 ixC00 $ \ixC01 c01 -> do
                        unsafeLinearWrite marrC ixC01 (c01 + a00 * b01 + a01 * b11)
                        withC10 ixC00 $ \ixC10 c10 ->
                          unsafeLinearWrite marrC ixC10 (c10 + a10 * b00 + a11 * b10)
                        withC11 ixC01 $ \ixC11 c11 ->
                          unsafeLinearWrite marrC ixC11 (c11 + a10 * b01 + a11 * b11)
                  loopColsB_UnRowBColA_UnRowA a00 a01 a10 a11 iA iB (jB + 2)
              | jB < nB = withB00B10 iB jB $ \_ b00 _ b10 ->
                  withC00 iA jB $ \ixC00 c00 -> do
                    unsafeLinearWrite marrC ixC00 (c00 + a00 * b00 + a01 * b10)
                    withC10 ixC00 $ \ixC10 c10 ->
                      unsafeLinearWrite marrC ixC10 (c10 + a10 * b00 + a11 * b10)
              | otherwise = pure ()

            loopColsB_UnRowBColA_RowA a00 a01 iA iB jB
              | jB < n2B = do
                  withB00B10 iB jB $ \ixB00 b00 ixB10 b10 -> do
                    let !b01 = unsafeLinearIndex arrB (ixB00 + 1)
                        !b11 = unsafeLinearIndex arrB (ixB10 + 1)
                    withC00 iA jB $ \ixC00 c00 -> do
                      unsafeLinearWrite marrC ixC00 (c00 + a00 * b00 + a01 * b10)
                      withC01 ixC00 $ \ixC01 c01 ->
                        unsafeLinearWrite marrC ixC01 (c01 + a00 * b01 + a01 * b11)
                  loopColsB_UnRowBColA_RowA a00 a01 iA iB (jB + 2)
              | jB < nB = withB00B10 iB jB $ \_ b00 _ b10 ->
                  withC00 iA jB $ \ixC00 c00 ->
                    unsafeLinearWrite marrC ixC00 (c00 + a00 * b00 + a01 * b10)
              | otherwise = pure ()

            loopColsB_RowBColA_UnRowA a00 a10 iA iB jB
              | jB < n2B = do
                  withB00 iB jB $ \ixB00 b00 -> do
                    let !b01 = unsafeLinearIndex arrB (ixB00 + 1)
                    withC00 iA jB $ \ixC00 c00 -> do
                      unsafeLinearWrite marrC ixC00 (c00 + a00 * b00)
                      withC01 ixC00 $ \ixC01 c01 -> do
                        unsafeLinearWrite marrC ixC01 (c01 + a00 * b01)
                        withC10 ixC00 $ \ixC10 c10 ->
                          unsafeLinearWrite marrC ixC10 (c10 + a10 * b00)
                        withC11 ixC01 $ \ixC11 c11 ->
                          unsafeLinearWrite marrC ixC11 (c11 + a10 * b01)
                  loopColsB_RowBColA_UnRowA a00 a10 iA iB (jB + 2)
              | jB < nB = withB00 iB jB $ \_ b00 ->
                  withC00 iA jB $ \ixC00 c00 -> do
                    unsafeLinearWrite marrC ixC00 (c00 + a00 * b00)
                    withC10 ixC00 $ \ixC10 c10 ->
                      unsafeLinearWrite marrC ixC10 (c10 + a10 * b00)
              | otherwise = pure ()

            loopColsB_RowBColA_RowA a00 iA iB jB
              | jB < n2B = do
                  withB00 iB jB $ \ixB00 b00 -> do
                    let !b01 = unsafeLinearIndex arrB (ixB00 + 1)
                    withC00 iA jB $ \ixC00 c00 -> do
                      unsafeLinearWrite marrC ixC00 (c00 + a00 * b00)
                      withC01 ixC00 $ \ixC01 c01 -> do
                        unsafeLinearWrite marrC ixC01 (c01 + a00 * b01)
                  loopColsB_RowBColA_RowA a00 iA iB (jB + 2)
              | jB < nB = withB00 iB jB $ \_ b00 ->
                  withC00 iA jB $ \ixC00 c00 ->
                    unsafeLinearWrite marrC ixC00 (c00 + a00 * b00)
              | otherwise = pure ()

            loopRowsB_UnRowA iA iB
              | iB < m2B = do
                  withA00A10 iA iB $ \ixA00 a00 ixA10 a10 -> do
                    let !a01 = unsafeLinearIndex arrA (ixA00 + 1)
                        !a11 = unsafeLinearIndex arrA (ixA10 + 1)
                    loopColsB_UnRowBColA_UnRowA a00 a01 a10 a11 iA iB 0
                  loopRowsB_UnRowA iA (iB + 2)
              | iB < mB =
                  withA00A10 iA iB $ \_ a00 _ a10 -> loopColsB_RowBColA_UnRowA a00 a10 iA iB 0
              | otherwise = pure ()

            loopRowsB_RowA iA iB
              | iB < m2B = do
                  withA00 iA iB $ \ixA00 a00 -> do
                    let !a01 = unsafeLinearIndex arrA (ixA00 + 1)
                    loopColsB_UnRowBColA_RowA a00 a01 iA iB 0
                  loopRowsB_RowA iA (iB + 2)
              | iB < mB = withA00 iA iB $ \_ a00 -> loopColsB_RowBColA_RowA a00 iA iB 0
              | otherwise = pure ()

            loopRowsA iA
              | iA < m2A = do
                  scheduleWork_ scheduler $ loopRowsB_UnRowA iA 0
                  loopRowsA (iA + 2)
              | iA < mA = scheduleWork_ scheduler $ loopRowsB_RowA iA 0
              | otherwise = pure ()
        loopRowsA 0

      unsafeFreeze comp marrC
  where
    comp = getComp arrA <> getComp arrB
    m2A = mA - mA `rem` 2
    m2B = mB - mB `rem` 2
    n2B = nB - nB `rem` 2
    Sz (mA :. nA) = size arrA
    Sz (mB :. nB) = size arrB
{-# INLINEABLE multiplyMatrices #-}

-- | Computes the matrix-matrix multiplication where second matrix is transposed (i.e. M
-- x N')
--
-- > m1 .><. transpose m2 == multiplyMatricesTransposed m1 m2
--
-- @since 0.5.6
multiplyMatricesTransposed
  :: (Numeric r e, Manifest r e, MonadThrow m)
  => Matrix r e
  -> Matrix r e
  -> m (Matrix D e)
multiplyMatricesTransposed arr1 arr2
  | n1 /= m2 = throwM $ SizeMismatchException (size arr1) (Sz2 m2 n2)
  | isEmpty arr1 || isEmpty arr2 = pure $ setComp comp empty
  | otherwise =
      pure $
        makeArray comp (SafeSz (m1 :. n2)) $ \(i :. j) ->
          unsafeDotProduct (unsafeLinearSlice (i * n1) n arr1) (unsafeLinearSlice (j * n1) n arr2)
  where
    comp = getComp arr1 <> getComp arr2
    n = SafeSz n1
    SafeSz (m1 :. n1) = size arr1
    SafeSz (n2 :. m2) = size arr2
{-# INLINE multiplyMatricesTransposed #-}

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
  makeLoadArrayS (Sz2 n n) 0 $ \w -> loopA_ 0 (< n) (+ 1) $ \i -> w (i :. i) 1
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
lowerTriangular :: forall e. Num e => Comp -> Sz1 -> (Ix2 -> e) -> Matrix DL e
lowerTriangular comp (Sz1 n) f = DLArray comp (SafeSz (n :. n)) load
  where
    load :: Loader e
    load scheduler startAt uWrite uSet = do
      forM_ (0 ..: n) $ \i -> do
        let !k = startAt + i * n
        scheduleWork_ scheduler $ do
          forM_ (0 ... i) $ \j -> uWrite (k + j) (f (i :. j))
          uSet (k + i + 1) (Sz (n - i - 1)) 0
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
upperTriangular :: forall e. Num e => Comp -> Sz1 -> (Ix2 -> e) -> Matrix DL e
upperTriangular comp (Sz1 n) f = DLArray comp (SafeSz (n :. n)) load
  where
    load :: Loader e
    load scheduler startAt uWrite uSet = do
      forM_ (0 ..: n) $ \i -> do
        let !k = startAt + i * n
        scheduleWork_ scheduler $ do
          uSet k (SafeSz i) 0
          forM_ (i ..: n) $ \j -> uWrite (k + j) (f (i :. j))
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

-- | Divide each element of one array by another pointwise. Same as `!/!` but produces
-- monadic computation that allows for handling failure.
--
-- /__Throws Exception__/: `SizeMismatchException` when array sizes do not match.
--
-- @since 0.4.0
(./.)
  :: (Index ix, NumericFloat r e, MonadThrow m)
  => Array r ix e
  -> Array r ix e
  -> m (Array r ix e)
(./.) = applyExactSize2M divisionPointwise
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
(!/!) :: (Index ix, NumericFloat r e) => Array r ix e -> Array r ix e -> Array r ix e
(!/!) a1 a2 = throwEither (a1 ./. a2)
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
(/.) :: (Index ix, NumericFloat r e) => e -> Array r ix e -> Array r ix e
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
(./) :: (Index ix, NumericFloat r e) => Array r ix e -> e -> Array r ix e
(./) = divideScalar
{-# INLINE (./) #-}

(.^^)
  :: (Index ix, Numeric r e, Fractional e, Integral b)
  => Array r ix e
  -> b
  -> Array r ix e
(.^^) arr n = unsafeLiftArray (^^ n) arr
{-# INLINE (.^^) #-}

-- | Apply reciprocal to each element of the array.
--
-- > recipA arr == 1 /. arr
--
-- @since 0.4.0
recipA :: (Index ix, NumericFloat r e) => Array r ix e -> Array r ix e
recipA = recipPointwise
{-# INLINE recipA #-}

-- | Apply exponent to each element of the array.
--
-- > expA arr == map exp arr
--
-- @since 0.4.0
expA :: (Index ix, NumericFloat r e) => Array r ix e -> Array r ix e
expA = unsafeLiftArray exp
{-# INLINE expA #-}

-- | Apply square root to each element of the array.
--
-- > sqrtA arr == map sqrt arr
--
-- @since 0.4.0
sqrtA :: (Index ix, NumericFloat r e) => Array r ix e -> Array r ix e
sqrtA = unsafeLiftArray sqrt
{-# INLINE sqrtA #-}

-- | Apply logarithm to each element of the array.
--
-- > logA arr == map log arr
--
-- @since 0.4.0
logA :: (Index ix, NumericFloat r e) => Array r ix e -> Array r ix e
logA = unsafeLiftArray log
{-# INLINE logA #-}

-- | Apply logarithm to each element of the array where the base is in the same cell in
-- the second array.
--
-- > logBaseA arr1 arr2 == zipWith logBase arr1 arr2
--
-- [Partial] Throws an error when arrays do not have matching sizes
--
-- @since 0.4.0
logBaseA
  :: (Index ix, Source r1 e, Source r2 e, Floating e)
  => Array r1 ix e
  -> Array r2 ix e
  -> Array D ix e
logBaseA = liftArray2' logBase
{-# INLINE logBaseA #-}

-- TODO: siwtch to
-- (breaking) logBaseA :: Array r ix e -> e -> Array D ix e
-- logBasesM :: Array r ix e -> Array r ix e -> m (Array D ix e)

-- | Apply power to each element of the array where the power value is in the same cell
-- in the second array.
--
-- > arr1 .** arr2 == zipWith (**) arr1 arr2
--
-- [Partial] Throws an error when arrays do not have matching sizes
--
-- @since 0.4.0
(.**)
  :: (Index ix, Source r1 e, Source r2 e, Floating e)
  => Array r1 ix e
  -> Array r2 ix e
  -> Array D ix e
(.**) = liftArray2' (**)
{-# INLINE (.**) #-}

-- TODO:
-- !**! :: Array r1 ix e -> Array r2 ix e -> Array D ix e
-- .**. :: Array r1 ix e -> Array r2 ix e -> m (Array D ix e)
-- (breaking) .** :: Array r1 ix e -> e -> Array D ix e

-- | Apply sine function to each element of the array.
--
-- > sinA arr == map sin arr
--
-- @since 0.4.0
sinA :: (Index ix, NumericFloat r e) => Array r ix e -> Array r ix e
sinA = unsafeLiftArray sin
{-# INLINE sinA #-}

-- | Apply cosine function to each element of the array.
--
-- > cosA arr == map cos arr
--
-- @since 0.4.0
cosA :: (Index ix, NumericFloat r e) => Array r ix e -> Array r ix e
cosA = unsafeLiftArray cos
{-# INLINE cosA #-}

-- | Apply tangent function to each element of the array.
--
-- > tanA arr == map tan arr
--
-- @since 0.4.0
tanA :: (Index ix, NumericFloat r e) => Array r ix e -> Array r ix e
tanA = unsafeLiftArray tan
{-# INLINE tanA #-}

-- | Apply arcsine function to each element of the array.
--
-- > asinA arr == map asin arr
--
-- @since 0.4.0
asinA :: (Index ix, NumericFloat r e) => Array r ix e -> Array r ix e
asinA = unsafeLiftArray asin
{-# INLINE asinA #-}

-- | Apply arctangent function to each element of the array.
--
-- > atanA arr == map atan arr
--
-- @since 0.4.0
atanA :: (Index ix, NumericFloat r e) => Array r ix e -> Array r ix e
atanA = unsafeLiftArray atan
{-# INLINE atanA #-}

-- | Apply arccosine function to each element of the array.
--
-- > acosA arr == map acos arr
--
-- @since 0.4.0
acosA :: (Index ix, NumericFloat r e) => Array r ix e -> Array r ix e
acosA = unsafeLiftArray acos
{-# INLINE acosA #-}

-- | Apply hyperbolic sine function to each element of the array.
--
-- > sinhA arr == map sinh arr
--
-- @since 0.4.0
sinhA :: (Index ix, NumericFloat r e) => Array r ix e -> Array r ix e
sinhA = unsafeLiftArray sinh
{-# INLINE sinhA #-}

-- | Apply hyperbolic tangent function to each element of the array.
--
-- > tanhA arr == map tanh arr
--
-- @since 0.4.0
tanhA :: (Index ix, NumericFloat r e) => Array r ix e -> Array r ix e
tanhA = unsafeLiftArray tanh
{-# INLINE tanhA #-}

-- | Apply hyperbolic cosine function to each element of the array.
--
-- > coshA arr == map cosh arr
--
-- @since 0.4.0
coshA :: (Index ix, NumericFloat r e) => Array r ix e -> Array r ix e
coshA = unsafeLiftArray cosh
{-# INLINE coshA #-}

-- | Apply inverse hyperbolic sine function to each element of the array.
--
-- > asinhA arr == map asinh arr
--
-- @since 0.4.0
asinhA :: (Index ix, NumericFloat r e) => Array r ix e -> Array r ix e
asinhA = unsafeLiftArray asinh
{-# INLINE asinhA #-}

-- | Apply inverse hyperbolic cosine function to each element of the array.
--
-- > acoshA arr == map acosh arr
--
-- @since 0.4.0
acoshA :: (Index ix, NumericFloat r e) => Array r ix e -> Array r ix e
acoshA = unsafeLiftArray acosh
{-# INLINE acoshA #-}

-- | Apply inverse hyperbolic tangent function to each element of the array.
--
-- > atanhA arr == map atanh arr
--
-- @since 0.4.0
atanhA :: (Index ix, NumericFloat r e) => Array r ix e -> Array r ix e
atanhA = unsafeLiftArray atanh
{-# INLINE atanhA #-}

-- | Perform a pointwise quotient where first array contains numerators and the second
-- one denominators
--
-- > quotA arr1 arr2 == zipWith quot arr1 arr2
--
-- [Partial] Mismatched array sizes will result in an impure exception being thrown.
--
-- @since 0.1.0
quotA
  :: (HasCallStack, Index ix, Source r1 e, Source r2 e, Integral e)
  => Array r1 ix e
  -> Array r2 ix e
  -> Array D ix e
quotA = liftArray2' quot
{-# INLINE quotA #-}

-- | Perform a pointwise remainder computation
--
-- > remA arr1 arr2 == zipWith rem arr1 arr2
--
-- [Partial] Mismatched array sizes will result in an impure exception being thrown.
--
-- @since 0.1.0
remA
  :: (HasCallStack, Index ix, Source r1 e, Source r2 e, Integral e)
  => Array r1 ix e
  -> Array r2 ix e
  -> Array D ix e
remA = liftArray2' rem
{-# INLINE remA #-}

-- | Perform a pointwise integer division where first array contains numerators and the
-- second one denominators
--
-- > divA arr1 arr2 == zipWith div arr1 arr2
--
-- [Partial] Mismatched array sizes will result in an impure exception being thrown.
--
-- @since 0.1.0
divA
  :: (HasCallStack, Index ix, Source r1 e, Source r2 e, Integral e)
  => Array r1 ix e
  -> Array r2 ix e
  -> Array D ix e
divA = liftArray2' div
{-# INLINE divA #-}

-- TODO:
--  * Array r ix e -> Array r ix e -> m (Array r ix e)
--  * Array r ix e -> e -> Array r ix e
--  * e -> Array r ix e -> Array r ix e

-- | Perform a pointwise modulo computation
--
-- > modA arr1 arr2 == zipWith mod arr1 arr2
--
-- [Partial] Mismatched array sizes will result in an impure exception being thrown.
--
-- @since 0.1.0
modA
  :: (HasCallStack, Index ix, Source r1 e, Source r2 e, Integral e)
  => Array r1 ix e
  -> Array r2 ix e
  -> Array D ix e
modA = liftArray2' mod
{-# INLINE modA #-}

-- | Perform a pointwise quotient with remainder where first array contains numerators
-- and the second one denominators
--
-- > quotRemA arr1 arr2 == zipWith quotRem arr1 arr2
--
-- [Partial] Mismatched array sizes will result in an impure exception being thrown.
--
-- @since 0.1.0
quotRemA
  :: (HasCallStack, Index ix, Source r1 e, Source r2 e, Integral e)
  => Array r1 ix e
  -> Array r2 ix e
  -> (Array D ix e, Array D ix e)
quotRemA arr1 = A.unzip . liftArray2' quotRem arr1
{-# INLINE quotRemA #-}

-- | Perform a pointwise integer division with modulo where first array contains
-- numerators and the second one denominators
--
-- > divModA arr1 arr2 == zipWith divMod arr1 arr2
--
-- [Partial] Mismatched array sizes will result in an impure exception being thrown.
--
-- @since 0.1.0
divModA
  :: (HasCallStack, Index ix, Source r1 e, Source r2 e, Integral e)
  => Array r1 ix e
  -> Array r2 ix e
  -> (Array D ix e, Array D ix e)
divModA arr1 = A.unzip . liftArray2' divMod arr1
{-# INLINE divModA #-}

-- | Truncate each element of the array.
--
-- > truncateA arr == map truncate arr
--
-- @since 0.1.0
truncateA :: (Index ix, Source r a, RealFrac a, Integral e) => Array r ix a -> Array D ix e
truncateA = A.map truncate
{-# INLINE truncateA #-}

-- | Round each element of the array.
--
-- > truncateA arr == map truncate arr
--
-- @since 0.1.0
roundA :: (Index ix, Source r a, RealFrac a, Integral e) => Array r ix a -> Array D ix e
roundA = A.map round
{-# INLINE roundA #-}

-- | Ceiling of each element of the array.
--
-- > truncateA arr == map truncate arr
--
-- @since 0.1.0
ceilingA :: (Index ix, Source r a, RealFrac a, Integral e) => Array r ix a -> Array D ix e
ceilingA = A.map ceiling
{-# INLINE ceilingA #-}

-- | Floor each element of the array.
--
-- > truncateA arr == map truncate arr
--
-- @since 0.1.0
floorA :: (Index ix, Source r a, RealFrac a, Integral e) => Array r ix a -> Array D ix e
floorA = A.map floor
{-# INLINE floorA #-}

-- | Perform atan2 pointwise
--
-- > atan2A arr1 arr2 == zipWith atan2 arr1 arr2
--
-- /__Throws Exception__/: `SizeMismatchException` when array sizes do not match.
--
-- @since 0.1.0
atan2A
  :: (Index ix, Numeric r e, RealFloat e, MonadThrow m)
  => Array r ix e
  -> Array r ix e
  -> m (Array r ix e)
atan2A = liftNumArray2M atan2
{-# INLINE atan2A #-}

-- | Same as `sumArraysM`, compute sum of arrays pointwise. All arrays must have the same
-- size, otherwise it will result in an error.
--
-- @since 1.0.0
sumArrays'
  :: (HasCallStack, Foldable t, Load r ix e, Numeric r e) => t (Array r ix e) -> Array r ix e
sumArrays' = throwEither . sumArraysM
{-# INLINE sumArrays' #-}

-- | Compute sum of arrays pointwise. All arrays must have the same size.
--
-- ====__Examples__
--
-- >>> import Data.Massiv.Array as A
-- >>> sumArraysM [] :: IO (Array P Ix3 Int)
-- Array P Seq (Sz (0 :> 0 :. 0))
--   [  ]
-- >>> arr = A.makeArrayR P Seq (Sz3 4 5 6) $ \(i :> j :. k) -> i + j * k
-- >>> arr
-- Array P Seq (Sz (4 :> 5 :. 6))
--   [ [ [ 0, 0, 0, 0, 0, 0 ]
--     , [ 0, 1, 2, 3, 4, 5 ]
--     , [ 0, 2, 4, 6, 8, 10 ]
--     , [ 0, 3, 6, 9, 12, 15 ]
--     , [ 0, 4, 8, 12, 16, 20 ]
--     ]
--   , [ [ 1, 1, 1, 1, 1, 1 ]
--     , [ 1, 2, 3, 4, 5, 6 ]
--     , [ 1, 3, 5, 7, 9, 11 ]
--     , [ 1, 4, 7, 10, 13, 16 ]
--     , [ 1, 5, 9, 13, 17, 21 ]
--     ]
--   , [ [ 2, 2, 2, 2, 2, 2 ]
--     , [ 2, 3, 4, 5, 6, 7 ]
--     , [ 2, 4, 6, 8, 10, 12 ]
--     , [ 2, 5, 8, 11, 14, 17 ]
--     , [ 2, 6, 10, 14, 18, 22 ]
--     ]
--   , [ [ 3, 3, 3, 3, 3, 3 ]
--     , [ 3, 4, 5, 6, 7, 8 ]
--     , [ 3, 5, 7, 9, 11, 13 ]
--     , [ 3, 6, 9, 12, 15, 18 ]
--     , [ 3, 7, 11, 15, 19, 23 ]
--     ]
--   ]
-- >>> sumArraysM $ outerSlices arr
-- Array P Seq (Sz (5 :. 6))
--   [ [ 6, 6, 6, 6, 6, 6 ]
--   , [ 6, 10, 14, 18, 22, 26 ]
--   , [ 6, 14, 22, 30, 38, 46 ]
--   , [ 6, 18, 30, 42, 54, 66 ]
--   , [ 6, 22, 38, 54, 70, 86 ]
--   ]
-- >>> sumArraysM $ innerSlices arr
-- Array D Seq (Sz (4 :. 5))
--   [ [ 0, 15, 30, 45, 60 ]
--   , [ 6, 21, 36, 51, 66 ]
--   , [ 12, 27, 42, 57, 72 ]
--   , [ 18, 33, 48, 63, 78 ]
--   ]
--
-- @since 1.0.0
sumArraysM
  :: (Foldable t, Load r ix e, Numeric r e, MonadThrow m) => t (Array r ix e) -> m (Array r ix e)
sumArraysM as =
  case F.toList as of
    [] -> pure empty
    (x : xs) -> F.foldlM (.+.) x xs
{-# INLINE sumArraysM #-}

-- OPTIMIZE: Allocate a single result array and write sums into it incrementally.

-- | Same as `productArraysM`. Compute product of arrays pointwise. All arrays must have
-- the same size, otherwise it
-- will result in an error.
--
-- @since 1.0.0
productArrays'
  :: (HasCallStack, Foldable t, Load r ix e, Numeric r e) => t (Array r ix e) -> Array r ix e
productArrays' = throwEither . productArraysM
{-# INLINE productArrays' #-}

-- | Compute product of arrays pointwise. All arrays must have the same size.
--
-- ====__Examples__
--
-- >>> import Data.Massiv.Array as A
-- >>> productArraysM [] :: IO (Array P Ix3 Int)
-- Array P Seq (Sz (0 :> 0 :. 0))
--   [  ]
-- >>> arr = A.makeArrayR P Seq (Sz3 4 5 6) $ \(i :> j :. k) -> i + j * k
-- >>> arr
-- Array P Seq (Sz (4 :> 5 :. 6))
--   [ [ [ 0, 0, 0, 0, 0, 0 ]
--     , [ 0, 1, 2, 3, 4, 5 ]
--     , [ 0, 2, 4, 6, 8, 10 ]
--     , [ 0, 3, 6, 9, 12, 15 ]
--     , [ 0, 4, 8, 12, 16, 20 ]
--     ]
--   , [ [ 1, 1, 1, 1, 1, 1 ]
--     , [ 1, 2, 3, 4, 5, 6 ]
--     , [ 1, 3, 5, 7, 9, 11 ]
--     , [ 1, 4, 7, 10, 13, 16 ]
--     , [ 1, 5, 9, 13, 17, 21 ]
--     ]
--   , [ [ 2, 2, 2, 2, 2, 2 ]
--     , [ 2, 3, 4, 5, 6, 7 ]
--     , [ 2, 4, 6, 8, 10, 12 ]
--     , [ 2, 5, 8, 11, 14, 17 ]
--     , [ 2, 6, 10, 14, 18, 22 ]
--     ]
--   , [ [ 3, 3, 3, 3, 3, 3 ]
--     , [ 3, 4, 5, 6, 7, 8 ]
--     , [ 3, 5, 7, 9, 11, 13 ]
--     , [ 3, 6, 9, 12, 15, 18 ]
--     , [ 3, 7, 11, 15, 19, 23 ]
--     ]
--   ]
-- >>> productArraysM $ outerSlices arr
-- Array P Seq (Sz (5 :. 6))
--   [ [ 0, 0, 0, 0, 0, 0 ]
--   , [ 0, 24, 120, 360, 840, 1680 ]
--   , [ 0, 120, 840, 3024, 7920, 17160 ]
--   , [ 0, 360, 3024, 11880, 32760, 73440 ]
--   , [ 0, 840, 7920, 32760, 93024, 212520 ]
--   ]
-- >>> productArraysM $ innerSlices arr
-- Array D Seq (Sz (4 :. 5))
--   [ [ 0, 0, 0, 0, 0 ]
--   , [ 1, 720, 10395, 58240, 208845 ]
--   , [ 64, 5040, 46080, 209440, 665280 ]
--   , [ 729, 20160, 135135, 524880, 1514205 ]
--   ]
--
-- @since 1.0.0
productArraysM
  :: (Foldable t, Load r ix e, Numeric r e, MonadThrow m) => t (Array r ix e) -> m (Array r ix e)
productArraysM as =
  case F.toList as of
    [] -> pure empty
    (x : xs) -> F.foldlM (.*.) x xs
{-# INLINE productArraysM #-}
