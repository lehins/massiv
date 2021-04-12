{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Data.Massiv.Array.Numeric.Internal
-- Copyright   : (c) Alexey Kuleshevich 2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Numeric.Internal
  ( unsafeMultiplyMatricesM
  ) where

import Data.Massiv.Array.Mutable
import Data.Massiv.Core
import Data.Massiv.Core.Common

-- | Multiply two matrices together and load the result into mutable array. Arrays sizes
-- are not verified.
--
-- @since 0.6.1
unsafeMultiplyMatricesM ::
     (Numeric r e, Mutable r Ix2 e, PrimMonad m)
  => Scheduler m ()
  -> MMatrix (PrimState m) r e -- ^ Target mutable array. It is expected to contain all zeros.
  -> Matrix r e
  -> Matrix r e
  -> m ()
unsafeMultiplyMatricesM scheduler marrC arrA arrB = loopRowsA 0
  where
    m2A = mA - mA `rem` 2
    m2B = mB - mB `rem` 2
    n2B = nB - nB `rem` 2
    Sz (mA :. nA) = size arrA
    Sz (mB :. nB) = size arrB
    withC00 iA jB f = let !ixC00 = iA * nB + jB
                      in f ixC00 =<< unsafeLinearRead marrC ixC00
    withC01 ixC00 f = let !ixC01 = ixC00 + 1
                      in f ixC01 =<< unsafeLinearRead marrC ixC01
    withC10 ixC00 f = let !ixC10 = ixC00 + nB
                      in f ixC10 =<< unsafeLinearRead marrC ixC10
    withC11 ixC01 f = let !ixC11 = ixC01 + nB
                      in f ixC11 =<< unsafeLinearRead marrC ixC11
    withB00 iB jB f = let !ixB00 = iB * nB + jB
                      in f ixB00 $! unsafeLinearIndex arrB ixB00
    withB00B10 iB jB f =
      withB00 iB jB $ \ixB00 b00 -> let !ixB10 = ixB00 + nB
                                    in f ixB00 b00 ixB10 $! unsafeLinearIndex arrB ixB10
    withA00 iA jA f = let !ixA00 = iA * nA + jA
                      in f ixA00 $! unsafeLinearIndex arrA ixA00
    withA00A10 iA jA f =
      withA00 iA jA $ \ixA00 a00 -> let !ixA10 = ixA00 + nA
                                    in f ixA00 a00 ixA10 $! unsafeLinearIndex arrA ixA10
    loopColsB_UnRowBColA_UnRowA a00 a01 a10 a11 iA iB jB
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
{-# INLINE unsafeMultiplyMatricesM #-}
