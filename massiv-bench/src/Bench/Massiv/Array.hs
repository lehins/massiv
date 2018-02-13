{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Bench.Massiv.Array (
  module A
  , sum'
  , tupleToIx2
  , tupleToIx2T
  , lightFuncIx2
  , lightFuncIx2T
  , arrDLightIx2
  , arrRLightIx2
  , arrDHeavyIx2
  , arrDLightIx2T
  , arrDHeavyIx2T
  , sobelKernelStencilX
  , sobelX
  , sobelY
  , sobelOperator
  , sobelOperatorUnfused
  , average3x3Filter
  , average3x3FilterConv
  ) where

import           Bench.Common             (heavyFunc, lightFunc)
import           Control.DeepSeq
import           Data.Default             (Default)
import           Data.Massiv.Array        as A
import           Data.Massiv.Array.Unsafe as A
import           Data.Monoid

-- | Bogus DeepSeq for delayed array so it can be fed to the `env`.
instance Index ix => NFData (Array D ix e) where
  rnf arr = size arr `seq` ()

-- | Bogus DeepSeq for Manifest array so it can be fed to the `env`.
instance Index ix => NFData (Array M ix e) where
  rnf arr = size arr `seq` ()


tupleToIx2 :: (Int, Int) -> Ix2
tupleToIx2 (i, j) = i :. j
{-# INLINE tupleToIx2 #-}

tupleToIx2T :: (Int, Int) -> Ix2T
tupleToIx2T = id
{-# INLINE tupleToIx2T #-}

lightFuncIx2 :: Ix2 -> Double
lightFuncIx2 (i :. j) = lightFunc i j
{-# INLINE lightFuncIx2 #-}

lightFuncIx2T :: Ix2T -> Double
lightFuncIx2T (i, j) = lightFunc i j
{-# INLINE lightFuncIx2T #-}


arrDLightIx2 :: Comp -> Ix2 -> Array D Ix2 Double
arrDLightIx2 comp arrSz = makeArray comp arrSz (\ (i :. j) -> lightFunc i j)
{-# INLINE arrDLightIx2 #-}

arrRLightIx2 :: Construct r Ix2 Double => r -> Comp -> Ix2 -> Array r Ix2 Double
arrRLightIx2 _ comp arrSz = makeArray comp arrSz (\ (i :. j) -> lightFunc i j)
{-# INLINE arrRLightIx2 #-}


arrDHeavyIx2 :: Comp -> Ix2 -> Array D Ix2 Double
arrDHeavyIx2 comp arrSz = makeArray comp arrSz (\ (i :. j) -> heavyFunc i j)
{-# INLINE arrDHeavyIx2 #-}

arrDLightIx2T :: Comp -> Ix2T -> Array D Ix2T Double
arrDLightIx2T comp arrSz = makeArray comp arrSz (\ (i, j) -> lightFunc i j)
{-# INLINE arrDLightIx2T #-}

arrDHeavyIx2T :: Comp -> Ix2T -> Array D Ix2T Double
arrDHeavyIx2T comp arrSz = makeArray comp arrSz (\ (i, j) -> heavyFunc i j)
{-# INLINE arrDHeavyIx2T #-}


sobelKernelStencilX
  :: forall e . (Eq e, Num e, Unbox e) => Border e -> Stencil Ix2 e e
sobelKernelStencilX b =
  makeConvolutionStencilFromKernel b $ (fromList' Seq [ [ 1, 0, -1 ]
                                                    , [ 2, 0, -2 ]
                                                    , [ 1, 0, -1 ] ] :: Array U Ix2 e)
{-# INLINE sobelKernelStencilX #-}


--sobelX :: Num e => Border e -> Stencil Ix2 e e
-- sobelX' :: Border Int -> Stencil Ix2 Int Int
-- sobelX' b = makeConvolutionStencil b (3 :. 3) (1 :. 1) $
--            \f -> f (f (-1 :. -1) 1 2 :. 0) 4
-- {-# INLINE sobelX' #-}

sobelX :: Num e => Border e -> Stencil Ix2 e e
sobelX b = makeConvolutionStencil b (3 :. 3) (1 :. 1) accum where
  accum f =
     f (-1 :. -1)   1  .
     f ( 0 :. -1)   2  .
     f ( 1 :. -1)   1  .
     f (-1 :.  1) (-1) .
     f ( 0 :.  1) (-2) .
     f ( 1 :.  1) (-1)
  {-# INLINE accum #-}
{-# INLINE sobelX #-}


sobelY :: Num e => Border e -> Stencil Ix2 e e
sobelY b = makeConvolutionStencil b (3 :. 3) (1 :. 1) accum where
  accum f =
     f (-1 :. -1)   1  .
     f (-1 :.  0)   2  .
     f (-1 :.  1)   1  .
     f ( 1 :. -1) (-1) .
     f ( 1 :.  0) (-2) .
     f ( 1 :.  1) (-1)
  {-# INLINE accum #-}
{-# INLINE sobelY #-}



sobelOperator :: (Default b, Floating b) => Border b -> Stencil Ix2 b b
sobelOperator b = sqrt (sX + sY)
  where
    !sX = fmap (^ (2 :: Int)) (sobelX b)
    !sY = fmap (^ (2 :: Int)) (sobelY b)
{-# INLINE sobelOperator #-}


sobelOperatorUnfused
  :: (Unbox b, Eq b, Floating b)
  => Border b -> Array U Ix2 b -> Array U Ix2 b
sobelOperatorUnfused b arr = computeAs U $ A.map sqrt (A.zipWith (+) sX sY)
  where
    !sX = A.map (^ (2 :: Int)) (computeAs U $ mapStencil (sobelX b) arr)
    !sY = A.map (^ (2 :: Int)) (computeAs U $ mapStencil (sobelY b) arr)
{-# INLINE sobelOperatorUnfused #-}


-- kirschWStencil
--   :: Num e
--   => Border e -> Stencil Ix2 e e
-- kirschWStencil b = makeConvolutionStencil b (3 :. 3) (1 :. 1) accum
--   where
--     accum f =
--       f (-1 :. -1)   5  .
--       f (-1 :.  0) (-3) .
--       f (-1 :.  1) (-3) .
--       f ( 0 :. -1)   5  .
--       f ( 0 :.  1) (-3) .
--       f ( 1 :. -1)   5  .
--       f ( 1 :.  0) (-3) .
--       f ( 1 :.  1) (-3)
--     {-# INLINE accum #-}
-- {-# INLINE kirschWStencil #-}


average3x3Filter :: (Default a, Fractional a) => Border a -> Stencil Ix2 a a
average3x3Filter b = makeStencil b (3 :. 3) (1 :. 1) $ \ get ->
  (  get (-1 :. -1) + get (-1 :. 0) + get (-1 :. 1) +
     get ( 0 :. -1) + get ( 0 :. 0) + get ( 0 :. 1) +
     get ( 1 :. -1) + get ( 1 :. 0) + get ( 1 :. 1)   ) / 9
{-# INLINE average3x3Filter #-}


average3x3FilterConv :: (Default a, Fractional a) => Border a -> Stencil Ix2 a a
average3x3FilterConv b = let _9th = 1/9 in
  makeConvolutionStencil b (3 :. 3) (1 :. 1) $ \ get ->
  get (-1 :. -1) _9th . get (-1 :. 0) _9th . get (-1 :. 1) _9th .
  get ( 0 :. -1) _9th . get ( 0 :. 0) _9th . get ( 0 :. 1) _9th .
  get ( 1 :. -1) _9th . get ( 1 :. 0) _9th . get ( 1 :. 1) _9th
{-# INLINE average3x3FilterConv #-}



-- | /O(n)/ - Unstructured fold of an array.
foldM ::
     (A.Source r ix m, Monoid m)
  => A.Array r ix m -- ^ Source array
  -> m
foldM = foldlS mappend mempty
{-# INLINE foldM #-}


-- | /O(n)/ - Compute sum of all elements.
sum' :: (A.Source r ix e, Num e) =>
        A.Array r ix e -> e
sum' arr =
  getSum $ foldM $ makeArrayR D (getComp arr) (A.size arr) (\ !ix -> Sum (A.unsafeIndex arr ix))
{-# INLINE sum' #-}
