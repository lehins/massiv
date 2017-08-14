{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Bench.Massiv where

import           Bench.Common              (heavyFunc, lightFunc)
import           Control.DeepSeq
import           Data.Array.Massiv         as M
import           Data.Array.Massiv.Stencil
import           Data.Default              (Default)

-- | Bogus DeepSeq for delayed array so it can be fed to the `env`.
instance Index ix => NFData (Array D ix e) where
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
  :: (Eq e, Num e, Unbox e) => Border e -> Stencil Ix2 e e
sobelKernelStencilX b =
  mkConvolutionStencilFromKernel b $ fromListIx2As U Seq [ [ 1, 0, -1 ]
                                                         , [ 2, 0, -2 ]
                                                         , [ 1, 0, -1 ] ]
{-# INLINE sobelKernelStencilX #-}


sobelX :: Num e => Border e -> Stencil Ix2 e e
sobelX b = mkConvolutionStencil b (3 :. 3) (1 :. 1) accum where
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
sobelY b = mkConvolutionStencil b (3 :. 3) (1 :. 1) accum where
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
sobelOperator b = sqrt <$> ((+) <$> sX <*> sY) where
  !sX = (^ (2 :: Int)) <$> (sobelX b)
  !sY = (^ (2 :: Int)) <$> (sobelY b)
{-# INLINE sobelOperator #-}


sobelOperatorAlt :: (Default b, Floating b) => Border b -> Stencil Ix2 b b
sobelOperatorAlt b = sqrt (sX + sY) where
  !sX = fmap (^ (2 :: Int)) (sobelX b)
  !sY = fmap (^ (2 :: Int)) (sobelY b)
{-# INLINE sobelOperatorAlt #-}


sobelOperatorUnfused
  :: (Unbox b, Eq b, Floating b)
  => Border b -> Array U Ix2 b -> Array U Ix2 b
sobelOperatorUnfused b arr = computeAs U $ M.map sqrt (M.zipWith (+) sX sY) where
  !sX = M.map (^ (2 :: Int)) (computeAs U $ mapStencil (sobelX b) arr)
  !sY = M.map (^ (2 :: Int)) (computeAs U $ mapStencil (sobelY b) arr)
{-# INLINE sobelOperatorUnfused #-}


kirschWStencil
  :: Num e
  => Border e -> Stencil Ix2 e e
kirschWStencil b = mkConvolutionStencil b (3 :. 3) (1 :. 1) accum
  where
    accum f =
      f (-1 :. -1)   5  .
      f (-1 :.  0) (-3) .
      f (-1 :.  1) (-3) .
      f ( 0 :. -1)   5  .
      f ( 0 :.  1) (-3) .
      f ( 1 :. -1)   5  .
      f ( 1 :.  0) (-3) .
      f ( 1 :.  1) (-3)
    {-# INLINE accum #-}
{-# INLINE kirschWStencil #-}



sobelTX :: Num e => Border e -> Stencil Ix2T e e
sobelTX b = mkConvolutionStencil b (3, 3) (1, 1) accum where
  accum f =
     f (-1, -1)   1  .
     f ( 0, -1)   2  .
     f ( 1, -1)   1  .
     f (-1,  1) (-1) .
     f ( 0,  1) (-2) .
     f ( 1,  1) (-1)
  {-# INLINE accum #-}
{-# INLINE sobelTX #-}


sobelTY :: Num e => Border e -> Stencil Ix2T e e
sobelTY b = mkConvolutionStencil b (3, 3) (1, 1) accum where
  accum f =
     f (-1, -1)   1  .
     f (-1,  0)   2  .
     f (-1,  1)   1  .
     f ( 1, -1) (-1) .
     f ( 1,  0) (-2) .
     f ( 1,  1) (-1)
  {-# INLINE accum #-}
{-# INLINE sobelTY #-}



sobelOperatorT :: (Default b, Floating b) => Border b -> Stencil Ix2T b b
sobelOperatorT b = sqrt <$> ((+) <$> sX <*> sY) where
  !sX = (^ (2 :: Int)) <$> sobelTX b
  !sY = (^ (2 :: Int)) <$> sobelTY b
{-# INLINE sobelOperatorT #-}


sum3x3Filter :: (Default a, Fractional a) => Border a -> Stencil Ix2 a a
sum3x3Filter b = mkConvolutionStencil b (3 :. 3) (1 :. 1) $ \ get ->
  get (-1 :. -1) 1 . get (-1 :. 0) 1 . get (-1 :. 1) 1 .
  get ( 0 :. -1) 1 . get ( 0 :. 0) 1 . get ( 0 :. 1) 1 .
  get ( 1 :. -1) 1 . get ( 1 :. 0) 1 . get ( 1 :. 1) 1
{-# INLINE sum3x3Filter #-}


average3x3Filter :: (Default a, Fractional a) => Border a -> Stencil Ix2 a a
average3x3Filter b = makeStencil b (3 :. 3) (1 :. 1) $ \ get ->
  (  get (-1 :. -1) + get (-1 :. 0) + get (-1 :. 1) +
     get ( 0 :. -1) + get ( 0 :. 0) + get ( 0 :. 1) +
     get ( 1 :. -1) + get ( 1 :. 0) + get ( 1 :. 1)   ) / 9
{-# INLINE average3x3Filter #-}


average3x3FilterConvMap :: (Default a, Fractional a) => Border a -> Stencil Ix2 a a
average3x3FilterConvMap b = fmap (/9) $ mkConvolutionStencil b (3 :. 3) (1 :. 1) $ \ get ->
  get (-1 :. -1) 1 . get (-1 :. 0) 1 . get (-1 :. 1) 1 .
  get ( 0 :. -1) 1 . get ( 0 :. 0) 1 . get ( 0 :. 1) 1 .
  get ( 1 :. -1) 1 . get ( 1 :. 0) 1 . get ( 1 :. 1) 1
{-# INLINE average3x3FilterConvMap #-}

average3x3FilterConvMap' :: (Default a, Fractional a) => Border a -> Stencil Ix2 a a
average3x3FilterConvMap' b = sMap (/9) $ mkConvolutionStencil b (3 :. 3) (1 :. 1) $ \ get ->
  get (-1 :. -1) 1 . get (-1 :. 0) 1 . get (-1 :. 1) 1 .
  get ( 0 :. -1) 1 . get ( 0 :. 0) 1 . get ( 0 :. 1) 1 .
  get ( 1 :. -1) 1 . get ( 1 :. 0) 1 . get ( 1 :. 1) 1
{-# INLINE average3x3FilterConvMap' #-}


average3x3FilterConv :: (Default a, Fractional a) => Border a -> Stencil Ix2 a a
average3x3FilterConv b = let _9th = 1/9 in
  mkConvolutionStencil b (3 :. 3) (1 :. 1) $ \ get ->
  get (-1 :. -1) _9th . get (-1 :. 0) _9th . get (-1 :. 1) _9th .
  get ( 0 :. -1) _9th . get ( 0 :. 0) _9th . get ( 0 :. 1) _9th .
  get ( 1 :. -1) _9th . get ( 1 :. 0) _9th . get ( 1 :. 1) _9th
{-# INLINE average3x3FilterConv #-}
