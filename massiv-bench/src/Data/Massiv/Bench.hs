{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Massiv.Bench where

import           Data.Massiv.Array

lightFunc :: Int -> Int -> Double
lightFunc !i !j =
  sin (fromIntegral (i ^ (2 :: Int) + j ^ (2 :: Int)) :: Double)
{-# INLINE lightFunc #-}

heavyFunc :: Int -> Int -> Double
heavyFunc !i !j =
  sin (sqrt (sqrt ((fromIntegral i) ** 2 + (fromIntegral j) ** 2)))
{-# INLINE heavyFunc #-}

lightFuncIx2 :: Ix2 -> Double
lightFuncIx2 (i :. j) = lightFunc i j
{-# INLINE lightFuncIx2 #-}

lightFuncIx2T :: Ix2T -> Double
lightFuncIx2T (i, j) = lightFunc i j
{-# INLINE lightFuncIx2T #-}


arrRLightIx2 :: Construct r Ix2 Double => r -> Comp -> Ix2 -> Array r Ix2 Double
arrRLightIx2 _ comp arrSz = makeArray comp arrSz (\ (i :. j) -> lightFunc i j)
{-# INLINE arrRLightIx2 #-}

arrRHeavyIx2 :: Construct r Ix2 Double => r -> Comp -> Ix2 -> Array r Ix2 Double
arrRHeavyIx2 _ comp arrSz = makeArray comp arrSz (\ (i :. j) -> heavyFunc i j)
{-# INLINE arrRHeavyIx2 #-}

