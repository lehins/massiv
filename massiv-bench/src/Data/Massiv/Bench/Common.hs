{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Massiv.Bench.Common
  ( makeRandomArray
  , showsType
  , stdGen
  , lightFunc
  , heavyFunc
  , lightFuncIx2
  , arrRLightIx2
  , arrRHeavyIx2
  ) where

import Data.Massiv.Array
import Data.Typeable
import System.Random

stdGen :: StdGen
stdGen = mkStdGen 2020

-- | Use Typeable to show the type.
showsType :: forall t. Typeable t => ShowS
showsType = showsTypeRep (typeRep (Proxy :: Proxy t))

makeRandomArray :: (Index ix, Manifest r e, Random e) => Sz ix -> IO (Array r ix e)
makeRandomArray sz = do
  gen <- newStdGen
  pure $! snd $ randomArrayS gen sz random

lightFunc :: Int -> Int -> Double
lightFunc !i !j =
  sin (fromIntegral (i ^ (2 :: Int) + j ^ (2 :: Int)) :: Double)
{-# INLINE lightFunc #-}

heavyFunc :: Int -> Int -> Double
heavyFunc !i !j =
  sin (sqrt (sqrt (fromIntegral i ** 2 + fromIntegral j ** 2)))
{-# INLINE heavyFunc #-}

lightFuncIx2 :: Ix2 -> Double
lightFuncIx2 (i :. j) = lightFunc i j
{-# INLINE lightFuncIx2 #-}

arrRLightIx2 :: Load r Ix2 Double => r -> Comp -> Sz2 -> Matrix r Double
arrRLightIx2 _ comp arrSz = makeArray comp arrSz (\(i :. j) -> lightFunc i j)
{-# INLINE arrRLightIx2 #-}

arrRHeavyIx2 :: Load r Ix2 Double => r -> Comp -> Sz2 -> Matrix r Double
arrRHeavyIx2 _ comp arrSz = makeArray comp arrSz (\(i :. j) -> heavyFunc i j)
{-# INLINE arrRHeavyIx2 #-}
