module Main where

import Data.Massiv.Array as A
import Data.Massiv.Array.Numeric.Integral
import System.Environment

gaussian2 :: Floating a => a -> a -> a -> a
gaussian2 stdDev y x = exp (-(x ^ (2 :: Int) + y ^ (2 :: Int)) / var2) / (var2 * pi)
  where
    var2 = 2 * stdDev ^ (2 :: Int)
{-# INLINE gaussian2 #-}

gaussian1 :: Floating a => a -> a -> a
gaussian1 stdDev x = exp (- (x ^ (2 :: Int)) / var2) / (sqrt (var2 * pi))
  where
    var2 = 2 * stdDev ^ (2 :: Int)
{-# INLINE gaussian1 #-}

mkGaussian2 :: Int -> Int -> Array M Ix2 Double
mkGaussian2 n side  =
  let f scale (i :. j) = gaussian2 (1 :: Double) (scale i) (scale j)
      {-# INLINE f #-}
      sz = Sz (side :. side)
      a = fromIntegral side / 2 - fromIntegral side
      d = 1
  in simpsonsRule Par P f a d sz n
{-# INLINE mkGaussian2 #-}


mkGaussian1 :: Int -> Int -> Array M Ix2 Double
mkGaussian1 n side  =
  let f scale i = gaussian1 (1 :: Double) (scale i)
      {-# INLINE f #-}
      a = fromIntegral side / 2 - fromIntegral side
      d = 1
  in resize' (Sz (1 :. side)) $ simpsonsRule Par P f a d (Sz side) n
{-# INLINE mkGaussian1 #-}




main :: IO ()
main = do
  [nStr] <- getArgs
  let n = Prelude.read nStr
      side = 7
  print $ mkGaussian1 n side
