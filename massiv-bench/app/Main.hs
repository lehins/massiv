module Main where


import Data.Massiv.Array as A


sobelX :: Stencil Ix2 Int Int
sobelX = makeStencil (3 :. 3) (1 :. 1) $ \ f -> f (0 :. -1) * 217
{-# INLINE sobelX #-}



main :: IO ()
main = do
  let largeArr = makeArrayR P Par (5 :. 5) (succ . toLinearIndex (5 :. 5))
  let arr = computeWithStrideAs P (Stride (1 :. 1)) $ mapStencil (Fill 0) sobelX largeArr
  --let a = (# 5, 6 #)
  print (arr ! (1 :. 1))
