module Main where


import Data.Massiv.Array as A


sobelX :: Stencil Ix2 Int Int
sobelX = makeStencil (3 :. 3) (1 :. 1) $ \ f -> f (0 :. -1) * 217
{-# INLINE sobelX #-}



main :: IO ()
main = do
  let largeArr = makeArrayR P Seq (5 :. 5) (succ . toLinearIndex (5 :. 5))
  let arr = computeAs P $ mapStencil (Fill 0) sobelX largeArr
  print $ A.sum arr
