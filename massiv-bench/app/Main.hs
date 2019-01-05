module Main where


import Data.Massiv.Array as A


main :: IO ()
main = do
  let sz = Sz2 5 5
      largeArr = makeArrayR P Seq sz (toLinearIndex sz)
      arr = computeAs P (A.zipWith (+) largeArr largeArr)
  print (arr ! (1 :. 1))
