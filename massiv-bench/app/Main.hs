module Main where


import Data.Massiv.Array as A


main :: IO ()
main = do
  let largeArr = makeArrayR P Seq (5 :. 5) (toLinearIndex (5 :. 5))
      arr = computeAs P (A.zipWith (+) largeArr largeArr)
  print (arr ! (1 :. 1))
