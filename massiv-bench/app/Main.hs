module Main where


import Data.Massiv.Array as A


main :: IO ()
main = do
  let largeArr = makeArrayR P Seq (5 :. 5) (toLinearIndex (5 :. 5))
      arr = largeArr |*| transpose largeArr
  print (arr ! (1 :. 1))
