{-# LANGUAGE BangPatterns #-}
module Main where


import Data.Massiv.Array as A
import Data.Monoid

main :: IO ()
main = do
  -- let sz = Sz2 5 5
  --     largeArr = makeArrayR P Seq sz (toLinearIndex sz)
  --     arr = computeAs P (A.zipWith (+) largeArr largeArr)
  -- print (arr ! (1 :. 1))
  let largeArr1 = makeArrayR DL Seq (Sz1 1000000) id
      --largeArr2 = makeArrayR DL Seq (Sz1 200000) succ
      --arr = computeAs P largeArr
      arr = computeAs P (largeArr1 <> largeArr1)
  -- let consArray :: Int -> Array DL Ix1 Int -> Array DL Ix1 Int
  --     consArray 0 !acc = acc
  --     consArray !n !acc = consArray (n - 1) (n `cons` acc)
  -- let snocArray :: Int -> Array DL Ix1 Int -> Array DL Ix1 Int
  --     snocArray 0 !acc = acc
  --     snocArray !n !acc = snocArray (n - 1) (acc `snoc` n)
  --     largeArr = snocArray 100000 empty
  --     arr = computeAs P largeArr
  print (arr ! 1)
