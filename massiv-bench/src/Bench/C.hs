module Bench.C where

import System.IO.Unsafe
import Data.Massiv.Array as A
import Foreign

foreign import ccall unsafe "c_sum" c_sum :: Ptr Double -> Int -> IO Double

sumDouble :: Index ix => Array S ix Double -> Double
sumDouble arr = unsafePerformIO $ A.unsafeWithPtr arr (`c_sum` (totalElem (size arr)))
