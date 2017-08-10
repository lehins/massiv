module Bench.Vector where

import           Bench.Common                (heavyFunc, lightFunc)
import           Data.Vector.Unboxed


vecLight2 :: (Int, Int) -> Vector Double
vecLight2 (m, n) = generate (m * n) $ \ k -> uncurry lightFunc (k `quotRem` n)
{-# INLINE vecLight2 #-}

vecHeavy2 :: (Int, Int) -> Vector Double
vecHeavy2 (m, n) = generate (m * n) $ \ k -> uncurry heavyFunc (k `quotRem` n)
{-# INLINE vecHeavy2 #-}

