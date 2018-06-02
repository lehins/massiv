{-# LANGUAGE FlexibleContexts #-}
module Examples.PixelGrid where

import Data.Massiv.Array as A
import Data.Massiv.Array.IO
import Data.Word
import Graphics.ColorSpace

-- | Scale the array, negate values and create an image with a grid.
pixelGrid :: ColorSpace cs Word8 => Int -> Image S cs Word8 -> Image D cs Word8
pixelGrid k8 arr = A.makeArray (getComp arr) sz' getNewElt
  where
    k = succ k8
    (m :. n) = size arr
    sz' = (1 + m * k :. 1 + n * k)
    getNewElt (i :. j) =
      if i `mod` k == 0 || j `mod` k == 0
        then 128
        else A.index' arr (((i - 1) `div` k) :. ((j - 1) `div` k))
