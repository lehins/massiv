{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
module Bench.Repa where

import           Bench.Common    (heavyFunc, lightFunc)
import           Control.DeepSeq
import           Data.Array.Repa as R


instance NFData Z where
  rnf z = z `seq` ()


instance Shape sh => NFData (sh :. Int) where
  rnf sh = sh `deepSeq` ()


tupleToSh2 :: (Int, Int) -> DIM2
tupleToSh2 (i, j) = Z :. i :. j
{-# INLINE tupleToSh2 #-}

arrDLightSh2 :: DIM2 -> Array D DIM2 Double
arrDLightSh2 sz = fromFunction sz (\(Z :. i :. j) -> lightFunc i j)
{-# INLINE arrDLightSh2 #-}

arrDHeavySh2 :: DIM2 -> Array D DIM2 Double
arrDHeavySh2 sz = fromFunction sz (\(Z :. i :. j) -> heavyFunc i j)
{-# INLINE arrDHeavySh2 #-}
