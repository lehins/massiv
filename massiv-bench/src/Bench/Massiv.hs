{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Bench.Massiv
  ( tupleToIx2
  , massDLightIx2
  , module M
  ) where

import           Bench.Common (lightFunc)
import           Data.Massiv  as M

tupleToIx2 :: (Int, Int) -> Ix2
tupleToIx2 (i, j) = i :. j
{-# INLINE tupleToIx2 #-}


massDLightIx2 :: Ix2 -> Massiv Ix2 Double
massDLightIx2 !massSz = makeMassiv massSz (\ (i :. j) -> lightFunc i j)
{-# INLINE massDLightIx2 #-}



