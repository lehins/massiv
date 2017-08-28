{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Bench.Massiv.Auto
  ( tupleToIx2
  , massDLightIx2
  , module M
  ) where

import           Bench.Common               (lightFunc)
import           Data.Array.Massiv          as M

tupleToIx2 :: (Int, Int) -> Ix2
tupleToIx2 (i, j) = i :. j
{-# INLINE tupleToIx2 #-}


massDLightIx2 :: Comp -> Ix2 -> Massiv Ix2 Double
massDLightIx2 !comp !massSz = makeMassiv comp massSz (\ (i :. j) -> lightFunc i j)
{-# INLINE massDLightIx2 #-}
