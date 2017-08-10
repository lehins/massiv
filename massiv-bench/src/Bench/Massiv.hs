{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Bench.Massiv where

import           Bench.Common                (heavyFunc, lightFunc)
import           Data.Array.Massiv


tupleToIx2 :: (Int, Int) -> Ix2
tupleToIx2 (i, j) = i :. j
{-# INLINE tupleToIx2 #-}

tupleToIx2T :: (Int, Int) -> Ix2T
tupleToIx2T = id
{-# INLINE tupleToIx2T #-}

arrDLightIx2 :: Comp -> Ix2 -> Array D Ix2 Double
arrDLightIx2 comp arrSz = makeArray comp arrSz (\ (i :. j) -> lightFunc i j)
{-# INLINE arrDLightIx2 #-}

arrDHeavyIx2 :: Comp -> Ix2 -> Array D Ix2 Double
arrDHeavyIx2 comp arrSz = makeArray comp arrSz (\ (i :. j) -> heavyFunc i j)
{-# INLINE arrDHeavyIx2 #-}


arrDLightIx2T :: Comp -> Ix2T -> Array D Ix2T Double
arrDLightIx2T comp arrSz = makeArray comp arrSz (\ (i, j) -> lightFunc i j)
{-# INLINE arrDLightIx2T #-}

arrDHeavyIx2T :: Comp -> Ix2T -> Array D Ix2T Double
arrDHeavyIx2T comp arrSz = makeArray comp arrSz (\ (i, j) -> heavyFunc i j)
{-# INLINE arrDHeavyIx2T #-}
