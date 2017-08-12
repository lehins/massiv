{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
module Bench.Repa where

import           Bench.Common    (heavyFunc, lightFunc)
import           Control.DeepSeq
import           Data.Array.Repa as R
--import           Data.Array.Repa.Repr.Partitioned   as R
--import           Data.Array.Repa.Repr.Undefined
import           Data.Array.Repa.Stencil            as R
import           Data.Array.Repa.Stencil.Dim2       as R
--import           Data.Array.Repa.Unsafe             as R


instance NFData Z where
  rnf z = z `seq` ()


instance Shape sh => NFData (sh :. Int) where
  rnf sh = sh `deepSeq` ()


instance (Shape sh, Source r e) => NFData (Array r sh e) where
  rnf arr = arr `deepSeqArray` ()


tupleToSh2 :: (Int, Int) -> DIM2
tupleToSh2 (i, j) = Z :. i :. j
{-# INLINE tupleToSh2 #-}

arrDLightSh2 :: DIM2 -> Array D DIM2 Double
arrDLightSh2 sz = fromFunction sz (\(Z :. i :. j) -> lightFunc i j)
{-# INLINE arrDLightSh2 #-}

arrDHeavySh2 :: DIM2 -> Array D DIM2 Double
arrDHeavySh2 sz = fromFunction sz (\(Z :. i :. j) -> heavyFunc i j)
{-# INLINE arrDHeavySh2 #-}



-- | Repa stencil base Sobel horizontal convolution
mapSobelRX
  :: (Source r e, Num e) => Array r DIM2 e
     -> Array PC5 DIM2 e
mapSobelRX = mapStencil2 BoundClamp stencil
  where stencil = makeStencil2 3 3
                  (\ix -> case ix of
                      Z :. -1 :.  1 -> Just 1
                      Z :.  0 :.  1 -> Just 2
                      Z :.  1 :.  1 -> Just 1
                      Z :. -1 :. -1 -> Just (-1)
                      Z :.  0 :. -1 -> Just (-2)
                      Z :.  1 :. -1 -> Just (-1)
                      _             -> Nothing)
{-# INLINE mapSobelRX #-}


-- | Repa stencil base Sobel vertical convolution
mapSobelRY
  :: (Source r e, Num e) => Array r DIM2 e
     -> Array PC5 DIM2 e
mapSobelRY = mapStencil2 BoundClamp stencil
  where stencil = makeStencil2 3 3
                  (\ix -> case ix of
                      Z :.  1 :. -1 -> Just 1
                      Z :.  1 :.  0 -> Just 2
                      Z :.  1 :.  1 -> Just 1
                      Z :. -1 :. -1 -> Just (-1)
                      Z :. -1 :.  0 -> Just (-2)
                      Z :. -1 :.  1 -> Just (-1)
                      _             -> Nothing)
{-# INLINE mapSobelRY #-}


-- | Repa stencil base Sobel horizontal convolution
sobelRX :: Num e => Stencil DIM2 e
sobelRX = makeStencil2 3 3
         (\ix -> case ix of
                   Z :. -1 :.  1 -> Just 1
                   Z :.  0 :.  1 -> Just 2
                   Z :.  1 :.  1 -> Just 1
                   Z :. -1 :. -1 -> Just (-1)
                   Z :.  0 :. -1 -> Just (-2)
                   Z :.  1 :. -1 -> Just (-1)
                   _             -> Nothing)
{-# INLINE sobelRX #-}



-- | Repa stencil base Sobel vertical convolution
sobelRY
  :: Num e => Stencil DIM2 e
sobelRY = makeStencil2 3 3
         (\ix -> case ix of
                   Z :.  1 :. -1 -> Just 1
                   Z :.  1 :.  0 -> Just 2
                   Z :.  1 :.  1 -> Just 1
                   Z :. -1 :. -1 -> Just (-1)
                   Z :. -1 :.  0 -> Just (-2)
                   Z :. -1 :.  1 -> Just (-1)
                   _             -> Nothing)
{-# INLINE sobelRY #-}



sobelOperatorR :: Array U DIM2 Double -> Array U DIM2 Double
sobelOperatorR arr =
  R.computeUnboxedS $ R.smap sqrt $ R.szipWith (+) arrX2 arrY2 where
    !arrX2 = R.smap (^ (2 :: Int)) (mapSobelRX arr)
    !arrY2 = R.smap (^ (2 :: Int)) (mapSobelRY arr)



sobelOperatorRUnfused :: Array U DIM2 Double -> Array U DIM2 Double
sobelOperatorRUnfused arr =
  R.computeUnboxedS $ R.map sqrt $ R.zipWith (+) arrX2 arrY2 where
    !arrX2 = R.map (^ (2 :: Int)) $ computeUnboxedS (mapSobelRX arr)
    !arrY2 = R.map (^ (2 :: Int)) $ computeUnboxedS (mapSobelRY arr)
