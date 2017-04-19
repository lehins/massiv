{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
module Compute where

import           Data.Array.Massiv                   as M
import           Data.Array.Massiv.Delayed.Windowed  as M
import           Data.Array.Repa.Stencil             as R
import           Data.Array.Repa.Stencil.Dim2        as R
import           Data.Array.Repa                     as R
import           Data.Array.Repa.Repr.Partitioned    as R
import           Data.Array.Repa.Repr.Undefined
import qualified Data.Vector.Unboxed                 as VU
import           Prelude                             as P


makeWindowed
  :: (R.Source r2 e)
  => R.DIM2
  -> R.DIM2
  -> R.Array r2 R.DIM2 e
  -> R.Array r1 R.DIM2 e
  -> R.Array (R.P r2 (R.P r1 (R.P r1 (R.P r1 (R.P r1 X))))) R.DIM2 e
makeWindowed (Z :. it :. jt) (Z :. wm :. wn) arrWindow arrBorder =
  let sh@(Z :. m :. n) = R.extent arrWindow
      inInternal !(Z :. i :. j) = i >= it && i < ib && j >= jt && j < jb
      {-# INLINE inInternal #-}
      inBorder = not . inInternal
      {-# INLINE inBorder #-}
      !(ib, jb) = (wm + it, wn + jt)
  in APart sh (Range (Z :. it :. jt) (Z :. wm :. wn) inInternal) arrWindow $
     APart sh (Range (Z :. 0 :. 0) (Z :. it :. n) inBorder) arrBorder $
     APart sh (Range (Z :. it :. 0) (Z :. wm :. jt) inBorder) arrBorder $
     APart sh (Range (Z :. it :. jb) (Z :. wm :. n - jb) inBorder) arrBorder $
     APart sh (Range (Z :. ib :. 0) (Z :. m - ib :. n) inBorder) arrBorder $
     AUndefined sh
{-# INLINE makeWindowed #-}


lightF :: Num b => (Int, Int) -> b
lightF !(i, j) =
  fromIntegral
    (round (sin (fromIntegral (i ^ (2 :: Int) + j ^ (2 :: Int)) :: Float)) :: Int)
{-# INLINE lightF #-}

heavyF :: (Floating a2, Integral a) => (a, a) -> a2
heavyF !(i, j) =
        sin (sqrt (sqrt ((fromIntegral i) ** 2 + (fromIntegral j) ** 2)))
{-# INLINE heavyF #-}

vecULight :: (VU.Unbox a, Num a) => (Int, Int) -> VU.Vector a
vecULight !(m, n) = VU.generate (m * n) $ \ !k -> lightF (k `quotRem` n)
{-# INLINE vecULight #-}

vecU :: (Int, Int) -> VU.Vector Double
vecU = vecULight
{-# INLINE vecU #-}

vecU' :: (Int, Int) -> VU.Vector Double
vecU' !(m, n) = VU.generate (m * n) $ \ !k -> heavyF (k `quotRem` n)
{-# INLINE vecU' #-}

arrRLight :: (Num a)
     => (Int, Int) -> R.Array R.D R.DIM2 a
arrRLight !(m, n) = fromFunction (Z :. m :. n) (\(Z :. i :. j) -> lightF (i, j))
{-# INLINE arrRLight #-}

arrR :: (Int, Int) -> R.Array R.D R.DIM2 Double
arrR = arrRLight
{-# INLINE arrR #-}

arrR' :: (Int, Int) -> R.Array R.D R.DIM2 Double
arrR' !(m, n) = fromFunction (Z :. m :. n) (\(Z :. i :. j) -> heavyF (i, j))
{-# INLINE arrR' #-}

arrWindowedR :: (Int, Int)
             -> R.Array (R.P R.D (R.P R.D (R.P R.D (R.P R.D (R.P R.D X))))) R.DIM2 Double
arrWindowedR sh@(m, n) =
  makeWindowed (Z :. 20 :. 25) (Z :. m - 20 :. n - 25) (arrR sh) (arrR' sh)
{-# INLINE arrWindowedR #-}

arrMLight :: (Num a) => (Int, Int) -> M.Array M.D M.DIM2 a
arrMLight !arrSz = makeArray2D arrSz lightF
{-# INLINE arrMLight #-}

arrM :: (Int, Int) -> M.Array M.D M.DIM2 Double
arrM = arrMLight
{-# INLINE arrM #-}

arrM' :: (Int, Int) -> M.Array M.D M.DIM2 Double
arrM' !arrSz = makeArray2D arrSz heavyF
{-# INLINE arrM' #-}

arrWindowedM :: (Int, Int) -> M.Array WD M.DIM2 Double
arrWindowedM !arrSz@(m, n) =
  makeArrayWindowed (makeArray2D arrSz heavyF) (20, 25) (m - 20, n - 25) lightF
{-# INLINE arrWindowedM #-}



-- | Repa stencil base Sobel horizontal convolution
sobelXR
  :: (R.Source r e, Num e) => R.Array r R.DIM2 e
     -> R.Array PC5 R.DIM2 e
sobelXR = mapStencil2 (BoundClamp) stencil
  where stencil = makeStencil2 3 3
                  (\ix -> case ix of
                      Z :. -1 :.  1 -> Just 1
                      Z :.  0 :.  1 -> Just 2
                      Z :.  1 :.  1 -> Just 1
                      Z :. -1 :. -1 -> Just (-1)
                      Z :.  0 :. -1 -> Just (-2)
                      Z :.  1 :. -1 -> Just (-1)
                      _             -> Nothing)
{-# INLINE sobelXR #-}



-- | Repa stencil base Sobel vertical convolution
sobelYR
  :: (R.Source r e, Num e) => R.Array r R.DIM2 e
     -> R.Array PC5 R.DIM2 e
sobelYR = mapStencil2 (BoundClamp) stencil
  where stencil = makeStencil2 3 3
                  (\ix -> case ix of
                      Z :.  1 :. -1 -> Just 1
                      Z :.  1 :.  0 -> Just 2
                      Z :.  1 :.  1 -> Just 1
                      Z :. -1 :. -1 -> Just (-1)
                      Z :. -1 :.  0 -> Just (-2)
                      Z :. -1 :.  1 -> Just (-1)
                      _             -> Nothing)
{-# INLINE sobelYR #-}
