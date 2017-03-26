{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
module Compute where

import           Data.Array.Massiv                   as M
import           Data.Array.Massiv.Windowed          as M
import           Data.Array.Repa                     as R
import           Data.Array.Repa.Repr.Partitioned    as R
import           Data.Array.Repa.Repr.Undefined
import qualified Data.Vector.Unboxed                 as VU
import           GHC.Base                            (quotRemInt)
import           Prelude                             as P


makeWindowed
  :: (R.Source r2 e)
  => R.DIM2
  -> R.DIM2
  -> R.Array r2 R.DIM2 e
  -> R.Array r1 R.DIM2 e
  -> R.Array (P r2 (P r1 (P r1 (P r1 (P r1 X))))) R.DIM2 e
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


lightF :: (Num b, Integral a) => (a, a) -> b
lightF !(i, j) = fromIntegral (max i j `div` (1 + min i j))
{-# INLINE lightF #-}

heavyF :: (Floating a2, Integral a1, Integral a) => (a1, a) -> a2
heavyF !(i, j) =
        sin (sqrt (sqrt ((fromIntegral i) ** 2 + (fromIntegral j) ** 2)))
{-# INLINE heavyF #-}

vecU, vecU' :: (Int, Int) -> VU.Vector Double
vecU !(m, n) = VU.generate (m * n) $ \ !k -> lightF (k `quotRemInt` n)
{-# INLINE vecU #-}
vecU' !(m, n) = VU.generate (m * n) $ \ !k -> heavyF (k `quotRemInt` n)
{-# INLINE vecU' #-}

arrR, arrR' :: (Int, Int) -> R.Array R.D R.DIM2 Double
arrR !(m, n) = fromFunction (Z :. m :. n) (\(Z :. i :. j) -> lightF (i, j))
{-# INLINE arrR #-}
arrR' !(m, n) = fromFunction (Z :. m :. n) (\(Z :. i :. j) -> heavyF (i, j))
{-# INLINE arrR' #-}

arrWindowedR :: (Int, Int)
             -> R.Array (P R.D (P R.D (P R.D (P R.D (P R.D X))))) R.DIM2 Double
arrWindowedR sh@(m, n) =
  makeWindowed (Z :. 20 :. 25) (Z :. m - 20 :. n - 25) (arrR sh) (arrR' sh)
{-# INLINE arrWindowedR #-}

arrM, arrM' :: (Int, Int) -> M.Array M.D M.DIM2 Double
arrM !arrSz = makeArray2D arrSz lightF
{-# INLINE arrM #-}
arrM' !arrSz = makeArray2D arrSz heavyF
{-# INLINE arrM' #-}

arrWindowedM :: (Int, Int) -> M.Array M.W M.DIM2 Double
arrWindowedM !arrSz@(m, n) =
  makeArrayWindowed (makeArray2D arrSz heavyF) (20, 25) (m - 20, n - 25) lightF
{-# INLINE arrWindowedM #-}
