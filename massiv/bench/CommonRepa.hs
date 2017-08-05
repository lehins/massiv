{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module CommonRepa where

import           CommonMassiv (lightF, heavyF)
import           Data.Array.Repa                    as R
import           Data.Array.Repa.Algorithms.Matrix  as R hiding (mmultP)
import           Data.Array.Repa.Repr.Partitioned   as R
import           Data.Array.Repa.Repr.Undefined
import           Data.Array.Repa.Stencil            as R
import           Data.Array.Repa.Stencil.Dim2       as R
import           Data.Array.Repa.Unsafe             as R
import           Prelude                            as P


makeWindowed
  :: (Source r2 e)
  => DIM2
  -> DIM2
  -> Array r2 DIM2 e
  -> Array r1 DIM2 e
  -> Array (P r2 (P r1 (P r1 (P r1 (P r1 X))))) DIM2 e
makeWindowed (Z :. it :. jt) (Z :. wm :. wn) arrWindow arrBorder =
  let sh@(Z :. m :. n) = extent arrWindow
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


arrRLight :: (Num a)
     => (Int, Int) -> Array D DIM2 a
arrRLight !(m, n) = fromFunction (Z :. m :. n) (\(Z :. i :. j) -> lightF (i, j))
{-# INLINE arrRLight #-}

arrR :: (Int, Int) -> Array D DIM2 Double
arrR = arrRLight
{-# INLINE arrR #-}

arrR' :: (Int, Int) -> Array D DIM2 Double
arrR' !(m, n) = fromFunction (Z :. m :. n) (\(Z :. i :. j) -> heavyF (i, j))
{-# INLINE arrR' #-}

arrWindowedR :: (Int, Int)
             -> Array (P D (P D (P D (P D (P D X))))) DIM2 Double
arrWindowedR sh@(m, n) =
  makeWindowed (Z :. 20 :. 25) (Z :. m - 20 :. n - 25) (arrR sh) (arrR' sh)
{-# INLINE arrWindowedR #-}



mmultP  :: Monad m
        => Array U DIM2 Double
        -> Array U DIM2 Double
        -> m (Array U DIM2 Double)
mmultP arr brr
 = [arr, brr] `deepSeqArrays`
   do   trr      <- transpose2P brr
        let (Z :. h1  :. _)  = extent arr
        let (Z :. _   :. w2) = extent brr
        trr `deepSeqArray` computeP
         $ fromFunction (Z :. h1 :. w2)
         $ \ix   -> sumAllS
                  $ R.zipWith (*)
                        (unsafeSlice arr (Any :. (row ix) :. All))
                        (unsafeSlice trr (Any :. (col ix) :. All))
{-# NOINLINE mmultP #-}

-- | Repa stencil base Sobel horizontal convolution
sobelXR
  :: (Source r e, Num e) => Array r DIM2 e
     -> Array PC5 DIM2 e
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
  :: (Source r e, Num e) => Array r DIM2 e
     -> Array PC5 DIM2 e
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
