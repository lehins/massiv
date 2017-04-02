{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Array.Massiv.Stencil
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Stencil where

import           Control.Applicative
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Delayed
import           Data.Array.Massiv.Delayed.Windowed
import           Data.Array.Massiv.Manifest
import           Data.Array.Massiv.Manifest.Unboxed
import           Data.Default                       (Default (def))
import qualified Data.Vector.Unboxed                as VU
import           GHC.Exts                           (inline)


data Orientation
  = Vertical
  | Horizontal deriving Show






-- makeStencil
--   :: (Eq e, Num e, Source r DIM2, VU.Unbox e)
--   => e -> Array r DIM2 e -> Stencil DIM2 e
-- makeStencil zero arr = Stencil zero (m2, n2) accumulator
--   where
--     !(m, n) = size arr
--     !(m2, n2) = (m `div` 2, n `div` 2)
--     !(kernelIx, kernelVal) = filterIx (/= zero) arr
--     !kernelIxM = computeUnboxedS $ mapA (\ (i, _) -> i - m2) kernelIx
--     !kernelIxN = computeUnboxedS $ mapA (\ (_, j) -> j - n2) kernelIx
--     accumulator getVal !(i, j) = ifoldl RowMajor apply zero kernelVal
--       where
--         apply !acc !k !kVal = {-# SCC "SCC:foldlApply" #-}
--           acc + kVal * getVal (i + unsafeIndex kernelIxM k, j + unsafeIndex kernelIxN k)
--         {-# INLINE apply #-}
--     {-# INLINE accumulator #-}
-- {-# INLINE makeStencil #-}


-- makeStencil
--   :: (Eq e, Num e, Source r DIM2 e, VU.Unbox e)
--   => e -> Array r DIM2 e -> Stencil DIM2 e
-- makeStencil zero arr = Stencil zero (m2, n2) accumulator
--   where
--     !(m, n) = size arr
--     !(m2, n2) = (m `div` 2, n `div` 2)
--     !(kernelIx, kernelVal) = filterIx (/= zero) arr
--     !kernelIx' = computeUnboxedS $ mapA (\ !(i, j) -> (i - m2, j - n2)) kernelIx
--     accumulator getVal !(i, j) = ifoldl RowMajor apply zero kernelVal
--       where
--         apply !acc !k !kVal =
--           let !(iD, jD) = unsafeIndex kernelIx' k
--           in acc + kVal * getVal (i + iD, j + jD)
--         {-# INLINE apply #-}
--     {-# INLINE accumulator #-}
-- {-# INLINE makeStencil #-}


-- makeStencil2D
--   :: (Eq e, Num e, Source r DIM2 e, VU.Unbox e)
--   => e -> Array r DIM2 e -> Stencil DIM2 e
-- makeStencil2D zero kernel = Stencil zero (m2, n2) accumulator
--   where
--     !(m, n) = size kernel
--     !(m2, n2) = (m `div` 2, n `div` 2)
--     accumulator getVal (i, j) = ifoldl RowMajor apply zero kernel
--       where
--         apply !acc (ki, kj) !kVal = if kVal == zero
--                                     then acc
--                                     else acc + kVal * getVal (i + ki - m2, j + kj -n2)
--         {-# INLINE apply #-}
--     {-# INLINE accumulator #-}
-- {-# INLINE makeStencil2D #-}



-- makeSobelStencil2D
--   :: (Num e)
--   => e -> Stencil DIM2 e
-- makeSobelStencil2D !zero = Stencil zero (1, 1) accumulator
--   where
--     accumulator getVal !(i, j) =
--       getVal (i-1, j-1) * 1 +
--       getVal (  i, j-1) * 2 +
--       getVal (i+1, j-1) * 1 +
--       getVal (i-1, j+1) * (-1) +
--       getVal (  i, j+1) * (-2) +
--       getVal (i+1, j+1) * (-1)
--     {-# INLINE accumulator #-}
-- {-# INLINE makeSobelStencil2D #-}

-- -- | KirschW stencil (already rotated 180 degrees for correlation
-- kirschWStencil :: (Num e, Eq e, VU.Unbox e) => Stencil DIM2 e
-- kirschWStencil =
--   makeStencil2D 0 $ fromListsUnboxed  [ [ -3, -3, 5 ]
--                                       , [ -3,  0, 5 ]
--                                       , [ -3, -3, 5 ] ]
-- {-# INLINE kirschWStencil #-}


-- -- | KirschW stencil (already rotated 180 degrees for correlation
-- kirschWStencil' :: (Num e, Eq e, VU.Unbox e) => Stencil DIM2 e
-- kirschWStencil' =
--   makeStencil 0 $ fromListsUnboxed  [ [ -3, -3, 5 ]
--                                     , [ -3,  0, 5 ]
--                                     , [ -3, -3, 5 ] ]
-- {-# INLINE kirschWStencil' #-}


-- -- | Sobel stencil (already rotated 180 degrees for correlation
-- sobelStencil :: (Num e, Eq e, VU.Unbox e) => Orientation -> Stencil DIM2 e
-- sobelStencil Vertical =
--   makeStencil2D 0 $ fromListsUnboxed [ [  1,  2,  1 ]
--                                      , [  0,  0,  0 ]
--                                      , [ -1, -2, -1 ] ]
-- sobelStencil Horizontal =
--   makeStencil2D 0 $ fromListsUnboxed  [ [ 1, 0, -1 ]
--                                       , [ 2, 0, -2 ]
--                                       , [ 1, 0, -1 ] ]
-- {-# INLINE sobelStencil #-}


-- -- | Sobel stencil (already rotated 180 degrees for correlation
-- sobelStencil' :: (Num e, Eq e, VU.Unbox e) => Orientation -> Stencil DIM2 e
-- sobelStencil' Vertical =
--   makeStencil 0 $ fromListsUnboxed [ [  1,  2,  1 ]
--                                    , [  0,  0,  0 ]
--                                    , [ -1, -2, -1 ] ]
-- sobelStencil' Horizontal =
--   makeStencil 0 $ fromListsUnboxed  [ [ 1, 0, -1 ]
--                                     , [ 2, 0, -2 ]
--                                     , [ 1, 0, -1 ] ]
-- {-# INLINE sobelStencil' #-}





data Stencil ix e a = Stencil
  { stencilBorder :: Border e
  , stencilSize   :: !ix
  , stencilCenter :: !ix
  , stencilFunc   :: (ix -> e) -> ix -> a
  }


instance Functor (Stencil ix e) where
  fmap f stencil@(Stencil { stencilFunc = g }) =
    stencil { stencilFunc = (\ s -> f . g s) }
  {-# INLINE fmap #-}


-- TODO: Figure out interchange law (u <*> pure y = pure ($ y) <*> u) and issue
-- with discarding size and center. Best idea so far is to increase stencil size to
-- the maximum one and shift the center of the other stencil so the yboth match
-- up. This approach would also remove requirement to validate the result
-- Stencil - both stencils are trusted, increasing the size will not affect the
-- safety.
instance (Default e, Index ix) => Applicative (Stencil ix e) where
  pure a = Stencil Edge (liftIndex (+1) zeroIndex) zeroIndex (const (const a))
  {-# INLINE pure #-}

  (<*>) (Stencil _ _ _ f) (Stencil sB sSz sC g) =
    validateStencil def (Stencil sB sSz sC (\ gV ix -> (f gV ix) (g gV ix)))
  {-# INLINE (<*>) #-}

instance (Index ix, Default e, Num a) => Num (Stencil ix e a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Index ix, Default e, Fractional a) => Fractional (Stencil ix e a) where
  (/) = liftA2 (/)
  recip = fmap recip
  fromRational = pure . fromRational

instance (Index ix, Default e, Floating a) => Floating (Stencil ix e a) where
  pi = pure pi
  exp = fmap exp
  log = fmap log
  sqrt = fmap sqrt
  (**) = liftA2 (**)
  logBase = liftA2 logBase
  sin = fmap sin
  cos = fmap cos
  tan = fmap tan
  asin = fmap asin
  acos = fmap acos
  atan = fmap atan
  sinh = fmap sinh
  cosh = fmap cosh
  tanh = fmap tanh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh


safeStencil :: Index a => Array D a t -> a -> t
safeStencil DArray {..} ix
  | isSafeIndex dSize ix = dUnsafeIndex ix
  | otherwise =
    error $
    "Index is out of bounds: " ++ show ix ++ " for stencil size: " ++ show dSize


-- | Correlate an image with a kernel. Border resolution technique is required.
correlate :: (Num e, Eq e, VU.Unbox e, Manifest r DIM2 e) =>
     Border e -> Array r DIM2 e -> Array W DIM2 e
correlate border !arr =
  WArray
    (m, n)
    (Just (3, 3))
    (getStencil (borderIndex border arr))
    (kM2, kN2)
    (m - kM2 * 2, n - kN2 * 2)
    (getStencil (unsafeIndex arr))
  where
    !(m, n) = size arr
    !(kM2, kN2) = (1, 1)
    getStencil getVal !(i, j) = stencil (\ !(iD, jD) -> getVal (i + iD, j + jD))
    {-# INLINE getStencil #-}
    stencil iArr =
      2 * (iArr (0, 1) - iArr (0, -1)) + iArr (-1, 1) + iArr (1, 1) -
      iArr (-1, -1) - iArr (1, -1)
    {-# INLINE stencil #-}
{-# INLINE correlate #-}



mapStencil :: (Source r ix e, Eq e, Num e, VU.Unbox e, Manifest r ix e) =>
              Stencil ix e a -> Array r ix e -> Array W ix a
mapStencil (Stencil b sSz sCenter stencilF) !arr =
  WArray
    sz
    (Just sSz)
    (stencilF (borderIndex b arr))
    sCenter
    (liftIndex2 (-) sz (liftIndex2 (+) sSz sCenter))
    (stencilF (unsafeIndex arr))
  where
    !sz = size arr
{-# INLINE mapStencil #-}



mkConvolutionStencil
  :: (Index ix, Num e)
  => Border e
  -> ix
  -> ix
  -> ((ix -> e -> e -> e) -> e -> e)
  -> Stencil ix e e
mkConvolutionStencil b !sSz !sCenter makeStencil =
  validateStencil 0 $ Stencil b sSz sCenter stencil
  where
    stencil getVal !ix =
      (inline makeStencil $ \ !ixD !kVal !acc ->
          getVal (liftIndex2 (-) ix ixD) * kVal + acc)
      0
    {-# INLINE stencil #-}
{-# INLINE mkConvolutionStencil #-}


-- | Make a stencil out of a Kernel Array
mkConvolutionStencilFromKernel
  :: (Iterator RowMajor ix, Manifest r ix e, Eq e, Num e)
  => Border e
  -> Array r ix e
  -> Stencil ix e e
mkConvolutionStencilFromKernel b kArr = Stencil b sz sCenter stencil
  where
    !sz = size kArr
    !sCenter = (liftIndex (`div` 2) sz)
    stencil getVal !ix = ifoldl RowMajor accum 0 kArr where
      accum !kIx !acc !kVal =
        getVal (liftIndex2 (+) ix (liftIndex2 (-) sCenter kIx)) * kVal + acc
      {-# INLINE accum #-}
    {-# INLINE stencil #-}
{-# INLINE mkConvolutionStencilFromKernel #-}


sobelKernelStencilX
  :: (Eq e, Num e, VU.Unbox e) => Border e -> Stencil DIM2 e e
sobelKernelStencilX b =
  mkConvolutionStencilFromKernel b $ fromListsUnboxed  [ [ 1, 0, -1 ]
                                                       , [ 2, 0, -2 ]
                                                       , [ 1, 0, -1 ] ]
{-# INLINE sobelKernelStencilX #-}


validateStencil
  :: Index ix
  => e -> Stencil ix e a -> Stencil ix e a
validateStencil d s@(Stencil _ sSz sCenter stencil) =
  let valArr = DArray sSz (const d)
  in stencil (safeStencil valArr) sCenter `seq` s
{-# INLINE validateStencil #-}



sobelStencilX :: Num e => Border e -> Stencil DIM2 e e
sobelStencilX b = mkConvolutionStencil b (3, 3) (1, 1) accum where
  accum f =
     f (-1, -1)   1  .
     f ( 0, -1)   2  .
     f ( 1, -1)   1  .
     f (-1,  1) (-1) .
     f ( 0,  1) (-2) .
     f ( 1,  1) (-1)
  {-# INLINE accum #-}
{-# INLINE sobelStencilX #-}


sobelStencilY :: Num e => Border e -> Stencil DIM2 e e
sobelStencilY b = mkConvolutionStencil b (3, 3) (1, 1) accum where
  accum f =
     f (-1, -1)   1  .
     f (-1,  0)   2  .
     f (-1,  1)   1  .
     f ( 1, -1) (-1) .
     f ( 1,  0) (-2) .
     f ( 1,  1) (-1)
  {-# INLINE accum #-}
{-# INLINE sobelStencilY #-}


sobelOperator :: (Default b, Floating b) => Border b -> Stencil DIM2 b b
sobelOperator b = fmap sqrt (liftA2 (+) sX sY) where
  !sX = (^ (2 :: Int)) <$> sobelStencilX b
  !sY = (^ (2 :: Int)) <$> sobelStencilY b
{-# INLINE sobelOperator #-}


sobelOperatorI :: (Default a, Integral a, Floating b) => Border a -> Stencil DIM2 a b
sobelOperatorI b = fmap (sqrt . fromIntegral) (sX + sY) where
  !sX = (^ (2 :: Int)) <$> sobelStencilX b
  !sY = (^ (2 :: Int)) <$> sobelStencilY b
{-# INLINE sobelOperatorI #-}

kirschWStencil
  :: Num e
  => Border e -> Stencil DIM2 e e
kirschWStencil b = mkConvolutionStencil b (3, 3) (1, 1) accum
  where
    accum f =
      f (-1, -1)   5  .
      f (-1,  0) (-3) .
      f (-1,  1) (-3) .
      f ( 0, -1)   5  .
      f ( 0,  1) (-3) .
      f ( 1, -1)   5  .
      f ( 1,  0) (-3) .
      f ( 1,  1) (-3)
    {-# INLINE accum #-}
{-# INLINE kirschWStencil #-}

-- {-# LANGUAGE BangPatterns          #-}
-- {-# LANGUAGE FlexibleContexts      #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE RecordWildCards       #-}
-- {-# LANGUAGE ScopedTypeVariables   #-}
-- {-# LANGUAGE TypeFamilies          #-}
-- {-# LANGUAGE UndecidableInstances  #-}
-- -- |
-- -- Module      : Data.Array.Massiv.Stencil
-- -- Copyright   : (c) Alexey Kuleshevich 2017
-- -- License     : BSD3
-- -- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- -- Stability   : experimental
-- -- Portability : non-portable
-- --
-- module Data.Array.Massiv.Stencil
--   ( Stencil(..)
--   , mapStencil
--   , mapStencil'
--   , applyStencil
--   ) where

-- import           Data.Array.Massiv.Common
-- import           Data.Array.Massiv.Delayed
-- import           Data.Array.Massiv.Delayed.Windowed
-- import           Data.Array.Massiv.Manifest
-- import           Data.Array.Massiv.Stencil.Internal

-- mapStencil :: Manifest r ix e =>
--               Stencil ix e a -> Array r ix e -> Array W ix a
-- mapStencil (Stencil b sSz sCenter stencilF) !arr =
--   WArray
--     sz
--     (Just sSz)
--     (stencilF (borderIndex b arr))
--     sCenter
--     (liftIndex2 (-) sz (liftIndex2 (+) sSz sCenter))
--     (stencilF (unsafeIndex arr))
--   where
--     !sz = size arr
-- {-# INLINE mapStencil #-}


-- mapStencil' :: Manifest r ix e =>
--               Stencil ix e e -> Array r ix e -> Array W ix e
-- mapStencil' = mapStencil
-- {-# INLINE mapStencil' #-}


-- applyStencil :: Manifest r ix e =>
--                 Stencil ix e a -> Array r ix e -> Array D ix a
-- applyStencil (Stencil b _ _ stencilF) !arr =
--   DArray (size arr) (stencilF (borderIndex b arr))
-- {-# INLINE applyStencil #-}
