{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
-- |
-- Module      : Data.Array.Massiv.Convolution
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Convolution where

-- import Control.Monad.ST
-- import qualified Data.Vector.Unboxed                 as VU
-- import qualified Data.Vector.Unboxed.Mutable as MVU
import           Prelude                             as P
import Data.Array.Massiv

import qualified Data.Vector.Unboxed             as VU
import Data.Maybe (fromMaybe)
import Data.Array.Massiv.Manifest.Loading
import Data.Array.Massiv.Windowed
-- import Data.Array.Massiv.Manifest.Unboxed

-- import Data.Maybe

data Orientation
  = Vertical
  | Horizontal deriving Show

-- data Kernel e
--   = Kernel2D {-# UNPACK #-} !Int
--              {-# UNPACK #-} !Int
--              !(VU.Vector (Int, Int, e)) deriving Show

-- toKernel :: (VU.Unbox e, Eq e, Num e, Source r DIM2) => Array r DIM2 e -> Kernel e
-- toKernel !kernelArr =
--     let !(m2, n2) = (m `div` 2, n `div` 2)
--     in Kernel2D m2 n2 $
--        VU.filter (\(_, _, x) -> x /= 0) $ VU.imap (addIx m2 n2) kernelVec
--   where
--     !kernelVec = VU.generate (totalElem (m, n)) $ unsafeLinearIndex kernelArr
--     !(m, n) = size kernelArr
--     addIx !m2 !n2 !k x =
--       let !(i, j) = fromLinearIndex (m, n) k
--       in (i - m2, j - n2, x)
--     {-# INLINE addIx #-}
-- {-# INLINE toKernel #-}


data Stencil ix e = Stencil
  { stencilZero :: !e
  , stencilSize :: !ix
  , stencilAcc :: (ix -> e) -> ix -> e
  }




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


makeStencil
  :: (Eq e, Num e, Source r DIM2, VU.Unbox e)
  => e -> Array r DIM2 e -> Stencil DIM2 e
makeStencil zero arr = Stencil zero (m2, n2) accumulator
  where
    !(m, n) = size arr
    !(m2, n2) = (m `div` 2, n `div` 2)
    !(kernelIx, kernelVal) = filterIx (/= zero) arr
    !kernelIx' = computeUnboxedS $ mapA (\ !(i, j) -> (i - m2, j - n2)) kernelIx
    accumulator getVal !(i, j) = ifoldl RowMajor apply zero kernelVal
      where
        apply !acc !k !kVal =
          let !(iD, jD) = unsafeIndex kernelIx' k
          in acc + kVal * getVal (i + iD, j + jD)
        {-# INLINE apply #-}
    {-# INLINE accumulator #-}
{-# INLINE makeStencil #-}


makeStencil2D
  :: (Eq e, Num e, Source r DIM2, VU.Unbox e)
  => e -> Array r DIM2 e -> Stencil DIM2 e
makeStencil2D zero kernel = Stencil zero (m2, n2) accumulator
  where
    !(m, n) = size kernel
    !(m2, n2) = (m `div` 2, n `div` 2)
    accumulator getVal (i, j) = ifoldl RowMajor apply zero kernel
      where
        apply !acc (ki, kj) !kVal = if kVal == zero
                                    then acc
                                    else acc + kVal * getVal (i + ki - m2, j + kj -n2)
        {-# INLINE apply #-}
    {-# INLINE accumulator #-}
{-# INLINE makeStencil2D #-}



makeSobelStencil2D
  :: (Num e)
  => e -> Stencil DIM2 e
makeSobelStencil2D !zero = Stencil zero (1, 1) accumulator
  where
    accumulator getVal !(i, j) =
      getVal (i-1, j-1) * 1 +
      getVal (  i, j-1) * 2 +
      getVal (i+1, j-1) * 1 +
      getVal (i-1, j+1) * (-1) +
      getVal (  i, j+1) * (-2) +
      getVal (i+1, j+1) * (-1)
    {-# INLINE accumulator #-}
{-# INLINE makeSobelStencil2D #-}

makeKirschWStencil2D
  :: (Num e)
  => e -> Stencil DIM2 e
makeKirschWStencil2D !zero = Stencil zero (1, 1) accumulator
  where
    accumulator getVal !(i, j) =
      getVal (i-1, j-1) * (-3) +
      getVal (i-1, j  ) * (-3) +
      getVal (i-1, j+1) * 5 +
      getVal (  i, j-1) * (-3) +
      getVal (  i, j+1) * 5 +
      getVal (i+1, j-1) * (-3) +
      getVal (i+1, j  ) * (-3) +
      getVal (i+1, j+1) * 5
    {-# INLINE accumulator #-}
{-# INLINE makeKirschWStencil2D #-}

-- | KirschW stencil (already rotated 180 degrees for correlation
kirschWStencil :: (Num e, Eq e, VU.Unbox e) => Stencil DIM2 e
kirschWStencil =
  makeStencil2D 0 $ fromListsUnboxed  [ [ -3, -3, 5 ]
                                      , [ -3,  0, 5 ]
                                      , [ -3, -3, 5 ] ]
{-# INLINE kirschWStencil #-}


-- | KirschW stencil (already rotated 180 degrees for correlation
kirschWStencil' :: (Num e, Eq e, VU.Unbox e) => Stencil DIM2 e
kirschWStencil' =
  makeStencil 0 $ fromListsUnboxed  [ [ -3, -3, 5 ]
                                    , [ -3,  0, 5 ]
                                    , [ -3, -3, 5 ] ]
{-# INLINE kirschWStencil' #-}


-- | Sobel stencil (already rotated 180 degrees for correlation
sobelStencil :: (Num e, Eq e, VU.Unbox e) => Orientation -> Stencil DIM2 e
sobelStencil Vertical =
  makeStencil2D 0 $ fromListsUnboxed [ [  1,  2,  1 ]
                                     , [  0,  0,  0 ]
                                     , [ -1, -2, -1 ] ]
sobelStencil Horizontal =
  makeStencil2D 0 $ fromListsUnboxed  [ [ 1, 0, -1 ]
                                      , [ 2, 0, -2 ]
                                      , [ 1, 0, -1 ] ]
{-# INLINE sobelStencil #-}


-- | Sobel stencil (already rotated 180 degrees for correlation
sobelStencil' :: (Num e, Eq e, VU.Unbox e) => Orientation -> Stencil DIM2 e
sobelStencil' Vertical =
  makeStencil 0 $ fromListsUnboxed [ [  1,  2,  1 ]
                                   , [  0,  0,  0 ]
                                   , [ -1, -2, -1 ] ]
sobelStencil' Horizontal =
  makeStencil 0 $ fromListsUnboxed  [ [ 1, 0, -1 ]
                                    , [ 2, 0, -2 ]
                                    , [ 1, 0, -1 ] ]
{-# INLINE sobelStencil' #-}

mapStencil2D :: (Source r DIM2, Eq e, Num e, VU.Unbox e) =>
                   Stencil DIM2 e -> Array r DIM2 e -> Array W DIM2 e
mapStencil2D (Stencil zero (kM2, kN2) getStencil) !arr =
  makeArrayWindowed
    (makeArray sz safeStencil)
    (kM2, kN2)
    (m - kM2 * 2, n - kN2 * 2)
    (getStencil (unsafeIndex arr))
  where
    !sz@(m, n) = size arr
    safeStencil = getStencil (fromMaybe zero . maybeIndex' arr)
    {-# INLINE safeStencil #-}
{-# INLINE mapStencil2D #-}


maybeIndex' :: Source r ix => Array r ix e -> ix -> Maybe e
maybeIndex' !arr !ix
  | isSafeIndex (size arr) ix = Just (unsafeIndex arr ix)
  | otherwise = Nothing
{-# INLINE maybeIndex' #-}


maybeLinearIndex :: Source r ix => Array r ix e -> Int -> Maybe e
maybeLinearIndex !arr !ix
  | 0 <= ix && ix < k = Just (unsafeLinearIndex arr ix)
  | otherwise = Nothing
  where k = totalElem (size arr)
{-# INLINE maybeLinearIndex #-}


-- sobelHorizontal :: (Num a) =>
--                    Array M DIM2 a -> Array W DIM2 a
-- sobelHorizontal arr@(MArray sz@(m, n) unsafeIndexArr) =
--   --makeUnboxed $
--   makeArrayWindowed
--     (MArray sz safeStencil)
--     (kM2, kN2)
--     (m - kM2 * 2, n - kN2 * 2)
--     (getStencil unsafeIndexArr)
--   where
--     !(kM2, kN2) = (1, 1)
--     safeStencil = getStencil borderIndexArr
--     borderIndexArr = fromMaybe 0 . maybeLinearIndex arr
--     -- getStencil getArrElem !(i, j) =
--     --   x1 + x2 + x3 + y1 + y2 + y3 where
--     --   !x1 = -1 * getArrElem (i-1, j-1)
--     --   !x2 = -2 * getArrElem (i-1, j)
--     --   !x3 = -1 * getArrElem (i-1, j+1)
--     --   !y1 = 1 * getArrElem (i+1, j-1)
--     --   !y2 = 2 * getArrElem (i+1, j)
--     --   !y3 = 1 * getArrElem (i+1, j+1)
--     -- {-# INLINE getStencil #-}
--     getStencil getArrElem !k =
--       x1 + x2 + x3 + y1 + y2 + y3 where
--       !kn = k + n
--       !kn' = k - n
--       !x1 = -1 * getArrElem (kn' - 1)
--       !x2 = -2 * getArrElem kn'
--       !x3 = -1 * getArrElem (kn' + 1)
--       !y1 = 1 * getArrElem (kn - 1)
--       !y2 = 2 * getArrElem kn
--       !y3 = 1 * getArrElem (kn + 1)
--     {-# INLINE getStencil #-}
-- {-# INLINE sobelHorizontal #-}


-- makeUnboxed
--   :: forall e . VU.Unbox e => Array W DIM2 e -> Array M DIM2 e
-- makeUnboxed WArray{..} = MArray wSize (VU.unsafeIndex vec)
--   where
--     !vec = VU.create generateArray
--     generateArray :: ST s (MVU.MVector s e)
--     generateArray = do
--       let !(m, n) = wSize
--           !k = m*n
--       mv <- MVU.unsafeNew k
--       let !(it, jt) = wWindowStartIndex
--           !(wm, wn) = wWindowSize
--           !(ib, jb) = (wm + it, wn + jt)
--       loopM_ 0 (< n) (+ 1) $ \ !j -> do
--         loopM_ 0 (< it) (+ 1) $ \ !i -> do
--           MVU.unsafeWrite mv (fromIx n (i, j)) (wSafeIndexBorder (i, j))
--         loopM_ ib (< m) (+ 1) $ \ !i -> do
--           MVU.unsafeWrite mv (fromIx n (i, j)) (wSafeIndexBorder (i, j))
--       loopM_ it (< ib) (+ 1) $ \ !i -> do
--         loopM_ 0 (< jt) (+ 1) $ \ !j -> do
--           MVU.unsafeWrite mv (fromIx n (i, j)) (wSafeIndexBorder (i, j))
--         loopM_ jt (< jb) (+ 1) $ \ !j -> do
--           MVU.unsafeWrite mv (fromIx n (i, j)) (wWindowUnsafeIndex (i, j))
--         loopM_ jb (< n) (+ 1) $ \ !j -> do
--           MVU.unsafeWrite mv (fromIx n (i, j)) (wSafeIndexBorder (i, j))
--       return mv
-- {-# INLINE makeUnboxed #-}


-- fromIx :: Int -- ^ @n@ columns
--        -> (Int, Int) -- ^ @(i, j)@ row, column index
--        -> Int -- ^ Flat vector index
-- fromIx !n !(i, j) = n * i + j
-- {-# INLINE fromIx #-}
