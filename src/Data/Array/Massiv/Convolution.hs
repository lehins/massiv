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
-- import Data.Array.Massiv.Manifest
-- -- import Data.Array.Massiv.Windowed

-- import Data.Maybe

-- data Orientation
--   = Vertical
--   | Horizontal deriving Show

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
  { stencilKernel :: !(Array M DIM1 e, Array M DIM1 ix)
  , stencilZero :: !e
  , stencilAcc :: ix -> e -> e -> e
  }




--makeStencil :: Source r ix => 


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
