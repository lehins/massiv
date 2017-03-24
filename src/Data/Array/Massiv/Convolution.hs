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

import qualified Data.Vector.Unboxed                 as VU
import           Prelude                             as P
import Data.Array.Massiv
-- import Data.Array.Massiv.Windowed

import Data.Maybe

data Orientation
  = Vertical
  | Horizontal deriving Show

data Kernel e
  = Kernel1D Orientation
             {-# UNPACK #-} !Int
             !(VU.Vector (Int, e))
  | Kernel2D {-# UNPACK #-} !Int
             {-# UNPACK #-} !Int
             !(VU.Vector (Int, Int, e)) deriving Show

toKernel :: (VU.Unbox e, Eq e, Num e, Source r DIM2) => Array r DIM2 e -> Kernel e
toKernel !kernelArr
  | m == 1 =
    let !n2 = n `div` 2
    in Kernel1D Horizontal n2 $ mkKernel1d n2
  | n == 1 =
    let !m2 = m `div` 2
    in Kernel1D Vertical m2 $ mkKernel1d m2
  | otherwise =
    let !(m2, n2) = (m `div` 2, n `div` 2)
    in Kernel2D m2 n2 $
       VU.filter (\(_, _, x) -> x /= 0) $ VU.imap (addIx m2 n2) kernelVec
  where
    kernelVec = VU.generate (totalElem (m, n)) $ unsafeLinearIndex kernelArr
    (m, n) = size kernelArr
    mkKernel1d !l2 =
      VU.filter ((/= 0) . snd) $ VU.imap (\ !k x -> (k - l2, x)) kernelVec
    {-# INLINE mkKernel1d #-}
    addIx !m2 !n2 !k x =
      let !(i, j) = fromLinearIndex (m, n) k
      in (i - m2, j - n2, x)
    {-# INLINE addIx #-}
{-# INLINE toKernel #-}


correlate :: (VU.Unbox e, Eq e, Num e) =>
             Array M DIM2 e -> Array M DIM2 e -> Array D DIM2 e
correlate !kernelImg !arr =
  (makeArray2D sz safeStencil)
  -- makeArrayWindowed (makeArray2D sz safeStencil) (kM2, kN2) (m - kM2 * 2, n - kN2 * 2)
  --   (getStencil kernel unsafeIndexArr)
  where
    safeStencil = getStencil kernel borderIndexArr
    unsafeIndexArr = unsafeIndex arr
    borderIndexArr = fromMaybe 0 . maybeIndex arr
    !sz@(m, n) = size arr
    !kernel = toKernel kernelImg
    !(kLen, kM2, kN2) =
      case kernel of
        Kernel1D Horizontal n2 v -> (VU.length v, 0, n2)
        Kernel1D Vertical m2 v   -> (VU.length v, m2, 0)
        Kernel2D m2 n2 v         -> (VU.length v, m2, n2)
    getStencil (Kernel1D Horizontal _ kernelV) getArrElem !(i, j) =
      loop 0 (/= kLen) (+ 1) 0 $ \ !k !acc ->
        let !(jDelta, x) = VU.unsafeIndex kernelV k
            !arrElem = getArrElem (i, j + jDelta)
            !acc' = acc + (x * arrElem)
        in acc'
    getStencil (Kernel1D Vertical _ kernelV) getArrElem !(i, j) =
      loop 0 (/= kLen) (+ 1) 0 $ \ !k !acc ->
        let !(iDelta, x) = VU.unsafeIndex kernelV k
            !arrElem = getArrElem (i + iDelta, j)
            !acc' = acc + (x * arrElem)
        in acc'
    getStencil (Kernel2D _ _ kernelV) getArrElem !(i, j) =
      loop 0 (/= kLen) (+ 1) 0 $ \ !k !acc ->
        let !(iDelta, jDelta, x) = VU.unsafeIndex kernelV k
            !arrElem = getArrElem (i + iDelta, j + jDelta)
            !acc' = acc + (x * arrElem)
        in acc'
    {-# INLINE getStencil #-}
{-# INLINE correlate #-}

