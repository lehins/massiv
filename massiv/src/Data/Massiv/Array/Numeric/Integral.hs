{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module Data.Massiv.Array.Numeric.Integral where

import           Data.Massiv.Core.Common
import           Data.Massiv.Array.Delayed
import           Data.Massiv.Array.Manifest.Internal
import           Data.Massiv.Array.Stencil
import           Data.Massiv.Array.Ops.Transform (extract')
import Data.Coerce

midpointStencil ::
     (Fractional e, Index ix)
  => e -- ^ Δx - distance between sample points
  -> Dim -- ^ Dimension along which to integrate
  -> Int -- ^ n - number of sample points
  -> Stencil ix e e
midpointStencil dx dim k =
  makeStencilDef 0 (setIndex' (pureIndex 1) dim k) zeroIndex $ \g ->
    pure dx * loop 0 (< k) (+ 1) 0 (\i -> (+ g (setIndex' zeroIndex dim i)))


trapezoidStencil ::
     (Fractional e, Index ix)
  => e -- ^ Δx - distance between sample points
  -> Dim -- ^ Dimension along which to integrate
  -> Int -- ^ n - number of sample points
  -> Stencil ix e e
trapezoidStencil dx dim k =
  makeStencilDef 0 (setIndex' (pureIndex 1) dim (k + 1)) zeroIndex $ \g ->
    pure dx / 2 *
    (loop 1 (< k) (+ 1) (g zeroIndex) (\i -> (+ 2 * g (setIndex' zeroIndex dim i))) +
     g (setIndex' zeroIndex dim k))


simpsonsStencil ::
     (Fractional e, Index ix)
  => e -- ^ Δx - distance between sample points
  -> Dim -- ^ Dimension along which to integrate
  -> Int -- ^ n - number of sample points
  -> Stencil ix e e
simpsonsStencil dx dim k =
  makeStencilDef 0 (setIndex' (pureIndex 1) dim (k + 1)) zeroIndex $ \g ->
    let simAcc i (prev, acc) =
          let !fx3 = g (setIndex' zeroIndex dim (i + 2))
              !newAcc = acc + prev + 4 * g (setIndex' zeroIndex dim (i + 1)) + fx3
           in (fx3, newAcc)
     in pure dx / 3 * snd (loop 2 (< k - 1) (+ 2) (simAcc 0 (g zeroIndex, 0)) simAcc)


integrateWith ::
     (Fractional e, Load DW ix e, Mutable r ix e)
  => (Dim -> Int -> Stencil ix e e)
  -> Dim
  -> Int
  -> Array r ix e
  -> Array r ix e
integrateWith stencil dim n arr =
  computeWithStride (Stride nsz) $
  mapStencil (Fill 0) (stencil dim n) arr
  where
    nsz = setIndex' (pureIndex 1) dim n



integralApprox1 ::
     (Fractional e, Mutable r Ix1 e)
  => (e -> Dim -> Int -> Stencil Ix1 e e) -- ^ Integration Stencil
  -> Ix1 -- ^ Result size of the matrix
  -> e -- ^ Length of interval @d@
  -> Array r Ix1 e
  -> Array M Ix1 e
integralApprox1 stencil sz d arr = extract' 0 sz $ toManifest $ applyStencil 1 (getIndex' n 1) arr
  where
    applyStencil = integrateWith (stencil dx)
    n = liftIndex2 div (size arr) sz
    nFrac = fromIntegral $ getIndex' n 1
    dx = d / nFrac

integralApprox2 ::
     (Fractional e, Mutable r Ix2 e)
  => (e -> Dim -> Int -> Stencil Ix2 e e) -- ^ Integration Stencil
  -> Ix2 -- ^ Result size of the matrix
  -> e -- ^ Length of interval @d@
  -> Array r Ix2 e
  -> Array M Ix2 e
integralApprox2 stencil sz d arr =
  extract' 0 sz $
  toManifest $
  applyStencil 2 (getIndex' n 2) $
  applyStencil 1 (getIndex' n 1) arr
  where
    applyStencil = integrateWith (stencil dx)
    n = liftIndex2 div (size arr) sz
    nFrac = fromIntegral $ getIndex' n 1
    dx = d / nFrac


-- | Compute an approximation of integral using supplied rule in form of a `Stencil`.
integralApprox ::
     (Fractional e, Load DW ix e, Mutable r ix e)
  => (e -> Dim -> Int -> Stencil ix e e) -- ^ Integration Stencil
  -> ix -- ^ Result size of the matrix
  -> Int
  -> e -- ^ Length of interval @d@
  -> Array r ix e
  -> Array M ix e
integralApprox stencil sz n d arr =
  extract' zeroIndex sz $ toManifest $ loop 1 (<= coerce (dimensions sz)) (+ 1) arr applyStencil
  where
    !dx = d / fromIntegral n
    applyStencil dim = integrateWith (stencil dx) (Dim dim) n


midpointRule1 ::
     (Fractional e, Mutable r Ix1 e)
  => r -- ^ Intermediate array representation
  -> (e -> e) -- ^ f(x) - function to integrate
  -> e -- ^ a - startig point
  -> e -- ^ d - distance per matrix cell
  -> Ix1 -- ^ end vector size
  -> Int -- ^ number of sample points per cell
  -> Array M Ix1 e
midpointRule1 r f a d sz n = integralApprox midpointStencil sz n d $ computeAs r arr
  where arr = fromFunctionMidpoint fromFunction1 f a d sz n


midpointRule2 ::
     (Fractional e, Mutable r Ix2 e)
  => r -- ^ Intermediate array representation
  -> (e -> e -> e) -- ^ f(x, y) - function to integrate
  -> e -- ^ a - startig point
  -> e -- ^ d - distance per matrix cell
  -> Ix2 -- ^ end matrix size
  -> Int -- ^ number of sample points per cell in each direction
  -> Array M Ix2 e
midpointRule2 r f a d sz n = integralApprox midpointStencil sz n d $ computeAs r arr
  where arr = fromFunctionMidpoint fromFunction2 f a d sz n


trapezoidRule2 ::
     (Fractional e, Mutable r Ix2 e)
  => r -- ^ Intermediate array representation
  -> (e -> e -> e) -- ^ f(x, y) - function to integrate
  -> e -- ^ a - startig point
  -> e -- ^ d - distance per matrix cell
  -> Ix2 -- ^ end matrix size
  -> Int -- ^ number of sample points per cell in each direction
  -> Array M Ix2 e
trapezoidRule2 r f a d sz n = integralApprox trapezoidStencil sz n d $ computeAs r arr
  where arr = fromFunction2 f a d sz n


simpsonsRule2 ::
     (Fractional e, Mutable r Ix2 e)
  => r -- ^ Intermediate array representation
  -> (e -> e -> e) -- ^ f(x, y) - function to integrate
  -> e -- ^ a - startig point
  -> e -- ^ d - distance per matrix cell
  -> Ix2 -- ^ end matrix size
  -> Int -- ^ number of sample points per cell in each direction
  -> Array M Ix2 e
simpsonsRule2 r f a d sz n = integralApprox simpsonsStencil sz n d $ computeAs r arr
  where arr = fromFunction2 f a d sz n


-- integrate stencil r f a d sz n = integralApprox stencil sz n d $ computeAs r arr
--   where arr = fromFunctionMidpoint fromFunction2 f a d sz n



-- midpointRuleIx fromFunIx r f a d sz n = integralApprox midpointStencil sz d $ computeAs r arr
--   where arr = fromFunctionMidpoint fromFun f a d sz n


fromFunction1
  :: Fractional a =>
     (a -> b) -> a -> a -> Ix1 -> Int -> Array D Ix1 b
fromFunction1 f a d sz n =
  fmap (\i -> f (scale i)) $ range' Par 0 (n * sz)
  where
    nFrac = fromIntegral n
    scale i = a + d * fromIntegral i / nFrac


fromFunctionMidpoint
  :: (Integral a, Fractional t1) =>
     (t2 -> t1 -> t1 -> t3 -> a -> t4)
     -> t2 -> t1 -> t1 -> t3 -> a -> t4
fromFunctionMidpoint fromFunction f a d sz n = fromFunction f (a + dx2) d sz n
  where
    nFrac = fromIntegral n
    dx2 = d / nFrac / 2


-- fromFunctionMidpoint1
--   :: Fractional a =>
--      (a -> b) -> a -> a -> Ix1 -> Int -> Array D Ix1 b
-- fromFunctionMidpoint1 f a d sz n = fromFunction1 f (a + dx2) d sz n
--   where
--     nFrac = fromIntegral n
--     dx2 = d / nFrac / 2


fromFunction2
  :: Fractional a =>
     (a -> a -> e) -> a -> a -> Ix2 -> Int -> Array D Ix2 e
fromFunction2 f a d sz n =
  fmap (\(i :. j) -> f (scale i) (scale j)) $ range' Par zeroIndex (pureIndex n * sz)
  where
    nFrac = fromIntegral n
    scale i = a + d * fromIntegral i / nFrac

-- fromFunctionMidpoint2
--   :: Fractional a =>
--      (a -> a -> e) -> a -> a -> Ix2 -> Int -> Array D Ix2 e
-- fromFunctionMidpoint2 f a d sz n = fromFunction2 f (a + dx2) d sz n
--   where
--     nFrac = fromIntegral n
--     dx2 = d / nFrac / 2




-- TODO: make this function external
-- https://github.com/lehins/massiv/issues/47
range' :: Index ix => Comp -> ix -> ix -> Array D ix ix
range' comp ixFrom ixTo =
  makeArray comp sz (\ix -> liftIndex2 (+) ixFrom ix)
  where
    sz = liftIndex2 (-) (liftIndex (+ 1) ixTo) ixFrom


gaussianIx2 :: Floating a => a -> a -> a -> a
gaussianIx2 stdDev y x = exp (-(x ^ (2 :: Int) + y ^ (2 :: Int)) / var2) / (var2 * pi)
  where
    var2 = 2 * stdDev ^ (2 :: Int)

ga1 :: Floating a => a -> a
ga1 x = exp (x ^ (2 :: Int))

-- calculateDx :: (Fractional e, Manifest r ix e) => Dim -> Int -> Array r ix e -> e
-- calculateDx dim n arr = ((arr ! setIndex' zeroIndex dim n) - (arr ! zeroIndex)) / fromIntegral n




-- trapezoidRule ::
--      (Fractional e, Load DW ix e, Manifest r ix e, Mutable r' ix e)
--   => r'
--   -> Dim
--   -> Int
--   -> e
--   -> Array r ix e
--   -> Array r' ix e
-- trapezoidRule _ dim n dx = integrateWith (trapezoidStencil dim (n + 1) dx) dim n


-- simpsonsRule ::
--      (Fractional e, Load DW ix e, Manifest r ix e, Mutable r' ix e)
--   => r'
--   -> Dim
--   -> Int
--   -> e
--   -> Array r ix e
--   -> Array r' ix e
-- simpsonsRule _ dim n dx = integrateWith (simpsonStencil dim (n + 1) dx) dim n


