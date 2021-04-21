{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Data.Massiv.Array.Numeric.Integral
-- Copyright   : (c) Alexey Kuleshevich 2018-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Numeric.Integral
  (
  -- $integral_intro
  --
  -- * Integral Approximation
  -- ** Midpoint Rule
    midpointRule
  , midpointStencil
  -- ** Trapezoid Rule
  , trapezoidRule
  , trapezoidStencil
  -- ** Simpson's Rule
  , simpsonsRule
  , simpsonsStencil
  -- * General Integral approximation
  , integrateWith
  , integralApprox
  -- * From functions
  -- ** Sampled at the edge
  , fromFunction
  -- ** Sampled at the midpoint
  , fromFunctionMidpoint
  ) where

import Data.Coerce
import Data.Massiv.Array.Delayed.Pull (D)
import Data.Massiv.Array.Delayed.Windowed (DW)
import Data.Massiv.Array.Manifest.Internal
import Data.Massiv.Array.Ops.Construct (rangeInclusive)
import Data.Massiv.Array.Ops.Transform (extract')
import Data.Massiv.Array.Stencil
import Data.Massiv.Array.Unsafe
import Data.Massiv.Core.Common


-- |
--
-- __Midpoint Rule__
--
-- \[
-- \int_{{\,a}}^{{\,b}}{{f\left( x \right)\,dx}} \approx \Delta x \cdot \,f\left( {x_1 + \frac{\Delta x}{2}} \right) + \Delta x \cdot \,f\left( {x_2 + \frac{\Delta x}{2}} \right) +  \cdots  + \Delta x \cdot \,f\left( {x_n + \frac{\Delta x}{2}} \right)
-- \]
midpointStencil ::
     (Fractional e, Index ix)
  => e -- ^ @Δx@ - distance between sample points
  -> Dim -- ^ Dimension along which to integrate
  -> Int -- ^ @n@ - number of sample points.
  -> Stencil ix e e
midpointStencil dx dim k =
  makeUnsafeStencil (Sz (setDim' (pureIndex 1) dim k)) zeroIndex $ \_ g ->
    dx * loop 0 (< k) (+ 1) 0 (\i -> (+ g (setDim' zeroIndex dim i)))
{-# INLINE midpointStencil #-}


-- |
--
-- __Trapezoid Rule__
--
-- \[
-- \int_{{\,a}}^{{\,b}}{{f\left( x \right)\,dx}} \approx \frac{{\Delta x}}{2}\cdot\left( {f\left( {{x_0}} \right) + f\left( {{x_1}} \right)} \right) + \frac{{\Delta x}}{2}\cdot\left( {f\left( {{x_1}} \right) + f\left( {{x_2}} \right)} \right) +  \cdots  + \frac{{\Delta x}}{2}\cdot\left( {f\left( {{x_{n - 1}}} \right) + f\left( {{x_n}} \right)} \right)
-- \]
trapezoidStencil ::
     (Fractional e, Index ix)
  => e -- ^ @Δx@ - distance between sample points
  -> Dim -- ^ Dimension along which to integrate
  -> Int -- ^ @n@ - number of sample points.
  -> Stencil ix e e
trapezoidStencil dx dim n =
  makeUnsafeStencil (Sz (setDim' (pureIndex 1) dim (n + 1))) zeroIndex $ \_ g ->
    dx / 2 *
    (loop 1 (< n) (+ 1) (g zeroIndex) (\i -> (+ 2 * g (setDim' zeroIndex dim i))) +
     g (setDim' zeroIndex dim n))
{-# INLINE trapezoidStencil #-}


-- |
--
-- __Simpson's Rule__
--
-- \[
-- \int_{{\,a}}^{{\,b}}{{f\left( x \right)\,dx}} \approx \frac{{\Delta x}}{3}\cdot\left( {f\left( {{x_0}} \right) + 4\cdot f\left( {{x_1}} \right) + f\left( {{x_2}} \right)} \right) + \frac{{\Delta x}}{3}\cdot\left( {f\left( {{x_2}} \right) + 4\cdot f\left( {{x_3}} \right) + f\left( {{x_4}} \right)} \right) +  \cdots  + \frac{{\Delta x}}{3}\cdot\left( {f\left( {{x_{n - 2}}} \right) + 4\cdot f\left( {{x_{n - 1}}} \right) + f\left( {{x_n}} \right)} \right)
-- \]
simpsonsStencil ::
     (Fractional e, Index ix)
  => e -- ^ @Δx@ - distance between sample points
  -> Dim -- ^ Dimension along which to integrate
  -> Int -- ^ @n@ - Number of sample points. This value should be even, otherwise error.
  -> Stencil ix e e
simpsonsStencil dx dim n
  | odd n =
    error $
    "Number of sample points for Simpson's rule stencil should be even, but received: " ++ show n
  | otherwise =
    makeUnsafeStencil (Sz (setDim' (pureIndex 1) dim (n + 1))) zeroIndex $ \_ g ->
      let simAcc i (prev, acc) =
            let !fx3 = g (setDim' zeroIndex dim (i + 2))
                !newAcc = acc + prev + 4 * g (setDim' zeroIndex dim (i + 1)) + fx3
             in (fx3, newAcc)
       in dx / 3 * snd (loop 2 (< n - 1) (+ 2) (simAcc 0 (g zeroIndex, 0)) simAcc)
{-# INLINE simpsonsStencil #-}

-- | Integrate with a stencil along a particular dimension.
integrateWith ::
     (Fractional e, StrideLoad DW ix e, Mutable r e)
  => (Dim -> Int -> Stencil ix e e)
  -> Dim -- ^ Dimension along which integration should be estimated.
  -> Int -- ^ @n@ - Number of samples
  -> Array r ix e
  -> Array r ix e
integrateWith stencil dim n arr =
  computeWithStride (Stride nsz) $ mapStencil (Fill 0) (stencil dim n) arr
  where
    !nsz = setDim' (pureIndex 1) dim n
{-# INLINE integrateWith #-}


-- | Compute an approximation of integral using a supplied rule in a form of `Stencil`.
integralApprox ::
     (Fractional e, StrideLoad DW ix e, Mutable r e)
  => (e -> Dim -> Int -> Stencil ix e e) -- ^ Integration Stencil
  -> e -- ^ @d@ - Length of interval per cell
  -> Sz ix -- ^ @sz@ - Result size of the matrix
  -> Int -- ^ @n@ - Number of samples
  -> Array r ix e -- ^ Array with values of @f(x,y,..)@ that will be used as source for integration.
  -> Array D ix e
integralApprox stencil d sz n arr =
  extract' zeroIndex sz $ loop 1 (<= coerce (dimensions sz)) (+ 1) arr integrateAlong
  where
    !dx = d / fromIntegral n
    integrateAlong dim = integrateWith (stencil dx) (Dim dim) n
    {-# INLINE integrateAlong #-}
{-# INLINE integralApprox #-}


-- | Use midpoint rule to approximate an integral.
midpointRule ::
     (Fractional e, StrideLoad DW ix e, Mutable r e)
  => Comp -- ^ Computation strategy.
  -> r -- ^ Intermediate array representation.
  -> ((Int -> e) -> ix -> e) -- ^ @f(x,y,...)@ - Function to integrate
  -> e -- ^ @a@ - Starting value point.
  -> e -- ^ @d@ - Distance per matrix cell.
  -> Sz ix -- ^ @sz@ - Result matrix size.
  -> Int -- ^ @n@ - Number of sample points per cell in each direction.
  -> Array D ix e
midpointRule comp r f a d sz n =
  integralApprox midpointStencil d sz n $ computeAs r $ fromFunctionMidpoint comp f a d sz n
{-# INLINE midpointRule #-}


-- | Use trapezoid rule to approximate an integral.
trapezoidRule ::
     (Fractional e, StrideLoad DW ix e, Mutable r e)
  => Comp -- ^ Computation strategy
  -> r -- ^ Intermediate array representation
  -> ((Int -> e) -> ix -> e) -- ^ @f(x,y,...)@ - function to integrate
  -> e -- ^ @a@ - Starting value point.
  -> e -- ^ @d@ - Distance per matrix cell.
  -> Sz ix -- ^ @sz@ - Result matrix size.
  -> Int -- ^ @n@ - Number of sample points per cell in each direction.
  -> Array D ix e
trapezoidRule comp r f a d sz n =
  integralApprox trapezoidStencil d sz n $ computeAs r $ fromFunction comp f a d sz n
{-# INLINE trapezoidRule #-}

-- | Use Simpson's rule to approximate an integral.
simpsonsRule ::
     (Fractional e, StrideLoad DW ix e, Mutable r e)
  => Comp -- ^ Computation strategy
  -> r -- ^ Intermediate array representation
  -> ((Int -> e) -> ix -> e) -- ^ @f(x,y,...)@ - Function to integrate
  -> e -- ^ @a@ - Starting value point.
  -> e -- ^ @d@ - Distance per matrix cell.
  -> Sz ix -- ^ @sz@ - Result matrix size.
  -> Int -- ^ @n@ - Number of sample points per cell in each direction. This value must be even,
         -- otherwise error.
  -> Array D ix e
simpsonsRule comp r f a d sz n =
  integralApprox simpsonsStencil d sz n $ computeAs r $ fromFunction comp f a d sz n
{-# INLINE simpsonsRule #-}


-- | Create an array from a function with sample points at the edges
--
-- >>> fromFunction Seq (\ scale (i :. j) -> scale i + scale j :: Double) (-2) 1 (Sz 4) 2
-- Array D Seq (Sz (9 :. 9))
--   [ [ -4.0, -3.5, -3.0, -2.5, -2.0, -1.5, -1.0, -0.5, 0.0 ]
--   , [ -3.5, -3.0, -2.5, -2.0, -1.5, -1.0, -0.5, 0.0, 0.5 ]
--   , [ -3.0, -2.5, -2.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0 ]
--   , [ -2.5, -2.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5 ]
--   , [ -2.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 2.0 ]
--   , [ -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 2.0, 2.5 ]
--   , [ -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0 ]
--   , [ -0.5, 0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5 ]
--   , [ 0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0 ]
--   ]
--
fromFunction ::
     (Index ix, Fractional a)
  => Comp -- ^ Computation strategy
  -> ((Int -> a) -> ix -> e)
  -- ^ A function that will produce elements of scaled up array. First argument is a scaling
  -- function that should be applied to individual indicies.
  -> a -- ^ @a@ - Starting point
  -> a -- ^ @d@ - Distance per cell
  -> Sz ix -- ^ @sz@ - Size of the desired array
  -> Int -- ^ @n@ - Scaling factor, i.e. number of sample points per cell.
  -> Array D ix e
fromFunction comp f a d (Sz sz) n =
  f scale <$> rangeInclusive comp zeroIndex (liftIndex (n *) sz)
  where
    nFrac = fromIntegral n
    scale i = a + d * fromIntegral i / nFrac
    {-# INLINE scale #-}
{-# INLINE fromFunction #-}


-- | Similar to `fromFunction`, but will create an array from a function with sample points in the
-- middle of cells.
--
-- >>> fromFunctionMidpoint Seq (\ scale (i :. j) -> scale i + scale j :: Double) (-2) 1 (Sz 4) 2
-- Array D Seq (Sz (8 :. 8))
--   [ [ -3.5, -3.0, -2.5, -2.0, -1.5, -1.0, -0.5, 0.0 ]
--   , [ -3.0, -2.5, -2.0, -1.5, -1.0, -0.5, 0.0, 0.5 ]
--   , [ -2.5, -2.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0 ]
--   , [ -2.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5 ]
--   , [ -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 2.0 ]
--   , [ -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 2.0, 2.5 ]
--   , [ -0.5, 0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0 ]
--   , [ 0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5 ]
--   ]
--
fromFunctionMidpoint
  :: (Index ix, Fractional a) =>
     Comp -> ((Int -> a) -> ix -> e) -> a -> a -> Sz ix -> Int -> Array D ix e
fromFunctionMidpoint comp f a d (Sz sz) n =
  f scale <$> rangeInclusive comp zeroIndex (liftIndex (\i -> n * i - 1) sz)
  where
    nFrac = fromIntegral n
    dx2 = d / nFrac / 2
    scale i = dx2 + a + d * fromIntegral i / nFrac
    {-# INLINE scale #-}
{-# INLINE fromFunctionMidpoint #-}


-- $integral_intro
--
-- Inspiration for the code in this module was taken from [Paul Dawkins Online
-- Notes](http://tutorial.math.lamar.edu). In particular the page on [Integral
-- Approximation](http://tutorial.math.lamar.edu/Classes/CalcII/ApproximatingDefIntegrals.aspx),
-- so if you need to brush up on some theory it is a great place to start.
--
-- Implementation-wise, integral approximation here relies heavily on stencils with stride, as such
-- computation is fast and is automatically parallelizable.
--
-- Here are some examples of where this can be useful:
--
-- === Integral of a function on a region
--
-- Say we have a gaussian @f(x) = e^(x^2)@ on interval @[0, 2]@ (as in Paul's tutorial above). For
-- this we define a function @f@, an array with equally spaced (/dx/) sample input values and apply
-- the function to that array, which will give us an array of @n + 1@ sample points, or looking from
-- a different angle @n@ intervals.
--
-- >>> import Data.Massiv.Array
-- >>> f x = exp ( x ^ (2 :: Int) ) :: Float
-- >>> fromFunction Seq (\ scale x -> f (scale x)) 0 2 (Sz1 1) 4
-- Array D Seq (Sz1 5)
--   [ 1.0, 1.2840254, 2.7182817, 9.487736, 54.59815 ]
--
-- Once we have that array of sample points ready, we could use `integralApprox` and one of the
-- stencils to compute an integral, but there are already functions that will do both steps for you:
--
-- >>> simpsonsRule Seq U (\ scale x -> f (scale x)) 0 2 (Sz1 1) 4
-- Array D Seq (Sz1 1)
--   [ 17.353626 ]
--
-- @scale@ is the function that will change an array index into equally spaced and
-- appropriately shifted values of @x, y, ...@ before they can get applied to @f(x, y, ...)@
--
-- === Accurate values of a function
--
-- Another very useful place where integral approximation can be used is when a more accurate
-- representation of a non-linear function is desired. Consider the same gaussian function applied
-- to equally spaced values, with zero being in the middle of the vector:
--
-- >>> xArr = makeArrayR D Seq (Sz1 4) $ \ i -> fromIntegral i - 1.5 :: Float
-- >>> xArr
-- Array D Seq (Sz1 4)
--   [ -1.5, -0.5, 0.5, 1.5 ]
-- >>> fmap f xArr
-- Array D Seq (Sz1 4)
--   [ 9.487736, 1.2840254, 1.2840254, 9.487736 ]
--
-- The problem with above example is that computed values do not accurately represent the total
-- value contained within each vector cell. For that reason if your were to later use it for example
-- as convolution stencil, approximation would be very poor. The way to solve it is to approximate
-- an integral across each cell of vector by drastically blowing up the `xArr` and then reducing it
-- to a smaller array by using one of the approximation rules:
--
-- >>> startValue = -2 :: Float
-- >>> distPerCell = 1 :: Float
-- >>> desiredSize = Sz1 4 :: Sz1
-- >>> numSamples = 4 :: Int
-- >>> xArrX4 = fromFunction Seq ($) startValue distPerCell desiredSize numSamples
-- >>> xArrX4
-- Array D Seq (Sz1 17)
--   [ -2.0, -1.75, -1.5, -1.25, -1.0, -0.75, -0.5, -0.25, 0.0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0 ]
-- >>> yArrX4 = computeAs U $ fmap f xArrX4
-- >>> integralApprox trapezoidStencil distPerCell desiredSize numSamples yArrX4
-- Array D Seq (Sz1 4)
--   [ 16.074406, 1.4906789, 1.4906789, 16.074408 ]
--
-- We can clearly see the difference is huge, but it doesn't mean it is much better than our
-- previous estimate. In order to get more accurate results we can use a better Simpson's rule for
-- approximation and many more sample points. There is no need to create individual arrays `xArr`
-- and `yArr`, there are functions like `simpsonRule` that will take care it for you:
--
-- >>> simpsonsRule Seq U (\ scale i -> f (scale i)) startValue distPerCell desiredSize 128
-- Array D Seq (Sz1 4)
--   [ 14.989977, 1.4626511, 1.4626517, 14.989977 ]
