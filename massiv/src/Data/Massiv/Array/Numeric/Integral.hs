{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
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

import           Data.Coerce
import           Data.Massiv.Array.Delayed
import           Data.Massiv.Array.Manifest.Internal
import           Data.Massiv.Array.Ops.Transform     (extract')
import           Data.Massiv.Array.Stencil
import           Data.Massiv.Core.Common


-- |
--
-- __Midpoint Rule__
--
-- \[
-- \int_{{\,a}}^{{\,b}}{{f\left( x \right)\,dx}} \approx \Delta x \cdot \,f\left( {x_1 + \frac{\Delta x}{2}} \right) + \Delta x \cdot \,f\left( {x_2 + \frac{\Delta x}{2}} \right) +  \cdots  + \Delta x \cdot \,f\left( {x_n + \frac{\Delta x}{2}} \right)
-- \]
midpointStencil ::
     (Fractional e, Index ix)
  => e -- ^ Δx - distance between sample points
  -> Dim -- ^ Dimension along which to integrate
  -> Int -- ^ n - number of sample points
  -> Stencil ix e e
midpointStencil dx dim k =
  makeStencilDef 0 (setIndex' (pureIndex 1) dim k) zeroIndex $ \g ->
    pure dx * loop 0 (< k) (+ 1) 0 (\i -> (+ g (setIndex' zeroIndex dim i)))
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
  => e -- ^ Δx - distance between sample points
  -> Dim -- ^ Dimension along which to integrate
  -> Int -- ^ n - number of sample points
  -> Stencil ix e e
trapezoidStencil dx dim k =
  makeStencilDef 0 (setIndex' (pureIndex 1) dim (k + 1)) zeroIndex $ \g ->
    pure dx / 2 *
    (loop 1 (< k) (+ 1) (g zeroIndex) (\i -> (+ 2 * g (setIndex' zeroIndex dim i))) +
     g (setIndex' zeroIndex dim k))
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
{-# INLINE simpsonsStencil #-}

-- | Integrate with a stencil along a particular dimension.
integrateWith ::
     (Fractional e, Load DW ix e, Mutable r ix e)
  => (Dim -> Int -> Stencil ix e e)
  -> Dim
  -> Int
  -> Array r ix e
  -> Array r ix e
integrateWith stencil dim n arr =
  computeWithStride (Stride nsz) $ mapStencil (Fill 0) (stencil dim n) arr
  where
    !nsz = setIndex' (pureIndex 1) dim n
{-# INLINE integrateWith #-}


-- | Compute an approximation of integral using a supplied rule in a form of `Stencil`.
integralApprox ::
     (Fractional e, Load DW ix e, Mutable r ix e)
  => (e -> Dim -> Int -> Stencil ix e e) -- ^ Integration Stencil
  -> e -- ^ @d@ - Length of interval per cell
  -> ix -- ^ @sz@ - Result size of the matrix
  -> Int -- ^ @n@ - number of samples
  -> Array r ix e -- ^ Array with values of @f(x,y,..)@ that will be used as source for integration.
  -> Array M ix e
integralApprox stencil d sz n arr =
  extract' zeroIndex sz $ toManifest $ loop 1 (<= coerce (dimensions sz)) (+ 1) arr applyStencil
  where
    !dx = d / fromIntegral n
    applyStencil dim = integrateWith (stencil dx) (Dim dim) n
    {-# INLINE applyStencil #-}
{-# INLINE integralApprox #-}


-- | Use midpoint rule to approximate an integral.
midpointRule ::
     (Fractional e, Load DW ix e, Mutable r ix e)
  => Comp -- ^ Computation strategy
  -> r -- ^ Intermediate array representation
  -> ((Int -> e) -> ix -> e) -- ^ @f(x,y,...)@ - function to integrate
  -> e -- ^ @a@ - startig point
  -> e -- ^ @d@ - distance per matrix cell
  -> ix -- ^ @sz@ - end matrix size
  -> Int -- ^ @n@ - number of sample points per cell in each direction
  -> Array M ix e
midpointRule comp r f a d sz n =
  integralApprox midpointStencil d sz n $ computeAs r $ fromFunctionMidpoint comp f a d sz n
{-# INLINE midpointRule #-}


-- | Use trapezoid rule to approximate an integral.
trapezoidRule ::
     (Fractional e, Load DW ix e, Mutable r ix e)
  => Comp -- ^ Computation strategy
  -> r -- ^ Intermediate array representation
  -> ((Int -> e) -> ix -> e) -- ^ @f(x,y,...)@ - function to integrate
  -> e -- ^ @a@ - startig point
  -> e -- ^ @d@ - distance per matrix cell
  -> ix -- ^ @sz@ - end matrix size
  -> Int -- ^ @n@ - number of sample points per cell in each direction
  -> Array M ix e
trapezoidRule comp r f a d sz n =
  integralApprox trapezoidStencil d sz n $ computeAs r $ fromFunction comp f a d sz n
{-# INLINE trapezoidRule #-}

-- | Use Simpson's rule to approximate an integral.
simpsonsRule ::
     (Fractional e, Load DW ix e, Mutable r ix e)
  => Comp -- ^ Computation strategy
  -> r -- ^ Intermediate array representation
  -> ((Int -> e) -> ix -> e) -- ^ @f(x,y,...)@ - function to integrate
  -> e -- ^ @a@ - startig point
  -> e -- ^ @d@ - distance per matrix cell
  -> ix -- ^ @sz@ - end matrix size
  -> Int -- ^ @n@ - number of sample points per cell in each direction
  -> Array M ix e
simpsonsRule comp r f a d sz n =
  integralApprox trapezoidStencil d sz n $ computeAs r $ fromFunction comp f a d sz n
{-# INLINE simpsonsRule #-}


-- | Create an array from a function with sample points at the edges
fromFunction
  :: (Index ix, Fractional a) =>
     Comp -> ((Int -> a) -> ix -> e) -> a -> a -> ix -> Int -> Array D ix e
fromFunction comp f a d sz n =
  fmap (\ix -> f scale ix) $ range' comp zeroIndex (liftIndex (n *) sz)
  where
    nFrac = fromIntegral n
    scale i = a + d * fromIntegral i / nFrac
    {-# INLINE scale #-}
{-# INLINE fromFunction #-}


-- | Create an array from a function with sample points in the middle of cells
fromFunctionMidpoint
  :: (Index ix, Fractional a) =>
     Comp -> ((Int -> a) -> ix -> e) -> a -> a -> ix -> Int -> Array D ix e
fromFunctionMidpoint comp f a d sz n =
  fmap (\ix -> f scale ix) $ range' comp zeroIndex (liftIndex (\i -> n * i - 1) sz)
  where
    nFrac = fromIntegral n
    dx2 = d / nFrac / 2
    scale i = dx2 + a + d * fromIntegral i / nFrac
    {-# INLINE scale #-}
{-# INLINE fromFunctionMidpoint #-}


-- TODO: make this function external
-- https://github.com/lehins/massiv/issues/47
range' :: Index ix => Comp -> ix -> ix -> Array D ix ix
range' comp ixFrom ixTo =
  makeArray comp sz (\ix -> liftIndex2 (+) ixFrom ix)
  where
    sz = liftIndex2 (-) (liftIndex (+ 1) ixTo) ixFrom
{-# INLINE range' #-}


-- $integral_intro
--
-- The inspiration for the code in this module was taken from [Paul Dawkins Online
-- Notes](http://tutorial.math.lamar.edu). In particular the page on [Integral
-- Approximation](http://tutorial.math.lamar.edu/Classes/CalcII/ApproximatingDefIntegrals.aspx),
-- so if you need to brush up on some theory it is a great place to start.
--
-- Implementation-wise approximation here relies heavily on stencils with stride, as such
-- computation is fast and is automatically parallelizable.
--
-- Here is an example of where this can be useful.
--
-- Calculate a value of an integral of a function, say a gaussian @f(x) = e^(x^2)@ on interval
-- @[0, 2]@ (as in Paul's tutorial above). For this we define a function @f@, an array with equally
-- spaced (/dx/) sample input values and apply the function to that array, which will give us an
-- array of @n + 1@ sample points, or looking from a different angle @n@ intervals.
--
-- >λ> let f x = exp ( x ^ 2 )
-- >λ> fromFunction Seq (\ scale x -> f (scale x)) 0 2 1 4
-- >(Array D Seq (5)
-- >  [ 1.0,1.2840254166877414,2.718281828459045,9.487735836358526,54.598150033144236 ])
--
-- Once we have that array of sample points ready we could use `integralApprox` and one of the
-- stencils to compute an integral, but there already functions that will do both steps for you:
--
-- >λ> trapezoidRule Seq U (\ scale x -> f (scale x)) 0 2 1 4
-- >(Array M Seq (1)
-- >  [ 20.644559049038715 ])
--
-- Scaling function @scale@ is what will change an array index into equally spaced and
-- appropriately shifted values of @x, y, ...@ before they can get applied to @f(x, y, ...)@
