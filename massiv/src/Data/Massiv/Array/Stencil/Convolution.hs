{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Data.Massiv.Array.Stencil.Convolution
-- Copyright   : (c) Alexey Kuleshevich 2018-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Stencil.Convolution
  ( makeConvolutionStencil
  , makeConvolutionStencilFromKernel
  , makeCorrelationStencil
  , makeCorrelationStencilFromKernel
  ) where

import Data.Massiv.Array.Ops.Fold (ifoldlS)
import Data.Massiv.Array.Stencil.Internal
import Data.Massiv.Core.Common
import GHC.Exts (inline)

-- | Create a convolution stencil by specifying border resolution technique and
-- an accumulator function.
--
-- /Note/ - Using `Data.Massiv.Array.Stencil.Unsafe.makeUnsafeConvolutionStencil` will be
-- slightly faster, therefore it is recommended to switch from this function, after manual
-- verification that the created stencil behaves as expected.
--
-- ==== __Examples__
--
-- Here is how to create a 2D horizontal Sobel Stencil:
--
-- > sobelX :: Num e => Stencil Ix2 e e
-- > sobelX = makeConvolutionStencil (Sz2 3 3) (1 :. 1) $
-- >            \f -> f (-1 :. -1) (-1) . f (-1 :. 1) 1 .
-- >                  f ( 0 :. -1) (-2) . f ( 0 :. 1) 2 .
-- >                  f ( 1 :. -1) (-1) . f ( 1 :. 1) 1
-- > {-# INLINE sobelX #-}
--
-- @since 0.1.0
makeConvolutionStencil
  :: (Index ix, Num e)
  => Sz ix
  -> ix
  -> ((ix -> e -> e -> e) -> e -> e)
  -> Stencil ix e e
makeConvolutionStencil !sz !sCenter relStencil =
  Stencil sz sInvertCenter stencil
  where
    !sInvertCenter = liftIndex2 (-) (liftIndex (subtract 1) (unSz sz)) sCenter
    stencil _ getVal !ix =
      (inline relStencil $ \ !ixD !kVal !acc -> getVal (liftIndex2 (-) ix ixD) * kVal + acc) 0
    {-# INLINE stencil #-}
{-# INLINE makeConvolutionStencil #-}


-- | Make a stencil out of a Kernel Array. This `Stencil` will be slower than if
-- `makeConvolutionStencil` is used, but sometimes we just really don't know the
-- kernel at compile time.
--
-- @since 0.1.0
makeConvolutionStencilFromKernel
  :: (Manifest r e, Index ix, Num e)
  => Array r ix e
  -> Stencil ix e e
makeConvolutionStencilFromKernel kArr = Stencil sz sInvertCenter stencil
  where
    !sz@(Sz szi) = size kArr
    !szi1 = liftIndex (subtract 1) szi
    !sInvertCenter = liftIndex2 (-) szi1 sCenter
    !sCenter = liftIndex (`quot` 2) szi
    stencil uget _ !ix = ifoldlS accum 0 kArr where
      !ixOff = liftIndex2 (+) ix sCenter
      accum !acc !kIx !kVal = uget (liftIndex2 (-) ixOff kIx) * kVal + acc
      {-# INLINE accum #-}
    {-# INLINE stencil #-}
{-# INLINE makeConvolutionStencilFromKernel #-}


-- | Make a <https://en.wikipedia.org/wiki/Cross-correlation cross-correlation> stencil
--
-- /Note/ - Using `Data.Massiv.Array.Stencil.Unsafe.makeUnsafeCorrelationStencil` will be
-- much faster, therefore it is recommended to switch from this function, after manual
-- verification that the created stencil behaves as expected.
--
-- @since 0.1.5
makeCorrelationStencil
  :: (Index ix, Num e)
  => Sz ix
  -> ix
  -> ((ix -> e -> e -> e) -> e -> e)
  -> Stencil ix e e
makeCorrelationStencil !sSz !sCenter relStencil = Stencil sSz sCenter stencil
  where
    stencil _ getVal !ix =
      (inline relStencil $ \ !ixD !kVal !acc -> getVal (liftIndex2 (+) ix ixD) * kVal + acc) 0
    {-# INLINE stencil #-}
{-# INLINE makeCorrelationStencil #-}

-- | Make a <https://en.wikipedia.org/wiki/Cross-correlation cross-correlation> stencil out of a
-- Kernel Array. This `Stencil` will be slower than if `makeCorrelationStencil` is used, but
-- sometimes we just really don't know the kernel at compile time.
--
-- @since 0.1.5
makeCorrelationStencilFromKernel
  :: (Manifest r e, Index ix, Num e)
  => Array r ix e
  -> Stencil ix e e
makeCorrelationStencilFromKernel kArr = Stencil sz sCenter stencil
  where
    !sz = size kArr
    !sCenter = liftIndex (`div` 2) $ unSz sz
    stencil uget _ !ix = ifoldlS accum 0 kArr where
      !ixOff = liftIndex2 (-) ix sCenter
      accum !acc !kIx !kVal = uget (liftIndex2 (+) ixOff kIx) * kVal + acc
      {-# INLINE accum #-}
    {-# INLINE stencil #-}
{-# INLINE makeCorrelationStencilFromKernel #-}
