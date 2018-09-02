{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns     #-}
-- |
-- Module      : Data.Massiv.Array.Stencil.Convolution
-- Copyright   : (c) Alexey Kuleshevich 2018
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

import           Data.Massiv.Core.Common
import           Data.Massiv.Array.Ops.Fold         (ifoldlS)
import           Data.Massiv.Array.Stencil.Internal
import           GHC.Exts                           (inline)

-- | Create a convolution stencil by specifying border resolution technique and
-- an accumulator function.
--
-- ==== __Examples__
--
-- Here is how to create a 2D horizontal Sobel Stencil:
--
-- > sobelX :: Num e => Stencil Ix2 e e
-- > sobelX = makeConvolutionStencil (3 :. 3) (1 :. 1) $
-- >            \f -> f (-1 :. -1) 1 . f (-1 :. 1) (-1) .
-- >                  f ( 0 :. -1) 2 . f ( 0 :. 1) (-2) .
-- >                  f ( 1 :. -1) 1 . f ( 1 :. 1) (-1)
-- > {-# INLINE sobelX #-}
--
makeConvolutionStencil
  :: (Index ix, Num e)
  => ix
  -> ix
  -> ((ix -> Value e -> Value e -> Value e) -> Value e -> Value e)
  -> Stencil ix e e
makeConvolutionStencil !sSz !sCenter relStencil = validateStencil 0 $ Stencil sSz sCenter stencil
  where
    stencil getVal !ix =
      (inline relStencil $ \ !ixD !kVal !acc -> getVal (liftIndex2 (-) ix ixD) * kVal + acc) 0
    {-# INLINE stencil #-}
{-# INLINE makeConvolutionStencil #-}


-- | Make a stencil out of a Kernel Array. This `Stencil` will be slower than if
-- `makeConvolutionStencil` is used, but sometimes we just really don't know the
-- kernel at compile time.
makeConvolutionStencilFromKernel
  :: (Manifest r ix e, Num e)
  => Array r ix e
  -> Stencil ix e e
makeConvolutionStencilFromKernel kArr = Stencil sz sCenter stencil
  where
    !sz = size kArr
    !sCenter = liftIndex (`div` 2) sz
    stencil getVal !ix = Value (ifoldlS accum 0 kArr) where
      accum !acc !kIx !kVal =
        unValue (getVal (liftIndex2 (+) ix (liftIndex2 (-) sCenter kIx))) * kVal + acc
      {-# INLINE accum #-}
    {-# INLINE stencil #-}
{-# INLINE makeConvolutionStencilFromKernel #-}



-- | Make a <https://en.wikipedia.org/wiki/Cross-correlation cross-correlation> stencil.
makeCorrelationStencil
  :: (Index ix, Num e)
  => ix
  -> ix
  -> ((ix -> Value e -> Value e -> Value e) -> Value e -> Value e)
  -> Stencil ix e e
makeCorrelationStencil !sSz !sCenter relStencil = validateStencil 0 $ Stencil sSz sCenter stencil
  where
    stencil getVal !ix =
      (inline relStencil $ \ !ixD !kVal !acc -> getVal (liftIndex2 (+) ix ixD) * kVal + acc) 0
    {-# INLINE stencil #-}
{-# INLINE makeCorrelationStencil #-}


-- | Make a stencil out of a Kernel Array. This `Stencil` will be slower than if
-- `makeCorrelationStencil` is used, but sometimes we just really don't know the
-- kernel at compile time.
makeCorrelationStencilFromKernel
  :: (Manifest r ix e, Num e)
  => Array r ix e
  -> Stencil ix e e
makeCorrelationStencilFromKernel kArr = Stencil sz sCenter stencil
  where
    !sz = size kArr
    !sCenter = liftIndex (`div` 2) sz
    stencil getVal !ix = Value (ifoldlS accum 0 kArr) where
      accum !acc !kIx !kVal =
        unValue (getVal (liftIndex2 (+) ix (liftIndex2 (+) sCenter kIx))) * kVal + acc
      {-# INLINE accum #-}
    {-# INLINE stencil #-}
{-# INLINE makeCorrelationStencilFromKernel #-}
