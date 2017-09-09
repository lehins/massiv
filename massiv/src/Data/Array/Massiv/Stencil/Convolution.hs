{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns     #-}
-- |
-- Module      : Data.Array.Massiv.Stencil.Convolution
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Stencil.Convolution where

import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Manifest
import           Data.Array.Massiv.Ops.Fold         (ifoldlS)
import           Data.Array.Massiv.Stencil.Internal
import           GHC.Exts                           (inline)



mkConvolutionStencil
  :: (Index ix, Num e)
  => Border e
  -> ix
  -> ix
  -> ((ix -> e -> e -> e) -> e -> e)
  -> Stencil ix e e
mkConvolutionStencil b !sSz !sCenter relStencil =
  validateStencil 0 $ Stencil b sSz sCenter stencil
  where
    stencil getVal !ix =
        (inline relStencil $ \ !ixD !kVal !acc ->
           getVal (liftIndex2 (-) ix ixD) * kVal + acc)
           0
    {-# INLINE stencil #-}
{-# INLINE mkConvolutionStencil #-}


-- | Make a stencil out of a Kernel Array
mkConvolutionStencilFromKernel
  :: (Manifest r ix e, Num e)
  => Border e
  -> Array r ix e
  -> Stencil ix e e
mkConvolutionStencilFromKernel b kArr = Stencil b sz sCenter stencil
  where
    !sz = size kArr
    !sCenter = liftIndex (`div` 2) sz
    stencil getVal !ix = ifoldlS accum 0 kArr where
      accum !acc !kIx !kVal =
        getVal (liftIndex2 (+) ix (liftIndex2 (-) sCenter kIx)) * kVal + acc
      {-# INLINE accum #-}
    {-# INLINE stencil #-}
{-# INLINE mkConvolutionStencilFromKernel #-}
