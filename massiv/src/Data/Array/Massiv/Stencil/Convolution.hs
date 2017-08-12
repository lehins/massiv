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
  :: (Manifest r ix e, Eq e, Num e)
  => Border e
  -> Array r ix e
  -> Stencil ix e e
mkConvolutionStencilFromKernel b kArr = Stencil b sz sCenter stencil
  where
    !sz = size kArr
    !sCenter = (liftIndex (`div` 2) sz)
    stencil getVal !ix = ifoldlS accum 0 kArr where
      accum !acc !kIx !kVal =
        getVal (liftIndex2 (+) ix (liftIndex2 (-) sCenter kIx)) * kVal + acc
      {-# INLINE accum #-}
    {-# INLINE stencil #-}
{-# INLINE mkConvolutionStencilFromKernel #-}




-- -- | KirschW stencil (already rotated 180 degrees for correlation
-- kirschWStencil :: (Num e, Eq e, VU.Unbox e) => Stencil Ix2 e
-- kirschWStencil =
--   makeStencil2D 0 $ fromListsUnboxed  [ [ -3, -3, 5 ]
--                                       , [ -3,  0, 5 ]
--                                       , [ -3, -3, 5 ] ]
-- {-# INLINE kirschWStencil #-}


-- -- | KirschW stencil (already rotated 180 degrees for correlation
-- kirschWStencil' :: (Num e, Eq e, VU.Unbox e) => Stencil Ix2 e
-- kirschWStencil' =
--   makeStencil 0 $ fromListsUnboxed  [ [ -3, -3, 5 ]
--                                     , [ -3,  0, 5 ]
--                                     , [ -3, -3, 5 ] ]
-- {-# INLINE kirschWStencil' #-}


-- -- | Sobel stencil (already rotated 180 degrees for correlation
-- sobelStencil :: (Num e, Eq e, VU.Unbox e) => Orientation -> Stencil Ix2 e
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
-- sobelStencil' :: (Num e, Eq e, VU.Unbox e) => Orientation -> Stencil Ix2 e
-- sobelStencil' Vertical =
--   makeStencil 0 $ fromListsUnboxed [ [  1,  2,  1 ]
--                                    , [  0,  0,  0 ]
--                                    , [ -1, -2, -1 ] ]
-- sobelStencil' Horizontal =
--   makeStencil 0 $ fromListsUnboxed  [ [ 1, 0, -1 ]
--                                     , [ 2, 0, -2 ]
--                                     , [ 1, 0, -1 ] ]
-- {-# INLINE sobelStencil' #-}






