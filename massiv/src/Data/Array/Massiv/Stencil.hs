{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : Data.Array.Massiv.Stencil
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Stencil
  ( Stencil
  , makeStencil
  , mkConvolutionStencil
  , mkConvolutionStencilFromKernel
  , mapStencil
  , sMap
--  , mapStencilM
  ) where

-- import           Control.Applicative
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Delayed
import           Data.Array.Massiv.Delayed.Windowed
--import           Data.Array.Massiv.Delayed.WindowedM
import           Data.Array.Massiv.Manifest
import           Data.Array.Massiv.Ops.Fold         (ifoldlS)
import           Data.Array.Massiv.Stencil.Internal
import           Data.Default                       (Default (def))
import           GHC.Exts                           (inline)


mapStencil :: (Source r ix e, Eq e, Num e, Manifest r ix e) =>
              Stencil ix e a -> Array r ix e -> Array WD ix a
mapStencil (Stencil b sSz sCenter stencilF) !arr =
  WDArray
    (DArray (getComp arr) sz (stencilF (borderIndex b arr)))
    (Just sSz)
    sCenter
    (liftIndex2 (-) sz (liftIndex2 (+) sSz sCenter))
    (stencilF (unsafeIndex arr))
  where
    !sz = size arr
{-# INLINE mapStencil #-}


-- mapStencilM :: (Source r ix e, Eq e, Num e, Manifest r ix e) =>
--                  StencilM ix e a -> Array r ix e -> Array WMD ix a
-- mapStencilM (StencilM b sSz sCenter deps stencilM) !arr =
--   WMDArray
--     sz
--     (stencilM (borderIndex b arr))
--     (Just sSz)
--     sCenter
--     (liftIndex2 (-) sz (liftIndex2 (+) sSz sCenter))
--     (stencilM (unsafeIndex arr))
--     deps
--   where
--     !sz = size arr
-- {-# INLINE mapStencilM #-}

sMap f stencil@(Stencil { stencilFunc = g }) =
    stencil { stencilFunc = (\ s -> f . g s) }
{-# INLINE sMap #-}


makeStencil
  :: (Index ix, Default e)
  => Border e -- ^ Border resolution technique
  -> ix -- ^ Size of the stencil
  -> ix -- ^ Center of the stencil
  -> ((ix -> e) -> a) -- ^ Stencil function that receives another function as
                      -- it's argument that can index elements of the source
                      -- array with respect to the center of the stencil.
  -> Stencil ix e a
makeStencil b !sSz !sCenter relStencil =
  validateStencil def $ Stencil b sSz sCenter stencil
  where
    stencil getVal !ix =
      (inline relStencil $ \ !ixD -> getVal (liftIndex2 (-) ix ixD))
    {-# INLINE stencil #-}
{-# INLINE makeStencil #-}


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
