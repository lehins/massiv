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
  , Identity
  , makeStencil
  , mkConvolutionStencil
  , mkConvolutionStencilFromKernel
  , mapStencil
--  , mapStencilM
  ) where

-- import           Control.Applicative
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Delayed
import           Data.Array.Massiv.Delayed.Windowed
--import           Data.Array.Massiv.Delayed.WindowedM
import           Data.Array.Massiv.Manifest
import           Data.Array.Massiv.Stencil.Internal
import           Data.Array.Massiv.Stencil.Convolution
import           Data.Default                       (Default (def))
import           Data.Functor.Identity
import           GHC.Exts                           (inline)



mapStencil :: (Source r ix e, Manifest r ix e) =>
              Stencil ix e a -> Array r ix e -> Array WD ix a
mapStencil (Stencil b sSz sCenter stencilF) !arr =
  WDArray
    (DArray (getComp arr) sz (runIdentity . stencilF (Identity . borderIndex b arr)))
    (Just sSz)
    sCenter
    (liftIndex2 (-) sz (liftIndex2 (-) sSz oneIndex))
    (runIdentity . stencilF (Identity . unsafeIndex arr))
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
--     (liftIndex2 (-) sz (liftIndex2 (-) sSz oneIndex))
--     (stencilM (unsafeIndex arr))
--     deps
--   where
--     !sz = size arr
-- {-# INLINE mapStencilM #-}

-- | Construct a stencil from a function, which describes how to calculate the
-- value at a point while having access to neighboring elements with a function
-- that accepts idices relative to the center of stencil. Trying to index
-- outside the stencil box will result in a runtime error upon stencil
-- creation.
--
-- ==== __Example__
--
-- Below is an example of creating a `Stencil`, which, when is mapped over a
-- 2-dimensional array, will compute an average of all elements in a 3x3 square
-- for each element in that array. /Note:/ Make sure to add @INLINE@ pragma,
-- otherwise performance will be terrible.
--
-- @@@
-- average3x3Stencil :: (Default a, Fractional a) => Border a -> Stencil Ix2 a a
-- average3x3Stencil b = makeStencil b (3 :. 3) (1 :. 1) $ \ get ->
--   (  get (-1 :. -1) + get (-1 :. 0) + get (-1 :. 1) +
--      get ( 0 :. -1) + get ( 0 :. 0) + get ( 0 :. 1) +
--      get ( 1 :. -1) + get ( 1 :. 0) + get ( 1 :. 1)   ) / 9
-- {-# INLINE average3x3Stencil #-}
-- @@@
--
makeStencil
  :: (Index ix, Default e)
  => Border e -- ^ Border resolution technique
  -> ix -- ^ Size of the stencil
  -> ix -- ^ Center of the stencil
  -> ((ix -> Identity e) -> Identity a) -- ^ Stencil function that receives another function as
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
