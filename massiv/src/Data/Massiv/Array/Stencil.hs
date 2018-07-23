{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
-- |
-- Module      : Data.Massiv.Array.Stencil
-- Copyright   : (c) Alexey Kuleshevich 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Stencil
  ( -- * Stencil
    Stencil
  , Value
  , makeStencil
  , mapStencil
  , mapStencilStride
  -- * Convolution
  , module Data.Massiv.Array.Stencil.Convolution
  ) where

import           Data.Default.Class                    (Default (def))
import           Data.Massiv.Array.Delayed.Windowed
import           Data.Massiv.Array.Manifest
import           Data.Massiv.Array.Stencil.Convolution
import           Data.Massiv.Array.Stencil.Internal
import           Data.Massiv.Core.Common
import           GHC.Exts                              (inline)


-- | Map a constructed stencil over an array. Resulting array must be `compute`d in order to be
-- useful.
mapStencil ::
     (Source r ix e, Manifest r ix e)
  => Border e -- ^ Border resolution technique
  -> Stencil ix e a -- ^ Stencil to map over the array
  -> Array r ix e -- ^ Source array
  -> Array DW ix a
mapStencil b (Stencil sSz sCenter stencilF) !arr =
  DWArray
    (DArray (getComp arr) sz (unValue . stencilF (Value . borderIndex b arr)))
    (Just sSz)
    sCenter
    (liftIndex2 (-) sz (liftIndex2 (-) sSz (pureIndex 1)))
    (pureIndex 1)
    (unValue . stencilF (Value . unsafeIndex arr))
  where
    !sz = size arr
{-# INLINE mapStencil #-}

-- | Same as `mapStencil` except with ability to set the stride, i.e. an index that specifies which
-- elements to keep around. See `setStride` for more info.
--
-- @since 0.2.0
mapStencilStride ::
     (Source r ix e, Manifest r ix e)
  => Border e -- ^ Border resolution technique
  -> ix -- ^ Stride. Eg. `(2 :. 3)` means to keep every even row and every column divisible by
        -- 3, while ignoring the rest.
  -> Stencil ix e a -- ^ Stencil to map over the array
  -> Array r ix e -- ^ Source array
  -> Array DW ix a
mapStencilStride b stride stencil = setStride stride . mapStencil b stencil
{-# INLINE mapStencilStride #-}


-- | Construct a stencil from a function, which describes how to calculate the
-- value at a point while having access to neighboring elements with a function
-- that accepts idices relative to the center of stencil. Trying to index
-- outside the stencil box will result in a runtime error upon stencil
-- creation.
--
-- ==== __Example__
--
-- Below is an example of creating a `Stencil`, which, when mapped over a
-- 2-dimensional array, will compute an average of all elements in a 3x3 square
-- for each element in that array. /Note:/ Make sure to add @INLINE@ pragma,
-- otherwise performance will be terrible.
--
-- > average3x3Stencil :: (Default a, Fractional a) => Stencil Ix2 a a
-- > average3x3Stencil = makeStencil (3 :. 3) (1 :. 1) $ \ get ->
-- >   (  get (-1 :. -1) + get (-1 :. 0) + get (-1 :. 1) +
-- >      get ( 0 :. -1) + get ( 0 :. 0) + get ( 0 :. 1) +
-- >      get ( 1 :. -1) + get ( 1 :. 0) + get ( 1 :. 1)   ) / 9
-- > {-# INLINE average3x3Stencil #-}
--
makeStencil
  :: (Index ix, Default e)
  => ix -- ^ Size of the stencil
  -> ix -- ^ Center of the stencil
  -> ((ix -> Value e) -> Value a)
  -- ^ Stencil function that receives a "get" function as it's argument that can
  -- retrieve values of cells in the source array with respect to the center of
  -- the stencil. Stencil function must return a value that will be assigned to
  -- the cell in the result array. Offset supplied to the "get" function
  -- cannot go outside the boundaries of the stencil, otherwise an error will be
  -- raised during stencil creation.
  -> Stencil ix e a
makeStencil !sSz !sCenter relStencil =
  validateStencil def $ Stencil sSz sCenter stencil
  where
    stencil getVal !ix =
      (inline relStencil $ \ !ixD -> getVal (liftIndex2 (+) ix ixD))
    {-# INLINE stencil #-}
{-# INLINE makeStencil #-}


