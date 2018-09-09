{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : Data.Massiv.Array.Stencil.Unsafe
-- Copyright   : (c) Alexey Kuleshevich 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Stencil.Unsafe
  ( -- * Stencil
    forStencilUnsafe
  ) where

import           Data.Massiv.Core.Common
import           Data.Massiv.Array.Delayed.Windowed (Window(..), DW, Array(..))
import           GHC.Exts                           (inline)


-- | This is an unsafe version of the stencil computation. There are no bounds check further from
-- the border, so if you make sure you don't go outside the size of the stencil, you will be safe,
-- but this is not enforced.
--
-- __/Note/__ - Still experimental and subject to change
--
-- @since 0.1.7
forStencilUnsafe ::
     (Source r ix e, Manifest r ix e)
  => Array r ix e
  -> ix -- ^ Size of the stencil
  -> ix -- ^ Center of the stencil
  -> ((ix -> Maybe e) -> a)
  -- ^ Stencil function that receives a "get" function as it's argument that can
  -- retrieve values of cells in the source array with respect to the center of
  -- the stencil. Stencil function must return a value that will be assigned to
  -- the cell in the result array. Offset supplied to the "get" function
  -- cannot go outside the boundaries of the stencil, otherwise an error will be
  -- raised during stencil creation.
  -> Array DW ix a
forStencilUnsafe !arr !sSz !sCenter relStencil =
  DWArray
    (DArray (getComp arr) sz (stencil (index arr)))
    (Just sSz)
    (Just window)
  where
    !window =
      Window
        { windowStart = sCenter
        , windowSize = liftIndex2 (-) sz (liftIndex2 (-) sSz (pureIndex 1))
        , windowIndex = stencil (Just . unsafeIndex arr)
        }
    stencil getVal !ix = inline relStencil $ \ !ixD -> getVal (liftIndex2 (+) ix ixD)
    {-# INLINE stencil #-}
    !sz = size arr
{-# INLINE forStencilUnsafe #-}
