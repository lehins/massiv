{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : Data.Massiv.Array.Stencil.Unsafe
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Stencil.Unsafe
  ( -- * Stencil
    makeUnsafeStencil
  , forStencilUnsafe
  ) where

import           Data.Massiv.Core.Common
import           Data.Massiv.Array.Delayed.Windowed (Window(..), DW, Array(..), insertWindow)
import           GHC.Exts                           (inline)
import           Data.Massiv.Array.Stencil.Internal


-- | This is an unsafe version of the stencil computation. There are no bounds checking further from
-- the border, so if you do make sure you are not going outside the size of the stencil, you will be
-- safe, but this is not enforced.
--
-- @since 0.1.7
forStencilUnsafe ::
     (Source r ix e, Manifest r ix e)
  => Array r ix e
  -> Sz ix -- ^ Size of the stencil
  -> ix -- ^ Center of the stencil
  -> ((ix -> Maybe e) -> a)
  -- ^ Stencil function that receives a "get" function as it's argument that can
  -- retrieve values of cells in the source array with respect to the center of
  -- the stencil. Stencil function must return a value that will be assigned to
  -- the cell in the result array. Offset supplied to the "get" function
  -- cannot go outside the boundaries of the stencil.
  -> Array DW ix a
forStencilUnsafe !arr !sSz !sCenter relStencil =
  insertWindow (DArray (getComp arr) sz (stencil (index arr))) window
  where
    !window =
      Window
        { windowStart = sCenter
        , windowSize = windowSz
        , windowIndex = stencil (Just . unsafeIndex arr)
        , windowUnrollIx2 = unSz . fst <$> pullOutSzM windowSz 2
        }
    !sz = size arr
    !windowSz = Sz (liftIndex2 (-) (unSz sz) (liftIndex (subtract 1) (unSz sSz)))
    stencil getVal !ix = inline relStencil $ \ !ixD -> getVal (liftIndex2 (+) ix ixD)
    {-# INLINE stencil #-}
{-# INLINE forStencilUnsafe #-}



-- | Similar to `Data.Massiv.Array.Stencil.makeStencil`, but there are no guarantees that the
-- stencil will not read out of bounds memory. This stencil is also a bit more powerful in sense it
-- gets an extra peice of information, namely the exact index for the element it is constructing.
--
-- @since 0.3.0
makeUnsafeStencil
  :: Index ix
  => Sz ix -- ^ Size of the stencil
  -> ix -- ^ Center of the stencil
  -> (ix -> (ix -> e) -> a)
  -- ^ Stencil function.
  -> Stencil ix e a
makeUnsafeStencil !sSz !sCenter relStencil = Stencil sSz sCenter stencil
  where
    stencil getVal !ix =
      Value $ inline $ relStencil ix (unValue . getVal . liftIndex2 (+) ix)
    {-# INLINE stencil #-}
{-# INLINE makeUnsafeStencil #-}
