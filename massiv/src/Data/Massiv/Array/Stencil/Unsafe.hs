{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
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
  , unsafeTransformStencil
  , unsafeMapStencil
  -- ** Deprecated
  , mapStencilUnsafe
  , forStencilUnsafe
  ) where

import Data.Massiv.Array.Delayed.Windowed (Array(..), DW, Window(..),
                                           insertWindow)
import Data.Massiv.Array.Stencil.Internal
import Data.Massiv.Core.Common
import GHC.Exts (inline)


-- | Just as `mapStencilUnsafe` this is an unsafe version of the stencil
-- mapping. Arguments are in slightly different order and the indexing function returns
-- `Nothing` for elements outside the border.
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
{-# DEPRECATED forStencilUnsafe "In favor of `unsafeMapStencil`" #-}


mapStencilUnsafe ::
     Manifest r ix e
  => Border e
  -> Sz ix
  -> ix
  -> ((ix -> e) -> a)
  -> Array r ix e
  -> Array DW ix a
mapStencilUnsafe b sz ix f = unsafeMapStencil b sz ix (const f)
{-# INLINE mapStencilUnsafe #-}
{-# DEPRECATED mapStencilUnsafe "In favor of `unsafeMapStencil`" #-}

-- | This is an unsafe version of `Data.Massiv.Array.Stencil.mapStencil`, that does no
-- take `Stencil` as argument, as such it does no stencil validation. There is no
-- performance difference between the two, but the unsafe version has an advantage of not
-- requiring to deal with `Value` wrapper and has access to the actual index with the
-- array.
--
-- @since 0.5.0
unsafeMapStencil ::
     Manifest r ix e
  => Border e
  -> Sz ix
  -> ix
  -> (ix -> (ix -> e) -> a)
  -> Array r ix e
  -> Array DW ix a
unsafeMapStencil b sSz sCenter stencilF !arr = insertWindow warr window
  where
    !warr = DArray (getComp arr) sz (stencil (borderIndex b arr))
    !window =
      Window
        { windowStart = sCenter
        , windowSize = windowSz
        , windowIndex = stencil (unsafeIndex arr)
        , windowUnrollIx2 = unSz . fst <$> pullOutSzM sSz 2
        }
    !sz = size arr
    !windowSz = Sz (liftIndex2 (-) (unSz sz) (liftIndex (subtract 1) (unSz sSz)))
    stencil getVal !ix = inline (stencilF ix) $ \ !ixD -> getVal (liftIndex2 (+) ix ixD)
    {-# INLINE stencil #-}
{-# INLINE unsafeMapStencil #-}


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


-- | Perform an arbitrary transformation of a stencil. This stencil modifier can be used for
-- example to turn a vector stencil into a matrix stencil implement, or transpose a matrix
-- stencil. It is really easy to get this wrong, so be extremely careful.
--
-- ====__Examples__
--
-- Convert a 1D stencil into a row or column 2D stencil:
--
-- >>> import Data.Massiv.Array
-- >>> import Data.Massiv.Array.Unsafe
-- >>> let arr = compute $ iterateN 3 succ 0 :: Array P Ix2 Int
-- >>> arr
-- Array P Seq (Sz (3 :. 3))
--   [ [ 1, 2, 3 ]
--   , [ 4, 5, 6 ]
--   , [ 7, 8, 9 ]
--   ]
-- >>> let rowStencil = unsafeTransformStencil (\(Sz n) -> Sz (1 :. n)) (0 :.) $ \ f getVal (i :. j) -> f (getVal . (i :.)) j
-- >>> applyStencil noPadding (rowStencil (sumStencil (Sz1 3))) arr
-- Array DW Seq (Sz (3 :. 1))
--   [ [ 6 ]
--   , [ 15 ]
--   , [ 24 ]
--   ]
-- >>> let columnStencil = unsafeTransformStencil (\(Sz n) -> Sz (n :. 1)) (:. 0) $ \ f getVal (i :. j) -> f (getVal . (:. j)) i
-- >>> applyStencil noPadding (columnStencil (sumStencil (Sz1 3))) arr
-- Array DW Seq (Sz (1 :. 3))
--   [ [ 12, 15, 18 ]
--   ]
--
-- @since 0.5.4
unsafeTransformStencil ::
     (Sz ix' -> Sz ix)
  -- ^ Forward modifier for the size
  -> (ix' -> ix)
  -- ^ Forward index modifier
  -> (((ix' -> Value e) -> ix' -> Value a) -> (ix -> Value e) -> ix -> Value a)
  -- ^ Inverse stencil function modifier
  -> Stencil ix' e a
  -- ^ Original stencil.
  -> Stencil ix e a
unsafeTransformStencil transformSize transformIndex transformFunc Stencil {..} =
  Stencil
    { stencilSize = transformSize stencilSize
    , stencilCenter = transformIndex stencilCenter
    , stencilFunc = transformFunc stencilFunc
    }
{-# INLINE unsafeTransformStencil #-}



{-

Invalid stencil transformer function.

TODO: figure out if there is a safe way to do stencil index trnasformation.


transformStencil ::
     (Default e, Index ix)
  => (Sz ix' -> Sz ix)
  -- ^ Forward modifier for the size
  -> (ix' -> ix)
  -- ^ Forward index modifier
  -> (ix -> ix')
  -- ^ Inverse index modifier
  -> Stencil ix' e a
  -- ^ Original stencil.
  -> Stencil ix e a
transformStencil transformSize transformIndex transformIndex' stencil =
  validateStencil def $! unsafeTransformStencil transformSize transformIndex transformIndex' stencil
{-# INLINE transformStencil #-}


-}
