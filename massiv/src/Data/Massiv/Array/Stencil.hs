{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Massiv.Array.Stencil
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
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
  , makeStencilDef
  , getStencilSize
  , getStencilCenter
  -- ** Padding
  , Padding(..)
  , noPadding
  , samePadding
  -- ** Application
  , mapStencil
  , applyStencil
  -- ** Common stencils
  , idStencil
  , sumStencil
  , productStencil
  , avgStencil
  , maxStencil
  , minStencil
  , foldlStencil
  , foldrStencil
  , foldStencil
  -- ** Profunctor
  , dimapStencil
  , lmapStencil
  , rmapStencil
  -- * Convolution
  , module Data.Massiv.Array.Stencil.Convolution
  -- * Re-export
  , Default(def)
  ) where

import Data.Coerce
import Data.Default.Class (Default(def))
import Data.Massiv.Array.Delayed.Windowed
import Data.Massiv.Array.Manifest
import Data.Massiv.Array.Stencil.Convolution
import Data.Massiv.Array.Stencil.Internal
import Data.Massiv.Array.Stencil.Unsafe
import Data.Massiv.Core.Common
import Data.Semigroup
import GHC.Exts (inline)

-- | Get the size of the stencil
--
-- @since 0.4.3
getStencilSize :: Stencil ix e a -> Sz ix
getStencilSize = stencilSize

-- | Get the index of the stencil's center
--
-- @since 0.4.3
getStencilCenter :: Stencil ix e a -> ix
getStencilCenter = stencilCenter

-- | Map a constructed stencil over an array. Resulting array must be
-- `Data.Massiv.Array.compute`d in order to be useful.
--
-- @since 0.1.0
mapStencil ::
     (Source r ix e, Manifest r ix e)
  => Border e -- ^ Border resolution technique
  -> Stencil ix e a -- ^ Stencil to map over the array
  -> Array r ix e -- ^ Source array
  -> Array DW ix a
mapStencil b stencil = applyStencil (samePadding stencil b) stencil
{-# INLINE mapStencil #-}


-- | Padding of the source array before stencil application.
--
-- ==== __Examples__
--
-- In order to see the affect of padding we can simply apply an identity stencil to an
-- array:
--
-- >>> import Data.Massiv.Array as A
-- >>> a = computeAs P $ resize' (Sz2 2 3) (Ix1 1 ... 6)
-- >>> applyStencil noPadding idStencil a
-- Array DW Seq (Sz (2 :. 3))
--   [ [ 1, 2, 3 ]
--   , [ 4, 5, 6 ]
--   ]
-- >>> applyStencil (Padding (Sz2 1 2) (Sz2 3 4) (Fill 0)) idStencil a
-- Array DW Seq (Sz (6 :. 9))
--   [ [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
--   , [ 0, 0, 1, 2, 3, 0, 0, 0, 0 ]
--   , [ 0, 0, 4, 5, 6, 0, 0, 0, 0 ]
--   , [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
--   , [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
--   , [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
--   ]
--
-- It is also a nice technique to see the border resolution strategy in action:
--
-- >>> applyStencil (Padding (Sz2 2 3) (Sz2 2 3) Wrap) idStencil a
-- Array DW Seq (Sz (6 :. 9))
--   [ [ 1, 2, 3, 1, 2, 3, 1, 2, 3 ]
--   , [ 4, 5, 6, 4, 5, 6, 4, 5, 6 ]
--   , [ 1, 2, 3, 1, 2, 3, 1, 2, 3 ]
--   , [ 4, 5, 6, 4, 5, 6, 4, 5, 6 ]
--   , [ 1, 2, 3, 1, 2, 3, 1, 2, 3 ]
--   , [ 4, 5, 6, 4, 5, 6, 4, 5, 6 ]
--   ]
-- >>> applyStencil (Padding (Sz2 2 3) (Sz2 2 3) Edge) idStencil a
-- Array DW Seq (Sz (6 :. 9))
--   [ [ 1, 1, 1, 1, 2, 3, 3, 3, 3 ]
--   , [ 1, 1, 1, 1, 2, 3, 3, 3, 3 ]
--   , [ 1, 1, 1, 1, 2, 3, 3, 3, 3 ]
--   , [ 4, 4, 4, 4, 5, 6, 6, 6, 6 ]
--   , [ 4, 4, 4, 4, 5, 6, 6, 6, 6 ]
--   , [ 4, 4, 4, 4, 5, 6, 6, 6, 6 ]
--   ]
-- >>> applyStencil (Padding (Sz2 2 3) (Sz2 2 3) Reflect) idStencil a
-- Array DW Seq (Sz (6 :. 9))
--   [ [ 6, 5, 4, 4, 5, 6, 6, 5, 4 ]
--   , [ 3, 2, 1, 1, 2, 3, 3, 2, 1 ]
--   , [ 3, 2, 1, 1, 2, 3, 3, 2, 1 ]
--   , [ 6, 5, 4, 4, 5, 6, 6, 5, 4 ]
--   , [ 6, 5, 4, 4, 5, 6, 6, 5, 4 ]
--   , [ 3, 2, 1, 1, 2, 3, 3, 2, 1 ]
--   ]
-- >>> applyStencil (Padding (Sz2 2 3) (Sz2 2 3) Continue) idStencil a
-- Array DW Seq (Sz (6 :. 9))
--   [ [ 1, 3, 2, 1, 2, 3, 2, 1, 3 ]
--   , [ 4, 6, 5, 4, 5, 6, 5, 4, 6 ]
--   , [ 1, 3, 2, 1, 2, 3, 2, 1, 3 ]
--   , [ 4, 6, 5, 4, 5, 6, 5, 4, 6 ]
--   , [ 1, 3, 2, 1, 2, 3, 2, 1, 3 ]
--   , [ 4, 6, 5, 4, 5, 6, 5, 4, 6 ]
--   ]
--
-- @since 0.4.3
data Padding ix e = Padding
  { paddingFromOrigin  :: !(Sz ix)
  , paddingFromBottom  :: !(Sz ix)
  , paddingWithElement :: !(Border e)
  -- ^ Element to do padding with
  } deriving (Eq, Show)

-- | Also known as "valid" padding. When stencil is applied to an array, that array will
-- shrink, unless the stencil is of size 1.
--
-- @since 0.4.3
noPadding :: Index ix => Padding ix e
noPadding = Padding zeroSz zeroSz Edge

-- | Padding that matches the size of the stencil, which is known as "same" padding,
-- because when a stencil is applied to an array with such matching padding, the resulting
-- array will be of the same size as the source array. This is exactly the behavior of
-- `mapStencil`
--
-- @since 0.4.3
samePadding :: Index ix => Stencil ix e a -> Border e -> Padding ix e
samePadding (Stencil (Sz sSz) sCenter _) border =
  Padding
    { paddingFromOrigin = Sz sCenter
    , paddingFromBottom = Sz (liftIndex2 (-) sSz (liftIndex (+1) sCenter))
    , paddingWithElement = border
    }

-- | Apply a constructed stencil over an array. Resulting array must be
-- `Data.Massiv.Array.compute`d in order to be useful. Unlike `mapStencil`, the size of
-- the resulting array will not necesserally be the same as the source array, which will
-- depend on the padding.
--
-- @since 0.4.3
applyStencil ::
     (Source r ix e, Manifest r ix e)
  => Padding ix e
  -- ^ Padding to be applied to the source array. This will dictate the resulting size of
  -- the array. No padding will cause it to shrink by the size of the stencil
  -> Stencil ix e a -- ^ Stencil to apply to the array
  -> Array r ix e -- ^ Source array
  -> Array DW ix a
applyStencil (Padding (Sz po) (Sz pb) border) (Stencil sSz sCenter stencilF) !arr =
  insertWindow warr window
  where
    !offset = liftIndex2 (-) sCenter po
    !warr =
      DArray
        (getComp arr)
        sz
        (unValue . stencilF (Value . borderIndex border arr) . liftIndex2 (+) offset)
    -- Size by which the resulting array will shrink (not accounting for padding)
    !shrinkSz = Sz (liftIndex (subtract 1) (unSz sSz))
    !sz = liftSz2 (-) (SafeSz (liftIndex2 (+) po (liftIndex2 (+) pb (unSz (size arr))))) shrinkSz
    !wsz = liftSz2 (-) (size arr) shrinkSz
    !window =
      Window
        { windowStart = po
        , windowSize = wsz
        , windowIndex = unValue . stencilF (Value . unsafeIndex arr) . liftIndex2 (+) offset
        , windowUnrollIx2 = unSz . fst <$> pullOutSzM sSz 2
        }
{-# INLINE applyStencil #-}


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
-- for each element in that array.
--
-- /Note/ - Make sure to add an @INLINE@ pragma, otherwise performance will be terrible.
--
-- > average3x3Stencil :: (Default a, Fractional a) => Stencil Ix2 a a
-- > average3x3Stencil = makeStencil (Sz (3 :. 3)) (1 :. 1) $ \ get ->
-- >   (  get (-1 :. -1) + get (-1 :. 0) + get (-1 :. 1) +
-- >      get ( 0 :. -1) + get ( 0 :. 0) + get ( 0 :. 1) +
-- >      get ( 1 :. -1) + get ( 1 :. 0) + get ( 1 :. 1)   ) / 9
-- > {-# INLINE average3x3Stencil #-}
--
-- @since 0.1.0
makeStencil
  :: (Index ix, Default e)
  => Sz ix -- ^ Size of the stencil
  -> ix -- ^ Center of the stencil
  -> ((ix -> Value e) -> Value a)
  -- ^ Stencil function that receives a "get" function as it's argument that can
  -- retrieve values of cells in the source array with respect to the center of
  -- the stencil. Stencil function must return a value that will be assigned to
  -- the cell in the result array. Offset supplied to the "get" function
  -- cannot go outside the boundaries of the stencil, otherwise an error will be
  -- raised during stencil creation.
  -> Stencil ix e a
makeStencil = makeStencilDef def
{-# INLINE makeStencil #-}

-- | Same as `makeStencil`, but with ability to specify default value for stencil validation.
--
-- @since 0.2.3
makeStencilDef
  :: Index ix
  => e -- ^ Default element that will be used for stencil validation only.
  -> Sz ix -- ^ Size of the stencil
  -> ix -- ^ Center of the stencil
  -> ((ix -> Value e) -> Value a)
  -- ^ Stencil function.
  -> Stencil ix e a
makeStencilDef defVal !sSz !sCenter relStencil =
  validateStencil defVal $ Stencil sSz sCenter stencil
  where
    stencil getVal !ix =
      inline relStencil $ \ !ixD -> getVal (liftIndex2 (+) ix ixD)
    {-# INLINE stencil #-}
{-# INLINE makeStencilDef #-}

-- | Identity stencil that does not change the elements of the source array.
--
-- @since 0.4.3
idStencil :: Index ix => Stencil ix e e
idStencil = makeUnsafeStencil oneSz zeroIndex $ \ _ get -> get zeroIndex
{-# INLINE idStencil #-}

-- | Stencil that does a left fold in a row-major order. Regardless of the supplied size
-- resulting stencil will be centered at zero, although by using `Padding` it is possible
-- to overcome this limitation.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array as A
-- >>> a = computeAs P $ iterateN (Sz2 3 4) (+1) (10 :: Int)
-- >>> a
-- Array P Seq (Sz (3 :. 4))
--   [ [ 11, 12, 13, 14 ]
--   , [ 15, 16, 17, 18 ]
--   , [ 19, 20, 21, 22 ]
--   ]
-- >>> applyStencil noPadding (foldlStencil (flip (:)) [] (Sz2 3 2)) a
-- Array DW Seq (Sz (1 :. 3))
--   [ [ [20,19,16,15,12,11], [21,20,17,16,13,12], [22,21,18,17,14,13] ]
--   ]
-- >>> applyStencil (Padding (Sz2 1 0) 0 (Fill 10)) (foldlStencil (flip (:)) [] (Sz2 3 2)) a
-- Array DW Seq (Sz (2 :. 3))
--   [ [ [16,15,12,11,10,10], [17,16,13,12,10,10], [18,17,14,13,10,10] ]
--   , [ [20,19,16,15,12,11], [21,20,17,16,13,12], [22,21,18,17,14,13] ]
--   ]
--
-- @since 0.4.3
foldlStencil :: Index ix => (a -> e -> a) -> a -> Sz ix -> Stencil ix e a
foldlStencil f acc0 sz =
  makeUnsafeStencil sz zeroIndex $ \_ get ->
    iter zeroIndex (unSz sz) oneIndex (<) acc0 $ \ix -> (`f` get ix)
{-# INLINE foldlStencil #-}

-- | Stencil that does a right fold in a row-major order. Regardless of the supplied size
-- resulting stencil will be centered at zero, although by using `Padding` it is possible
-- to overcome this limitation.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array as A
-- >>> a = computeAs P $ iterateN (Sz2 3 4) (+1) (10 :: Int)
-- >>> a
-- Array P Seq (Sz (3 :. 4))
--   [ [ 11, 12, 13, 14 ]
--   , [ 15, 16, 17, 18 ]
--   , [ 19, 20, 21, 22 ]
--   ]
-- >>> applyStencil noPadding (foldrStencil (:) [] (Sz2 2 3)) a
-- Array DW Seq (Sz (2 :. 2))
--   [ [ [11,12,13,15,16,17], [12,13,14,16,17,18] ]
--   , [ [15,16,17,19,20,21], [16,17,18,20,21,22] ]
--   ]
--
-- @since 0.4.3
foldrStencil :: Index ix => (e -> a -> a) -> a -> Sz ix -> Stencil ix e a
foldrStencil f acc0 sz =
  let ixStart = liftIndex2 (-) (unSz sz) oneIndex
   in makeUnsafeStencil sz zeroIndex $ \_ get ->
        iter ixStart zeroIndex (pureIndex (-1)) (>=) acc0 $ \ix -> f (get ix)
{-# INLINE foldrStencil #-}


foldStencil :: (Monoid e, Index ix) => Sz ix -> Stencil ix e e
foldStencil = foldlStencil (<>) mempty
{-# INLINE foldStencil #-}

-- | Create a stencil centered at 0 that will extract the maximum value in the region of
-- supplied size.
--
-- ==== __Example__
--
-- Here is a sample implementation of min pooling.
--
-- >>> import Data.Massiv.Array as A
-- >>> a <- computeAs P <$> resizeM (Sz2 9 9) (Ix1 10 ..: 91)
-- >>> a
-- Array P Seq (Sz (9 :. 9))
--   [ [ 10, 11, 12, 13, 14, 15, 16, 17, 18 ]
--   , [ 19, 20, 21, 22, 23, 24, 25, 26, 27 ]
--   , [ 28, 29, 30, 31, 32, 33, 34, 35, 36 ]
--   , [ 37, 38, 39, 40, 41, 42, 43, 44, 45 ]
--   , [ 46, 47, 48, 49, 50, 51, 52, 53, 54 ]
--   , [ 55, 56, 57, 58, 59, 60, 61, 62, 63 ]
--   , [ 64, 65, 66, 67, 68, 69, 70, 71, 72 ]
--   , [ 73, 74, 75, 76, 77, 78, 79, 80, 81 ]
--   , [ 82, 83, 84, 85, 86, 87, 88, 89, 90 ]
--   ]
-- >>> computeWithStrideAs P (Stride 3) $ mapStencil Edge (maxStencil (Sz 3)) a
-- Array P Seq (Sz (3 :. 3))
--   [ [ 30, 33, 36 ]
--   , [ 57, 60, 63 ]
--   , [ 84, 87, 90 ]
--   ]
--
-- @since 0.4.3
maxStencil :: (Bounded e, Ord e, Index ix) => Sz ix -> Stencil ix e e
maxStencil = dimapStencil coerce getMax . foldStencil
{-# INLINE maxStencil #-}


-- | Create a stencil centered at 0 that will extract the maximum value in the region of
-- supplied size.
--
-- ==== __Example__
--
-- Here is a sample implementation of min pooling.
--
-- >>> import Data.Massiv.Array as A
-- >>> a <- computeAs P <$> resizeM (Sz2 9 9) (Ix1 10 ..: 91)
-- >>> a
-- Array P Seq (Sz (9 :. 9))
--   [ [ 10, 11, 12, 13, 14, 15, 16, 17, 18 ]
--   , [ 19, 20, 21, 22, 23, 24, 25, 26, 27 ]
--   , [ 28, 29, 30, 31, 32, 33, 34, 35, 36 ]
--   , [ 37, 38, 39, 40, 41, 42, 43, 44, 45 ]
--   , [ 46, 47, 48, 49, 50, 51, 52, 53, 54 ]
--   , [ 55, 56, 57, 58, 59, 60, 61, 62, 63 ]
--   , [ 64, 65, 66, 67, 68, 69, 70, 71, 72 ]
--   , [ 73, 74, 75, 76, 77, 78, 79, 80, 81 ]
--   , [ 82, 83, 84, 85, 86, 87, 88, 89, 90 ]
--   ]
-- >>> computeWithStrideAs P (Stride 3) $ mapStencil Edge (minStencil (Sz 3)) a
-- Array P Seq (Sz (3 :. 3))
--   [ [ 10, 13, 16 ]
--   , [ 37, 40, 43 ]
--   , [ 64, 67, 70 ]
--   ]
--
-- @since 0.4.3
minStencil :: (Bounded e, Ord e, Index ix) => Sz ix -> Stencil ix e e
minStencil = dimapStencil coerce getMin . foldStencil
{-# INLINE minStencil #-}

-- | Sum all elements in the stencil region
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array as A
-- >>> a = computeAs P $ iterateN (Sz2 2 5) (* 2) (1 :: Int)
-- >>> a
-- Array P Seq (Sz (2 :. 5))
--   [ [ 2, 4, 8, 16, 32 ]
--   , [ 64, 128, 256, 512, 1024 ]
--   ]
-- >>> applyStencil noPadding (sumStencil (Sz2 1 2)) a
-- Array DW Seq (Sz (2 :. 4))
--   [ [ 6, 12, 24, 48 ]
--   , [ 192, 384, 768, 1536 ]
--   ]
-- >>> [2 + 4, 4 + 8, 8 + 16, 16 + 32] :: [Int]
-- [6,12,24,48]
--
-- @since 0.4.3
sumStencil :: (Num e, Index ix) => Sz ix -> Stencil ix e e
sumStencil = dimapStencil coerce getSum . foldStencil
{-# INLINE sumStencil #-}

-- | Multiply all elements in the stencil region
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array as A
-- >>> a = computeAs P $ iterateN (Sz2 2 2) (+1) (0 :: Int)
-- >>> a
-- Array P Seq (Sz (2 :. 2))
--   [ [ 1, 2 ]
--   , [ 3, 4 ]
--   ]
-- >>> applyStencil (Padding 0 2 (Fill 0)) (productStencil 2) a
-- Array DW Seq (Sz (3 :. 3))
--   [ [ 24, 0, 0 ]
--   , [ 0, 0, 0 ]
--   , [ 0, 0, 0 ]
--   ]
-- >>> applyStencil (Padding 0 2 Reflect) (productStencil 2) a
-- Array DW Seq (Sz (3 :. 3))
--   [ [ 24, 64, 24 ]
--   , [ 144, 256, 144 ]
--   , [ 24, 64, 24 ]
--   ]
--
-- @since 0.4.3
productStencil :: (Num e, Index ix) => Sz ix -> Stencil ix e e
productStencil = dimapStencil coerce getProduct . foldStencil
{-# INLINE productStencil #-}

-- | Find the average value of all elements in the stencil region
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Array as A
-- >>> a = computeAs P $ iterateN (Sz2 3 4) (+1) (10 :: Double)
-- >>> a
-- Array P Seq (Sz (3 :. 4))
--   [ [ 11.0, 12.0, 13.0, 14.0 ]
--   , [ 15.0, 16.0, 17.0, 18.0 ]
--   , [ 19.0, 20.0, 21.0, 22.0 ]
--   ]
-- >>> applyStencil noPadding (avgStencil (Sz2 2 3)) a
-- Array DW Seq (Sz (2 :. 2))
--   [ [ 14.0, 15.0 ]
--   , [ 18.0, 19.0 ]
--   ]
-- >>> Prelude.sum [11.0, 12.0, 13.0, 15.0, 16.0, 17.0] / 6 :: Double
-- 14.0
--
-- @since 0.4.3
avgStencil :: (Fractional e, Index ix) => Sz ix -> Stencil ix e e
avgStencil sz = sumStencil sz / fromIntegral (totalElem sz)
{-# INLINE avgStencil #-}
