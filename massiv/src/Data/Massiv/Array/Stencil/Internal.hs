{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Massiv.Array.Stencil.Internal
-- Copyright   : (c) Alexey Kuleshevich 2018-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Stencil.Internal
  ( Stencil(..)
  , dimapStencil
  , lmapStencil
  , rmapStencil
  ) where

import Control.Applicative
import Control.DeepSeq
import Data.Massiv.Core.Common

-- | Stencil is abstract description of how to handle elements in the neighborhood of
-- every array cell in order to compute a value for the cells in the new array. Use
-- `Data.Massiv.Array.makeStencil` and `Data.Massiv.Array.makeConvolutionStencil` in order
-- to create a stencil.
data Stencil ix e a = Stencil
  { stencilSize   :: !(Sz ix)
  , stencilCenter :: !ix
  , stencilFunc   :: (ix -> e) -> (ix -> e) -> ix -> a
  }


instance Index ix => NFData (Stencil ix e a) where
  rnf (Stencil sz ix f) = sz `deepseq` ix `deepseq` f `seq` ()

instance Functor (Stencil ix e) where
  fmap = rmapStencil
  {-# INLINE fmap #-}


-- Profunctor

-- | A Profunctor dimap. Same caviat applies as in `lmapStencil`
--
-- @since 0.2.3
dimapStencil :: (c -> d) -> (a -> b) -> Stencil ix d a -> Stencil ix c b
dimapStencil f g stencil@Stencil {stencilFunc = sf} = stencil {stencilFunc = sf'}
  where
    sf' us s = g . sf (f . us) (f . s)
    {-# INLINE sf' #-}
{-# INLINE dimapStencil #-}

-- | A contravariant map of a second type parameter. In other words map a function over each element
-- of the array, that the stencil will be applied to.
--
-- __Note__: This map can be very inefficient, since for stencils larger than 1 element in size, the
-- supllied function will be repeatedly applied to the same element. It is better to simply map that
-- function over the source array instead.
--
-- @since 0.2.3
lmapStencil :: (c -> d) -> Stencil ix d a -> Stencil ix c a
lmapStencil f stencil@Stencil {stencilFunc = sf} = stencil {stencilFunc = sf'}
  where
    sf' us s = sf (f . us) (f . s)
    {-# INLINE sf' #-}
{-# INLINE lmapStencil #-}

-- | A covariant map over the right most type argument. In other words the usual `fmap`
-- from `Functor`:
--
-- > fmap == rmapStencil
--
-- @since 0.2.3
rmapStencil :: (a -> b) -> Stencil ix e a -> Stencil ix e b
rmapStencil f stencil@Stencil {stencilFunc = sf} = stencil {stencilFunc = sf'}
  where
    sf' us s = f . sf us s
    {-# INLINE sf' #-}
{-# INLINE rmapStencil #-}



-- TODO: Test interchange law (u <*> pure y = pure ($ y) <*> u)
instance Index ix => Applicative (Stencil ix e) where
  pure a = Stencil oneSz zeroIndex (\_ _ _ -> a)
  {-# INLINE pure #-}
  (<*>) (Stencil (SafeSz sSz1) sC1 f1) (Stencil (SafeSz sSz2) sC2 f2) = Stencil newSz maxCenter stF
    where
      stF ug gV !ix = f1 ug gV ix (f2 ug gV ix)
      {-# INLINE stF #-}
      !newSz =
        Sz $
        liftIndex2 (+) maxCenter $
        liftIndex2 max (liftIndex2 (-) sSz1 sC1) (liftIndex2 (-) sSz2 sC2)
      !maxCenter = liftIndex2 max sC1 sC2
  {-# INLINE (<*>) #-}

instance (Index ix, Num a) => Num (Stencil ix e a) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (-) = liftA2 (-)
  {-# INLINE (-) #-}
  (*) = liftA2 (*)
  {-# INLINE (*) #-}
  negate = fmap negate
  {-# INLINE negate #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}

instance (Index ix, Fractional a) => Fractional (Stencil ix e a) where
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  recip = fmap recip
  {-# INLINE recip #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}

instance (Index ix, Floating a) => Floating (Stencil ix e a) where
  pi = pure pi
  {-# INLINE pi #-}
  exp = fmap exp
  {-# INLINE exp #-}
  log = fmap log
  {-# INLINE log #-}
  sqrt = fmap sqrt
  {-# INLINE sqrt #-}
  (**) = liftA2 (**)
  {-# INLINE (**) #-}
  logBase = liftA2 logBase
  {-# INLINE logBase #-}
  sin = fmap sin
  {-# INLINE sin #-}
  cos = fmap cos
  {-# INLINE cos #-}
  tan = fmap tan
  {-# INLINE tan #-}
  asin = fmap asin
  {-# INLINE asin #-}
  acos = fmap acos
  {-# INLINE acos #-}
  atan = fmap atan
  {-# INLINE atan #-}
  sinh = fmap sinh
  {-# INLINE sinh #-}
  cosh = fmap cosh
  {-# INLINE cosh #-}
  tanh = fmap tanh
  {-# INLINE tanh #-}
  asinh = fmap asinh
  {-# INLINE asinh #-}
  acosh = fmap acosh
  {-# INLINE acosh #-}
  atanh = fmap atanh
  {-# INLINE atanh #-}
