{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Massiv.Array.Stencil.Internal
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Stencil.Internal
  ( Stencil(..)
  , Value(..)
  , dimapStencil
  , lmapStencil
  , rmapStencil
  , validateStencil
  ) where

import Control.Applicative
import Control.DeepSeq
import Data.Massiv.Array.Delayed.Pull
import Data.Massiv.Core.Common

-- | Stencil is abstract description of how to handle elements in the neighborhood of every array
-- cell in order to compute a value for the cells in the new array. Use `Data.Array.makeStencil` and
-- `Data.Array.makeConvolutionStencil` in order to create a stencil.
data Stencil ix e a = Stencil
  { stencilSize   :: !(Sz ix)
  , stencilCenter :: !ix
  , stencilFunc   :: (ix -> Value e) -> ix -> Value a
  }

instance Index ix => NFData (Stencil ix e a) where
  rnf (Stencil sz ix f) = sz `deepseq` ix `deepseq` f `seq` ()

-- | This is a simple wrapper for value of an array cell. It is used in order to improve safety of
-- `Stencil` mapping. Using various class instances, such as `Num` and `Functor` for example, make
-- it possible to manipulate the value, without having direct access to it.
newtype Value e = Value { unValue :: e } deriving (Show, Bounded)

instance Functor Value where
  fmap f (Value e) = Value (f e)
  {-# INLINE fmap #-}

instance Applicative Value where
  pure = Value
  {-# INLINE pure #-}
  (<*>) (Value f) (Value e) = Value (f e)
  {-# INLINE (<*>) #-}

-- | @since 0.1.5
instance Semigroup a => Semigroup (Value a) where
  Value a <> Value b = Value (a <> b)
  {-# INLINE (<>) #-}

-- | @since 0.1.5
instance Monoid a => Monoid (Value a) where
  mempty = Value mempty
  {-# INLINE mempty #-}
  Value a `mappend` Value b = Value (a `mappend` b)
  {-# INLINE mappend #-}

instance Num e => Num (Value e) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (*) = liftA2 (*)
  {-# INLINE (*) #-}
  negate = fmap negate
  {-# INLINE negate #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = Value . fromInteger
  {-# INLINE fromInteger #-}

instance Fractional e => Fractional (Value e) where
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  recip = fmap recip
  {-# INLINE recip #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}

instance Floating e => Floating (Value e) where
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
    sf' s = Value . g . unValue . sf (Value . f . unValue . s)
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
    sf' s = sf (Value . f . unValue . s)
    {-# INLINE sf' #-}
{-# INLINE lmapStencil #-}

-- | A covariant map over the right most type argument. In other words a usual Functor `fmap`:
--
-- > fmap == rmapStencil
--
-- @since 0.2.3
rmapStencil :: (a -> b) -> Stencil ix e a -> Stencil ix e b
rmapStencil f stencil@Stencil {stencilFunc = sf} = stencil {stencilFunc = sf'}
  where
    sf' s = Value . f . unValue . sf s
    {-# INLINE sf' #-}
{-# INLINE rmapStencil #-}



-- TODO: Figure out interchange law (u <*> pure y = pure ($ y) <*> u) and issue
-- with discarding size and center. Best idea so far is to increase stencil size to
-- the maximum one and shift the center of the other stencil so that they both match
-- up. This approach would also remove requirement to validate the result
-- Stencil - both stencils are trusted, increasing the size will not affect the
-- safety.
instance Index ix => Applicative (Stencil ix e) where
  pure a = Stencil oneSz zeroIndex (const (const (Value a)))
  {-# INLINE pure #-}
  (<*>) (Stencil (SafeSz sSz1) sC1 f1) (Stencil (SafeSz sSz2) sC2 f2) = Stencil newSz maxCenter stF
    where
      stF gV !ix = Value (unValue (f1 gV ix) (unValue (f2 gV ix)))
      {-# INLINE stF #-}
      !newSz =
        Sz
          (liftIndex2
             (+)
             maxCenter
             (liftIndex2 max (liftIndex2 (-) sSz1 sC1) (liftIndex2 (-) sSz2 sC2)))
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


safeStencilIndex :: Index ix => Array D ix e -> ix -> e
safeStencilIndex DArray {..} ix
  | isSafeIndex dSize ix = dIndex ix
  | otherwise = throw $ IndexOutOfBoundsException dSize ix


-- | Make sure constructed stencil doesn't index outside the allowed stencil size boundary.
validateStencil
  :: Index ix
  => e -> Stencil ix e a -> Stencil ix e a
validateStencil d s@(Stencil sSz sCenter stencil) =
  let valArr = DArray Seq sSz (const d)
  in stencil (Value . safeStencilIndex valArr) sCenter `seq` s
{-# INLINE validateStencil #-}
