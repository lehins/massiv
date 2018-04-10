{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- |
-- Module      : Data.Massiv.Array.Stencil.Internal
-- Copyright   : (c) Alexey Kuleshevich 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Stencil.Internal where

import           Control.Applicative
import           Control.DeepSeq
import           Data.Default.Class                 (Default (def))
import           Data.Massiv.Array.Delayed.Internal
import           Data.Massiv.Array.Delayed.Windowed
import           Data.Massiv.Core.Common
import           GHC.Exts                           (inline)

-- | Stencil is abstract description of how to handle elements in the neighborhood of every array
-- cell in order to compute a value for the cells in the new array. Use `Data.Array.makeStencil` and
-- `Data.Array.makeConvolutionStencil` in order to create a stencil.
data Stencil ix e a = Stencil
  { stencilBorder :: Border e
  , stencilSize   :: !ix
  , stencilCenter :: !ix
  , stencilFunc   :: (ix -> Value e) -> ix -> Value a
  }

instance (NFData e, Index ix) => NFData (Stencil ix e a) where
  rnf (Stencil b sz ix f) = b `deepseq` sz `deepseq` ix `deepseq` f `seq` ()

-- | This is a simple wrapper for value of an array cell. It is used in order to improve safety of
-- `Stencil` mapping. Using various class instances, such as `Num` and `Functor` for example, make
-- it possible to manipulate the value, without having direct access to it.
newtype Value e = Value { unValue :: e } deriving (Show, Eq, Ord, Bounded)


instance Functor Value where
  fmap f (Value e) = Value (f e)
  {-# INLINE fmap #-}

instance Applicative Value where
  pure = Value
  {-# INLINE pure #-}
  (<*>) (Value f) (Value e) = Value (f e)
  {-# INLINE (<*>) #-}

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
  fmap f stencil@(Stencil {stencilFunc = g}) = stencil {stencilFunc = stF}
    where
      stF s = Value . f . unValue . g s
      {-# INLINE stF #-}
  {-# INLINE fmap #-}


-- TODO: Figure out interchange law (u <*> pure y = pure ($ y) <*> u) and issue
-- with discarding size and center. Best idea so far is to increase stencil size to
-- the maximum one and shift the center of the other stencil so that they both match
-- up. This approach would also remove requirement to validate the result
-- Stencil - both stencils are trusted, increasing the size will not affect the
-- safety.
instance (Default e, Index ix) => Applicative (Stencil ix e) where
  pure a = Stencil Edge (pureIndex 1) zeroIndex (const (const (Value a)))
  {-# INLINE pure #-}
  (<*>) (Stencil _ sSz1 sC1 f1) (Stencil sB sSz2 sC2 f2) =
    validateStencil def (Stencil sB newSz maxCenter stF)
    where
      stF gV !ix = Value ((unValue (f1 gV ix)) (unValue (f2 gV ix)))
      {-# INLINE stF #-}
      !newSz =
        liftIndex2
          (+)
          maxCenter
          (liftIndex2 max (liftIndex2 (-) sSz1 sC1) (liftIndex2 (-) sSz2 sC2))
      !maxCenter = liftIndex2 max sC1 sC2
  {-# INLINE (<*>) #-}

instance (Index ix, Default e, Num a) => Num (Stencil ix e a) where
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

instance (Index ix, Default e, Fractional a) => Fractional (Stencil ix e a) where
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  recip = fmap recip
  {-# INLINE recip #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}

instance (Index ix, Default e, Floating a) => Floating (Stencil ix e a) where
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
  | isSafeIndex dSize ix = dUnsafeIndex ix
  | otherwise =
    error $
    "Index is out of bounds: " ++ show ix ++ " for stencil size: " ++ show dSize


-- | Make sure constructed stencil doesn't index outside the allowed stencil size boundary.
validateStencil
  :: Index ix
  => e -> Stencil ix e a -> Stencil ix e a
validateStencil d s@(Stencil _ sSz sCenter stencil) =
  let valArr = DArray Seq sSz (const d)
  in stencil (Value . safeStencilIndex valArr) sCenter `seq` s
{-# INLINE validateStencil #-}


-- | This is an unsafe version of the stencil computation. There are no bounds check further from
-- the border, so if you make sure you don't go outside the size of the stencil, you will be safe,
-- but this is not enforces.
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
    sCenter
    (liftIndex2 (-) sz (liftIndex2 (-) sSz (pureIndex 1)))
    (stencil (Just . unsafeIndex arr))
  where
    stencil getVal !ix = (inline relStencil $ \ !ixD -> getVal (liftIndex2 (+) ix ixD))
    {-# INLINE stencil #-}
    !sz = size arr
{-# INLINE forStencilUnsafe #-}
