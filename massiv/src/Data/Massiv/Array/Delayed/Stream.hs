{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Massiv.Array.Delayed.Stream
-- Copyright   : (c) Alexey Kuleshevich 2019-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Delayed.Stream
  ( DS(..)
  , Array (..)
  , toStreamArray
  , toStreamM
  , toStreamIxM
  , toSteps
  , fromSteps
  , fromStepsM
  ) where

import Control.Applicative
import Data.Coerce
import Data.Foldable
import Data.Massiv.Array.Delayed.Pull
import qualified Data.Massiv.Vector.Stream as S
import Data.Massiv.Core.Common
import GHC.Exts
import Prelude hiding (take, drop)

-- | Delayed stream array that represents a sequence of values that can be loaded
-- sequentially. Important distinction from other arrays is that its size might no be
-- known until it is computed.
data DS = DS

newtype instance Array DS Ix1 e = DSArray
  { dsArray :: S.Steps S.Id e
  }

-- | /O(1)/ - Convert delayed stream array into `Steps`.
--
-- @since 0.4.1
toSteps :: Array DS Ix1 e -> Steps Id e
toSteps = coerce
{-# INLINE toSteps #-}

-- | /O(1)/ - Convert `Steps` into delayed stream array
--
-- @since 0.4.1
fromSteps :: Steps Id e -> Array DS Ix1 e
fromSteps = coerce
{-# INLINE fromSteps #-}

-- | /O(1)/ - Convert monadic `Steps` into delayed stream array
--
-- @since 0.5.0
fromStepsM :: Monad m => Steps m e -> m (Array DS Ix1 e)
fromStepsM = fmap DSArray . S.transSteps
{-# INLINE fromStepsM #-}


instance Shape DS Ix1 where
  linearSizeHint = stepsSize . dsArray
  {-# INLINE linearSizeHint #-}

  linearSize = SafeSz . unId . S.length . dsArray
  {-# INLINE linearSize #-}

  outerSize = linearSize
  {-# INLINE outerSize #-}

  isNull = S.unId . S.null . coerce
  {-# INLINE isNull #-}


--TODO remove
instance Strategy DS where
  getComp _ = Seq
  setComp _ = id


instance Functor (Array DS Ix1) where
  fmap f = coerce . S.map f . dsArray
  {-# INLINE fmap #-}
  (<$) e = coerce . (e <$) . dsArray
  {-# INLINE (<$) #-}

instance Applicative (Array DS Ix1) where
  pure = fromSteps . S.singleton
  {-# INLINE pure #-}
  (<*>) a1 a2 = fromSteps (S.zipWith ($) (coerce a1) (coerce a2))
  {-# INLINE (<*>) #-}

#if MIN_VERSION_base(4,10,0)
  liftA2 f a1 a2 = fromSteps (S.zipWith f (coerce a1) (coerce a2))
  {-# INLINE liftA2 #-}
#endif

instance Monad (Array DS Ix1) where
  return = fromSteps . S.singleton
  {-# INLINE return #-}
  (>>=) arr f = coerce (S.concatMap (coerce . f) (dsArray arr))
  {-# INLINE (>>=) #-}


instance Foldable (Array DS Ix1) where
  foldr f acc = S.unId . S.foldrLazy f acc . toSteps
  {-# INLINE foldr #-}
  foldl f acc = S.unId . S.foldlLazy f acc . toSteps
  {-# INLINE foldl #-}
  foldl' f acc = S.unId . S.foldl f acc . toSteps
  {-# INLINE foldl' #-}
  foldr1 f = S.unId . S.foldr1Lazy f . toSteps
  {-# INLINE foldr1 #-}
  foldl1 f = S.unId . S.foldl1Lazy f . toSteps
  {-# INLINE foldl1 #-}
  toList = S.toList . coerce
  {-# INLINE toList #-}
  length = S.unId . S.length . coerce
  {-# INLINE length #-}
  null = S.unId . S.null . coerce
  {-# INLINE null #-}
  sum = S.unId . S.foldl (+) 0 . toSteps
  {-# INLINE sum #-}
  product = S.unId . S.foldl (*) 1 . toSteps
  {-# INLINE product #-}
  maximum = S.unId . S.foldl1 max . toSteps
  {-# INLINE maximum #-}
  minimum = S.unId . S.foldl1 min . toSteps
  {-# INLINE minimum #-}

instance Semigroup (Array DS Ix1 e) where
  (<>) a1 a2 = fromSteps (coerce a1 `S.append` coerce a2)
  {-# INLINE (<>) #-}


instance Monoid (Array DS Ix1 e) where
  mempty = DSArray S.empty
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}

instance IsList (Array DS Ix1 e) where
  type Item (Array DS Ix1 e) = e
  fromList = fromSteps . S.fromList
  {-# INLINE fromList #-}
  fromListN n = fromSteps . S.fromListN n
  {-# INLINE fromListN #-}
  toList = S.toList . coerce
  {-# INLINE toList #-}


instance S.Stream DS Ix1 e where
  toStream = coerce
  {-# INLINE toStream #-}
  toStreamIx = S.indexed . coerce
  {-# INLINE toStreamIx #-}


-- | Flatten an array into a stream of values.
--
-- @since 0.4.1
toStreamArray :: (Index ix, Source r e) => Array r ix e -> Array DS Ix1 e
toStreamArray = DSArray . S.steps
{-# INLINE[1] toStreamArray #-}
{-# RULES "toStreamArray/id" toStreamArray = id #-}

-- | /O(1)/ - Convert an array into monadic `Steps`
--
-- @since 0.5.0
toStreamM :: (Stream r ix e, Monad m) => Array r ix e -> Steps m e
toStreamM = S.transStepsId . toStream
{-# INLINE toStreamM #-}

-- | /O(1)/ - Convert an array into monadic `Steps`
--
-- @since 0.5.0
toStreamIxM :: (Stream r ix e, Monad m) => Array r ix e -> Steps m (ix, e)
toStreamIxM = S.transStepsId . toStreamIx
{-# INLINE toStreamIxM #-}


-- | /O(n)/ - `size` implementation.
instance Load DS Ix1 e where

  makeArrayLinear _ k = fromSteps . S.generate k
  {-# INLINE makeArrayLinear #-}
  replicate _ k = fromSteps . S.replicate k
  {-# INLINE replicate #-}

  loadArrayWithST _scheduler arr uWrite =
    case stepsSize (dsArray arr) of
      LengthExact _ ->
        void $ S.foldlM (\i e -> uWrite i e >> pure (i + 1)) 0 (S.transStepsId (coerce arr))
      _ -> error "Loading Stream array is not supported with loadArrayWithM"
  {-# INLINE loadArrayWithST #-}

  unsafeLoadIntoST marr (DSArray sts) =
    S.unstreamIntoM marr (stepsSize sts) (stepsStream sts)
  {-# INLINE unsafeLoadIntoST #-}

  unsafeLoadIntoIO marr arr = liftST $ unsafeLoadIntoST marr arr
  {-# INLINE unsafeLoadIntoIO #-}


-- cons :: e -> Array DS Ix1 e -> Array DS Ix1 e
-- cons e = coerce . S.cons e . dsArray
-- {-# INLINE cons #-}

-- uncons :: Array DS Ix1 e -> Maybe (e, Array DS Ix1 e)
-- uncons = coerce . S.uncons . dsArray
-- {-# INLINE uncons #-}

-- snoc :: Array DS Ix1 e -> e -> Array DS Ix1 e
-- snoc (DSArray sts) e = DSArray (S.snoc sts e)
-- {-# INLINE snoc #-}


-- TODO: skip the stride while loading
-- instance StrideLoad DS Ix1 e where
--   loadArrayWithStrideM scheduler stride resultSize arr uWrite =
--     let strideIx = unStride stride
--         DIArray (DArray _ _ f) = arr
--     in loopM_ 0 (< numWorkers scheduler) (+ 1) $ \ !start ->
--           scheduleWork scheduler $
--           iterLinearM_ resultSize start (totalElem resultSize) (numWorkers scheduler) (<) $
--             \ !i ix -> uWrite i (f (liftIndex2 (*) strideIx ix))
--   {-# INLINE loadArrayWithStrideM #-}


