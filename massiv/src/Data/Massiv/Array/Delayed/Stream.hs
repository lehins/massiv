{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Massiv.Array.Delayed.Stream
-- Copyright   : (c) Alexey Kuleshevich 2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Delayed.Stream
  ( DS(..)
  , toStreamArray
  , filter
  , unfoldr
  , unfoldrN
  ) where

import Control.Applicative
import Control.Monad (void)
import Data.Coerce
import Data.Massiv.Array.Delayed.Pull
import Data.Massiv.Array.Manifest.Boxed (B(..))
import Data.Massiv.Array.Manifest.Internal (computeAs)
import qualified Data.Massiv.Array.Manifest.Vector.Stream as S
import Data.Massiv.Core.Common
import Data.Massiv.Core.List (showArrayList, showsArrayPrec)

-- | Delayed array that will be loaded in an interleaved fashion during parallel
-- computation.
data DS = DS

newtype instance Array DS Ix1 e = DSArray
  { dsArray :: S.Steps S.Id e
  }

instance Functor (Array DS Ix1) where

  fmap f = coerce . fmap f . dsArray
  {-# INLINE fmap #-}

instance Applicative (Array DS Ix1) where

  pure = DSArray . S.singleton
  {-# INLINE pure #-}

  (<*>) a1 a2 = DSArray (S.zipWith ($) (coerce a1) (coerce a2))
  {-# INLINE (<*>) #-}

  liftA2 f a1 a2 = DSArray (S.zipWith f (coerce a1) (coerce a2))
  {-# INLINE liftA2 #-}


instance Monad (Array DS Ix1) where

  return = DSArray . S.singleton
  {-# INLINE return #-}

  (>>=) arr f = coerce (S.concatMap (coerce . f) (dsArray arr))
  {-# INLINE (>>=) #-}


instance Foldable (Array DS Ix1) where

  foldr f acc (DSArray sts) = S.foldr f acc sts
  {-# INLINE foldr #-}

  length = S.length . coerce
  {-# INLINE length #-}

  -- TODO: add more

-- | Flatten an array into a stream of values.
--
-- @since 0.4.1
toStreamArray :: Source r ix e => Array r ix e -> Array DS Ix1 e
toStreamArray = DSArray . S.steps
{-# INLINE toStreamArray #-}

instance Construct DS Ix1 e where
  setComp _ arr = arr
  {-# INLINE setComp #-}

  makeArrayLinear _ (Sz k) = DSArray . S.generate k
  {-# INLINE makeArrayLinear #-}


instance Show e => Show (Array DS Ix1 e) where
  showsPrec = showsArrayPrec (computeAs B)
  showList = showArrayList

-- take . drop
-- instance Index ix => Extract DI ix e where
--   unsafeExtract sIx newSz = DIArray . unsafeExtract sIx newSz . diArray
--   {-# INLINE unsafeExtract #-}

-- | /O(n)/ - `size` implementation.
instance Load DS Ix1 e where
  size = SafeSz . S.length . coerce
  {-# INLINE size #-}

  getComp _ = Seq
  {-# INLINE getComp #-}

  loadArrayM _scheduler arr uWrite =
    case S.sSize (dsArray arr) of
      S.Exact _ ->
        void $ S.foldlM (\i e -> uWrite i e >> pure (i + 1)) 0 (S.transStepsId (coerce arr))
      _ -> error "Loading Stream array is not supported with loadArrayM"
  {-# INLINE loadArrayM #-}

  unsafeLoadIntoS marr (DSArray sts) =
    S.unstreamIntoM marr (S.sSize sts) (S.sSteps sts)
  {-# INLINE unsafeLoadIntoS #-}

  unsafeLoadInto marr arr = liftIO $ unsafeLoadIntoS marr arr
  {-# INLINE unsafeLoadInto #-}


filterS :: Source r ix e => (e -> Bool) -> Array r ix e -> Array DS Ix1 e
filterS f = DSArray . S.filter f . S.steps
{-# INLINE filterS #-}

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


-- | Right unfolding function. Useful when we do not have any idea ahead of time on how
-- many elements the vector will have.
--
-- ====__Example__
--
-- >>> unfoldr (\i -> if i < 9 then Just (i*i, i + 1) else Nothing) (0 :: Int)
-- Array DS Seq (Sz1 9)
--   [ 0, 1, 4, 9, 16, 25, 36, 49, 64 ]
-- >>> unfoldr (\i -> if sqrt i < 3 then Just (i * i, i + 1) else Nothing) (0 :: Double)
-- Array DS Seq (Sz1 9)
--   [ 0.0, 1.0, 4.0, 9.0, 16.0, 25.0, 36.0, 49.0, 64.0 ]
--
-- @since 0.4.1
unfoldr :: (s -> Maybe (e, s)) -> s -> Array DS Ix1 e
unfoldr f = DSArray . S.unfoldr f
{-# INLINE unfoldr #-}


-- | Right unfolding function with limited number of elements.
--
-- >>> unfoldrN 9 (\i -> Just (i*i, i + 1)) (0 :: Int)
-- Array DS Seq (Sz1 9)
--   [ 0, 1, 4, 9, 16, 25, 36, 49, 64 ]
--
-- @since 0.4.1
unfoldrN ::
     Sz1
  -- ^ Maximum number of elements that the vector can have
  -> (s -> Maybe (e, s))
  -- ^ Unfolding function. Stops when `Nothing` is reaturned or maximum number of elements
  -- is reached.
  -> s -- ^ Inititial element.
  -> Array DS Ix1 e
unfoldrN n f = DSArray . S.unfoldrN n f
{-# INLINE unfoldrN #-}
