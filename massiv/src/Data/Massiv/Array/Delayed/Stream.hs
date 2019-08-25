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
  , filterS
  ) where

import Control.Monad (void)
import Data.Coerce
import Data.Massiv.Array.Delayed.Pull
import Data.Massiv.Array.Manifest.Boxed (B(..))
import Data.Massiv.Array.Manifest.Internal (computeAs)
import Data.Massiv.Array.Manifest.Vector.Stream as S
import Data.Massiv.Core.Common
import Data.Massiv.Core.List (showArrayList, showsArrayPrec)

-- | Delayed array that will be loaded in an interleaved fashion during parallel
-- computation.
data DS = DS

newtype instance Array DS Ix1 e = DSArray
  { dsArray :: Steps Id e
  }

instance Functor (Array DS Ix1) where

  fmap f = coerce . fmap f . dsArray
  {-# INLINE fmap #-}


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
toStreamArray = DSArray . steps
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
    case sSize (dsArray arr) of
      Exact _ ->
        void $ S.foldlM (\i e -> uWrite i e >> pure (i + 1)) 0 (S.transStepsId (coerce arr))
      _ -> error "Loading Stream array is not supported with loadArrayM"
  {-# INLINE loadArrayM #-}

  unsafeLoadIntoS marr (DSArray sts) =
    S.unstreamIntoM marr (S.sSize sts) (S.sSteps sts)
  {-# INLINE unsafeLoadIntoS #-}

  unsafeLoadInto marr arr = liftIO $ unsafeLoadIntoS marr arr
  {-# INLINE unsafeLoadInto #-}


filterS :: Source r ix e => (e -> Bool) -> Array r ix e -> Array DS Ix1 e
filterS f = DSArray . S.filter f . steps
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
