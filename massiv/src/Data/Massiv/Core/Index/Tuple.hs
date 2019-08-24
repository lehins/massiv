{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Massiv.Core.Index.Tuple
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Core.Index.Tuple
  ( -- * Tuple based indices
    -- ** 1-dimensional
    Ix1T
    -- ** 2-dimensional
  , Ix2T
  , toIx2
  , fromIx2
    -- ** 3-dimensional
  , Ix3T
  , toIx3
  , fromIx3
    -- ** 4-dimensional
  , Ix4T
  , toIx4
  , fromIx4
    -- ** 5-dimensional
  , Ix5T
  , toIx5
  , fromIx5
  ) where

import Control.Monad.Catch (MonadThrow(..))
import Data.Massiv.Core.Index.Internal (Index(..), IndexException(..), Lower,
                                        Sz(..))
import Data.Massiv.Core.Index.Ix

-- | Another 1-dimensional index type synonym for `Int`, same as `Ix1` and is here just for
-- consistency.
type Ix1T = Int

-- | 2-dimensional index as tuple of `Int`s.
type Ix2T = (Int, Int)

-- | 3-dimensional index as 3-tuple of `Int`s.
type Ix3T = (Int, Int, Int)

-- | 4-dimensional index as 4-tuple of `Int`s.
type Ix4T = (Int, Int, Int, Int)

-- | 5-dimensional index as 5-tuple of `Int`s.
type Ix5T = (Int, Int, Int, Int, Int)

type instance Lower Ix2T = Ix1T
type instance Lower Ix3T = Ix2T
type instance Lower Ix4T = Ix3T
type instance Lower Ix5T = Ix4T



-- | Convert an `Int` tuple to `Ix2`
--
-- ==== __Example__
--
-- >>> toIx2 (2, 3)
-- 2 :. 3
--
-- @since 0.1.0
toIx2 :: Ix2T -> Ix2
toIx2 (i, j) = i :. j
{-# INLINE toIx2 #-}

-- | Convert an `Ix2` to `Int` tuple
--
-- ==== __Example__
--
-- >>> fromIx2 (2 :. 3)
-- (2,3)
--
-- @since 0.1.0
fromIx2 :: Ix2 -> Ix2T
fromIx2 (i :. j) = (i, j)
{-# INLINE fromIx2 #-}

-- | Convert a `Int` 3-tuple to `Ix3`
--
-- ==== __Example__
--
-- >>> toIx3 (1, 2, 3)
-- 1 :> 2 :. 3
--
-- @since 0.1.0
toIx3 :: Ix3T -> Ix3
toIx3 (i, j, k) = i :> j :. k
{-# INLINE toIx3 #-}

-- | Convert an `Ix3` to `Int` 3-tuple
--
-- ==== __Example__
--
-- >>> fromIx3 (1 :>  2 :. 3)
-- (1,2,3)
--
-- @since 0.1.0
fromIx3 :: Ix3 -> Ix3T
fromIx3 (i :> j :. k) = (i, j, k)
{-# INLINE fromIx3 #-}

-- | Convert a `Int` 4-tuple to `Ix4`
--
-- ==== __Example__
--
-- >>> toIx4 (1, 2, 3, 4)
-- 1 :> 2 :> 3 :. 4
--
-- @since 0.1.0
toIx4 :: Ix4T -> Ix4
toIx4 (i, j, k, l) = i :> j :> k :. l
{-# INLINE toIx4 #-}

-- | Convert an `Ix4` to `Int` 4-tuple
--
-- ==== __Example__
--
-- >>> fromIx4 (1 :> 2 :> 3 :. 4)
-- (1,2,3,4)
--
-- @since 0.1.0
fromIx4 :: Ix4 -> Ix4T
fromIx4 (i :> j :> k :. l) = (i, j, k, l)
{-# INLINE fromIx4 #-}

-- | Convert a `Int` 5-tuple to `Ix5`
--
-- ==== __Example__
--
-- >>> toIx5 (1, 2, 3, 4, 5)
-- 1 :> 2 :> 3 :> 4 :. 5
--
-- @since 0.1.0
toIx5 :: Ix5T -> Ix5
toIx5 (i, j, k, l, m) = i :> j :> k :> l :. m
{-# INLINE toIx5 #-}

-- | Convert an `Ix5` to `Int` 5-tuple
--
-- ==== __Example__
--
-- >>> fromIx5 (1 :> 2 :> 3 :> 4 :. 5)
-- (1,2,3,4,5)
--
-- @since 0.1.0
fromIx5 :: Ix5 -> Ix5T
fromIx5 (i :> j :> k :> l :. m) = (i, j, k, l, m)
{-# INLINE fromIx5 #-}

-- |
-- @since 0.1.0
instance Index Ix2T where
  type Dimensions Ix2T = 2
  dimensions _ = 2
  {-# INLINE [1] dimensions #-}
  totalElem (SafeSz (k2, k1)) = k2 * k1
  {-# INLINE [1] totalElem #-}
  toLinearIndex (SafeSz (_, k1)) (i2, i1) = k1 * i2 + i1
  {-# INLINE [1] toLinearIndex #-}
  fromLinearIndex (SafeSz (_, k1)) !i = i `quotRem` k1
  {-# INLINE [1] fromLinearIndex #-}
  consDim = (,)
  {-# INLINE [1] consDim #-}
  unconsDim = id
  {-# INLINE [1] unconsDim #-}
  snocDim = (,)
  {-# INLINE [1] snocDim #-}
  unsnocDim = id
  {-# INLINE [1] unsnocDim #-}
  getDimM (i2,  _) 2 = pure i2
  getDimM ( _, i1) 1 = pure i1
  getDimM ix       d = throwM $ IndexDimensionException ix d
  {-# INLINE [1] getDimM #-}
  setDimM (_, i1) 2 i2 = pure (i2, i1)
  setDimM (i2, _) 1 i1 = pure (i2, i1)
  setDimM ix      d _  = throwM $ IndexDimensionException ix d
  {-# INLINE [1] setDimM #-}
  modifyDimM (i2, i1) 2 f = pure (i2, (f i2,   i1))
  modifyDimM (i2, i1) 1 f = pure (i1, (  i2, f i1))
  modifyDimM ix       d _  = throwM $ IndexDimensionException ix d
  {-# INLINE [1] modifyDimM #-}
  pullOutDimM (i2, i1) 2 = pure (i2, i1)
  pullOutDimM (i2, i1) 1 = pure (i1, i2)
  pullOutDimM ix       d = throwM $ IndexDimensionException ix d
  {-# INLINE [1] pullOutDimM #-}
  insertDimM i1 2 i2 = pure (i2, i1)
  insertDimM i2 1 i1 = pure (i2, i1)
  insertDimM ix d  _ = throwM $ IndexDimensionException ix d
  {-# INLINE [1] insertDimM #-}
  pureIndex i = (i, i)
  {-# INLINE [1] pureIndex #-}
  liftIndex2 f (i2, i1) (i2', i1') = (f i2 i2', f i1 i1')
  {-# INLINE [1] liftIndex2 #-}


-- |
-- @since 0.1.0
instance Index Ix3T where
  type Dimensions Ix3T = 3
  dimensions _ = 3
  {-# INLINE [1] dimensions #-}
  totalElem  (SafeSz (k3, k2, k1)) = k3 * k2 * k1
  {-# INLINE [1] totalElem #-}
  consDim i3 (i2, i1) = (i3, i2, i1)
  {-# INLINE [1] consDim #-}
  unconsDim (i3, i2, i1) = (i3, (i2, i1))
  {-# INLINE [1] unconsDim #-}
  snocDim (i3, i2) i1 = (i3, i2, i1)
  {-# INLINE [1] snocDim #-}
  unsnocDim (i3, i2, i1) = ((i3, i2), i1)
  {-# INLINE [1] unsnocDim #-}
  getDimM (i3,  _,  _) 3 = pure i3
  getDimM ( _, i2,  _) 2 = pure i2
  getDimM ( _,  _, i1) 1 = pure i1
  getDimM ix           d = throwM $ IndexDimensionException ix d
  {-# INLINE [1] getDimM #-}
  setDimM ( _, i2, i1) 3 i3 = pure (i3, i2, i1)
  setDimM (i3,  _, i1) 2 i2 = pure (i3, i2, i1)
  setDimM (i3, i2,  _) 1 i1 = pure (i3, i2, i1)
  setDimM ix           d _  = throwM $ IndexDimensionException ix d
  {-# INLINE [1] setDimM #-}
  modifyDimM (i3, i2, i1) 3 f = pure (i3, (f i3,   i2,   i1))
  modifyDimM (i3, i2, i1) 2 f = pure (i2, (  i3, f i2,   i1))
  modifyDimM (i3, i2, i1) 1 f = pure (i1, (  i3,   i2, f i1))
  modifyDimM ix           d _  = throwM $ IndexDimensionException ix d
  {-# INLINE [1] modifyDimM #-}
  pullOutDimM (i3, i2, i1) 3 = pure (i3, (i2, i1))
  pullOutDimM (i3, i2, i1) 2 = pure (i2, (i3, i1))
  pullOutDimM (i3, i2, i1) 1 = pure (i1, (i3, i2))
  pullOutDimM ix           d = throwM $ IndexDimensionException ix d
  {-# INLINE [1] pullOutDimM #-}
  insertDimM (i2, i1) 3 i3 = pure (i3, i2, i1)
  insertDimM (i3, i1) 2 i2 = pure (i3, i2, i1)
  insertDimM (i3, i2) 1 i1 = pure (i3, i2, i1)
  insertDimM ix       d _  = throwM $ IndexDimensionException ix d
  pureIndex i = (i, i, i)
  {-# INLINE [1] pureIndex #-}
  liftIndex2 f (i3, i2, i1) (i3', i2', i1') = (f i3 i3', f i2 i2', f i1 i1')
  {-# INLINE [1] liftIndex2 #-}


instance Index Ix4T where
  type Dimensions Ix4T = 4
  dimensions _ = 4
  {-# INLINE [1] dimensions #-}
  totalElem (SafeSz (k4, k3, k2, k1)) = k4 * k3 * k2 * k1
  {-# INLINE [1] totalElem #-}
  consDim i4 (i3, i2, i1) = (i4, i3, i2, i1)
  {-# INLINE [1] consDim #-}
  unconsDim (i4, i3, i2, i1) = (i4, (i3, i2, i1))
  {-# INLINE [1] unconsDim #-}
  snocDim (i4, i3, i2) i1 = (i4, i3, i2, i1)
  {-# INLINE [1] snocDim #-}
  unsnocDim (i4, i3, i2, i1) = ((i4, i3, i2), i1)
  {-# INLINE [1] unsnocDim #-}
  getDimM (i4,  _,  _,  _) 4 = pure i4
  getDimM ( _, i3,  _,  _) 3 = pure i3
  getDimM ( _,  _, i2,  _) 2 = pure i2
  getDimM ( _,  _,  _, i1) 1 = pure i1
  getDimM ix               d = throwM $ IndexDimensionException ix d
  {-# INLINE [1] getDimM #-}
  setDimM ( _, i3, i2, i1) 4 i4 = pure (i4, i3, i2, i1)
  setDimM (i4,  _, i2, i1) 3 i3 = pure (i4, i3, i2, i1)
  setDimM (i4, i3,  _, i1) 2 i2 = pure (i4, i3, i2, i1)
  setDimM (i4, i3, i2,  _) 1 i1 = pure (i4, i3, i2, i1)
  setDimM ix               d  _ = throwM $ IndexDimensionException ix d
  {-# INLINE [1] setDimM #-}
  modifyDimM (i4, i3, i2, i1) 4 f = pure (i4, (f i4,   i3,   i2,   i1))
  modifyDimM (i4, i3, i2, i1) 3 f = pure (i3, (  i4, f i3,   i2,   i1))
  modifyDimM (i4, i3, i2, i1) 2 f = pure (i2, (  i4,   i3, f i2,   i1))
  modifyDimM (i4, i3, i2, i1) 1 f = pure (i1, (  i4,   i3,   i2, f i1))
  modifyDimM ix               d _ = throwM $ IndexDimensionException ix d
  {-# INLINE [1] modifyDimM #-}
  pullOutDimM (i4, i3, i2, i1) 4 = pure (i4, (i3, i2, i1))
  pullOutDimM (i4, i3, i2, i1) 3 = pure (i3, (i4, i2, i1))
  pullOutDimM (i4, i3, i2, i1) 2 = pure (i2, (i4, i3, i1))
  pullOutDimM (i4, i3, i2, i1) 1 = pure (i1, (i4, i3, i2))
  pullOutDimM ix               d = throwM $ IndexDimensionException ix d
  {-# INLINE [1] pullOutDimM #-}
  insertDimM (i3, i2, i1) 4 i4 = pure (i4, i3, i2, i1)
  insertDimM (i4, i2, i1) 3 i3 = pure (i4, i3, i2, i1)
  insertDimM (i4, i3, i1) 2 i2 = pure (i4, i3, i2, i1)
  insertDimM (i4, i3, i2) 1 i1 = pure (i4, i3, i2, i1)
  insertDimM ix           d  _ = throwM $ IndexDimensionException ix d
  {-# INLINE [1] insertDimM #-}
  pureIndex i = (i, i, i, i)
  {-# INLINE [1] pureIndex #-}
  liftIndex2 f (i4, i3, i2, i1) (i4', i3', i2', i1') = (f i4 i4', f i3 i3', f i2 i2', f i1 i1')
  {-# INLINE [1] liftIndex2 #-}


instance Index Ix5T where
  type Dimensions Ix5T = 5
  dimensions _ = 5
  {-# INLINE [1] dimensions #-}
  totalElem (SafeSz (n5, n4, n3, n2, n1)) = n5 * n4 * n3 * n2 * n1
  {-# INLINE [1] totalElem #-}
  consDim i5 (i4, i3, i2, i1) = (i5, i4, i3, i2, i1)
  {-# INLINE [1] consDim #-}
  unconsDim (i5, i4, i3, i2, i1) = (i5, (i4, i3, i2, i1))
  {-# INLINE [1] unconsDim #-}
  snocDim (i5, i4, i3, i2) i1 = (i5, i4, i3, i2, i1)
  {-# INLINE [1] snocDim #-}
  unsnocDim (i5, i4, i3, i2, i1) = ((i5, i4, i3, i2), i1)
  {-# INLINE [1] unsnocDim #-}
  getDimM (i5,  _,  _,  _,  _) 5 = pure i5
  getDimM ( _, i4,  _,  _,  _) 4 = pure i4
  getDimM ( _,  _, i3,  _,  _) 3 = pure i3
  getDimM ( _,  _,  _, i2,  _) 2 = pure i2
  getDimM ( _,  _,  _,  _, i1) 1 = pure i1
  getDimM ix                   d = throwM $ IndexDimensionException ix d
  {-# INLINE [1] getDimM #-}
  setDimM ( _, i4, i3, i2, i1) 5 i5 = pure (i5, i4, i3, i2, i1)
  setDimM (i5,  _, i3, i2, i1) 4 i4 = pure (i5, i4, i3, i2, i1)
  setDimM (i5, i4,  _, i2, i1) 3 i3 = pure (i5, i4, i3, i2, i1)
  setDimM (i5, i4, i3,  _, i1) 2 i2 = pure (i5, i4, i3, i2, i1)
  setDimM (i5, i4, i3, i2,  _) 1 i1 = pure (i5, i4, i3, i2, i1)
  setDimM ix                   d  _ = throwM $ IndexDimensionException ix d
  {-# INLINE [1] setDimM #-}
  modifyDimM (i5, i4, i3, i2, i1) 5 f = pure (i5, (f i5,   i4,   i3,   i2,   i1))
  modifyDimM (i5, i4, i3, i2, i1) 4 f = pure (i4, (  i5, f i4,   i3,   i2,   i1))
  modifyDimM (i5, i4, i3, i2, i1) 3 f = pure (i3, (  i5,   i4, f i3,   i2,   i1))
  modifyDimM (i5, i4, i3, i2, i1) 2 f = pure (i2, (  i5,   i4,   i3, f i2,   i1))
  modifyDimM (i5, i4, i3, i2, i1) 1 f = pure (i1, (  i5,   i4,   i3,   i2, f i1))
  modifyDimM ix                   d _ = throwM $ IndexDimensionException ix d
  {-# INLINE [1] modifyDimM #-}
  pullOutDimM (i5, i4, i3, i2, i1) 5 = pure (i5, (i4, i3, i2, i1))
  pullOutDimM (i5, i4, i3, i2, i1) 4 = pure (i4, (i5, i3, i2, i1))
  pullOutDimM (i5, i4, i3, i2, i1) 3 = pure (i3, (i5, i4, i2, i1))
  pullOutDimM (i5, i4, i3, i2, i1) 2 = pure (i2, (i5, i4, i3, i1))
  pullOutDimM (i5, i4, i3, i2, i1) 1 = pure (i1, (i5, i4, i3, i2))
  pullOutDimM ix                   d = throwM $ IndexDimensionException ix d
  {-# INLINE [1] pullOutDimM #-}
  insertDimM (i4, i3, i2, i1) 5 i5 = pure (i5, i4, i3, i2, i1)
  insertDimM (i5, i3, i2, i1) 4 i4 = pure (i5, i4, i3, i2, i1)
  insertDimM (i5, i4, i2, i1) 3 i3 = pure (i5, i4, i3, i2, i1)
  insertDimM (i5, i4, i3, i1) 2 i2 = pure (i5, i4, i3, i2, i1)
  insertDimM (i5, i4, i3, i2) 1 i1 = pure (i5, i4, i3, i2, i1)
  insertDimM ix               d  _ = throwM $ IndexDimensionException ix d
  {-# INLINE [1] insertDimM #-}
  pureIndex i = (i, i, i, i, i)
  {-# INLINE [1] pureIndex #-}
  liftIndex2 f (i5, i4, i3, i2, i1) (i5', i4', i3', i2', i1') =
    (f i5 i5', f i4 i4', f i3 i3', f i2 i2', f i1 i1')
  {-# INLINE [1] liftIndex2 #-}
