{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
#else
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE StandaloneDeriving         #-}
#endif
-- |
-- Module      : Data.Massiv.Core.Index.Internal
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Core.Index.Internal
  ( Sz(SafeSz)
  , pattern Sz
  , type Sz1
  , pattern Sz1
  , unSz
  , zeroSz
  , oneSz
  , consSz
  , unconsSz
  , snocSz
  , unsnocSz
  , setSz
  , insertSz
  , pullOutSz
  , Dim(..)
  , Dimension(..)
  , IsIndexDimension
  , pattern Dim1
  , pattern Dim2
  , pattern Dim3
  , pattern Dim4
  , pattern Dim5
  , Lower
  , Index(..)
  , Ix0(..)
  , type Ix1
  , pattern Ix1
  ) where

import Data.Coerce
import           Control.DeepSeq
import           Data.Massiv.Core.Iterator
import           GHC.TypeLits

-- | `Sz` provides type safety guarantees preventing mixup of index that is used to looking into
-- array cells from the size, that describes total number of elements in the array along each
-- dimension. Moreover the @Sz@ constructor will prevent creation of invalid sizes with negative
-- numbers
--
#if __GLASGOW_HASKELL__ >= 800
newtype Sz ix = SafeSz ix deriving (Eq, Ord, NFData)
{-# COMPLETE Sz #-}
#else
-- There is an issue in GHC 7.10 which prevents from placing `Index` constraint on a pattern.
data Sz ix where
  SafeSz :: Index ix => ix -> Sz ix

deriving instance Eq ix => Eq (Sz ix)
deriving instance Ord ix => Ord (Sz ix)
instance NFData ix => NFData (Sz ix) where
  rnf (SafeSz ix) = rnf ix
#endif


-- | A safe bidirectional pattern synonym for `Sz` construction that will make sure that none of
-- the size elements are negative.
pattern Sz :: Index ix => ix -> Sz ix
pattern Sz ix <- SafeSz ix where
        Sz ix = SafeSz (liftIndex (max 0) ix)

type Sz1 = Sz Ix1
pattern Sz1 :: Ix1 -> Sz1
pattern Sz1 ix  <- SafeSz ix where
        Sz1 ix = SafeSz (max 0 ix)


instance Index ix => Show (Sz ix) where
  show sz@(SafeSz usz) = "Sz" ++ show (unDim (dimensions sz)) ++ " (" ++ show usz ++ ")"


-- | Just a helper function for unwrapping `Sz`.
unSz :: Sz ix -> ix
unSz (SafeSz ix) = ix
{-# INLINE unSz #-}

-- | An empty size with all elements in size equal to @0@.
zeroSz :: Index ix => Sz ix
zeroSz = SafeSz (pureIndex 0)
{-# INLINE zeroSz #-}

-- | A singleton size with all elements in size equal to @1@.
oneSz :: Index ix => Sz ix
oneSz = SafeSz (pureIndex 1)
{-# INLINE oneSz #-}


consSz :: Index ix => Sz1 -> Sz (Lower ix) -> Sz ix
consSz (SafeSz i) (SafeSz ix) = SafeSz (consDim i ix)
{-# INLINE consSz #-}

unconsSz :: Index ix => Sz ix -> (Sz1, Sz (Lower ix))
unconsSz (SafeSz sz) = coerce (unconsDim sz)
{-# INLINE unconsSz #-}

snocSz :: Index ix => Sz (Lower ix) -> Sz1 -> Sz ix
snocSz (SafeSz i) (SafeSz ix) = SafeSz (snocDim i ix)
{-# INLINE snocSz #-}

unsnocSz :: Index ix => Sz ix -> (Sz (Lower ix), Sz1)
unsnocSz (SafeSz sz) = coerce (unsnocDim sz)
{-# INLINE unsnocSz #-}

setSz :: Index ix => Sz ix -> Dim -> Sz Int -> Maybe (Sz ix)
setSz (SafeSz sz) dim (SafeSz sz1) = SafeSz <$> setDim sz dim sz1
{-# INLINE setSz #-}

insertSz :: Index ix => Sz (Lower ix) -> Dim -> Sz Int -> Maybe (Sz ix)
insertSz (SafeSz sz) dim (SafeSz sz1) = SafeSz <$> insertDim sz dim sz1
{-# INLINE insertSz #-}

pullOutSz :: Index ix => Sz ix -> Dim -> Maybe (Sz Ix1, Sz (Lower ix))
pullOutSz (SafeSz sz) = coerce . pullOutDim sz
{-# INLINE pullOutSz #-}

-- | A way to select Array dimension at a value level.
newtype Dim = Dim { unDim :: Int } deriving (Show, Eq, Ord, Num, Real, Integral, Enum)

-- | A way to select Array dimension at a type level.
data Dimension (n :: Nat) where
  DimN :: (1 <= n, KnownNat n) => Dimension n

pattern Dim1 :: Dimension 1
pattern Dim1 = DimN
pattern Dim2 :: Dimension 2
pattern Dim2 = DimN
pattern Dim3 :: Dimension 3
pattern Dim3 = DimN
pattern Dim4 :: Dimension 4
pattern Dim4 = DimN
pattern Dim5 :: Dimension 5
pattern Dim5 = DimN


-- | A type level constraint that ensures index is indeed valid and that supplied dimension can be
-- safely used with it.
type IsIndexDimension ix n = (1 <= n, n <= Dimensions ix, Index ix, KnownNat n)


-- | This type family will always point to a type for a dimension that is one lower than the type
-- argument.
type family Lower ix :: *

-- | This is bread and butter of multi-dimensional array indexing. It is unlikely that any of the
-- functions in this class will be useful to a regular user, unless general algorithms are being
-- implemented that do span multiple dimensions.
class (Eq ix, Ord ix, Show ix, NFData ix) => Index ix where
  type Dimensions ix :: Nat

  -- | Dimensions of an array that has this index type, i.e. what is the dimensionality.
  dimensions :: proxy ix -> Dim

  -- | Total number of elements in an array of this size.
  totalElem :: Sz ix -> Int

  -- | Prepend a dimension to the index
  consDim :: Int -> Lower ix -> ix

  -- | Take a dimension from the index from the outside
  unconsDim :: ix -> (Int, Lower ix)

  -- | Apppend a dimension to the index
  snocDim :: Lower ix -> Int -> ix

  -- | Take a dimension from the index from the inside
  unsnocDim :: ix -> (Lower ix, Int)

  -- | Pull out value at specified dimension from the index, thus also lowering it dimensionality.
  pullOutDim :: ix -> Dim -> Maybe (Int, Lower ix)

  -- | Insert a dimension into the index
  insertDim :: Lower ix -> Dim -> Int -> Maybe ix

  -- | Extract the value index has at specified dimension.
  getDim :: ix -> Dim -> Maybe Int

  -- | Set the value for an index at specified dimension.
  setDim :: ix -> Dim -> Int -> Maybe ix

  -- | Lift an `Int` to any index by replicating the value as many times as there are dimensions.
  pureIndex :: Int -> ix

  -- | Zip together two indices with a function
  liftIndex2 :: (Int -> Int -> Int) -> ix -> ix -> ix

  -- | Map a function over an index
  liftIndex :: (Int -> Int) -> ix -> ix
  liftIndex f = liftIndex2 (\_ i -> f i) (pureIndex 0)
  {-# INLINE [1] liftIndex #-}

  foldlIndex :: (a -> Int -> a) -> a -> ix -> a
  default foldlIndex :: Index (Lower ix) => (a -> Int -> a) -> a -> ix -> a
  foldlIndex f !acc !ix = foldlIndex f (f acc i0) ixL
    where
      !(i0, ixL) = unconsDim ix
  {-# INLINE [1] foldlIndex #-}

  -- TODO: implement in terms of foldlIndex and pull out of the class
  -- | Check whether index is positive and is within the size.
  isSafeIndex :: Sz ix -- ^ Size
              -> ix -- ^ Index
              -> Bool
  default isSafeIndex :: Index (Lower ix) => Sz ix -> ix -> Bool
  isSafeIndex sz !ix = isSafeIndex n0 i0 && isSafeIndex szL ixL
    where
      !(n0, szL) = unconsSz sz
      !(i0, ixL) = unconsDim ix
  {-# INLINE [1] isSafeIndex #-}

  -- | Convert linear index from size and index
  toLinearIndex :: Sz ix -- ^ Size
                -> ix -- ^ Index
                -> Int
  default toLinearIndex :: Index (Lower ix) => Sz ix -> ix -> Int
  toLinearIndex (SafeSz sz) !ix = toLinearIndex (SafeSz szL) ixL * n + i
    where !(szL, n) = unsnocDim sz
          !(ixL, i) = unsnocDim ix
  {-# INLINE [1] toLinearIndex #-}

  -- | Convert linear index from size and index with an accumulator. Currently is useless and will
  -- likley be removed in future versions.
  toLinearIndexAcc :: Int -> ix -> ix -> Int
  default toLinearIndexAcc :: Index (Lower ix) => Int -> ix -> ix -> Int
  toLinearIndexAcc !acc !sz !ix = toLinearIndexAcc (acc * n + i) szL ixL
    where !(n, szL) = unconsDim sz
          !(i, ixL) = unconsDim ix
  {-# INLINE [1] toLinearIndexAcc #-}

  -- | Compute an index from size and linear index
  fromLinearIndex :: Sz ix -> Int -> ix
  default fromLinearIndex :: Index (Lower ix) => Sz ix -> Int -> ix
  fromLinearIndex (SafeSz sz) k = consDim q ixL
    where !(q, ixL) = fromLinearIndexAcc (snd (unconsDim sz)) k
  {-# INLINE [1] fromLinearIndex #-}

  -- | Compute an index from size and linear index using an accumulator, thus trying to optimize for
  -- tail recursion while getting the index computed.
  fromLinearIndexAcc :: ix -> Int -> (Int, ix)
  default fromLinearIndexAcc :: Index (Lower ix) => ix -> Int -> (Int, ix)
  fromLinearIndexAcc ix' !k = (q, consDim r ixL)
    where !(m, ix) = unconsDim ix'
          !(kL, ixL) = fromLinearIndexAcc ix k
          !(q, r) = quotRem kL m
  {-# INLINE [1] fromLinearIndexAcc #-}

  -- | A way to make sure index is withing the bounds for the supplied size. Takes two functions
  -- that will be invoked whenever index (2nd arg) is outsize the supplied size (1st arg)
  repairIndex :: Sz ix -- ^ Size
              -> ix -- ^ Index
              -> (Sz Int -> Int -> Int) -- ^ Repair when below zero
              -> (Sz Int -> Int -> Int) -- ^ Repair when higher than size
              -> ix
  default repairIndex :: Index (Lower ix)
    => Sz ix -> ix -> (Sz Int -> Int -> Int) -> (Sz Int -> Int -> Int) -> ix
  repairIndex sz !ix rBelow rOver =
    consDim (repairIndex n i rBelow rOver) (repairIndex szL ixL rBelow rOver)
    where !(n, szL) = unconsSz sz
          !(i, ixL) = unconsDim ix
  {-# INLINE [1] repairIndex #-}

  -- | This function is what makes it possible to iterate over an array of any dimension.
  iterM :: Monad m =>
           ix -- ^ Start index
        -> ix -- ^ End index
        -> ix -- ^ Increment
        -> (Int -> Int -> Bool) -- ^ Continue iterating while predicate is True (eg. until end of row)
        -> a -- ^ Initial value for an accumulator
        -> (ix -> a -> m a) -- ^ Accumulator function
        -> m a
  default iterM :: (Index (Lower ix), Monad m)
    => ix -> ix -> ix -> (Int -> Int -> Bool) -> a -> (ix -> a -> m a) -> m a
  iterM !sIx eIx !incIx cond !acc f =
    loopM s (`cond` e) (+ inc) acc $ \ !i !acc0 ->
      iterM sIxL eIxL incIxL cond acc0 $ \ !ix ->
        f (consDim i ix)
    where
      !(s, sIxL) = unconsDim sIx
      !(e, eIxL) = unconsDim eIx
      !(inc, incIxL) = unconsDim incIx
  {-# INLINE iterM #-}

  -- TODO: Implement in terms of iterM, benchmark it and remove from `Index`
  -- | Same as `iterM`, but don't bother with accumulator and return value.
  iterM_ :: Monad m => ix -> ix -> ix -> (Int -> Int -> Bool) -> (ix -> m a) -> m ()
  default iterM_ :: (Index (Lower ix), Monad m)
    => ix -> ix -> ix -> (Int -> Int -> Bool) -> (ix -> m a) -> m ()
  iterM_ !sIx eIx !incIx cond f =
    loopM_ s (`cond` e) (+ inc) $ \ !i ->
      iterM_ sIxL eIxL incIxL cond $ \ !ix ->
        f (consDim i ix)
    where
      !(s, sIxL) = unconsDim sIx
      !(e, eIxL) = unconsDim eIx
      !(inc, incIxL) = unconsDim incIx
  {-# INLINE iterM_ #-}

-- | Zero-dimension, i.e. a scalar. Can't really be used directly as there is no instance of
-- `Index` for it, and is included for completeness.
data Ix0 = Ix0 deriving (Eq, Ord, Show)

-- | A type synonym for 1-dimensional index, i.e. `Int`.
type Ix1 = Int

-- | This is a very handy pattern synonym to indicate that any arbitrary `Integral` literal is an
-- `Int`, e.g. a 1-dimensional index: @(Ix1 5) == (5 :: Int)@
pattern Ix1 :: Int -> Ix1
pattern Ix1 i = i

type instance Lower Int = Ix0


instance Index Ix1 where
  type Dimensions Ix1 = 1
  dimensions _ = 1
  {-# INLINE [1] dimensions #-}
  totalElem = unSz
  {-# INLINE [1] totalElem #-}
  isSafeIndex (SafeSz k) !i = 0 <= i && i < k
  {-# INLINE [1] isSafeIndex #-}
  toLinearIndex _ = id
  {-# INLINE [1] toLinearIndex #-}
  toLinearIndexAcc !acc m i  = acc * m + i
  {-# INLINE [1] toLinearIndexAcc #-}
  fromLinearIndex _ = id
  {-# INLINE [1] fromLinearIndex #-}
  fromLinearIndexAcc n k = k `quotRem` n
  {-# INLINE [1] fromLinearIndexAcc #-}
  repairIndex k@(SafeSz ksz) !i rBelow rOver
    | i < 0 = rBelow k i
    | i >= ksz = rOver k i
    | otherwise = i
  {-# INLINE [1] repairIndex #-}
  consDim i _ = i
  {-# INLINE [1] consDim #-}
  unconsDim i = (i, Ix0)
  {-# INLINE [1] unconsDim #-}
  snocDim _ i = i
  {-# INLINE [1] snocDim #-}
  unsnocDim i = (Ix0, i)
  {-# INLINE [1] unsnocDim #-}
  getDim i 1 = Just i
  getDim _ _ = Nothing
  {-# INLINE [1] getDim #-}
  setDim _ 1 i = Just i
  setDim _ _ _ = Nothing
  {-# INLINE [1] setDim #-}
  pullOutDim i 1 = Just (i, Ix0)
  pullOutDim _ _ = Nothing
  {-# INLINE [1] pullOutDim #-}
  insertDim Ix0 1 i = Just i
  insertDim _   _ _ = Nothing
  {-# INLINE [1] insertDim #-}
  pureIndex i = i
  {-# INLINE [1] pureIndex #-}
  liftIndex f = f
  {-# INLINE [1] liftIndex #-}
  liftIndex2 f = f
  {-# INLINE [1] liftIndex2 #-}
  foldlIndex f = f
  {-# INLINE [1] foldlIndex #-}
  iterM k0 k1 inc cond = loopM k0 (`cond` k1) (+inc)
  {-# INLINE iterM #-}
  iterM_ k0 k1 inc cond = loopM_ k0 (`cond` k1) (+inc)
  {-# INLINE iterM_ #-}
