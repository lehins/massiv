{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
#if __GLASGOW_HASKELL__ < 820
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
#endif
-- |
-- Module      : Data.Massiv.Core.Index.Internal
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Core.Index.Internal
  ( Sz(SafeSz)
  , pattern Sz
  , pattern Sz1
  , type Sz1
  , unSz
  , zeroSz
  , oneSz
  , liftSz
  , liftSz2
  , consSz
  , unconsSz
  , snocSz
  , unsnocSz
  , setSzM
  , insertSzM
  , pullOutSzM
  , Dim(..)
  , Dimension(DimN)
  , pattern Dim1
  , pattern Dim2
  , pattern Dim3
  , pattern Dim4
  , pattern Dim5
  , IsIndexDimension
  , IsDimValid
  , ReportInvalidDim
  , Lower
  , Index(..)
  , Ix0(..)
  , type Ix1
  , pattern Ix1
  , IndexException(..)
  , SizeException(..)
  , ShapeException(..)
  , showsPrecWrapped
  ) where

import Control.DeepSeq
import Control.Exception (Exception(..), throw)
import Control.Monad.Catch (MonadThrow(..))
import Data.Coerce
import Data.Massiv.Core.Iterator
import Data.Typeable
import GHC.TypeLits

-- | `Sz` provides type safety guarantees preventing mixup with index, which is used for looking into
-- array cells, from the size, that describes total number of elements along each dimension in the
-- array. Moreover the @Sz@ constructor will prevent creation of invalid sizes with negative numbers.
--
-- @since 0.3.0
newtype Sz ix =
  SafeSz ix
  -- ^ Safe size constructor. It is unsafe to use it without making sure that it does not contain
  -- negative components. Use `Data.Massiv.Core.Index.Sz` pattern instead.
  --
  -- @since 0.3.0
  deriving (Eq, Ord, NFData)

-- | A safe bidirectional pattern synonym for `Sz` construction that will make sure that none of
-- the size elements are negative.
--
-- @since 0.3.0
pattern Sz :: Index ix => ix -> Sz ix
pattern Sz ix <- SafeSz ix where
        Sz ix = SafeSz (liftIndex (max 0) ix)
{-# COMPLETE Sz #-}

-- | 1-dimensional type synonym for size.
--
-- @since 0.3.0
type Sz1 = Sz Ix1

-- | 1-dimensional size constructor. Especially useful with literals: @(Sz1 5) == Sz (5 :: Int)@.
--
-- @since 0.3.0
pattern Sz1 :: Ix1 -> Sz1
pattern Sz1 ix  <- SafeSz ix where
        Sz1 ix = SafeSz (max 0 ix)
{-# COMPLETE Sz1 #-}


instance Index ix => Show (Sz ix) where
  showsPrec n sz@(SafeSz usz) = showsPrecWrapped n (str ++)
    where
      str =
        "Sz" ++
        case unDim (dimensions sz) of
          1 -> "1 " ++ show usz
          _ -> " (" ++ shows usz ")"

instance (Num ix, Index ix) => Num (Sz ix) where
  (+) x y = Sz (coerce x + coerce y)
  {-# INLINE (+) #-}
  (-) x y = Sz (coerce x - coerce y)
  {-# INLINE (-) #-}
  (*) x y = SafeSz (coerce x * coerce y)
  {-# INLINE (*) #-}
  abs !x = x
  {-# INLINE abs #-}
  negate !_x = 0
  {-# INLINE negate #-}
  signum x = SafeSz (signum (coerce x))
  {-# INLINE signum #-}
  fromInteger = Sz . fromInteger
  {-# INLINE fromInteger #-}


-- | Function for unwrapping `Sz`.
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Core.Index
-- >>> unSz $ Sz3 1 2 3
-- 1 :> 2 :. 3
--
-- @since 0.3.0
unSz :: Sz ix -> ix
unSz (SafeSz ix) = ix
{-# INLINE unSz #-}

-- | An empty size with all elements in size equal to @0@.
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Core.Index
-- >>> zeroSz :: Sz5
-- Sz (0 :> 0 :> 0 :> 0 :. 0)
--
-- @since 0.3.0
zeroSz :: Index ix => Sz ix
zeroSz = SafeSz (pureIndex 0)
{-# INLINE zeroSz #-}

-- | A singleton size with all elements in size equal to @1@.
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Core.Index
-- >>> oneSz :: Sz3
-- Sz (1 :> 1 :. 1)
--
-- @since 0.3.0
oneSz :: Index ix => Sz ix
oneSz = SafeSz (pureIndex 1)
{-# INLINE oneSz #-}


-- | Same as `liftIndex`, but for `Sz`
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Core.Index
-- >>> liftSz succ (Sz2 2 3)
-- Sz (3 :. 4)
--
-- @since 0.4.0
liftSz :: Index ix => (Int -> Int) -> Sz ix -> Sz ix
liftSz f (SafeSz ix) = Sz (liftIndex f ix)
{-# INLINE liftSz #-}

-- | Same as `liftIndex2`, but for `Sz`
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Core.Index
-- >>> liftSz2 (-) (Sz2 2 3) (Sz2 3 1)
-- Sz (0 :. 2)
--
-- @since 0.4.3
liftSz2 :: Index ix => (Int -> Int -> Int) -> Sz ix -> Sz ix -> Sz ix
liftSz2 f sz1 sz2 = Sz (liftIndex2 f (coerce sz1) (coerce sz2))
{-# INLINE liftSz2 #-}


-- | Same as `consDim`, but for `Sz`
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Core.Index
-- >>> consSz (Sz1 1) (Sz2 2 3) :: Sz3
-- Sz (1 :> 2 :. 3)
--
-- @since 0.3.0
consSz :: Index ix => Sz1 -> Sz (Lower ix) -> Sz ix
consSz (SafeSz i) (SafeSz ix) = SafeSz (consDim i ix)
{-# INLINE consSz #-}


-- | Same as `snocDim`, but for `Sz`
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Core.Index
-- >>> snocSz (Sz2 2 3) (Sz1 1) :: Sz3
-- Sz (2 :> 3 :. 1)
--
-- @since 0.3.0
snocSz :: Index ix => Sz (Lower ix) -> Sz1 -> Sz ix
snocSz (SafeSz i) (SafeSz ix) = SafeSz (snocDim i ix)
{-# INLINE snocSz #-}

-- | Same as `setDimM`, but for `Sz`
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Core.Index
-- >>> setSzM (Sz2 2 3) 2 (Sz1 1) :: IO Sz2
-- Sz (1 :. 3)
-- >>> setSzM (Sz2 2 3) 3 (Sz1 1) :: IO Sz2
-- *** Exception: IndexDimensionException: (Dim 3) for (2 :. 3)
--
-- @since 0.3.0
setSzM :: (MonadThrow m, Index ix) => Sz ix -> Dim -> Sz Int -> m (Sz ix)
setSzM (SafeSz sz) dim (SafeSz sz1) = SafeSz <$> setDimM sz dim sz1
{-# INLINE setSzM #-}

-- | Same as `insertDimM`, but for `Sz`
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Core.Index
-- >>> insertSzM (Sz2 2 3) 3 (Sz1 1) :: IO Sz3
-- Sz (1 :> 2 :. 3)
-- >>> insertSzM (Sz2 2 3) 4 (Sz1 1) :: IO Sz3
-- *** Exception: IndexDimensionException: (Dim 4) for (2 :. 3)
--
-- @since 0.3.0
insertSzM :: (MonadThrow m, Index ix) => Sz (Lower ix) -> Dim -> Sz Int -> m (Sz ix)
insertSzM (SafeSz sz) dim (SafeSz sz1) = SafeSz <$> insertDimM sz dim sz1
{-# INLINE insertSzM #-}

-- | Same as `unconsDim`, but for `Sz`
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Core.Index
-- >>> unconsSz $ Sz3 1 2 3
-- (Sz1 1,Sz (2 :. 3))
--
-- @since 0.3.0
unconsSz :: Index ix => Sz ix -> (Sz1, Sz (Lower ix))
unconsSz (SafeSz sz) = coerce (unconsDim sz)
{-# INLINE unconsSz #-}

-- | Same as `unsnocDim`, but for `Sz`
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Core.Index
-- >>> unsnocSz $ Sz3 1 2 3
-- (Sz (1 :. 2),Sz1 3)
--
-- @since 0.3.0
unsnocSz :: Index ix => Sz ix -> (Sz (Lower ix), Sz1)
unsnocSz (SafeSz sz) = coerce (unsnocDim sz)
{-# INLINE unsnocSz #-}

-- | Same as `pullOutDim`, but for `Sz`
--
-- >>> import Data.Massiv.Core.Index
-- >>> pullOutSzM (Sz3 1 2 3) 3
-- (Sz1 1,Sz (2 :. 3))
-- >>> pullOutSzM (Sz3 1 2 3) 0
-- *** Exception: IndexDimensionException: (Dim 0) for (1 :> 2 :. 3)
--
-- @since 0.3.0
pullOutSzM :: (MonadThrow m, Index ix) => Sz ix -> Dim -> m (Sz Ix1, Sz (Lower ix))
pullOutSzM (SafeSz sz) = fmap coerce . pullOutDimM sz
{-# INLINE pullOutSzM #-}


-- | A way to select Array dimension at a value level.
--
-- @since 0.1.0
newtype Dim = Dim { unDim :: Int } deriving (Eq, Ord, Num, Real, Integral, Enum, NFData)

instance Show Dim where
  show (Dim d) = "(Dim " ++ show d ++ ")"

-- | A way to select Array dimension at a type level.
--
-- @since 0.2.4
data Dimension (n :: Nat) where
  DimN :: (1 <= n, KnownNat n) => Dimension n

-- | Construct 1st dimension
--
-- @since 0.2.4
pattern Dim1 :: Dimension 1
pattern Dim1 = DimN

-- | Construct 2nd dimension
--
-- @since 0.2.4
pattern Dim2 :: Dimension 2
pattern Dim2 = DimN

-- | Construct 3rd dimension
--
-- @since 0.2.4
pattern Dim3 :: Dimension 3
pattern Dim3 = DimN

-- | Construct 4th dimension
--
-- @since 0.2.4
pattern Dim4 :: Dimension 4
pattern Dim4 = DimN

-- | Construct 5th dimension
--
-- @since 0.2.4
pattern Dim5 :: Dimension 5
pattern Dim5 = DimN


-- | A type level constraint that ensures index is indeed valid and that supplied dimension can be
-- safely used with it.
--
-- @since 0.2.4
type IsIndexDimension ix n = (1 <= n, n <= Dimensions ix, Index ix, KnownNat n)


-- | This type family will always point to a type for a dimension that is one lower than the type
-- argument.
--
-- @since 0.1.0
type family Lower ix :: *


type family ReportInvalidDim (dims :: Nat) (n :: Nat) isNotZero isLess :: Bool where
  ReportInvalidDim dims n True True = True
  ReportInvalidDim dims n True False =
    TypeError (Text "Dimension " :<>: ShowType n :<>: Text " is higher than " :<>:
                Text "the maximum expected " :<>: ShowType dims)
  ReportInvalidDim dims n False isLess =
    TypeError (Text "Zero dimensional indices are not supported")

type family IsDimValid ix n :: Bool where
  IsDimValid ix n = ReportInvalidDim (Dimensions ix) n (1 <=? n) (n <=? Dimensions ix)

-- | This is bread and butter of multi-dimensional array indexing. It is unlikely that any of the
-- functions in this class will be useful to a regular user, unless general algorithms are being
-- implemented that do span multiple dimensions.
class ( Eq ix
      , Ord ix
      , Show ix
      , NFData ix
      , Eq (Lower ix)
      , Ord (Lower ix)
      , Show (Lower ix)
      , NFData (Lower ix)
      , KnownNat (Dimensions ix)
      ) =>
      Index ix
  where
  -- | Type level information on how many dimensions this index has.
  --
  -- @since 0.2.0
  type Dimensions ix :: Nat

  -- | What is the dimensionality of this index.
  --
  -- @since 0.2.0
  dimensions :: proxy ix -> Dim

  -- | Total number of elements in an array of this size.
  --
  -- @since 0.1.0
  totalElem :: Sz ix -> Int

  -- | Prepend a dimension to the index
  --
  -- @since 0.1.0
  consDim :: Int -> Lower ix -> ix

  -- | Take a dimension from the index from the outside
  --
  -- @since 0.1.0
  unconsDim :: ix -> (Int, Lower ix)

  -- | Apppend a dimension to the index
  --
  -- @since 0.1.0
  snocDim :: Lower ix -> Int -> ix

  -- | Take a dimension from the index from the inside
  --
  -- @since 0.1.0
  unsnocDim :: ix -> (Lower ix, Int)

  -- | Pull out value at specified dimension from the index, thus also lowering it dimensionality.
  --
  -- @since 0.2.5
  pullOutDimM :: MonadThrow m => ix -> Dim -> m (Int, Lower ix)

  -- | Insert a dimension into the index
  insertDimM :: MonadThrow m => Lower ix -> Dim -> Int -> m ix

  -- | Extract the value index has at specified dimension.
  --
  -- @since 0.3.0
  getDimM :: MonadThrow m => ix -> Dim -> m Int
  getDimM ix dim = fst <$> modifyDimM ix dim id
  {-# INLINE [1] getDimM #-}

  -- | Set the value for an index at specified dimension.
  --
  -- @since 0.3.0
  setDimM :: MonadThrow m => ix -> Dim -> Int -> m ix
  setDimM ix dim i = snd <$> modifyDimM ix dim (const i)
  {-# INLINE [1] setDimM #-}

  -- | Update the value for an index at specified dimension and return the old value as
  -- well as the updated index.
  --
  -- @since 0.4.1
  modifyDimM :: MonadThrow m => ix -> Dim -> (Int -> Int) -> m (Int, ix)
  modifyDimM ix dim f = do
    i <- getDimM ix dim
    ix' <- setDimM ix dim (f i)
    pure (i, ix')
  {-# INLINE [1] modifyDimM #-}

  -- | Lift an `Int` to any index by replicating the value as many times as there are dimensions.
  --
  -- @since 0.1.0
  pureIndex :: Int -> ix

  -- | Zip together two indices with a function
  --
  -- @since 0.1.0
  liftIndex2 :: (Int -> Int -> Int) -> ix -> ix -> ix

  -- | Map a function over an index
  --
  -- @since 0.1.0
  liftIndex :: (Int -> Int) -> ix -> ix
  liftIndex f = liftIndex2 (\_ i -> f i) (pureIndex 0)
  {-# INLINE [1] liftIndex #-}

  -- | Perform a left fold over the index
  foldlIndex :: (a -> Int -> a) -> a -> ix -> a
  default foldlIndex :: Index (Lower ix) =>
    (a -> Int -> a) -> a -> ix -> a
  foldlIndex f !acc !ix = foldlIndex f (f acc i0) ixL
    where
      !(i0, ixL) = unconsDim ix
  {-# INLINE [1] foldlIndex #-}

  -- TODO: implement in terms of foldlIndex and pull out of the class
  -- | Check whether index is positive and is within the size.
  --
  -- @since 0.1.0
  isSafeIndex ::
       Sz ix -- ^ Size
    -> ix -- ^ Index
    -> Bool
  default isSafeIndex :: Index (Lower ix) =>
    Sz ix -> ix -> Bool
  isSafeIndex sz !ix = isSafeIndex n0 i0 && isSafeIndex szL ixL
    where
      !(n0, szL) = unconsSz sz
      !(i0, ixL) = unconsDim ix
  {-# INLINE [1] isSafeIndex #-}

  -- | Convert linear index from size and index
  --
  -- @since 0.1.0
  toLinearIndex ::
       Sz ix -- ^ Size
    -> ix -- ^ Index
    -> Int
  default toLinearIndex :: Index (Lower ix) =>
    Sz ix -> ix -> Int
  toLinearIndex (SafeSz sz) !ix = toLinearIndex (SafeSz szL) ixL * n + i
    where
      !(szL, n) = unsnocDim sz
      !(ixL, i) = unsnocDim ix
  {-# INLINE [1] toLinearIndex #-}

  -- | Convert linear index from size and index with an accumulator. Currently is useless and will
  -- likley be removed in future versions.
  --
  -- @since 0.1.0
  toLinearIndexAcc :: Int -> ix -> ix -> Int
  default toLinearIndexAcc :: Index (Lower ix) =>
    Int -> ix -> ix -> Int
  toLinearIndexAcc !acc !sz !ix = toLinearIndexAcc (acc * n + i) szL ixL
    where
      !(n, szL) = unconsDim sz
      !(i, ixL) = unconsDim ix
  {-# INLINE [1] toLinearIndexAcc #-}

  -- | Compute an index from size and linear index
  --
  -- @since 0.1.0
  fromLinearIndex :: Sz ix -> Int -> ix
  default fromLinearIndex :: Index (Lower ix) =>
    Sz ix -> Int -> ix
  fromLinearIndex (SafeSz sz) k = consDim q ixL
    where
      !(q, ixL) = fromLinearIndexAcc (snd (unconsDim sz)) k
  {-# INLINE [1] fromLinearIndex #-}

  -- | Compute an index from size and linear index using an accumulator, thus trying to optimize for
  -- tail recursion while getting the index computed.
  --
  -- @since 0.1.0
  fromLinearIndexAcc :: ix -> Int -> (Int, ix)
  default fromLinearIndexAcc :: Index (Lower ix) =>
    ix -> Int -> (Int, ix)
  fromLinearIndexAcc ix' !k = (q, consDim r ixL)
    where
      !(m, ix) = unconsDim ix'
      !(kL, ixL) = fromLinearIndexAcc ix k
      !(q, r) = quotRem kL m
  {-# INLINE [1] fromLinearIndexAcc #-}

  -- | A way to make sure index is withing the bounds for the supplied size. Takes two functions
  -- that will be invoked whenever index (2nd arg) is outsize the supplied size (1st arg)
  --
  -- @since 0.1.0
  repairIndex ::
       Sz ix -- ^ Size
    -> ix -- ^ Index
    -> (Sz Int -> Int -> Int) -- ^ Repair when below zero
    -> (Sz Int -> Int -> Int) -- ^ Repair when higher than size
    -> ix
  default repairIndex :: Index (Lower ix) =>
    Sz ix -> ix -> (Sz Int -> Int -> Int) -> (Sz Int -> Int -> Int) -> ix
  repairIndex sz !ix rBelow rOver =
    consDim (repairIndex n i rBelow rOver) (repairIndex szL ixL rBelow rOver)
    where
      !(n, szL) = unconsSz sz
      !(i, ixL) = unconsDim ix
  {-# INLINE [1] repairIndex #-}

  -- | This function is what makes it possible to iterate over an array of any dimension.
  --
  -- @since 0.1.0
  iterM ::
       Monad m
    => ix -- ^ Start index
    -> ix -- ^ End index
    -> ix -- ^ Increment
    -> (Int -> Int -> Bool) -- ^ Continue iterating while predicate is True (eg. until end of row)
    -> a -- ^ Initial value for an accumulator
    -> (ix -> a -> m a) -- ^ Accumulator function
    -> m a
  default iterM :: (Index (Lower ix), Monad m) =>
    ix -> ix -> ix -> (Int -> Int -> Bool) -> a -> (ix -> a -> m a) -> m a
  iterM !sIx eIx !incIx cond !acc f =
    loopM s (`cond` e) (+ inc) acc $ \ !i !acc0 ->
      iterM sIxL eIxL incIxL cond acc0 $ \ !ix -> f (consDim i ix)
    where
      !(s, sIxL) = unconsDim sIx
      !(e, eIxL) = unconsDim eIx
      !(inc, incIxL) = unconsDim incIx
  {-# INLINE iterM #-}

  -- TODO: Implement in terms of iterM, benchmark it and remove from `Index`
  -- | Same as `iterM`, but don't bother with accumulator and return value.
  --
  -- @since 0.1.0
  iterM_ :: Monad m => ix -> ix -> ix -> (Int -> Int -> Bool) -> (ix -> m a) -> m ()
  default iterM_ :: (Index (Lower ix), Monad m) =>
    ix -> ix -> ix -> (Int -> Int -> Bool) -> (ix -> m a) -> m ()
  iterM_ !sIx eIx !incIx cond f =
    loopM_ s (`cond` e) (+ inc) $ \ !i -> iterM_ sIxL eIxL incIxL cond $ \ !ix -> f (consDim i ix)
    where
      !(s, sIxL) = unconsDim sIx
      !(e, eIxL) = unconsDim eIx
      !(inc, incIxL) = unconsDim incIx
  {-# INLINE iterM_ #-}

-- | Zero-dimension, i.e. a scalar. Can't really be used directly as there is no instance of
-- `Index` for it, and is included for completeness.
data Ix0 = Ix0 deriving (Eq, Ord, Show)

instance NFData Ix0 where
  rnf Ix0 = ()

-- | A type synonym for 1-dimensional index, i.e. `Int`.
--
-- >>> 5 :: Ix1
-- 5
--
-- @since 0.1.0
type Ix1 = Int

-- | This is a very handy pattern synonym to indicate that any arbitrary `Integral` literal is an
-- `Int`, e.g. a 1-dimensional index: @(Ix1 5) == (5 :: Int)@
--
-- >>> Ix1 5
-- 5
-- >>> :t Ix1 5
-- Ix1 5 :: Ix1
--
-- @since 0.1.0
pattern Ix1 :: Int -> Ix1
pattern Ix1 i = i
{-# COMPLETE Ix1 #-}

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
    | ksz <= 0 = throw $ IndexZeroException ksz
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
  getDimM ix 1 = pure ix
  getDimM ix d = throwM $ IndexDimensionException ix d
  {-# INLINE [1] getDimM #-}
  setDimM _  1 ix = pure ix
  setDimM ix d _  = throwM $ IndexDimensionException ix d
  {-# INLINE [1] setDimM #-}
  modifyDimM ix 1 f = pure (ix, f ix)
  modifyDimM ix d _ = throwM $ IndexDimensionException ix d
  {-# INLINE [1] modifyDimM #-}
  pullOutDimM ix 1 = pure (ix, Ix0)
  pullOutDimM ix d = throwM $ IndexDimensionException ix d
  {-# INLINE [1] pullOutDimM #-}
  insertDimM Ix0 1 i = pure i
  insertDimM ix  d _ = throwM $ IndexDimensionException ix d
  {-# INLINE [1] insertDimM #-}
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


-- | Exceptions that get thrown when there is a problem with an index, size or dimension.
--
-- @since 0.3.0
data IndexException where
  -- | Index contains a zero value along one of the dimensions.
  IndexZeroException :: Index ix => !ix -> IndexException
  -- | Dimension is out of reach.
  IndexDimensionException :: (NFData ix, Show ix, Typeable ix) => !ix -> !Dim -> IndexException
  -- | Index is out of bounds.
  IndexOutOfBoundsException :: Index ix => !(Sz ix) -> !ix -> IndexException

instance Show IndexException where
  show (IndexZeroException ix) = "IndexZeroException: " ++ showsPrec 1 ix ""
  show (IndexDimensionException ix dim) =
    "IndexDimensionException: " ++ showsPrec 1 dim " for " ++ showsPrec 1 ix ""
  show (IndexOutOfBoundsException sz ix) =
    "IndexOutOfBoundsException: " ++ showsPrec 1 ix " is not safe for " ++ showsPrec 1 sz ""
  showsPrec n exc = showsPrecWrapped n (show exc ++)

instance Eq IndexException where
  e1 == e2 =
    case (e1, e2) of
      (IndexZeroException i1, IndexZeroException i2) -> show i1 == show i2
      (IndexDimensionException i1 d1, IndexDimensionException i2 d2) ->
        show i1 == show i2 && d1 == d2
      (IndexOutOfBoundsException sz1 i1, IndexOutOfBoundsException sz2 i2) ->
        show sz1 == show sz2 && show i1 == show i2
      _ -> False

instance NFData IndexException where
  rnf =
    \case
      IndexZeroException i -> rnf i
      IndexDimensionException i d -> i `deepseq` rnf d
      IndexOutOfBoundsException sz i -> sz `deepseq` rnf i

instance Exception IndexException

-- | Exception that indicates an issue with an array size.
--
-- @since 0.3.0
data SizeException where
  -- | Two sizes are expected to be equal along some or all dimensions, but they are not.
  SizeMismatchException :: Index ix => !(Sz ix) -> !(Sz ix) -> SizeException
  -- | Total number of elements does not match between the two sizes.
  SizeElementsMismatchException :: (Index ix, Index ix') => !(Sz ix) -> !(Sz ix') -> SizeException
  -- | Described subregion is too big for the specified size.
  SizeSubregionException :: Index ix => !(Sz ix) -> !ix -> !(Sz ix) -> SizeException
  -- | An array with the size cannot contain any elements.
  SizeEmptyException :: Index ix => !(Sz ix) -> SizeException

instance Eq SizeException where
  e1 == e2 =
    case (e1, e2) of
      (SizeMismatchException sz1 sz1', SizeMismatchException sz2 sz2') ->
        show sz1 == show sz2 && show sz1' == show sz2'
      (SizeElementsMismatchException sz1 sz1', SizeElementsMismatchException sz2 sz2') ->
        show sz1 == show sz2 && show sz1' == show sz2'
      (SizeSubregionException sz1 i1 sz1', SizeSubregionException sz2 i2 sz2') ->
        show sz1 == show sz2 && show i1 == show i2 && show sz1' == show sz2'
      (SizeEmptyException sz1, SizeEmptyException sz2) -> show sz1 == show sz2
      _ -> False

instance NFData SizeException where
  rnf =
    \case
      SizeMismatchException sz sz' -> sz `deepseq` rnf sz'
      SizeElementsMismatchException sz sz' -> sz `deepseq` rnf sz'
      SizeSubregionException sz i sz' -> sz `deepseq` i `deepseq` rnf sz'
      SizeEmptyException sz -> rnf sz

instance Exception SizeException


instance Show SizeException where
  show (SizeMismatchException sz sz') =
    "SizeMismatchException: (" ++ show sz ++ ") vs (" ++ show sz' ++ ")"
  show (SizeElementsMismatchException sz sz') =
    "SizeElementsMismatchException: (" ++ show sz ++ ") vs (" ++ show sz' ++ ")"
  show (SizeSubregionException sz' ix sz) =
    "SizeSubregionException: (" ++
    show sz' ++ ") is to small for " ++ show ix ++ " (" ++ show sz ++ ")"
  show (SizeEmptyException sz) =
    "SizeEmptyException: (" ++ show sz ++ ") corresponds to an empty array"
  showsPrec n exc = showsPrecWrapped n (show exc ++)

-- | Exception that can happen upon conversion of a ragged type array into the rectangular kind. Which
-- means conversion from lists is susceptible to this exception.
--
-- @since 0.3.0
data ShapeException
  = DimTooShortException !Sz1 !Sz1
  | DimTooLongException
  deriving Eq

instance Show ShapeException where
  showsPrec _ DimTooLongException = ("DimTooLongException" ++)
  showsPrec n (DimTooShortException sz sz') =
    showsPrecWrapped
      n
      (("DimTooShortException: expected (" ++) . shows sz . ("), got (" ++) . shows sz' . (")" ++))

instance Exception ShapeException


showsPrecWrapped :: Int -> ShowS -> ShowS
showsPrecWrapped n inner
  | n < 1 = inner
  | otherwise = ('(':) . inner . (")" ++)
