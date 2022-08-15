{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Massiv.Core.Index.Ix
-- Copyright   : (c) Alexey Kuleshevich 2018-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Core.Index.Ix
  ( Ix
  , IxN((:>))
  , type Sz
  , pattern Sz
  , type Ix1
  , pattern Ix1
  , pattern Sz1
  , type Ix2(Ix2, (:.))
  , pattern Sz2
  , type Ix3
  , pattern Ix3
  , pattern Sz3
  , type Ix4
  , pattern Ix4
  , pattern Sz4
  , type Ix5
  , pattern Ix5
  , pattern Sz5
  , HighIxN
  ) where

import Control.DeepSeq
import Control.Monad.Catch (MonadThrow(..))
import Data.Massiv.Core.Index.Internal
import Data.Proxy
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified GHC.Arr as I
import GHC.TypeLits
import System.Random.Stateful
import Data.Massiv.Core.Loop
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif


infixr 5 :>, :.


-- | 2-dimensional index. This is also a base index for higher dimensions.
--
-- @since 0.1.0
data Ix2 = {-# UNPACK #-} !Int :. {-# UNPACK #-} !Int

-- | 2-dimensional index constructor. Useful when infix notation is inconvenient. @(Ix2 i j) == (i :. j)@
--
-- @since 0.1.0
pattern Ix2 :: Int -> Int -> Ix2
pattern Ix2 i2 i1 = i2 :. i1
{-# COMPLETE Ix2 #-}

-- | 2-dimensional size constructor. @(Sz2 i j) == Sz (i :. j)@
--
-- @since 0.3.0
pattern Sz2 :: Int -> Int -> Sz Ix2
pattern Sz2 i2 i1 = Sz (i2 :. i1)
{-# COMPLETE Sz2 #-}

-- | 3-dimensional type synonym. Useful as a alternative to enabling @DataKinds@ and using type
-- level Nats.
--
-- @since 0.1.0
type Ix3 = IxN 3

-- | 3-dimensional index constructor. @(Ix3 i j k) == (i :> j :. k)@
--
-- @since 0.1.0
pattern Ix3 :: Int -> Int -> Int -> Ix3
pattern Ix3 i3 i2 i1 = i3 :> i2 :. i1
{-# COMPLETE Ix3 #-}

-- | 3-dimensional size constructor. @(Sz3 i j k) == Sz (i :> j :. k)@
--
-- @since 0.3.0
pattern Sz3 :: Int -> Int -> Int -> Sz Ix3
pattern Sz3 i3 i2 i1 = Sz (i3 :> i2 :. i1)
{-# COMPLETE Sz3 #-}

-- | 4-dimensional type synonym.
--
-- @since 0.1.0
type Ix4 = IxN 4

-- | 4-dimensional index constructor. @(Ix4 i j k l) == (i :> j :> k :. l)@
--
-- @since 0.1.0
pattern Ix4 :: Int -> Int -> Int -> Int -> Ix4
pattern Ix4 i4 i3 i2 i1 = i4 :> i3 :> i2 :. i1
{-# COMPLETE Ix4 #-}

-- | 4-dimensional size constructor. @(Sz4 i j k l) == Sz (i :> j :> k :. l)@
--
-- @since 0.3.0
pattern Sz4 :: Int -> Int -> Int -> Int -> Sz Ix4
pattern Sz4 i4 i3 i2 i1 = Sz (i4 :> i3 :> i2 :. i1)
{-# COMPLETE Sz4 #-}

-- | 5-dimensional type synonym.
--
-- @since 0.1.0
type Ix5 = IxN 5

-- | 5-dimensional index constructor.  @(Ix5 i j k l m) == (i :> j :> k :> l :. m)@
--
-- @since 0.1.0
pattern Ix5 :: Int -> Int -> Int -> Int -> Int -> Ix5
pattern Ix5 i5 i4 i3 i2 i1 = i5 :> i4 :> i3 :> i2 :. i1
{-# COMPLETE Ix5 #-}

-- | 5-dimensional size constructor.  @(Sz5 i j k l m) == Sz (i :> j :> k :> l :. m)@
--
-- @since 0.3.0
pattern Sz5 :: Int -> Int -> Int -> Int -> Int -> Sz Ix5
pattern Sz5 i5 i4 i3 i2 i1 = Sz (i5 :> i4 :> i3 :> i2 :. i1)
{-# COMPLETE Sz5 #-}

-- | n-dimensional index. Needs a base case, which is the `Ix2`.
--
-- @since 0.1.0
data IxN (n :: Nat) = {-# UNPACK #-} !Int :> !(Ix (n - 1))

-- | Defines n-dimensional index by relating a general `IxN` with few base cases.
--
-- @since 0.1.0
type family Ix (n :: Nat) = r | r -> n where
  Ix 0 = Ix0
  Ix 1 = Ix1
  Ix 2 = Ix2
  Ix n = IxN n


type instance Lower Ix2 = Ix1
type instance Lower (IxN n) = Ix (n - 1)


instance Show Ix2 where
  showsPrec n (i :. j) = showsPrecWrapped n (shows i . (" :. " ++) . shows j)

instance Show (Ix (n - 1)) => Show (IxN n) where
  showsPrec n (i :> ix) = showsPrecWrapped n (shows i . (" :> " ++) . shows ix)

instance Uniform Ix2 where
  uniformM g = (:.) <$> uniformM g <*> uniformM g
  {-# INLINE uniformM #-}

instance UniformRange Ix2 where
  uniformRM (l1 :. l2, u1 :. u2) g = (:.) <$> uniformRM (l1, u1) g <*> uniformRM (l2, u2) g
  {-# INLINE uniformRM #-}

instance Random Ix2

instance Uniform (Ix (n - 1)) => Uniform (IxN n) where
  uniformM g = (:>) <$> uniformM g <*> uniformM g
  {-# INLINE uniformM #-}

instance UniformRange (Ix (n - 1)) => UniformRange (IxN n) where
  uniformRM (l1 :> l2, u1 :> u2) g = (:>) <$> uniformRM (l1, u1) g <*> uniformRM (l2, u2) g
  {-# INLINE uniformRM #-}

instance Random (Ix (n - 1)) => Random (IxN n) where
  random g =
    case random g of
      (i, g') ->
        case random g' of
          (n, g'') -> (i :> n, g'')
  {-# INLINE random #-}
  randomR (l1 :> l2, u1 :> u2) g =
    case randomR (l1, u1) g of
      (i, g') ->
        case randomR (l2, u2) g' of
          (n, g'') -> (i :> n, g'')
  {-# INLINE randomR #-}

instance I.Ix Ix2 where
  range (i1 :. j1, i2 :. j2) = [i :. j | i <- [i1 .. i2], j <- [j1 .. j2]]
  {-# INLINE range #-}
  unsafeIndex (l1 :. l2, u1 :. u2) (i1 :. i2) =
    I.unsafeIndex (l1, u1) i1 * I.unsafeRangeSize (l2, u2) + I.unsafeIndex (l2, u2) i2
  {-# INLINE unsafeIndex #-}
  inRange (l1 :. l2, u1 :. u2) (i1 :. i2) = I.inRange (l1, u1) i1 && I.inRange (l2, u2) i2
  {-# INLINE inRange #-}

instance I.Ix (Ix (n - 1)) => I.Ix (IxN n) where
  range (i1 :> j1, i2 :> j2) = [i :> j | i <- [i1 .. i2], j <- I.range (j1, j2)]
  {-# INLINE range #-}
  unsafeIndex (l1 :> l2, u1 :> u2) (i1 :> i2) =
    I.unsafeIndex (l1, u1) i1 * I.unsafeRangeSize (l2, u2) + I.unsafeIndex (l2, u2) i2
  {-# INLINE unsafeIndex #-}
  inRange (l1 :> l2, u1 :> u2) (i1 :> i2) = I.inRange (l1, u1) i1 && I.inRange (l2, u2) i2
  {-# INLINE inRange #-}


instance Num Ix2 where
  (+) = liftIndex2 (+)
  {-# INLINE [1] (+) #-}
  (-) = liftIndex2 (-)
  {-# INLINE [1] (-) #-}
  (*) = liftIndex2 (*)
  {-# INLINE [1] (*) #-}
  negate = liftIndex negate
  {-# INLINE [1] negate #-}
  abs = liftIndex abs
  {-# INLINE [1] abs #-}
  signum = liftIndex signum
  {-# INLINE [1] signum #-}
  fromInteger = pureIndex . fromInteger
  {-# INLINE [1] fromInteger #-}

instance Num Ix3 where
  (+) = liftIndex2 (+)
  {-# INLINE [1] (+) #-}
  (-) = liftIndex2 (-)
  {-# INLINE [1] (-) #-}
  (*) = liftIndex2 (*)
  {-# INLINE [1] (*) #-}
  negate = liftIndex negate
  {-# INLINE [1] negate #-}
  abs = liftIndex abs
  {-# INLINE [1] abs #-}
  signum = liftIndex signum
  {-# INLINE [1] signum #-}
  fromInteger = pureIndex . fromInteger
  {-# INLINE [1] fromInteger #-}

instance {-# OVERLAPPABLE #-} HighIxN n => Num (IxN n) where
  (+) = liftIndex2 (+)
  {-# INLINE [1] (+) #-}
  (-) = liftIndex2 (-)
  {-# INLINE [1] (-) #-}
  (*) = liftIndex2 (*)
  {-# INLINE [1] (*) #-}
  negate = liftIndex negate
  {-# INLINE [1] negate #-}
  abs = liftIndex abs
  {-# INLINE [1] abs #-}
  signum = liftIndex signum
  {-# INLINE [1] signum #-}
  fromInteger = pureIndex . fromInteger
  {-# INLINE [1] fromInteger #-}



instance Bounded Ix2 where
  minBound = pureIndex minBound
  {-# INLINE minBound #-}
  maxBound = pureIndex maxBound
  {-# INLINE maxBound #-}

instance Bounded Ix3 where
  minBound = pureIndex minBound
  {-# INLINE minBound #-}
  maxBound = pureIndex maxBound
  {-# INLINE maxBound #-}

instance {-# OVERLAPPABLE #-} HighIxN n => Bounded (IxN n) where
  minBound = pureIndex minBound
  {-# INLINE minBound #-}
  maxBound = pureIndex maxBound
  {-# INLINE maxBound #-}

instance NFData Ix2 where
  rnf ix = ix `seq` ()

instance NFData (IxN n) where
  rnf ix = ix `seq` ()


instance Eq Ix2 where
  (i1 :. j1)  == (i2 :. j2) = i1 == i2 && j1 == j2

instance Eq (Ix (n - 1)) => Eq (IxN n) where
  (i1 :> ix1) == (i2 :> ix2) = i1 == i2 && ix1 == ix2


instance Ord Ix2 where
  compare (i1 :. j1) (i2 :. j2) = compare i1 i2 <> compare j1 j2

instance Ord (Ix (n - 1)) => Ord (IxN n) where
  compare (i1 :> ix1) (i2 :> ix2) = compare i1 i2 <> compare ix1 ix2


instance Index Ix2 where
  type Dimensions Ix2 = 2
  dimensions _ = 2
  {-# INLINE [1] dimensions #-}
  totalElem (SafeSz (k2 :. k1)) = k2 * k1
  {-# INLINE [1] totalElem #-}
  isSafeIndex (SafeSz (k2 :. k1)) (i2 :. i1) = 0 <= i2 && 0 <= i1 && i2 < k2 && i1 < k1
  {-# INLINE [1] isSafeIndex #-}
  toLinearIndex (SafeSz (_ :. k1)) (i2 :. i1) = k1 * i2 + i1
  {-# INLINE [1] toLinearIndex #-}
  fromLinearIndex (SafeSz (_ :. k1)) i =
    case i `quotRem` k1 of
      (i2, i1) -> i2 :. i1
  {-# INLINE [1] fromLinearIndex #-}
  consDim = (:.)
  {-# INLINE [1] consDim #-}
  unconsDim (i2 :. i1) = (i2, i1)
  {-# INLINE [1] unconsDim #-}
  snocDim i2 i1 = i2 :. i1
  {-# INLINE [1] snocDim #-}
  unsnocDim (i2 :. i1) = (i2, i1)
  {-# INLINE [1] unsnocDim #-}
  getDimM (i2 :. _) 2 = pure i2
  getDimM (_ :. i1) 1 = pure i1
  getDimM ix d        = throwM $ IndexDimensionException ix d
  {-# INLINE [1] getDimM #-}
  setDimM (_ :. i1) 2 i2 = pure (i2 :. i1)
  setDimM (i2 :. _) 1 i1 = pure (i2 :. i1)
  setDimM ix d _         = throwM $ IndexDimensionException ix d
  {-# INLINE [1] setDimM #-}
  pullOutDimM (i2 :. i1) 2 = pure (i2, i1)
  pullOutDimM (i2 :. i1) 1 = pure (i1, i2)
  pullOutDimM ix d         = throwM $ IndexDimensionException ix d
  {-# INLINE [1] pullOutDimM #-}
  insertDimM i1 2 i2 = pure (i2 :. i1)
  insertDimM i2 1 i1 = pure (i2 :. i1)
  insertDimM ix d _  = throwM $ IndexDimensionException ix d
  {-# INLINE [1] insertDimM #-}
  pureIndex i = i :. i
  {-# INLINE [1] pureIndex #-}
  liftIndex f (i2 :. i1) = f i2 :. f i1
  {-# INLINE [1] liftIndex #-}
  liftIndex2 f (i2 :. i1) (i2' :. i1') = f i2 i2' :. f i1 i1'
  {-# INLINE [1] liftIndex2 #-}
  repairIndex (SafeSz (k :. szL)) (i :. ixL) rBelow rOver =
    repairIndex (SafeSz k) i rBelow rOver :. repairIndex (SafeSz szL) ixL rBelow rOver
  {-# INLINE [1] repairIndex #-}
  iterF (s :. sIxL) (e :. eIxL) (inc :. incIxL) cond initAct f =
    loopF s (`cond` e) (+ inc) initAct $ \ !i g ->
      loopF sIxL (`cond` eIxL) (+ incIxL) g $ \ !j -> f (i :. j)
  {-# INLINE iterF #-}


instance {-# OVERLAPPING #-} Index (IxN 3) where
  type Dimensions Ix3 = 3
  dimensions _ = 3
  {-# INLINE [1] dimensions #-}
  totalElem (SafeSz (k3 :> k2 :. k1)) = k3 * k2 * k1
  {-# INLINE [1] totalElem #-}
  isSafeIndex (SafeSz (k3 :> k2 :. k1)) (i3 :> i2 :. i1) =
    0 <= i3 && 0 <= i2 && 0 <= i1 && i3 < k3 && i2 < k2 && i1 < k1
  {-# INLINE [1] isSafeIndex #-}
  toLinearIndex (SafeSz (_ :> k2 :. k1)) (i3 :> i2 :. i1) = (i3 * k2 + i2) * k1 + i1
  {-# INLINE [1] toLinearIndex #-}
  fromLinearIndex (SafeSz (_ :> ix)) i = let !(q, ixL) = fromLinearIndexAcc ix i in q :> ixL
  {-# INLINE fromLinearIndex #-}
  fromLinearIndexAcc (m :> ix) !k = (q, r :> ixL)
    where
      !(!kL, !ixL) = fromLinearIndexAcc ix k
      !(!q, !r) = quotRem kL m
  {-# INLINE fromLinearIndexAcc #-}
  consDim = (:>)
  {-# INLINE [1] consDim #-}
  unconsDim (i3 :> ix) = (i3, ix)
  {-# INLINE [1] unconsDim #-}
  snocDim (i3 :. i2) i1 = i3 :> i2 :. i1
  {-# INLINE [1] snocDim #-}
  unsnocDim (i3 :> i2 :. i1) = (i3 :. i2, i1)
  {-# INLINE [1] unsnocDim #-}
  getDimM (i3 :>  _ :.  _) 3 = pure i3
  getDimM ( _ :> i2 :.  _) 2 = pure i2
  getDimM ( _ :>  _ :. i1) 1 = pure i1
  getDimM ix               d = throwM $ IndexDimensionException ix d
  {-# INLINE [1] getDimM #-}
  setDimM ( _ :> i2 :. i1) 3 i3 = pure (i3 :> i2 :. i1)
  setDimM (i3 :>  _ :. i1) 2 i2 = pure (i3 :> i2 :. i1)
  setDimM (i3 :> i2 :.  _) 1 i1 = pure (i3 :> i2 :. i1)
  setDimM ix               d _  = throwM $ IndexDimensionException ix d
  {-# INLINE [1] setDimM #-}
  pullOutDimM (i3 :> i2 :. i1) 3 = pure (i3, i2 :. i1)
  pullOutDimM (i3 :> i2 :. i1) 2 = pure (i2, i3 :. i1)
  pullOutDimM (i3 :> i2 :. i1) 1 = pure (i1, i3 :. i2)
  pullOutDimM ix               d = throwM $ IndexDimensionException ix d
  {-# INLINE [1] pullOutDimM #-}
  insertDimM (i2 :. i1) 3 i3 = pure (i3 :> i2 :. i1)
  insertDimM (i3 :. i1) 2 i2 = pure (i3 :> i2 :. i1)
  insertDimM (i3 :. i2) 1 i1 = pure (i3 :> i2 :. i1)
  insertDimM ix         d  _ = throwM $ IndexDimensionException ix d
  {-# INLINE [1] insertDimM #-}
  pureIndex i = i :> i :. i
  {-# INLINE [1] pureIndex #-}
  liftIndex f (i3 :> i2 :. i1) = f i3 :> f i2 :. f i1
  {-# INLINE [1] liftIndex #-}
  liftIndex2 f (i3 :> i2 :. i1) (i3' :> i2' :. i1') = f i3 i3' :> f i2 i2' :. f i1 i1'
  {-# INLINE [1] liftIndex2 #-}
  repairIndex (SafeSz (n :> szL)) (i :> ixL) rBelow rOver =
    repairIndex (SafeSz n) i rBelow rOver :> repairIndex (SafeSz szL) ixL rBelow rOver
  {-# INLINE [1] repairIndex #-}
  iterTargetRowMajorAccM iAcc iStart sz (b3 :> b2 :. b1) (s3 :> s2 :. s1) initAcc action =
    let n = totalElem sz
        iShift = iStart + iAcc * n
     in loopM 0 (< n) (+ 1) initAcc $ \ !i !acc ->
          let (i3 :> i2 :. i1) = fromLinearIndex sz i
          in action (iShift + i) ((b3 + s3 * i3) :> (b2 + s2 * i2) :. (b1 + s1 * i1)) acc
  {-# INLINE iterTargetRowMajorAccM #-}
  iterTargetRowMajorAccST_ iAcc fact scheduler iStart sz (b3 :> b2 :. b1) (s3 :> s2 :. s1) acc splitAcc action =
    let n = totalElem sz
        iShift = iStart + iAcc * n
     in iterLinearAccST_ fact scheduler 0 1 n acc splitAcc $ \ !i ->
          let (i3 :> i2 :. i1) = fromLinearIndex sz i
          in action (iShift + i) ((b3 + s3 * i3) :> (b2 + s2 * i2) :. (b1 + s1 * i1))
  {-# INLINE iterTargetRowMajorAccST_ #-}
  iterTargetRowMajorAccST iAcc fact scheduler iStart sz (b3 :> b2 :. b1) (s3 :> s2 :. s1) acc splitAcc action =
    let n = totalElem sz
        iShift = iStart + iAcc * n
     in iterLinearAccST fact scheduler 0 1 n acc splitAcc $ \ !i ->
          let (i3 :> i2 :. i1) = fromLinearIndex sz i
          in action (iShift + i) ((b3 + s3 * i3) :> (b2 + s2 * i2) :. (b1 + s1 * i1))
  {-# INLINE iterTargetRowMajorAccST #-}

-- | Constraint synonym that encapsulates all constraints needed for dimension 4 and higher.
--
-- @since 1.0.0
type HighIxN n
   = (4 <= n, KnownNat n, KnownNat (n - 1), Index (IxN (n - 1)), IxN (n - 1) ~ Ix (n - 1))

instance {-# OVERLAPPABLE #-} HighIxN n => Index (IxN n) where
  type Dimensions (IxN n) = n
  dimensions _ = fromInteger $ natVal (Proxy :: Proxy n)
  {-# INLINE [1] dimensions #-}
  totalElem (SafeSz (i :> ixl)) = foldlIndex (*) i ixl
  {-# INLINE [1] totalElem #-}
  consDim = (:>)
  {-# INLINE [1] consDim #-}
  unconsDim (i :> ixl) = (i, ixl)
  {-# INLINE [1] unconsDim #-}
  snocDim (i :> ixl) i1 = i :> snocDim ixl i1
  {-# INLINE [1] snocDim #-}
  unsnocDim (i :> ixl) =
    case unsnocDim ixl of
      (ix, i1) -> (i :> ix, i1)
  {-# INLINE [1] unsnocDim #-}
  getDimM ix@(i :> ixl) d
    | d == dimensions (Proxy :: Proxy (IxN n)) = pure i
    | otherwise = maybe (throwM $ IndexDimensionException ix d) pure (getDimM ixl d)
  {-# INLINE [1] getDimM #-}
  setDimM ix@(i :> ixl) d di
    | d == dimensions (Proxy :: Proxy (IxN n)) = pure (di :> ixl)
    | otherwise = maybe (throwM $ IndexDimensionException ix d) (pure . (i :>)) (setDimM ixl d di)
  {-# INLINE [1] setDimM #-}
  pullOutDimM ix@(i :> ixl) d
    | d == dimensions (Proxy :: Proxy (IxN n)) = pure (i, ixl)
    | otherwise =
      maybe (throwM $ IndexDimensionException ix d) (pure . fmap (i :>)) (pullOutDimM ixl d)
  {-# INLINE [1] pullOutDimM #-}
  insertDimM ix@(i :> ixl) d di
    | d == dimensions (Proxy :: Proxy (IxN n)) = pure (di :> ix)
    | otherwise =
      maybe (throwM $ IndexDimensionException ix d) (pure . (i :>)) (insertDimM ixl d di)
  {-# INLINE [1] insertDimM #-}
  pureIndex i = i :> (pureIndex i :: Ix (n - 1))
  {-# INLINE [1] pureIndex #-}
  liftIndex f (i :> ix) = f i :> liftIndex f ix
  {-# INLINE [1] liftIndex #-}
  liftIndex2 f (i :> ix) (i' :> ix') = f i i' :> liftIndex2 f ix ix'
  {-# INLINE [1] liftIndex2 #-}
  repairIndex (SafeSz (n :> szL)) (i :> ixL) rBelow rOver =
    repairIndex (SafeSz n) i rBelow rOver :> repairIndex (SafeSz szL) ixL rBelow rOver
  {-# INLINE [1] repairIndex #-}



---- Unbox Ix

-- | Unboxing of a `Ix2`.
instance VU.Unbox Ix2

newtype instance VU.MVector s Ix2 = MV_Ix2 (VU.MVector s (Int, Int))

instance VM.MVector VU.MVector Ix2 where
  basicLength (MV_Ix2 mvec) = VM.basicLength mvec
  {-# INLINE basicLength #-}
  basicUnsafeSlice idx len (MV_Ix2 mvec) = MV_Ix2 (VM.basicUnsafeSlice idx len mvec)
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_Ix2 mvec) (MV_Ix2 mvec') = VM.basicOverlaps mvec mvec'
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew len = MV_Ix2 <$> VM.basicUnsafeNew len
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeReplicate len (i :. j) = MV_Ix2 <$> VM.basicUnsafeReplicate len (i, j)
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_Ix2 mvec) idx = uncurry (:.) <$> VM.basicUnsafeRead mvec idx
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_Ix2 mvec) idx (i :. j) = VM.basicUnsafeWrite mvec idx (i, j)
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_Ix2 mvec) = VM.basicClear mvec
  {-# INLINE basicClear #-}
  basicSet (MV_Ix2 mvec) (i :. j) = VM.basicSet mvec (i, j)
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV_Ix2 mvec) (MV_Ix2 mvec') = VM.basicUnsafeCopy mvec mvec'
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_Ix2 mvec) (MV_Ix2 mvec') = VM.basicUnsafeMove mvec mvec'
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_Ix2 mvec) len = MV_Ix2 <$> VM.basicUnsafeGrow mvec len
  {-# INLINE basicUnsafeGrow #-}
#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_Ix2 mvec) = VM.basicInitialize mvec
  {-# INLINE basicInitialize #-}
#endif


newtype instance VU.Vector Ix2 = V_Ix2 (VU.Vector (Int, Int))

instance V.Vector VU.Vector Ix2 where
  basicUnsafeFreeze (MV_Ix2 mvec) = V_Ix2 <$> V.basicUnsafeFreeze mvec
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_Ix2 vec) = MV_Ix2 <$> V.basicUnsafeThaw vec
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_Ix2 vec) = V.basicLength vec
  {-# INLINE basicLength #-}
  basicUnsafeSlice idx len (V_Ix2 vec) = V_Ix2 (V.basicUnsafeSlice idx len vec)
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_Ix2 vec) idx = uncurry (:.) <$> V.basicUnsafeIndexM vec idx
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_Ix2 mvec) (V_Ix2 vec) = V.basicUnsafeCopy mvec vec
  {-# INLINE basicUnsafeCopy #-}
  elemseq _ = seq
  {-# INLINE elemseq #-}



---- Unbox Ix



-- | Unboxing of a `IxN`.
instance (3 <= n, VU.Unbox (Ix (n - 1))) => VU.Unbox (IxN n)

newtype instance VU.MVector s (IxN n) = MV_IxN (VU.MVector s Int, VU.MVector s (Ix (n-1)))

instance (3 <= n, VU.Unbox (Ix (n - 1))) => VM.MVector VU.MVector (IxN n) where
  basicLength (MV_IxN (_, mvec)) = VM.basicLength mvec
  {-# INLINE basicLength #-}
  basicUnsafeSlice idx len (MV_IxN (mvec1, mvec)) =
    MV_IxN (VM.basicUnsafeSlice idx len mvec1, VM.basicUnsafeSlice idx len mvec)
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_IxN (mvec1, mvec)) (MV_IxN (mvec1', mvec')) =
    VM.basicOverlaps mvec1 mvec1' && VM.basicOverlaps mvec mvec'
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew len = do
    iv <- VM.basicUnsafeNew len
    ivs <- VM.basicUnsafeNew len
    return $ MV_IxN (iv, ivs)
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeReplicate len (i :> ix) = do
    iv <- VM.basicUnsafeReplicate len i
    ivs <- VM.basicUnsafeReplicate len ix
    return $ MV_IxN (iv, ivs)
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_IxN (mvec1, mvec)) idx = do
    i <- VM.basicUnsafeRead mvec1 idx
    ix <- VM.basicUnsafeRead mvec idx
    return (i :> ix)
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_IxN (mvec1, mvec)) idx (i :> ix) = do
    VM.basicUnsafeWrite mvec1 idx i
    VM.basicUnsafeWrite mvec idx ix
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_IxN (mvec1, mvec)) = VM.basicClear mvec1 >> VM.basicClear mvec
  {-# INLINE basicClear #-}
  basicSet (MV_IxN (mvec1, mvec)) (i :> ix) = VM.basicSet mvec1 i >> VM.basicSet mvec ix
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV_IxN (mvec1, mvec)) (MV_IxN (mvec1', mvec')) =
    VM.basicUnsafeCopy mvec1 mvec1' >> VM.basicUnsafeCopy mvec mvec'
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_IxN (mvec1, mvec)) (MV_IxN (mvec1', mvec')) =
    VM.basicUnsafeMove mvec1 mvec1' >> VM.basicUnsafeMove mvec mvec'
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_IxN (mvec1, mvec)) len = do
    iv <- VM.basicUnsafeGrow mvec1 len
    ivs <- VM.basicUnsafeGrow mvec len
    return $ MV_IxN (iv, ivs)
  {-# INLINE basicUnsafeGrow #-}
#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_IxN (mvec1, mvec)) =
    VM.basicInitialize mvec1 >> VM.basicInitialize mvec
  {-# INLINE basicInitialize #-}
#endif


newtype instance VU.Vector (IxN n) = V_IxN (VU.Vector Int, VU.Vector (Ix (n-1)))

instance (3 <= n, VU.Unbox (Ix (n - 1))) => V.Vector VU.Vector (IxN n) where
  basicUnsafeFreeze (MV_IxN (mvec1, mvec)) = do
    iv <- V.basicUnsafeFreeze mvec1
    ivs <- V.basicUnsafeFreeze mvec
    return $ V_IxN (iv, ivs)
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_IxN (vec1, vec)) = do
    imv <- V.basicUnsafeThaw vec1
    imvs <- V.basicUnsafeThaw vec
    return $ MV_IxN (imv, imvs)
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_IxN (_, vec)) = V.basicLength vec
  {-# INLINE basicLength #-}
  basicUnsafeSlice idx len (V_IxN (vec1, vec)) =
    V_IxN (V.basicUnsafeSlice idx len vec1, V.basicUnsafeSlice idx len vec)
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_IxN (vec1, vec)) idx = do
    i <- V.basicUnsafeIndexM vec1 idx
    ix <- V.basicUnsafeIndexM vec idx
    return (i :> ix)
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_IxN (mvec1, mvec)) (V_IxN (vec1, vec)) =
    V.basicUnsafeCopy mvec1 vec1 >> V.basicUnsafeCopy mvec vec
  {-# INLINE basicUnsafeCopy #-}
  elemseq _ = seq
  {-# INLINE elemseq #-}
