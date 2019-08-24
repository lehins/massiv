{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE TypeFamilyDependencies #-}
-- |
-- Module      : Data.Massiv.Core.Index.Ix
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
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
  , type Sz1
  , pattern Sz1
  , type Ix2(Ix2, (:.))
  , type Sz2
  , pattern Sz2
  , type Ix3
  , pattern Ix3
  , type Sz3
  , pattern Sz3
  , type Ix4
  , pattern Ix4
  , type Sz4
  , pattern Sz4
  , type Ix5
  , pattern Ix5
  , type Sz5
  , pattern Sz5
  ) where

import           Control.Monad.Catch             (MonadThrow(..))
import           Control.DeepSeq
import           Control.Monad                   (liftM)
import           Data.Massiv.Core.Index.Internal
import           Data.Monoid                     ((<>))
import           Data.Proxy
import qualified Data.Vector.Generic             as V
import qualified Data.Vector.Generic.Mutable     as VM
import qualified Data.Vector.Unboxed             as VU
import           GHC.TypeLits


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

-- | 2-dimensional size type synonym.
--
-- @since 0.3.0
type Sz2 = Sz Ix2

-- | 2-dimensional size constructor. @(Sz2 i j) == Sz (i :. j)@
--
-- @since 0.3.0
pattern Sz2 :: Int -> Int -> Sz2
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

-- | 3-dimensional size type synonym.
--
-- @since 0.3.0
type Sz3 = Sz Ix3

-- | 3-dimensional size constructor. @(Sz3 i j k) == Sz (i :> j :. k)@
--
-- @since 0.3.0
pattern Sz3 :: Int -> Int -> Int -> Sz3
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

-- | 4-dimensional size type synonym.
--
-- @since 0.3.0
type Sz4 = Sz Ix4

-- | 4-dimensional size constructor. @(Sz4 i j k l) == Sz (i :> j :> k :. l)@
--
-- @since 0.3.0
pattern Sz4 :: Int -> Int -> Int -> Int -> Sz4
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

-- | 5-dimensional size type synonym.
--
-- @since 0.3.0
type Sz5 = Sz Ix5

-- | 5-dimensional size constructor.  @(Sz5 i j k l m) == Sz (i :> j :> k :> l :. m)@
--
-- @since 0.3.0
pattern Sz5 :: Int -> Int -> Int -> Int -> Int -> Sz5
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


instance {-# OVERLAPPABLE #-} ( 1 <= n
                              , 4 <= n
                              , KnownNat n
                              , KnownNat (n - 1)
                              , Index (Ix (n - 1))
                              , IxN (n - 1) ~ Ix (n - 1)
                              ) =>
                              Num (IxN n) where
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

instance {-# OVERLAPPABLE #-} ( 1 <= n
                              , 4 <= n
                              , KnownNat n
                              , KnownNat (n - 1)
                              , Index (Ix (n - 1))
                              , IxN (n - 1) ~ Ix (n - 1)
                              ) =>
                              Bounded (IxN n) where
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
  fromLinearIndex (SafeSz (_ :. k1)) i = case i `quotRem` k1 of
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
  getDimM (i2 :.  _) 2 = pure i2
  getDimM ( _ :. i1) 1 = pure i1
  getDimM ix         d = throwM $ IndexDimensionException ix d
  {-# INLINE [1] getDimM #-}
  setDimM ( _ :. i1) 2 i2 = pure (i2 :. i1)
  setDimM (i2 :.  _) 1 i1 = pure (i2 :. i1)
  setDimM ix         d _  = throwM $ IndexDimensionException ix d
  {-# INLINE [1] setDimM #-}
  modifyDimM (i2 :. i1) 2 f = pure (i2, f i2 :.   i1)
  modifyDimM (i2 :. i1) 1 f = pure (i1,   i2 :. f i1)
  modifyDimM ix         d _  = throwM $ IndexDimensionException ix d
  {-# INLINE [1] modifyDimM #-}
  pullOutDimM (i2 :. i1) 2 = pure (i2, i1)
  pullOutDimM (i2 :. i1) 1 = pure (i1, i2)
  pullOutDimM ix         d = throwM $ IndexDimensionException ix d
  {-# INLINE [1] pullOutDimM #-}
  insertDimM i1 2 i2 = pure (i2 :. i1)
  insertDimM i2 1 i1 = pure (i2 :. i1)
  insertDimM ix d  _ = throwM $ IndexDimensionException ix d
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


instance {-# OVERLAPPING #-} Index (IxN 3) where
  type Dimensions Ix3 = 3
  dimensions _ = 3
  {-# INLINE [1] dimensions #-}
  totalElem (SafeSz (k3 :> k2 :. k1)) = k3 * k2 * k1
  {-# INLINE [1] totalElem #-}
  isSafeIndex (SafeSz (k3 :> k2 :. k1)) (i3 :> i2 :. i1) =
    0 <= i3 && 0 <= i2 && 0 <= i1 && i3 < k3 && i2 < k2 && i1 < k1
  {-# INLINE [1] isSafeIndex #-}
  toLinearIndex (SafeSz (_ :> k2 :. k1)) (i3 :> i2 :. i1) = (k2 * i3 + i2) * k1 + i1
  {-# INLINE [1] toLinearIndex #-}
  fromLinearIndex (SafeSz (_ :> ix)) i = let !(q, ixL) = fromLinearIndexAcc ix i in q :> ixL
  {-# INLINE [1] fromLinearIndex #-}
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
  modifyDimM (i3 :> i2 :. i1) 3 f = pure (i3, f i3 :>   i2 :.   i1)
  modifyDimM (i3 :> i2 :. i1) 2 f = pure (i2,   i3 :> f i2 :.   i1)
  modifyDimM (i3 :> i2 :. i1) 1 f = pure (i1,   i3 :>   i2 :. f i1)
  modifyDimM ix               d _  = throwM $ IndexDimensionException ix d
  {-# INLINE [1] modifyDimM #-}
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

instance {-# OVERLAPPABLE #-} ( 1 <= n
                              , 4 <= n
                              , KnownNat n
                              , KnownNat (n - 1)
                              , Index (Ix (n - 1))
                              , IxN (n - 1) ~ Ix (n - 1)
                              ) =>
                              Index (IxN n) where
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
  modifyDimM ix@(i :> ixl) d f
    | d == dimensions (Proxy :: Proxy (IxN n)) = pure (i, f i :> ixl)
    | otherwise =
      case modifyDimM ixl d f of
        Just (di, ixl') -> pure (di, i :> ixl')
        Nothing -> throwM $ IndexDimensionException ix d
  {-# INLINE [1] modifyDimM #-}
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
  basicUnsafeNew len = MV_Ix2 `liftM` VM.basicUnsafeNew len
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeReplicate len (i :. j) = MV_Ix2 `liftM` VM.basicUnsafeReplicate len (i, j)
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_Ix2 mvec) idx = uncurry (:.) `liftM` VM.basicUnsafeRead mvec idx
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
  basicUnsafeGrow (MV_Ix2 mvec) len = MV_Ix2 `liftM` VM.basicUnsafeGrow mvec len
  {-# INLINE basicUnsafeGrow #-}
#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_Ix2 mvec) = VM.basicInitialize mvec
  {-# INLINE basicInitialize #-}
#endif


newtype instance VU.Vector Ix2 = V_Ix2 (VU.Vector (Int, Int))

instance V.Vector VU.Vector Ix2 where
  basicUnsafeFreeze (MV_Ix2 mvec) = V_Ix2 `liftM` V.basicUnsafeFreeze mvec
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_Ix2 vec) = MV_Ix2 `liftM` V.basicUnsafeThaw vec
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_Ix2 vec) = V.basicLength vec
  {-# INLINE basicLength #-}
  basicUnsafeSlice idx len (V_Ix2 vec) = V_Ix2 (V.basicUnsafeSlice idx len vec)
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_Ix2 vec) idx = uncurry (:.) `liftM` V.basicUnsafeIndexM vec idx
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_Ix2 mvec) (V_Ix2 vec) = V.basicUnsafeCopy mvec vec
  {-# INLINE basicUnsafeCopy #-}
  elemseq _ = seq
  {-# INLINE elemseq #-}



---- Unbox Ix



-- | Unboxing of a `IxN`.
instance (3 <= n, VU.Unbox (Ix (n - 1))) => VU.Unbox (IxN n)

newtype instance VU.MVector s (IxN n) = MV_IxN (VU.MVector s Int, VU.MVector s (Ix (n - 1)))

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


newtype instance VU.Vector (IxN n) = V_IxN (VU.Vector Int, VU.Vector (Ix (n - 1)))

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
