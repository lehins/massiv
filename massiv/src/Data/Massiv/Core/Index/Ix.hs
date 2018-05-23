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

#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TypeFamilyDependencies #-}
#else
{-# LANGUAGE GADTs                  #-}
#endif
-- |
-- Module      : Data.Massiv.Core.Index.Ix
-- Copyright   : (c) Alexey Kuleshevich 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Core.Index.Ix where

import           Control.DeepSeq
import           Control.Monad                (liftM)
import           Data.Massiv.Core.Index.Class
import           Data.Monoid                  ((<>))
import           Data.Proxy
import qualified Data.Vector.Generic          as V
import qualified Data.Vector.Generic.Mutable  as VM
import qualified Data.Vector.Unboxed          as VU
import           GHC.TypeLits


infixr 5 :>, :.

-- | Another type synonym for 1-dimensional index, i.e. `Int` and `Ix1T`. Provided here purely for
-- consistency.
type Ix1 = Int

-- | This is a very handy pattern synonym to indicate that any arbitrary whole number is an `Int`,
-- i.e. a 1-dimensional index: @(Ix1 i) == (i :: Int)@
pattern Ix1 :: Int -> Ix1
pattern Ix1 i = i

-- | 2-dimensional index. This also a base index for higher dimensions.
data Ix2 = (:.) {-# UNPACK #-} !Int {-# UNPACK #-} !Int

-- | 2-dimensional index constructor. Useful when @TypeOperators@ extension isn't enabled, or simply
-- infix notation is inconvenient. @(Ix2 i j) == (i :. j)@.
pattern Ix2 :: Int -> Int -> Ix2
pattern Ix2 i j = i :. j

-- | 3-dimensional type synonym. Useful as a alternative to enabling @DataKinds@ and using type
-- level Nats.
type Ix3 = IxN 3

-- | 3-dimensional index constructor. @(Ix3 i j k) == (i :> j :. k)@.
pattern Ix3 :: Int -> Int -> Int -> Ix3
pattern Ix3 i j k = i :> j :. k

-- | 4-dimensional type synonym.
type Ix4 = IxN 4
-- | 4-dimensional index constructor. @(Ix4 i j k l) == (i :> j :> k :. l)@.
pattern Ix4 :: Int -> Int -> Int -> Int -> Ix4
pattern Ix4 i j k l = i :> j :> k :. l

-- | 5-dimensional type synonym.
type Ix5 = IxN 5
-- | 5-dimensional index constructor.  @(Ix5 i j k l m) = (i :> j :> k :> l :. m)@.
pattern Ix5 :: Int -> Int -> Int -> Int -> Int -> Ix5
pattern Ix5 i j k l m = i :> j :> k :> l :. m


#if __GLASGOW_HASKELL__ >= 800

-- | n-dimensional index. Needs a base case, which is the `Ix2`.
data IxN (n :: Nat) = (:>) {-# UNPACK #-} !Int !(Ix (n - 1))

-- | Defines n-dimensional index by relating a general `IxN` with few base cases.
type family Ix (n :: Nat) = r | r -> n where
  Ix 0 = Ix0
  Ix 1 = Ix1
  Ix 2 = Ix2
  Ix n = IxN n

#else

data IxN (n :: Nat) where
  (:>) :: Rank (Ix (n - 1)) ~ (n - 1) => {-# UNPACK #-} !Int -> !(Ix (n - 1)) -> IxN n

type family Ix (n :: Nat) where
  Ix 0 = Ix0
  Ix 1 = Ix1
  Ix 2 = Ix2
  Ix n = IxN n

#endif


type instance Lower Ix2 = Ix1
type instance Lower (IxN n) = Ix (n - 1)


instance Show Ix2 where
  show (i :. j)  = show i ++ " :. " ++ show j

instance Show (Ix (n - 1)) => Show (IxN n) where
  show (i :> ix) = show i ++ " :> " ++ show ix


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


instance {-# OVERLAPPABLE #-} (4 <= n,
          KnownNat n,
          Index (Ix (n - 1)),
#if __GLASGOW_HASKELL__ < 800
          Rank (Ix ((n - 1) - 1)) ~ ((n - 1) - 1),
#endif
          IxN (n - 1) ~ Ix (n - 1)
          ) => Num (IxN n) where
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

instance {-# OVERLAPPABLE #-} (4 <= n,
          KnownNat n,
          Index (Ix (n - 1)),
#if __GLASGOW_HASKELL__ < 800
          Rank (Ix ((n - 1) - 1)) ~ ((n - 1) - 1),
#endif
          IxN (n - 1) ~ Ix (n - 1)
          ) => Bounded (IxN n) where
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

-- | Convert a `Int` tuple to `Ix2`
toIx2 :: Ix2T -> Ix2
toIx2 (i, j) = i :. j
{-# INLINE toIx2 #-}

-- | Convert an `Ix2` to `Int` tuple
fromIx2 :: Ix2 -> Ix2T
fromIx2 (i :. j) = (i, j)
{-# INLINE fromIx2 #-}

-- | Convert a `Int` 3-tuple to `Ix3`
toIx3 :: Ix3T -> Ix3
toIx3 (i, j, k) = i :> j :. k
{-# INLINE toIx3 #-}

-- | Convert an `Ix3` to `Int` 3-tuple
fromIx3 :: Ix3 -> Ix3T
fromIx3 (i :> j :. k) = (i, j, k)
{-# INLINE fromIx3 #-}

-- | Convert a `Int` 4-tuple to `Ix4`
toIx4 :: Ix4T -> Ix4
toIx4 (i, j, k, l) = i :> j :> k :. l
{-# INLINE toIx4 #-}

-- | Convert an `Ix4` to `Int` 4-tuple
fromIx4 :: Ix4 -> Ix4T
fromIx4 (i :> j :> k :. l) = (i, j, k, l)
{-# INLINE fromIx4 #-}

-- | Convert a `Int` 5-tuple to `Ix5`
toIx5 :: Ix5T -> Ix5
toIx5 (i, j, k, l, m) = i :> j :> k :> l :. m
{-# INLINE toIx5 #-}

-- | Convert an `Ix5` to `Int` 5-tuple
fromIx5 :: Ix5 -> Ix5T
fromIx5 (i :> j :> k :> l :. m) = (i, j, k, l, m)
{-# INLINE fromIx5 #-}


instance {-# OVERLAPPING #-} Index Ix2 where
  type Rank Ix2 = 2
  rank _ = 2
  {-# INLINE [1] rank #-}
  totalElem (m :. n) = m * n
  {-# INLINE [1] totalElem #-}
  isSafeIndex (m :. n) (i :. j) = 0 <= i && 0 <= j && i < m && j < n
  {-# INLINE [1] isSafeIndex #-}
  toLinearIndex (_ :. n) (i :. j) = n * i + j
  {-# INLINE [1] toLinearIndex #-}
  fromLinearIndex (_ :. n) k = case k `quotRem` n of
                                 (i, j) -> i :. j
  {-# INLINE [1] fromLinearIndex #-}
  consDim = (:.)
  {-# INLINE [1] consDim #-}
  unconsDim (i :. ix) = (i, ix)
  {-# INLINE [1] unconsDim #-}
  snocDim i j = i :. j
  {-# INLINE [1] snocDim #-}
  unsnocDim (i :. j) = (i, j)
  {-# INLINE [1] unsnocDim #-}
  getIndex (i :. _) 2 = Just i
  getIndex (_ :. j) 1 = Just j
  getIndex _        _ = Nothing
  {-# INLINE [1] getIndex #-}
  setIndex (_ :. j) 2 i = Just (i :. j)
  setIndex (i :. _) 1 j = Just (i :. j)
  setIndex _        _ _ = Nothing
  {-# INLINE [1] setIndex #-}
  dropDim (_ :. j) 2 = Just j
  dropDim (i :. _) 1 = Just i
  dropDim _      _   = Nothing
  {-# INLINE [1] dropDim #-}
  pureIndex i = i :. i
  {-# INLINE [1] pureIndex #-}
  liftIndex f (i :. j) = f i :. f j
  {-# INLINE [1] liftIndex #-}
  liftIndex2 f (i0 :. j0) (i1 :. j1) = f i0 i1 :. f j0 j1
  {-# INLINE [1] liftIndex2 #-}
  repairIndex (n :. szL) (i :. ixL) rBelow rOver =
    repairIndex n i rBelow rOver :. repairIndex szL ixL rBelow rOver
  {-# INLINE [1] repairIndex #-}


instance {-# OVERLAPPING #-} Index (IxN 3) where
  type Rank Ix3 = 3
  rank _ = 3
  {-# INLINE [1] rank #-}
  totalElem (m :> n :. o) = m * n * o
  {-# INLINE [1] totalElem #-}
  isSafeIndex (m :> n :. o) (i :> j :. k) =
    0 <= i && 0 <= j && 0 <= k && i < m && j < n && k < o
  {-# INLINE [1] isSafeIndex #-}
  toLinearIndex (_ :> n :. o) (i :> j :. k) = (n * i + j) * o + k
  {-# INLINE [1] toLinearIndex #-}
  fromLinearIndex (_ :> ix) k = let !(q, ixL) = fromLinearIndexAcc ix k in q :> ixL
  {-# INLINE [1] fromLinearIndex #-}
  consDim = (:>)
  {-# INLINE [1] consDim #-}
  unconsDim (i :> ix) = (i, ix)
  {-# INLINE [1] unconsDim #-}
  snocDim (i :. j) k = i :> j :. k
  {-# INLINE [1] snocDim #-}
  unsnocDim (i :> j :. k) = (i :. j, k)
  {-# INLINE [1] unsnocDim #-}
  getIndex (i :> _ :. _) 3 = Just i
  getIndex (_ :> j :. _) 2 = Just j
  getIndex (_ :> _ :. k) 1 = Just k
  getIndex _             _ = Nothing
  {-# INLINE [1] getIndex #-}
  setIndex (_ :> j :. k) 3 i = Just (i :> j :. k)
  setIndex (i :> _ :. k) 2 j = Just (i :> j :. k)
  setIndex (i :> j :. _) 1 k = Just (i :> j :. k)
  setIndex _             _ _ = Nothing
  {-# INLINE [1] setIndex #-}
  dropDim (_ :> j :. k) 3 = Just (j :. k)
  dropDim (i :> _ :. k) 2 = Just (i :. k)
  dropDim (i :> j :. _) 1 = Just (i :. j)
  dropDim _             _ = Nothing
  {-# INLINE [1] dropDim #-}
  pureIndex i = i :> i :. i
  {-# INLINE [1] pureIndex #-}
  liftIndex f (i :> j :. k) = f i :> f j :. f k
  {-# INLINE [1] liftIndex #-}
  liftIndex2 f (i0 :> j0 :. k0) (i1 :> j1 :. k1) = f i0 i1 :> f j0 j1 :. f k0 k1
  {-# INLINE [1] liftIndex2 #-}
  repairIndex (n :> szL) (i :> ixL) rBelow rOver =
    repairIndex n i rBelow rOver :> repairIndex szL ixL rBelow rOver
  {-# INLINE [1] repairIndex #-}

instance {-# OVERLAPPABLE #-} (4 <= n,
          KnownNat n,
          Index (Ix (n - 1)),
#if __GLASGOW_HASKELL__ < 800
          Rank (Ix ((n - 1) - 1)) ~ ((n - 1) - 1),
#endif
          IxN (n - 1) ~ Ix (n - 1)
          ) => Index (IxN n) where
  type Rank (IxN n) = n
  rank _ = fromInteger $ natVal (Proxy :: Proxy n)
  {-# INLINE [1] rank #-}
  totalElem (i :> ix) = i * totalElem ix
  {-# INLINE [1] totalElem #-}
  consDim = (:>)
  {-# INLINE [1] consDim #-}
  unconsDim (i :> ix) = (i, ix)
  {-# INLINE [1] unconsDim #-}
  snocDim (i :> ix) k = i :> snocDim ix k
  {-# INLINE [1] snocDim #-}
  unsnocDim (i :> ix) = case unsnocDim ix of
                          (jx, j) -> (i :> jx, j)
  {-# INLINE [1] unsnocDim #-}
  getIndex ix@(j :> jx) k | k == rank ix = Just j
                          | otherwise = getIndex jx k
  {-# INLINE [1] getIndex #-}
  setIndex ix@(j :> jx) k o | k == rank ix = Just (o :> jx)
                            | otherwise = (j :>) <$> setIndex jx k o
  {-# INLINE [1] setIndex #-}
  dropDim ix@(j :> jx) k | k == rank ix = Just jx
                           | otherwise = (j :>) <$> dropDim jx k
  {-# INLINE [1] dropDim #-}
  pureIndex i = i :> (pureIndex i :: Ix (n - 1))
  {-# INLINE [1] pureIndex #-}
  liftIndex f (i :> ix) = f i :> liftIndex f ix
  {-# INLINE [1] liftIndex #-}
  liftIndex2 f (i1 :> ix1) (i2 :> ix2) = f i1 i2 :> liftIndex2 f ix1 ix2
  {-# INLINE [1] liftIndex2 #-}
  repairIndex (n :> szL) (i :> ixL) rBelow rOver =
    repairIndex n i rBelow rOver :> repairIndex szL ixL rBelow rOver
  {-# INLINE [1] repairIndex #-}



---- Unbox Ix

-- | Unboxing of a `Ix2`.
instance VU.Unbox Ix2

newtype instance VU.MVector s Ix2 = MV_Ix2 (VU.MVector s Ix2T)

instance VM.MVector VU.MVector Ix2 where
  basicLength (MV_Ix2 mvec) = VM.basicLength mvec
  {-# INLINE basicLength #-}
  basicUnsafeSlice idx len (MV_Ix2 mvec) = MV_Ix2 (VM.basicUnsafeSlice idx len mvec)
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_Ix2 mvec) (MV_Ix2 mvec') = VM.basicOverlaps mvec mvec'
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew len = MV_Ix2 `liftM` VM.basicUnsafeNew len
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeReplicate len val = MV_Ix2 `liftM` VM.basicUnsafeReplicate len (fromIx2 val)
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_Ix2 mvec) idx = toIx2 `liftM` VM.basicUnsafeRead mvec idx
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_Ix2 mvec) idx val = VM.basicUnsafeWrite mvec idx (fromIx2 val)
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_Ix2 mvec) = VM.basicClear mvec
  {-# INLINE basicClear #-}
  basicSet (MV_Ix2 mvec) val = VM.basicSet mvec (fromIx2 val)
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


newtype instance VU.Vector Ix2 = V_Ix2 (VU.Vector Ix2T)

instance V.Vector VU.Vector Ix2 where
  basicUnsafeFreeze (MV_Ix2 mvec) = V_Ix2 `liftM` V.basicUnsafeFreeze mvec
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_Ix2 vec) = MV_Ix2 `liftM` V.basicUnsafeThaw vec
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_Ix2 vec) = V.basicLength vec
  {-# INLINE basicLength #-}
  basicUnsafeSlice idx len (V_Ix2 vec) = V_Ix2 (V.basicUnsafeSlice idx len vec)
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_Ix2 vec) idx = toIx2 `liftM` V.basicUnsafeIndexM vec idx
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_Ix2 mvec) (V_Ix2 vec) = V.basicUnsafeCopy mvec vec
  {-# INLINE basicUnsafeCopy #-}
  elemseq _ = seq
  {-# INLINE elemseq #-}



---- Unbox Ix



-- | Unboxing of a `IxN`.
instance (3 <= n,
#if __GLASGOW_HASKELL__ < 800
          Rank (Ix (n - 1)) ~ (n - 1),
#endif
          VU.Unbox (Ix (n-1))) => VU.Unbox (IxN n)

newtype instance VU.MVector s (IxN n) = MV_IxN (VU.MVector s Int, VU.MVector s (Ix (n-1)))

instance (3 <= n,
#if __GLASGOW_HASKELL__ < 800
          Rank (Ix (n - 1)) ~ (n - 1),
#endif
          VU.Unbox (Ix (n - 1))) =>
         VM.MVector VU.MVector (IxN n) where
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

instance (3 <= n,
#if __GLASGOW_HASKELL__ < 800
          Rank (Ix (n - 1)) ~ (n - 1),
#endif
          VU.Unbox (Ix (n-1))) => V.Vector VU.Vector (IxN n) where
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
  basicUnsafeSlice idx len (V_IxN (vec1, vec)) = do
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

