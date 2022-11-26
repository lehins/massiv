{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Massiv.VectorSpec (spec) where

import Control.Applicative
import Control.Arrow (first)
import Control.Exception
import Data.Bits
import Data.Int
import qualified Data.List as List
import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe as A
import Data.Massiv.Vector as V
import Data.Maybe
import Data.Primitive.MutVar
import qualified Data.Tuple as Tuple
import qualified Data.Vector as VB
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Data.Word
import Test.Massiv.Core

import System.Random.MWC as MWC

infix 4 !==!, !!==!!

toUnboxV2
  :: Unbox e
  => (VU.Vector e1 -> VU.Vector e2 -> VU.Vector e)
  -> Array U ix1 e1
  -> Array U ix2 e2
  -> Array U Ix1 e
toUnboxV2 f v1 v2 =
  fromUnboxedVector (getComp v1 <> getComp v2) (f (toUnboxedVector v1) (toUnboxedVector v2))

toUnboxV3
  :: Unbox e
  => (VU.Vector e1 -> VU.Vector e2 -> VU.Vector e3 -> VU.Vector e)
  -> Array U ix1 e1
  -> Array U ix2 e2
  -> Array U ix3 e3
  -> Array U Ix1 e
toUnboxV3 f v1 v2 v3 = appComp (getComp v1) (toUnboxV2 (f (toUnboxedVector v1)) v2 v3)

toUnboxV4
  :: Unbox e
  => (VU.Vector e1 -> VU.Vector e2 -> VU.Vector e3 -> VU.Vector e4 -> VU.Vector e)
  -> Array U ix1 e1
  -> Array U ix2 e2
  -> Array U ix3 e3
  -> Array U ix4 e4
  -> Array U Ix1 e
toUnboxV4 f v1 v2 v3 v4 = appComp (getComp v1) (toUnboxV3 (f (toUnboxedVector v1)) v2 v3 v4)

toUnboxV5
  :: Unbox e
  => (VU.Vector e1 -> VU.Vector e2 -> VU.Vector e3 -> VU.Vector e4 -> VU.Vector e5 -> VU.Vector e)
  -> Array U ix1 e1
  -> Array U ix2 e2
  -> Array U ix3 e3
  -> Array U ix4 e4
  -> Array U ix5 e5
  -> Array U Ix1 e
toUnboxV5 f v1 v2 v3 v4 v5 = appComp (getComp v1) (toUnboxV4 (f (toUnboxedVector v1)) v2 v3 v4 v5)

toUnboxV6
  :: Unbox e
  => (VU.Vector e1 -> VU.Vector e2 -> VU.Vector e3 -> VU.Vector e4 -> VU.Vector e5 -> VU.Vector e6 -> VU.Vector e)
  -> Array U ix1 e1
  -> Array U ix2 e2
  -> Array U ix3 e3
  -> Array U ix4 e4
  -> Array U ix5 e5
  -> Array U ix6 e6
  -> Array U Ix1 e
toUnboxV6 f v1 v2 v3 v4 v5 v6 =
  appComp (getComp v1) (toUnboxV5 (f (toUnboxedVector v1)) v2 v3 v4 v5 v6)

toPrimV2 :: (Index ix) => (VP.Vector e1 -> VP.Vector e2 -> t) -> Array P ix e1 -> Array P ix e2 -> t
toPrimV2 f v1 v2 = f (toPrimitiveVector v1) (toPrimitiveVector v2)

toPrimV3
  :: Index ix
  => (VP.Vector e -> VP.Vector e1 -> VP.Vector e2 -> t)
  -> Array P ix e
  -> Array P ix e1
  -> Array P ix e2
  -> t
toPrimV3 f v1 = toPrimV2 (f (toPrimitiveVector v1))

toPrimV4
  :: Index ix
  => (VP.Vector e1 -> VP.Vector e2 -> VP.Vector e3 -> VP.Vector e4 -> t)
  -> Array P ix e1
  -> Array P ix e2
  -> Array P ix e3
  -> Array P ix e4
  -> t
toPrimV4 f v1 = toPrimV3 (f (toPrimitiveVector v1))

toPrimV5
  :: Index ix
  => (VP.Vector e -> VP.Vector e1 -> VP.Vector e2 -> VP.Vector e3 -> VP.Vector e4 -> t)
  -> Array P ix e
  -> Array P ix e1
  -> Array P ix e2
  -> Array P ix e3
  -> Array P ix e4
  -> t
toPrimV5 f v1 = toPrimV4 (f (toPrimitiveVector v1))

toPrimV6
  :: Index ix
  => (VP.Vector e -> VP.Vector e1 -> VP.Vector e2 -> VP.Vector e3 -> VP.Vector e4 -> VP.Vector e5 -> t)
  -> Array P ix e
  -> Array P ix e1
  -> Array P ix e2
  -> Array P ix e3
  -> Array P ix e4
  -> Array P ix e5
  -> t
toPrimV6 f v1 = toPrimV5 (f (toPrimitiveVector v1))

(!==!)
  :: (Eq e, Show e, Prim e, Load r Ix1 e) => V.Vector r e -> VP.Vector e -> Property
(!==!) arr vec = toPrimitiveVector (convert arr) === vec

(!!==!!)
  :: (Eq e, Show e, Prim e, Load r Ix1 e) => V.Vector r e -> VP.Vector e -> Property
(!!==!!) arr vec = property $ do
  eRes <- try (pure $! vec)
  case eRes of
    Right vec' -> toPrimitiveVector (compute arr) `shouldBe` vec'
    Left (_exc :: ErrorCall) ->
      shouldThrow (pure $! computeAs P arr) selectErrorCall

newtype SeedVector = SeedVector (VP.Vector Word32) deriving (Eq, Show)

instance Arbitrary SeedVector where
  arbitrary = SeedVector . VP.fromList <$> arbitrary

withSeed :: forall a. SeedVector -> (forall s. MWC.Gen s -> ST s a) -> a
withSeed (SeedVector seed) f = runST $ MWC.initialize seed >>= f

withSeed2
  :: forall a
   . (Eq a, Show a)
  => SeedVector
  -> (forall s. MWC.Gen s -> ST s a)
  -> (forall s. MWC.Gen s -> ST s a)
  -> Property
withSeed2 seed f g = withSeed @a seed f === withSeed seed g

withSeedV2
  :: forall r e
   . (Eq e, Show e, Prim e, Load r Ix1 e)
  => SeedVector
  -> (forall s. MWC.Gen s -> ST s (V.Vector r e))
  -> (forall s. MWC.Gen s -> ST s (VP.Vector e))
  -> Property
withSeedV2 seed f g = withSeed @(V.Vector r e) seed f !==! withSeed seed g

prop_sreplicateM :: SeedVector -> Int -> Property
prop_sreplicateM seed k =
  withSeedV2 @DS @Word
    seed
    (V.sreplicateM (Sz k) . uniform)
    (VP.replicateM k . uniform)

prop_sgenerateM :: SeedVector -> Int -> Fun Int Word -> Property
prop_sgenerateM seed k f =
  withSeedV2 @DS @Word
    seed
    (genWith (V.sgenerateM (Sz k)))
    (genWith (VP.generateM k))
  where
    genWith :: PrimMonad f => ((Int -> f Word) -> t) -> MWC.Gen (PrimState f) -> t
    genWith genM gen = genM (\i -> xor (apply f i) <$> uniform gen)

prop_siterateNM :: SeedVector -> Int -> Word -> Property
prop_siterateNM seed k a =
  withSeed @(V.Vector DS Word) seed (genWith (\f -> V.siterateNM (Sz k) f a))
    !==! withSeed seed (genWith (\f -> VP.iterateNM k f a))
  where
    genWith :: PrimMonad f => ((Word -> f Word) -> t) -> MWC.Gen (PrimState f) -> t
    genWith genM gen = genM (\prev -> xor prev <$> uniform gen)

genWithUnfoldrM
  :: PrimMonad f => ((Word -> f (Maybe (Word, Word))) -> t) -> MWC.Gen (PrimState f) -> t
genWithUnfoldrM genM gen = genM $ \prev -> do
  x <- uniform gen
  let cur = prev `xor` x
  pure $ if cur `mod` 17 == 0 then Nothing else Just (x, cur)

prop_sunfoldrM :: SeedVector -> Word -> Property
prop_sunfoldrM seed a =
  withSeedV2 @DS @Word
    seed
    (genWithUnfoldrM (`V.sunfoldrM` a))
    (genWithUnfoldrM (`VP.unfoldrM` a))

prop_sunfoldrNM :: SeedVector -> Int -> Word -> Property
prop_sunfoldrNM seed k a =
  withSeedV2 @DS @Word
    seed
    (genWithUnfoldrM (\f -> V.sunfoldrNM (Sz k) f a))
    (genWithUnfoldrM (\f -> VP.unfoldrNM k f a))
    .&&. withSeedV2 @DS @Word
      seed
      (genWithUnfoldrM (\f -> A.unsafeUnfoldrNM (Sz k) f a))
      (genWithUnfoldrM (\f -> VP.unfoldrNM k f a))

prop_sunfoldrExactNM :: SeedVector -> Int -> Word -> Property
prop_sunfoldrExactNM seed k a =
  withSeedV2 @DS @Word
    seed
    (genWith (\f -> V.sunfoldrExactNM (Sz k) f a))
    (genWith (\f -> VP.unfoldrNM k (fmap Just . f) a))
  where
    genWith :: PrimMonad f => ((Word -> f (Word, Word)) -> t) -> MWC.Gen (PrimState f) -> t
    genWith genM gen = genM $ \prev -> do
      x <- uniform gen
      pure (x, prev `xor` x)

genWithMapM :: PrimMonad m => ((Word -> m Word) -> m a) -> MWC.Gen (PrimState m) -> m a
genWithMapM genM gen = genM $ \e -> xor e <$> uniform gen
genWithMapWS :: PrimMonad m => ((Word -> MWC.Gen (PrimState m) -> m Word) -> m a) -> m a
genWithMapWS genM = genM $ \e gen -> xor e <$> uniform gen

genWithIMapM :: PrimMonad m => ((Int -> Word -> m Word) -> m a) -> MWC.Gen (PrimState m) -> m a
genWithIMapM genM gen = genM $ \i e -> do
  ir <- uniformR (0, fromIntegral i) gen
  xor ir . xor e <$> uniform gen

genWithIMapWS :: PrimMonad m => ((Int -> Word -> MWC.Gen (PrimState m) -> m Word) -> m a) -> m a
genWithIMapWS genM =
  genM $ \i e gen -> do
    ir <- uniformR (0, fromIntegral i) gen
    xor ir . xor e <$> uniform gen

genWithMapM_ :: PrimMonad m => ((Word -> m ()) -> m ()) -> MWC.Gen (PrimState m) -> m Word
genWithMapM_ genM gen = do
  ref <- newMutVar =<< uniform gen
  genM $ \e -> do
    e' <- xor e <$> uniform gen
    modifyMutVar ref (xor e')
  readMutVar ref

genWithIMapM_ :: PrimMonad m => ((Int -> Word -> m ()) -> m ()) -> MWC.Gen (PrimState m) -> m Word
genWithIMapM_ genM gen = do
  ref <- newMutVar =<< uniform gen
  genM $ \i e -> do
    ir <- uniformR (0, fromIntegral i) gen
    e' <- xor ir . xor e <$> uniform gen
    modifyMutVar ref (xor e')
  readMutVar ref

prop_straverse :: SeedVector -> Array P Ix2 Word -> Property
prop_straverse seed a =
  withSeed @(V.Vector DS Word) seed (genWithMapM (`V.straverse` a))
    !==! withSeed seed (genWithMapM (`VP.mapM` toPrimitiveVector a))

prop_smapM :: SeedVector -> Array P Ix2 Word -> Property
prop_smapM seed a =
  withSeed @(V.Vector DS Word) seed (genWithMapM (`V.smapM` a))
    !==! withSeed seed (genWithMapM (`VP.mapM` toPrimitiveVector a))

prop_smapMaybeM :: SeedVector -> Array BL Ix2 Word -> Fun Word (Maybe Word16) -> Property
prop_smapMaybeM seed a gm =
  withSeed @(V.Vector DS Word16) seed (genWithMapM (\f -> V.smapMaybeM (fmap g . f) a))
    !==! withSeed
      seed
      (genWithMapM (\f -> VP.convert . VB.mapMaybe id <$> VB.mapM (fmap g . f) (toBoxedVector a)))
  where
    g = apply gm

prop_sitraverse :: SeedVector -> Vector P Word -> Property
prop_sitraverse seed a =
  withSeed @(V.Vector DS Word) seed (genWithIMapM (`V.sitraverse` a))
    !==! withSeed seed (genWithIMapM (\f -> VP.convert <$> VU.mapM (uncurry f) vp))
  where
    vp = VU.imap (,) $ toUnboxedVector (compute a)

prop_simapM :: SeedVector -> Vector U Word -> Property
prop_simapM seed a =
  withSeed @(V.Vector DS Word) seed (genWithIMapM (V.siforM a))
    !==! withSeed seed (genWithIMapM (\f -> VP.convert <$> VU.mapM (uncurry f) vp))
  where
    vp = VU.imap (,) $ toUnboxedVector a

prop_smapM_ :: SeedVector -> Array P Ix2 Word -> Property
prop_smapM_ seed a =
  withSeed seed (genWithMapM_ (V.sforM_ a))
    === withSeed seed (genWithMapM_ (VP.forM_ (toPrimitiveVector a)))

prop_simapM_ :: SeedVector -> Vector U Word -> Property
prop_simapM_ seed a =
  withSeed seed (genWithIMapM_ (V.siforM_ a))
    === withSeed seed (genWithIMapM_ (\f -> VU.mapM_ (uncurry f) vp))
  where
    vp = VU.imap (,) $ toUnboxedVector a

prop_sfilterM :: SeedVector -> Fun Word Bool -> Vector P Word -> Property
prop_sfilterM seed g a =
  withSeed @(V.Vector DS Word) seed (genWith (`V.sfilterM` a))
    !==! withSeed seed (genWith (`VP.filterM` toPrimitiveVector a))
  where
    genWith :: PrimMonad f => ((Word -> f Bool) -> t) -> MWC.Gen (PrimState f) -> t
    genWith genM gen = genM $ \e -> do
      x <- xor e <$> uniform gen
      pure $ apply g x

prop_sifilterM :: SeedVector -> Fun Word Bool -> Vector U Word -> Property
prop_sifilterM seed g a =
  withSeed @(V.Vector DS Word) seed (genWith (`V.sifilterM` a))
    !==! withSeed seed (genWith (\f -> VP.convert . VU.map snd <$> VU.filterM (uncurry f) vp))
  where
    vp = VU.imap (,) $ toUnboxedVector a
    genWith :: PrimMonad f => ((Int -> Word -> f Bool) -> t) -> MWC.Gen (PrimState f) -> t
    genWith genM gen = genM $ \i e -> do
      ir <- uniformR (0, fromIntegral i) gen
      x <- xor ir . xor e <$> uniform gen
      pure $ apply g x

applyFun4 :: Fun (a, b, c, d) e -> (a -> b -> c -> d -> e)
applyFun4 (Fun _ f) a b c d = f (a, b, c, d)
applyFun5 :: Fun (a, b, c, d, e) f -> (a -> b -> c -> d -> e -> f)
applyFun5 (Fun _ g) a b c d f = g (a, b, c, d, f)
applyFun6 :: Fun (a, (b, c, d, e, f)) g -> (a -> b -> c -> d -> e -> f -> g)
applyFun6 (Fun _ h) a b c d f g = h (a, (b, c, d, f, g))
applyFun7 :: Fun (a, b, (c, d, e, f, g)) h -> (a -> b -> c -> d -> e -> f -> g -> h)
applyFun7 (Fun _ i) a b c d f g h = i (a, b, (c, d, f, g, h))

com2M :: Fun (a, b) d -> (d -> c) -> a -> b -> c
com2M f g a = g . applyFun2 f a

com3M :: Fun (a, b, c) d -> (d -> e) -> a -> b -> c -> e
com3M f g a b = g . applyFun3 f a b

com4M :: Fun (a, b, c, d) e -> (e -> h) -> a -> b -> c -> d -> h
com4M f g a b c = g . applyFun4 f a b c

com5M :: Fun (a, b, c, d, e) h -> (h -> i) -> a -> b -> c -> d -> e -> i
com5M f g a b c d = g . applyFun5 f a b c d

com6M :: Fun (a, (b, c, d, e, h)) i -> (i -> j) -> a -> b -> c -> d -> e -> h -> j
com6M f g a b c d e = g . applyFun6 f a b c d e

com7M :: Fun (a, b, (c, d, e, h, i)) j -> (j -> k) -> a -> b -> c -> d -> e -> h -> i -> k
com7M f g a b c d e h = g . applyFun7 f a b c d e h

prop_szip :: Vector U Word -> Vector U Int -> Property
prop_szip v1 v2 = compute (V.szip v1 v2) === toUnboxV2 VU.zip v1 v2

prop_szip3 :: Vector U Word64 -> Vector U Word32 -> Vector U Word16 -> Property
prop_szip3 v1 v2 v3 = compute (V.szip3 v1 v2 v3) === toUnboxV3 VU.zip3 v1 v2 v3

prop_szip4 :: Vector U Word64 -> Vector U Word32 -> Vector U Word16 -> Vector U Word8 -> Property
prop_szip4 v1 v2 v3 v4 = compute (V.szip4 v1 v2 v3 v4) === toUnboxV4 VU.zip4 v1 v2 v3 v4

prop_szip5
  :: Vector U Word64
  -> Vector U Word32
  -> Vector U Word16
  -> Vector U Word8
  -> Vector U Int8
  -> Property
prop_szip5 v1 v2 v3 v4 v5 = compute (V.szip5 v1 v2 v3 v4 v5) === toUnboxV5 VU.zip5 v1 v2 v3 v4 v5

prop_szip6
  :: Vector U Word64
  -> Vector U Word32
  -> Vector U Word16
  -> Vector U Word8
  -> Vector U Int8
  -> Vector U Int16
  -> Property
prop_szip6 v1 v2 v3 v4 v5 v6 =
  compute (V.szip6 v1 v2 v3 v4 v5 v6) === toUnboxV6 VU.zip6 v1 v2 v3 v4 v5 v6

prop_szipWith :: Vector P Word -> Vector P Int -> Fun (Word, Int) Int -> Property
prop_szipWith v1 v2 f =
  V.szipWith (applyFun2 f) v1 v2 !==! toPrimV2 (VP.zipWith (applyFun2 f)) v1 v2

prop_szipWith3
  :: Vector P Word64
  -> Vector P Word32
  -> Vector P Word16
  -> Fun (Word64, Word32, Word16) Int
  -> Property
prop_szipWith3 v1 v2 v3 f =
  V.szipWith3 (applyFun3 f) v1 v2 v3 !==! toPrimV3 (VP.zipWith3 (applyFun3 f)) v1 v2 v3

prop_szipWith4
  :: Vector P Word64
  -> Vector P Word32
  -> Vector P Word16
  -> Vector P Word8
  -> Fun (Word64, Word32, Word16, Word8) Int
  -> Property
prop_szipWith4 v1 v2 v3 v4 f =
  V.szipWith4 (applyFun4 f) v1 v2 v3 v4 !==! toPrimV4 (VP.zipWith4 (applyFun4 f)) v1 v2 v3 v4

prop_szipWith5
  :: Vector P Word64
  -> Vector P Word32
  -> Vector P Word16
  -> Vector P Word8
  -> Vector P Int8
  -> Fun (Word64, Word32, Word16, Word8, Int8) Int
  -> Property
prop_szipWith5 v1 v2 v3 v4 v5 f =
  V.szipWith5 (applyFun5 f) v1 v2 v3 v4 v5 !==! toPrimV5 (VP.zipWith5 (applyFun5 f)) v1 v2 v3 v4 v5

prop_szipWith6
  :: Vector DS Word64
  -> Vector B Word32
  -> Vector BN Word16
  -> Vector S Word8
  -> Vector U Int8
  -> Vector P Int16
  -> Fun (Word64, (Word32, Word16, Word8, Int8, Int16)) Int
  -> Property
prop_szipWith6 v1 v2 v3 v4 v5 v6 f =
  V.szipWith6 (applyFun6 f) v1 v2 v3 v4 v5 v6
    !==! toPrimV6
      (VP.zipWith6 (applyFun6 f))
      (compute v1)
      (compute v2)
      (compute v3)
      (compute v4)
      (compute v5)
      v6

prop_sizipWith :: Vector DS Word64 -> Vector DS Word32 -> Fun (Ix1, Word64, Word32) Int -> Property
prop_sizipWith v1 v2 f =
  sizipWith (applyFun3 f) v1 v2
    !==! toPrimV2 (VP.izipWith (applyFun3 f)) (compute v1) (compute v2)

prop_sizipWith3
  :: Vector P Word64
  -> Vector D Word32
  -> Vector D Word16
  -> Fun (Ix1, Word64, Word32, Word16) Int
  -> Property
prop_sizipWith3 v1 v2 v3 f =
  sizipWith3 (applyFun4 f) v1 v2 v3
    !==! toPrimV3 (VP.izipWith3 (applyFun4 f)) (compute v1) (compute v2) (compute v3)

prop_sizipWith4
  :: Vector D Word64
  -> Vector DS Word32
  -> Vector P Word16
  -> Vector U Word8
  -> Fun (Ix1, Word64, Word32, Word16, Word8) Int
  -> Property
prop_sizipWith4 v1 v2 v3 v4 f =
  sizipWith4 (applyFun5 f) v1 v2 v3 v4
    !==! toPrimV4 (VP.izipWith4 (applyFun5 f)) (compute v1) (compute v2) (compute v3) (compute v4)

prop_sizipWith5
  :: Vector DS Word64
  -> Vector S Word32
  -> Vector P Word16
  -> Vector U Word8
  -> Vector BN Int8
  -> Fun (Ix1, (Word64, Word32, Word16, Word8, Int8)) Int
  -> Property
prop_sizipWith5 v1 v2 v3 v4 v5 f =
  sizipWith5 (applyFun6 f) v1 v2 v3 v4 v5
    !==! toPrimV5 (VP.izipWith5 (applyFun6 f)) (compute v1) (compute v2) v3 (compute v4) (compute v5)

prop_sizipWith6
  :: Vector DS Word64
  -> Vector D Word32
  -> Vector BL Word16
  -> Vector BN Word8
  -> Vector P Int8
  -> Vector P Int16
  -> Fun (Ix1, Word64, (Word32, Word16, Word8, Int8, Int16)) Int
  -> Property
prop_sizipWith6 v1 v2 v3 v4 v5 v6 f =
  sizipWith6 (applyFun7 f) v1 v2 v3 v4 v5 v6
    !==! toPrimV6 (VP.izipWith6 (applyFun7 f)) (compute v1) (compute v2) (compute v3) (compute v4) v5 v6

prop_szipWithM
  :: SeedVector -> Vector P Word64 -> Vector P Word32 -> Fun (Word64, Word32) Word -> Property
prop_szipWithM seed v1 v2 f =
  withSeedV2 @DS @Word
    seed
    (genWithMapM (\g -> V.szipWithM (com2M f g) v1 v2))
    (genWithMapM (\g -> toPrimV2 (VP.zipWithM (com2M f g)) v1 v2))

prop_szipWith3M
  :: SeedVector
  -> Vector P Word64
  -> Vector P Word32
  -> Vector P Word16
  -> Fun (Word64, Word32, Word16) Word
  -> Property
prop_szipWith3M seed v1 v2 v3 f =
  withSeedV2 @DS @Word
    seed
    (genWithMapM (\g -> V.szipWith3M (com3M f g) v1 v2 v3))
    (genWithMapM (VP.forM (toPrimV3 (VP.zipWith3 (applyFun3 f)) v1 v2 v3)))

prop_szipWith4M
  :: SeedVector
  -> Vector P Word64
  -> Vector P Word32
  -> Vector P Word16
  -> Vector P Word8
  -> Fun (Word64, Word32, Word16, Word8) Word
  -> Property
prop_szipWith4M seed v1 v2 v3 v4 f =
  withSeedV2 @DS @Word
    seed
    (genWithMapM (\g -> V.szipWith4M (com4M f g) v1 v2 v3 v4))
    (genWithMapM (VP.forM (toPrimV4 (VP.zipWith4 (applyFun4 f)) v1 v2 v3 v4)))

prop_szipWith5M
  :: SeedVector
  -> Vector P Word64
  -> Vector P Word32
  -> Vector P Word16
  -> Vector P Word8
  -> Vector P Int8
  -> Fun (Word64, Word32, Word16, Word8, Int8) Word
  -> Property
prop_szipWith5M seed v1 v2 v3 v4 v5 f =
  withSeedV2 @DS @Word
    seed
    (genWithMapM (\g -> V.szipWith5M (com5M f g) v1 v2 v3 v4 v5))
    (genWithMapM (VP.forM (toPrimV5 (VP.zipWith5 (applyFun5 f)) v1 v2 v3 v4 v5)))

prop_szipWith6M
  :: SeedVector
  -> Vector P Int16
  -> Vector P Word64
  -> Vector P Word32
  -> Vector P Word16
  -> Vector P Word8
  -> Vector P Int8
  -> Fun (Int16, (Word64, Word32, Word16, Word8, Int8)) Word
  -> Property
prop_szipWith6M seed v1 v2 v3 v4 v5 v6 f =
  withSeedV2 @DS @Word
    seed
    (genWithMapM (\g -> V.szipWith6M (com6M f g) v1 v2 v3 v4 v5 v6))
    (genWithMapM (VP.forM (toPrimV6 (VP.zipWith6 (applyFun6 f)) v1 v2 v3 v4 v5 v6)))

prop_szipWithM_
  :: SeedVector -> Vector P Word64 -> Vector P Word32 -> Fun (Word64, Word32) Word -> Property
prop_szipWithM_ seed v1 v2 f =
  withSeed2
    seed
    (genWithMapM_ (\g -> V.szipWithM_ (com2M f g) v1 v2))
    (genWithMapM_ (\g -> toPrimV2 (VP.zipWithM_ (com2M f g)) v1 v2))

prop_szipWith3M_
  :: SeedVector
  -> Vector P Word64
  -> Vector P Word32
  -> Vector P Word16
  -> Fun (Word64, Word32, Word16) Word
  -> Property
prop_szipWith3M_ seed v1 v2 v3 f =
  withSeed2
    seed
    (genWithMapM_ (\g -> V.szipWith3M_ (com3M f g) v1 v2 v3))
    (genWithMapM_ (VP.forM_ (toPrimV3 (VP.zipWith3 (applyFun3 f)) v1 v2 v3)))

prop_szipWith4M_
  :: SeedVector
  -> Vector P Word64
  -> Vector P Word32
  -> Vector P Word16
  -> Vector P Word8
  -> Fun (Word64, Word32, Word16, Word8) Word
  -> Property
prop_szipWith4M_ seed v1 v2 v3 v4 f =
  withSeed2
    seed
    (genWithMapM_ (\g -> V.szipWith4M_ (com4M f g) v1 v2 v3 v4))
    (genWithMapM_ (VP.forM_ (toPrimV4 (VP.zipWith4 (applyFun4 f)) v1 v2 v3 v4)))

prop_szipWith5M_
  :: SeedVector
  -> Vector P Word64
  -> Vector P Word32
  -> Vector P Word16
  -> Vector P Word8
  -> Vector P Int8
  -> Fun (Word64, Word32, Word16, Word8, Int8) Word
  -> Property
prop_szipWith5M_ seed v1 v2 v3 v4 v5 f =
  withSeed2
    seed
    (genWithMapM_ (\g -> V.szipWith5M_ (com5M f g) v1 v2 v3 v4 v5))
    (genWithMapM_ (VP.forM_ (toPrimV5 (VP.zipWith5 (applyFun5 f)) v1 v2 v3 v4 v5)))

prop_szipWith6M_
  :: SeedVector
  -> Vector P Int16
  -> Vector P Word64
  -> Vector P Word32
  -> Vector P Word16
  -> Vector P Word8
  -> Vector P Int8
  -> Fun (Int16, (Word64, Word32, Word16, Word8, Int8)) Word
  -> Property
prop_szipWith6M_ seed v1 v2 v3 v4 v5 v6 f =
  withSeed2
    seed
    (genWithMapM_ (\g -> V.szipWith6M_ (com6M f g) v1 v2 v3 v4 v5 v6))
    (genWithMapM_ (VP.forM_ (toPrimV6 (VP.zipWith6 (applyFun6 f)) v1 v2 v3 v4 v5 v6)))

prop_sizipWithM
  :: SeedVector -> Vector U Word64 -> Vector U Word32 -> Fun (Ix1, Word64, Word32) Word -> Property
prop_sizipWithM seed v1 v2 f =
  withSeedV2 @DS @Word
    seed
    (genWithMapM (\g -> V.sizipWithM (com3M f g) v1 v2))
    ( genWithMapM
        ( \g ->
            VP.convert
              <$> VU.izipWithM (com3M f g) (toUnboxedVector v1) (toUnboxedVector v2)
        )
    )

prop_sizipWith3M
  :: SeedVector
  -> Vector P Word64
  -> Vector P Word32
  -> Vector P Word16
  -> Fun (Ix1, Word64, Word32, Word16) Word
  -> Property
prop_sizipWith3M seed v1 v2 v3 f =
  withSeedV2 @DS @Word
    seed
    (genWithMapM (\g -> V.sizipWith3M (com4M f g) v1 v2 v3))
    (genWithMapM (VP.forM (toPrimV3 (VP.izipWith3 (applyFun4 f)) v1 v2 v3)))

prop_sizipWith4M
  :: SeedVector
  -> Vector P Word64
  -> Vector P Word32
  -> Vector P Word16
  -> Vector P Word8
  -> Fun (Ix1, Word64, Word32, Word16, Word8) Word
  -> Property
prop_sizipWith4M seed v1 v2 v3 v4 f =
  withSeedV2 @DS @Word
    seed
    (genWithMapM (\g -> V.sizipWith4M (com5M f g) v1 v2 v3 v4))
    (genWithMapM (VP.forM (toPrimV4 (VP.izipWith4 (applyFun5 f)) v1 v2 v3 v4)))

prop_sizipWith5M
  :: SeedVector
  -> Vector P Word64
  -> Vector P Word32
  -> Vector P Word16
  -> Vector P Word8
  -> Vector P Int8
  -> Fun (Ix1, (Word64, Word32, Word16, Word8, Int8)) Word
  -> Property
prop_sizipWith5M seed v1 v2 v3 v4 v5 f =
  withSeedV2 @DS @Word
    seed
    (genWithMapM (\g -> V.sizipWith5M (com6M f g) v1 v2 v3 v4 v5))
    (genWithMapM (VP.forM (toPrimV5 (VP.izipWith5 (applyFun6 f)) v1 v2 v3 v4 v5)))

prop_sizipWith6M
  :: SeedVector
  -> Vector P Int16
  -> Vector P Word64
  -> Vector P Word32
  -> Vector P Word16
  -> Vector P Word8
  -> Vector P Int8
  -> Fun (Ix1, Int16, (Word64, Word32, Word16, Word8, Int8)) Word
  -> Property
prop_sizipWith6M seed v1 v2 v3 v4 v5 v6 f =
  withSeedV2 @DS @Word
    seed
    (genWithMapM (\g -> V.sizipWith6M (com7M f g) v1 v2 v3 v4 v5 v6))
    (genWithMapM (VP.forM (toPrimV6 (VP.izipWith6 (applyFun7 f)) v1 v2 v3 v4 v5 v6)))

prop_sizipWithM_
  :: SeedVector -> Vector U Word64 -> Vector U Word32 -> Fun (Ix1, Word64, Word32) Word -> Property
prop_sizipWithM_ seed v1 v2 f =
  withSeed2
    seed
    (genWithMapM_ (\g -> V.sizipWithM_ (com3M f g) v1 v2))
    (genWithMapM_ (\g -> VU.izipWithM_ (com3M f g) (toUnboxedVector v1) (toUnboxedVector v2)))

prop_sizipWith3M_
  :: SeedVector
  -> Vector P Word64
  -> Vector P Word32
  -> Vector P Word16
  -> Fun (Ix1, Word64, Word32, Word16) Word
  -> Property
prop_sizipWith3M_ seed v1 v2 v3 f =
  withSeed2
    seed
    (genWithMapM_ (\g -> V.sizipWith3M_ (com4M f g) v1 v2 v3))
    (genWithMapM_ (VP.forM_ (toPrimV3 (VP.izipWith3 (applyFun4 f)) v1 v2 v3)))

prop_sizipWith4M_
  :: SeedVector
  -> Vector P Word64
  -> Vector P Word32
  -> Vector P Word16
  -> Vector P Word8
  -> Fun (Ix1, Word64, Word32, Word16, Word8) Word
  -> Property
prop_sizipWith4M_ seed v1 v2 v3 v4 f =
  withSeed2
    seed
    (genWithMapM_ (\g -> V.sizipWith4M_ (com5M f g) v1 v2 v3 v4))
    (genWithMapM_ (VP.forM_ (toPrimV4 (VP.izipWith4 (applyFun5 f)) v1 v2 v3 v4)))

prop_sizipWith5M_
  :: SeedVector
  -> Vector P Word64
  -> Vector P Word32
  -> Vector P Word16
  -> Vector P Word8
  -> Vector P Int8
  -> Fun (Ix1, (Word64, Word32, Word16, Word8, Int8)) Word
  -> Property
prop_sizipWith5M_ seed v1 v2 v3 v4 v5 f =
  withSeed2
    seed
    (genWithMapM_ (\g -> V.sizipWith5M_ (com6M f g) v1 v2 v3 v4 v5))
    (genWithMapM_ (VP.forM_ (toPrimV5 (VP.izipWith5 (applyFun6 f)) v1 v2 v3 v4 v5)))

prop_sizipWith6M_
  :: SeedVector
  -> Vector P Int16
  -> Vector P Word64
  -> Vector P Word32
  -> Vector P Word16
  -> Vector P Word8
  -> Vector P Int8
  -> Fun (Ix1, Int16, (Word64, Word32, Word16, Word8, Int8)) Word
  -> Property
prop_sizipWith6M_ seed v1 v2 v3 v4 v5 v6 f =
  withSeed2
    seed
    (genWithMapM_ (\g -> V.sizipWith6M_ (com7M f g) v1 v2 v3 v4 v5 v6))
    (genWithMapM_ (VP.forM_ (toPrimV6 (VP.izipWith6 (applyFun7 f)) v1 v2 v3 v4 v5 v6)))

spec :: Spec
spec =
  describe "Vector" $ do
    describe "same-as-array" $ do
      describe "traverse" $ do
        prop "straverse == traversePrim" prop_straverse_traversePrim
        prop "sitraverse == itraversePrim" prop_sitraverse_itraversePrim
        prop "sitraverse == itraverseA" prop_sitraverse_itraverseA
        prop "simapM_ == itraverseA_" prop_simapM_itraverseA_
        prop "smapM_ == traverseA_" prop_smapM_traverseA_
        prop "sforM == forM" prop_sforM_forM
        prop "siforM == iforM" prop_siforM_iforM
        prop "sforM_ == forM_" prop_sforM_forM_
        prop "siforM_ == iforM_" prop_siforM_iforM_
        prop "sforM_ == forIO_ (ParN 1)" prop_sforM_forIO_
        prop "sforM == forIO (Seq)" prop_sforM_forIO
        prop "siforM == iforIO (ParN 1)" prop_siforM_iforIO
        prop "siforM == iforIO_ (ParN 1)" prop_siforM_iforIO_
        prop "siforM == iforWS (ParN 1)" prop_siforM_iforWS
        prop "smapM == mapWS (Seq)" prop_smapM_mapWS
      describe "Enumeration" $ do
        prop "senumFromN" $ \comp (i :: Int) sz ->
          computeAs S (V.senumFromN i sz) === compute (A.enumFromN comp i sz)
        prop "senumFromStepN" $ \comp (i :: Int) s sz ->
          computeAs S (V.senumFromStepN i s sz) === compute (A.enumFromStepN comp i s sz)
    describe "same-as-vector-package" $ do
      describe "Accessors" $ do
        describe "Size" $ do
          it "slength" $ do
            slength (sfromList []) `shouldBe` Nothing
            slength (sfromListN 1 []) `shouldBe` Nothing
            slength (sgenerate 1 id) `shouldBe` Just 1
          it "isNull" $ do
            isNull sempty `shouldBe` True
            isNull (fromLists' Seq [[]] :: Array P Ix2 Int) `shouldBe` True
            isNull (siterateN 3 id ()) `shouldBe` False
            isNull (0 ..: 1 :> 2 :> 3 :. 0) `shouldBe` True
        describe "Indexing" $ do
          prop "head' (non-empty)" $ \(ArrNE arr :: ArrNE D Ix1 Int) ->
            head' arr === evaluate' arr 0 .&&. head' arr === shead' arr
          prop "head'" $ \(arr :: Array D Ix1 Int) ->
            (singleton (head' arr) :: Array D Ix1 Int)
              !!==!! VP.singleton (VP.head (toPrimitiveVector (compute arr)))
          prop "shead'" $ \(arr :: Array P Ix1 Int) ->
            (singleton (shead' arr) :: Array D Ix1 Int)
              !!==!! VP.singleton (VP.head (toPrimitiveVector arr))
          prop "last'" $ \(arr :: Array P Ix1 Int) ->
            (singleton (last' arr) :: Array D Ix1 Int)
              !!==!! VP.singleton (VP.last (toPrimitiveVector arr))
          prop "unconsM" $ \(v :: Vector D Int) ->
            fmap (computeAs P <$>) (A.unconsM v :: Maybe (Int, Vector D Int))
              === fmap (fmap (A.fromList Seq)) (List.uncons (A.toList v))
          prop "unsnocM" $ \(v :: Vector D Int) ->
            fmap (first (computeAs P)) (A.unsnocM v :: Maybe (Vector D Int, Int))
              === fmap
                (Tuple.swap . fmap (A.fromList Seq . List.reverse))
                (List.uncons (A.toList (A.reverse Dim1 v)))
        describe "Slicing" $ do
          prop "slice" $ \i sz (arr :: Array P Ix1 Word) ->
            V.slice i sz arr !!==!! VP.take (unSz sz) (VP.drop i (toPrimitiveVector arr))
          prop "sslice" $ \i sz (arr :: Array P Ix1 Word) ->
            computeAs B (V.sslice i sz arr)
              !!==!! VP.take (unSz sz) (VP.drop i (toPrimitiveVector arr))
          prop "slice'" $ \i sz (arr :: Array P Ix1 Word) ->
            V.slice' i sz arr !!==!! VP.slice i (unSz sz) (toPrimitiveVector arr)
          prop "init" $ \(arr :: Array P Ix1 Word) ->
            V.init arr !==! VP.reverse (VP.drop 1 (VP.reverse (toPrimitiveVector arr)))
          prop "init'" $ \(arr :: Array P Ix1 Word) ->
            V.init' arr !!==!! VP.init (toPrimitiveVector arr)
          prop "tail" $ \(arr :: Array P Ix1 Word) ->
            let vp = toPrimitiveVector arr
             in (V.tail arr !==! VP.drop 1 vp)
                  .&&. (not (isEmpty arr) ==> V.tail arr !==! VP.tail vp)
          prop "tail'" $ \(arr :: Array P Ix1 Word) ->
            V.tail' arr !!==!! VP.tail (toPrimitiveVector arr)
          prop "take" $ \n (arr :: Array P Ix1 Word) ->
            V.take (Sz n) arr !==! VP.take n (toPrimitiveVector arr)
          prop "takeWhile" $ \f (arr :: Array P Ix1 Word) ->
            V.takeWhile (applyFun f) arr !==! VP.takeWhile (applyFun f) (toPrimitiveVector arr)
          prop "take'" $ \sz@(Sz n) (arr :: Array P Ix1 Word) ->
            V.take' sz arr !!==!! VP.slice 0 n (toPrimitiveVector arr)
          prop "stake" $ \n (arr :: Array P Ix1 Word) ->
            V.stake (Sz n) arr !==! VP.take n (toPrimitiveVector arr)
          prop "drop" $ \n (arr :: Array P Ix1 Word) ->
            V.drop (Sz n) arr !==! VP.drop n (toPrimitiveVector arr)
          prop "dropWhile" $ \f (arr :: Array P Ix1 Word) ->
            V.dropWhile (applyFun f) arr !==! VP.dropWhile (applyFun f) (toPrimitiveVector arr)
          prop "drop'" $ \sz@(Sz n) (arr :: Array P Ix1 Word) ->
            V.drop' sz arr !!==!! VP.slice n (unSz (size arr) - n) (toPrimitiveVector arr)
          prop "sdrop" $ \n (arr :: Array P Ix1 Word) ->
            V.sdrop (Sz n) arr !==! VP.drop n (toPrimitiveVector arr)
          prop "sliceAt" $ \sz (arr :: Array P Ix1 Word) ->
            let (larr, rarr) = V.sliceAt (Sz sz) arr
                (lvec, rvec) = VP.splitAt sz (toPrimitiveVector arr)
             in (larr !==! lvec) .&&. (rarr !==! rvec)
          prop "sliceAt'" $ \sz@(Sz n) (arr :: Array P Ix1 Word) ->
            let (larr, rarr) = V.sliceAt' sz arr
                lvec = VP.slice 0 n (toPrimitiveVector arr)
                rvec = VP.slice n (unSz (size arr) - n) (toPrimitiveVector arr)
             in (larr !!==!! lvec) .&&. (rarr !!==!! rvec)
      describe "Constructors" $ do
        describe "Initialization" $ do
          it "empty" $ toPrimitiveVector (V.empty :: V.Vector P Word) `shouldBe` VP.empty
          it "sempty" $
            toPrimitiveVector (compute (V.sempty :: V.Vector DS Word)) `shouldBe` VP.empty
          prop "singleton" $ \e -> (V.singleton e :: V.Vector P Word) !==! VP.singleton e
          prop "ssingleton" $ \(e :: Word) -> V.ssingleton e !==! VP.singleton e
          prop "replicate" $ \comp k (e :: Word) ->
            V.replicate @DL comp (Sz k) e !==! VP.replicate k e
          prop "replicate" $ \k (e :: Word) ->
            V.replicate @DS Seq (Sz k) e !==! VP.replicate k e
          prop "sreplicate" $ \k (e :: Word) -> V.sreplicate (Sz k) e !==! VP.replicate k e
          prop "generate" $ \comp k (f :: Fun Int Word) ->
            V.generate comp (Sz k) (apply f) !==! VP.generate k (apply f)
          prop "sgenerate" $ \k (f :: Fun Int Word) ->
            V.sgenerate (Sz k) (apply f) !==! VP.generate k (apply f)
          prop "siterateN" $ \n (f :: Fun Word Word) a ->
            V.siterateN (Sz n) (apply f) a !==! VP.iterateN n (apply f) a
          prop "siterate" $ \n (f :: Fun Word Word) a ->
            computeAs P (V.stake n (V.siterate (apply f) a))
              === computeAs P (V.siterateN n (apply f) a)
          prop "cons" $ \e (v :: Vector P Word) ->
            computeAs P (V.cons e (toLoadArray v)) !!==!! VP.cons e (toPrimitiveVector v)
        describe "Monadic initialization" $ do
          prop "sreplicateM" prop_sreplicateM
          prop "sgenerateM" prop_sgenerateM
          prop "siterateNM" prop_siterateNM
        describe "Unfolding" $ do
          prop "sunfoldr" $ \(a :: Word) ->
            let f b
                  | b > 10000 || b `div` 17 == 0 = Nothing
                  | otherwise = Just (b * b, b + 1)
             in V.sunfoldr f a !==! VP.unfoldr f a
          prop "sunfoldrN" $ \n (a :: Word) ->
            let f b
                  | b > 10000 || b `div` 19 == 0 = Nothing
                  | otherwise = Just (b * b, b + 1)
             in (V.sunfoldrN (Sz n) f a !==! VP.unfoldrN n f a)
                  .&&. (A.unsafeUnfoldrN (Sz n) f a !==! VP.unfoldrN n f a)
          it "sunfoldrN (maxBound)" $
            let maxv = V.sunfoldrN (Sz maxBound) (const (Nothing :: Maybe (Word8, Word8))) 0
             in computeAs P maxv `shouldBe` A.empty
          prop "sunfoldrExactN" $ \n (a :: Word) ->
            let f b = (b * b, b + 1)
             in V.sunfoldrExactN (Sz n) f a !==! VP.unfoldrN n (Just . f) a
          prop "sunfoldrM" prop_sunfoldrM
          prop "sunfoldrNM" prop_sunfoldrNM
          it "sunfoldrNM (maxBound)" $
            let maxv = V.sunfoldrNM (Sz maxBound) (pure . const (Nothing :: Maybe (Word8, Word8))) 0
             in computeAs P <$> maxv `shouldReturn` A.empty
          prop "sunfoldrExactM" prop_sunfoldrExactNM
        describe "Enumeration" $ do
          prop "senumFromN" $ \(i :: Int) n -> V.senumFromN i (Sz n) !==! VP.enumFromN i n
          prop "senumFromStepN" $ \(i :: Int) s n ->
            V.senumFromStepN i s (Sz n) !==! VP.enumFromStepN i s n
        describe "Concatenation" $ do
          prop "sappend" $ \(v1 :: Vector D Int) (v2 :: Vector P Int) ->
            V.sappend v1 v2 !==! toPrimitiveVector (compute v1) VP.++ toPrimitiveVector v2
          prop "sconcat" $ \(vs :: [Vector P Int]) ->
            V.sconcat vs !==! VP.concat (fmap toPrimitiveVector vs)
      describe "Predicates" $ do
        describe "Searching" $ do
          prop "sfilter" $ \(v :: Vector P Word) (f :: Fun Word Bool) ->
            V.findIndex (apply f) v === VP.findIndex (apply f) (toPrimitiveVector v)
        describe "Filtering" $ do
          prop "sfilter" $ \(v :: Vector P Word) (f :: Fun Word Bool) ->
            V.sfilter (apply f) v !==! VP.filter (apply f) (toPrimitiveVector v)
          prop "sifilter" $ \(v :: Vector P Word) (f :: Fun (Int, Word) Bool) ->
            V.sifilter (applyFun2 f) v !==! VP.ifilter (applyFun2 f) (toPrimitiveVector v)
          prop "sfilterM" prop_sfilterM
          prop "sifilterM" prop_sifilterM
          prop "smapMaybe" $ \(v :: Vector P Word) (f :: Fun Word (Maybe Int)) ->
            V.smapMaybe (apply f) v !==! VP.mapMaybe (apply f) (toPrimitiveVector v)
          prop "simapMaybe" $ \(v :: Vector P Word) (f :: Fun (Int, Word) (Maybe Int)) ->
            V.simapMaybe (applyFun2 f) v !==! VP.imapMaybe (applyFun2 f) (toPrimitiveVector v)
          prop "scatMaybes" $ \(v :: Vector D (Maybe Word)) ->
            V.scatMaybes v !==! toPrimitiveVector (compute (smap fromJust (sfilter isJust v)))
          prop "smapMaybeM" prop_smapMaybeM
        describe "Mapping" $ do
          prop "fmap" $ \(v :: Vector DS Word) (f :: Fun Word Int) ->
            fmap (apply f) v !==! VP.map (apply f) (toPrimitiveVector (compute v))
          prop "<$" $ \(v :: Vector DS Word) (a :: Char) ->
            (a <$ v) !==! VP.replicate (length v) a
          prop "smap" $ \(v :: Vector P Word) (f :: Fun Word Int) ->
            V.smap (apply f) v !==! VP.map (apply f) (toPrimitiveVector v)
          prop "simap" $ \(v :: Vector P Word) (f :: Fun (Int, Word) Int) ->
            V.simap (applyFun2 f) v !==! VP.imap (applyFun2 f) (toPrimitiveVector v)
          prop "straverse" prop_straverse
          prop "sitraverse" prop_sitraverse
          prop "smapM" prop_smapM
          prop "simapM" prop_simapM
          prop "smapM" prop_smapM_
          prop "simapM" prop_simapM_
          prop "szip" prop_szip
          prop "szip3" prop_szip3
          prop "szip4" prop_szip4
          prop "szip5" prop_szip5
          prop "szip6" prop_szip6
          prop "szipWith" prop_szipWith
          prop "szipWith3" prop_szipWith3
          prop "szipWith4" prop_szipWith4
          prop "szipWith5" prop_szipWith5
          prop "szipWith6" prop_szipWith6
          prop "sizipWith" prop_sizipWith
          prop "sizipWith3" prop_sizipWith3
          prop "sizipWith4" prop_sizipWith4
          prop "sizipWith5" prop_sizipWith5
          prop "sizipWith6" prop_sizipWith6
          prop "liftA2" $ \(v1 :: Vector DS Word) (v2 :: Vector DS Int) (f :: Fun (Word, Int) Int) ->
            liftA2 (applyFun2 f) v1 v2
              !==! toPrimV2 (VP.zipWith (applyFun2 f)) (compute v1) (compute v2)
          prop "szipWithM" prop_szipWithM
          prop "szipWith3M" prop_szipWith3M
          prop "szipWith4M" prop_szipWith4M
          prop "szipWith5M" prop_szipWith5M
          prop "szipWith6M" prop_szipWith6M
          prop "sizipWithM" prop_sizipWithM
          prop "sizipWith3M" prop_sizipWith3M
          prop "sizipWith4M" prop_sizipWith4M
          prop "sizipWith5M" prop_sizipWith5M
          prop "sizipWith6M" prop_sizipWith6M
          prop "szipWithM_" prop_szipWithM_
          prop "szipWith3M_" prop_szipWith3M_
          prop "szipWith4M_" prop_szipWith4M_
          prop "szipWith5M_" prop_szipWith5M_
          prop "szipWith6M_" prop_szipWith6M_
          prop "sizipWithM_" prop_sizipWithM_
          prop "sizipWith3M_" prop_sizipWith3M_
          prop "sizipWith4M_" prop_sizipWith4M_
          prop "sizipWith5M_" prop_sizipWith5M_
          prop "sizipWith6M_" prop_sizipWith6M_
      describe "Folding" $ do
        prop "sfoldl" $ \(v :: Vector P Word32) (f :: Fun (Word, Word32) Word) a0 ->
          V.sfoldl (applyFun2 f) a0 v === VP.foldl (applyFun2 f) a0 (toPrimitiveVector v)
        prop "sifoldl" $ \(v :: Vector P Word32) (f :: Fun (Word, Ix1, Word32) Word) a0 ->
          V.sifoldl (applyFun3 f) a0 v === VP.ifoldl (applyFun3 f) a0 (toPrimitiveVector v)
        prop "sfoldl1'" prop_sfoldl1'
        describe "Specialized" $ do
          prop "sor" $ \(v :: Vector S Bool) -> V.sor v === VS.or (toStorableVector v)
          prop "sand" $ \(v :: Vector S Bool) -> V.sand v === VS.and (toStorableVector v)
          prop "sany" $ \(v :: Vector P Word) (f :: Fun Word Bool) ->
            V.sany (apply f) v === VP.any (apply f) (toPrimitiveVector v)
          prop "sall" $ \(v :: Vector P Word) (f :: Fun Word Bool) ->
            V.sall (apply f) v === VP.all (apply f) (toPrimitiveVector v)
          prop "ssum" $ \(v :: Vector P Word) -> V.ssum v === VP.sum (toPrimitiveVector v)
          prop "sproduct" $ \(v :: Vector P Word) ->
            V.sproduct v === VP.product (toPrimitiveVector v)
          prop "maximum'" prop_maximum'
          prop "minimum'" prop_minimum'
          prop "maximumM" prop_maximumM
          prop "minimumM" prop_minimumM
      describe "Conversion" $
        describe "Lists" $ do
          prop "sfromList" $ \comp (xs :: [Word]) ->
            sfromList xs !==! toPrimitiveVector (fromList comp xs)
          prop "sfromList" $ \(xs :: [Word]) -> sfromList xs !==! VP.fromList xs
          prop "sfromListN" $ \sz@(Sz n) (xs :: [Word]) -> sfromListN sz xs !==! VP.fromListN n xs
          prop "sfromListN (maxBound)" $ \(xs :: [Word]) ->
            sfromListN (Sz (maxBound `div` 8)) xs !==! VP.fromList xs
          prop "unsafeFromListN" $ \sz@(Sz n) (xs :: [Word]) ->
            A.unsafeFromListN sz xs !==! VP.fromListN n xs

prop_sfoldl1' :: Vector P Word -> Fun (Word, Word) Word -> Property
prop_sfoldl1' v f =
  V.singleton @D (V.sfoldl1' (applyFun2 f) v)
    !!==!! VP.singleton (VP.foldl1' (applyFun2 f) (toPrimitiveVector v))

prop_maximum' :: Vector P Word -> Property
prop_maximum' v =
  V.singleton @D (V.smaximum' v) !!==!! VP.singleton (VP.maximum (toPrimitiveVector v))

prop_minimum' :: Vector P Word -> Property
prop_minimum' v =
  V.singleton @D (V.sminimum' v) !!==!! VP.singleton (VP.minimum (toPrimitiveVector v))

prop_maximumM :: Vector P Word -> Property
prop_maximumM v =
  let vp = toPrimitiveVector v
   in V.smaximumM v === (guard (not (VP.null vp)) >> Just (VP.maximum vp))

prop_minimumM :: Vector P Word -> Property
prop_minimumM v =
  let vp = toPrimitiveVector v
   in V.sminimumM v === (guard (not (VP.null vp)) >> Just (VP.minimum vp))

prop_sitraverse_itraverseA :: SeedVector -> Vector S Word -> Property
prop_sitraverse_itraverseA seed a =
  withSeed2 @(V.Vector P Word)
    seed
    (fmap compute . genWithIMapM (`V.sitraverse` a))
    (genWithIMapM (`itraverseA` a))

prop_straverse_traversePrim :: SeedVector -> Vector S Word -> Property
prop_straverse_traversePrim seed a =
  withSeed2 @(V.Vector P Word)
    seed
    (fmap compute . genWithIMapM (\f -> V.straverse (f 0) a))
    (genWithIMapM (\f -> traversePrim (f 0) a))

prop_sitraverse_itraversePrim :: SeedVector -> Array P Ix3 Word -> Property
prop_sitraverse_itraversePrim seed a =
  withSeed2 @(V.Vector P Word)
    seed
    (genWithIMapM (\f -> compute <$> V.sitraverse (xorToLinear f) a))
    (genWithIMapM (\f -> flatten <$> itraversePrim @P (xorToLinear f) a))
  where
    xorToLinear f i = f (foldlIndex xor 0 i)

prop_smapM_traverseA_ :: SeedVector -> Array P Ix2 Word -> Property
prop_smapM_traverseA_ seed a =
  withSeed seed (genWithMapM_ (`V.smapM_` a)) === withSeed seed (genWithMapM_ (`traverseA_` a))

prop_simapM_itraverseA_ :: SeedVector -> Array P Ix2 Word -> Property
prop_simapM_itraverseA_ seed a =
  withSeed2
    seed
    (genWithIMapM_ (\f -> V.simapM_ (xorToLinear f) a))
    (genWithIMapM_ (\f -> itraverseA_ (xorToLinear f) a))
  where
    xorToLinear f i = f (foldlIndex xor 0 i)

prop_sforM_forM :: SeedVector -> Vector S Word -> Expectation
prop_sforM_forM seed a =
  withSeed @(V.Vector P Word) seed (fmap compute . genWithMapM (V.sforM a))
    `shouldBe` withSeed seed (genWithMapM (A.forM a))

prop_siforM_iforM :: SeedVector -> Vector S Word -> Expectation
prop_siforM_iforM seed a =
  withSeed @(V.Vector P Word) seed (fmap compute . genWithIMapM (V.siforM a))
    `shouldBe` withSeed seed (genWithIMapM (iforM a))

withSeedIO :: forall a. SeedVector -> (MWC.Gen (PrimState IO) -> IO a) -> IO a
withSeedIO (SeedVector seed) f = MWC.initialize seed >>= f

prop_sforM_forIO :: SeedVector -> Vector S Word -> Expectation
prop_sforM_forIO seed a =
  withSeedIO seed (genWithMapM (forIO (setComp Seq a)))
    `shouldReturn` withSeed @(V.Vector P Word) seed (fmap compute . genWithMapM (V.sforM a))

prop_siforM_iforIO :: SeedVector -> Vector S Word -> Expectation
prop_siforM_iforIO seed a =
  withSeedIO seed (genWithIMapM (iforIO (setComp (ParN 1) a)))
    `shouldReturn` withSeed @(V.Vector P Word) seed (fmap compute . genWithIMapM (V.siforM a))

prop_sforM_forM_ :: SeedVector -> Vector S Word -> Expectation
prop_sforM_forM_ seed a =
  withSeed seed (genWithMapM_ (A.forM_ a))
    `shouldBe` withSeed @Word seed (genWithMapM_ (V.sforM_ a))

prop_siforM_iforM_ :: SeedVector -> Vector S Word -> Expectation
prop_siforM_iforM_ seed a =
  withSeed seed (genWithIMapM_ (iforM_ a))
    `shouldBe` withSeed @Word seed (genWithIMapM_ (V.siforM_ a))

prop_sforM_forIO_ :: SeedVector -> Vector S Word -> Expectation
prop_sforM_forIO_ seed a =
  withSeedIO seed (genWithMapM_ (forIO_ (setComp (ParN 1) a)))
    `shouldReturn` withSeed @Word seed (genWithMapM_ (V.sforM_ a))

prop_siforM_iforIO_ :: SeedVector -> Vector S Word -> Expectation
prop_siforM_iforIO_ seed a =
  withSeedIO seed (genWithIMapM_ (iforIO_ (setComp (ParN 1) a)))
    `shouldReturn` withSeed @Word seed (genWithIMapM_ (V.siforM_ a))

prop_siforM_iforWS :: SeedVector -> Vector S Word -> Expectation
prop_siforM_iforWS seed@(SeedVector sv) a = do
  wsArray <-
    do
      ws <- initWorkerStates (ParN 1) (const (MWC.initialize sv))
      genWithIMapWS (iforWS ws a)
  wsArray `shouldBe` withSeed @(V.Vector P Word) seed (fmap compute . genWithIMapM (V.siforM a))

prop_smapM_mapWS :: SeedVector -> Vector S Word -> Expectation
prop_smapM_mapWS seed@(SeedVector sv) a = do
  wsArray <-
    do
      ws <- initWorkerStates Seq (const (MWC.initialize sv))
      genWithMapWS (\f -> mapWS ws f a)
  wsArray `shouldBe` withSeed @(V.Vector P Word) seed (fmap compute . genWithMapM (`V.smapM` a))
