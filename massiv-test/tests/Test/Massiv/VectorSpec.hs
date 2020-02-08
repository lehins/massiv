{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Test.Massiv.VectorSpec (spec) where

import Control.Exception
import Data.Bits
import Data.Massiv.Array as A
import Data.Massiv.Vector as V
import qualified Data.Vector.Primitive as VP
import Data.Word
import Test.Massiv.Core

import System.Random.MWC as MWC

infix 4 !==!, !!==!!

sizeException :: SizeException -> Bool
sizeException _ = True

(!==!) :: (Eq e, Show e, Prim e, Load r Ix1 e) => V.Vector r e -> VP.Vector e -> Property
(!==!) arr vec = toPrimitiveVector (convert arr) === vec

(!!==!!) :: (Eq e, Show e, Prim e, Source r Ix1 e) => V.Vector r e -> VP.Vector e -> Property
(!!==!!) arr vec = property $ do
  eRes <- try (pure $! vec)
  case eRes of
    Right vec' -> toPrimitiveVector (computeSource arr) `shouldBe` vec'
    Left (_exc :: ErrorCall) ->
      shouldThrow (pure $! toPrimitiveVector (computeSource arr)) sizeException

newtype SeedVector = SeedVector (VP.Vector Word32) deriving (Eq, Show)

instance Arbitrary SeedVector where
  arbitrary = SeedVector . VP.fromList <$> arbitrary

withSeed :: forall a. SeedVector -> (forall s. MWC.Gen s -> ST s a) -> a
withSeed (SeedVector seed) f = runST $ do
  gen <- MWC.initialize seed
  f gen

prop_replicateM :: SeedVector -> Int -> Property
prop_replicateM seed k =
  withSeed @(V.Vector DS Word) seed (V.replicateM (Sz k) . uniform)
  !==! withSeed seed (VP.replicateM k . uniform)

prop_generateM :: SeedVector -> Int -> Fun Int Word -> Property
prop_generateM seed k f =
  withSeed @(V.Vector DS Word) seed (genWith (V.generateM (Sz k)))
  !==! withSeed seed (genWith (VP.generateM k))
  where
    genWith :: PrimMonad f => ((Int -> f Word) -> t) -> MWC.Gen (PrimState f) -> t
    genWith genM gen = genM (\i -> xor (apply f i) <$> uniform gen)


prop_iterateNM :: SeedVector -> Int -> Word -> Property
prop_iterateNM seed k a =
  withSeed @(V.Vector DS Word) seed (genWith (\action -> V.iterateNM (Sz k) action a))
  !==! withSeed seed (genWith (\action -> VP.iterateNM k action a))
  where
    genWith :: PrimMonad f => ((Word -> f Word) -> t) -> MWC.Gen (PrimState f) -> t
    genWith genM gen = genM (\prev -> xor prev <$> uniform gen)


spec :: Spec
spec = do
  describe "Vector" $ do
    describe "same-as-vector-package" $ do
      describe "Accessors" $ do
        describe "Slicing" $ do
          prop "slice'" $ \i sz (arr :: Array P Ix1 Word) ->
            V.slice' i sz arr !!==!! VP.slice i (unSz sz) (toPrimitiveVector arr)
          prop "init'" $ \(arr :: Array P Ix1 Word) ->
            V.init' arr !!==!! VP.init (toPrimitiveVector arr)
          prop "tail'" $ \(arr :: Array P Ix1 Word) ->
            V.tail' arr !!==!! VP.tail (toPrimitiveVector arr)
          prop "take" $ \n (arr :: Array P Ix1 Word) ->
            V.take (Sz n) arr !==! VP.take n (toPrimitiveVector arr)
          prop "drop" $ \n (arr :: Array P Ix1 Word) ->
            V.drop (Sz n) arr !==! VP.drop n (toPrimitiveVector arr)
          prop "splitAt" $ \sz (arr :: Array P Ix1 Word) ->
            let (larr, rarr) = V.splitAt (Sz sz) arr
                (lvec, rvec) = VP.splitAt sz (toPrimitiveVector arr)
             in (larr !==! lvec) .&&. (rarr !==! rvec)
      describe "Constructors" $ do
        describe "Initialization" $ do
          it "empty" $ toPrimitiveVector (V.empty :: V.Vector P Word) `shouldBe` VP.empty
          prop "singleton" $ \e -> (V.singleton e :: V.Vector P Word) !==! VP.singleton e
          prop "replicate" $ \comp k (e :: Word) -> V.replicate comp (Sz k) e !==! VP.replicate k e
          prop "generate" $ \comp k (f :: Fun Int Word) ->
            V.generate comp (Sz k) (apply f) !==! VP.generate k (apply f)
          prop "iterateN" $ \n (f :: Fun Word Word) a ->
            V.iterateN (Sz n) (apply f) a !==! VP.iterateN n (apply f) a
        describe "Monadic initialization" $ do
          prop "replicateM" prop_replicateM
          prop "generateM" prop_generateM
          prop "iterateNM" prop_iterateNM
        describe "Unfolding" $ do
          prop "unfoldr" $ \(a :: Word) ->
            let f b
                  | b > 10000 || b `div` 17 == 0 = Nothing
                  | otherwise = Just (b * b, b + 1)
             in V.unfoldr f a !==! VP.unfoldr f a
          prop "unfoldrN" $ \n (a :: Word) ->
            let f b
                  | b > 10000 || b `div` 19 == 0 = Nothing
                  | otherwise = Just (b * b, b + 1)
             in V.unfoldrN (Sz n) f a !==! VP.unfoldrN n f a
