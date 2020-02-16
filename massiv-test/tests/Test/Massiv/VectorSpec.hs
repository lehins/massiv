{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
      shouldThrow (pure $! computeAs P arr) sizeException

newtype SeedVector = SeedVector (VP.Vector Word32) deriving (Eq, Show)

instance Arbitrary SeedVector where
  arbitrary = SeedVector . VP.fromList <$> arbitrary

withSeed :: forall a. SeedVector -> (forall s. MWC.Gen s -> ST s a) -> a
withSeed (SeedVector seed) f = runST $ do
  gen <- MWC.initialize seed
  f gen

prop_sreplicateM :: SeedVector -> Int -> Property
prop_sreplicateM seed k =
  withSeed @(V.Vector DS Word) seed (V.sreplicateM (Sz k) . uniform)
  !==! withSeed seed (VP.replicateM k . uniform)

prop_sgenerateM :: SeedVector -> Int -> Fun Int Word -> Property
prop_sgenerateM seed k f =
  withSeed @(V.Vector DS Word) seed (genWith (V.sgenerateM (Sz k)))
  !==! withSeed seed (genWith (VP.generateM k))
  where
    genWith :: PrimMonad f => ((Int -> f Word) -> t) -> MWC.Gen (PrimState f) -> t
    genWith genM gen = genM (\i -> xor (apply f i) <$> uniform gen)


prop_siterateNM :: SeedVector -> Int -> Word -> Property
prop_siterateNM seed k a =
  withSeed @(V.Vector DS Word) seed (genWith (\action -> V.siterateNM (Sz k) action a))
  !==! withSeed seed (genWith (\action -> VP.iterateNM k action a))
  where
    genWith :: PrimMonad f => ((Word -> f Word) -> t) -> MWC.Gen (PrimState f) -> t
    genWith genM gen = genM (\prev -> xor prev <$> uniform gen)

--slicingSpec = 

spec :: Spec
spec = do
  describe "Vector" $ do
    -- describe "same-as-array" $ do
    --   prop "sliceAt'" $ \sz (arr :: Array P Ix1 Word) ->
    --     let ~(l, r) = V.sliceAt' sz arr
    --         ~(l', r') = splitAt' 1 (unSz sz) arr
    --      in l !!==!! toPrimitiveVector (compute l') .&&. r !!==!! toPrimitiveVector (compute r')
    describe "same-as-vector-package" $ do
      describe "Accessors" $ do
        describe "Size" $ do
          it "slength" $ do
            slength (sfromList []) `shouldBe` Nothing
            slength (sfromListN 1 []) `shouldBe` Nothing
            slength (sgenerate 1 id) `shouldBe` Just 1
          it "snull" $ do
            snull sempty `shouldBe` True
            snull (fromLists' Seq [[]] :: Array P Ix2 Int) `shouldBe` True
            snull (siterateN 3 id ()) `shouldBe` False
            snull (0 ..: 1 :> 2 :> 3 :. 0) `shouldBe` True
        describe "Indexing" $ do
          prop "head'" $ \(ArrNE arr :: ArrNE D Ix1 Int) ->
            head' arr === evaluate' arr 0 .&&. head' arr === shead' arr
          prop "shead'" $ \(arr :: Array P Ix1 Int) ->
            (singleton (shead' arr) :: Array D Ix1 Int) !!==!!
            VP.singleton (VP.head (toPrimitiveVector arr))
          prop "last'" $ \(arr :: Array P Ix1 Int) ->
            (singleton (last' arr) :: Array D Ix1 Int) !!==!!
            VP.singleton (VP.last (toPrimitiveVector arr))
        describe "Slicing" $ do
          prop "slice" $ \i sz (arr :: Array P Ix1 Word) ->
            V.slice i sz arr !!==!! VP.take (unSz sz) (VP.drop i (toPrimitiveVector arr))
          prop "sslice" $ \i sz (arr :: Array P Ix1 Word) ->
            computeAs B (V.sslice i sz arr) !!==!!
            VP.take (unSz sz) (VP.drop i (toPrimitiveVector arr))
          prop "slice'" $ \i sz (arr :: Array P Ix1 Word) ->
            V.slice' i sz arr !!==!! VP.slice i (unSz sz) (toPrimitiveVector arr)
          prop "init'" $ \(arr :: Array P Ix1 Word) ->
            V.init' arr !!==!! VP.init (toPrimitiveVector arr)
          prop "tail'" $ \(arr :: Array P Ix1 Word) ->
            V.tail' arr !!==!! VP.tail (toPrimitiveVector arr)
          prop "take" $ \n (arr :: Array P Ix1 Word) ->
            V.take (Sz n) arr !==! VP.take n (toPrimitiveVector arr)
          prop "stake" $ \n (arr :: Array P Ix1 Word) ->
            V.stake (Sz n) arr !==! VP.take n (toPrimitiveVector arr)
          prop "drop" $ \n (arr :: Array P Ix1 Word) ->
            V.drop (Sz n) arr !==! VP.drop n (toPrimitiveVector arr)
          prop "sdrop" $ \n (arr :: Array P Ix1 Word) ->
            V.sdrop (Sz n) arr !==! VP.drop n (toPrimitiveVector arr)
          prop "sliceAt" $ \sz (arr :: Array P Ix1 Word) ->
            let (larr, rarr) = V.sliceAt (Sz sz) arr
                (lvec, rvec) = VP.splitAt sz (toPrimitiveVector arr)
             in (larr !==! lvec) .&&. (rarr !==! rvec)
      describe "Constructors" $ do
        describe "Initialization" $ do
          it "empty" $ toPrimitiveVector (V.empty :: V.Vector P Word) `shouldBe` VP.empty
          it "sempty" $
            toPrimitiveVector (compute (V.sempty :: V.Vector DS Word)) `shouldBe` VP.empty
          prop "singleton" $ \e -> (V.singleton e :: V.Vector P Word) !==! VP.singleton e
          prop "ssingleton" $ \(e :: Word) -> V.ssingleton e !==! VP.singleton e
          prop "replicate" $ \comp k (e :: Word) -> V.replicate comp (Sz k) e !==! VP.replicate k e
          prop "sreplicate" $ \k (e :: Word) -> V.sreplicate (Sz k) e !==! VP.replicate k e
          prop "generate" $ \comp k (f :: Fun Int Word) ->
            V.generate comp (Sz k) (apply f) !==! VP.generate k (apply f)
          prop "sgenerate" $ \k (f :: Fun Int Word) ->
            V.sgenerate (Sz k) (apply f) !==! VP.generate k (apply f)
          prop "siterateN" $ \n (f :: Fun Word Word) a ->
            V.siterateN (Sz n) (apply f) a !==! VP.iterateN n (apply f) a
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
             in V.sunfoldrN (Sz n) f a !==! VP.unfoldrN n f a
      describe "Conversion" $ do
        describe "Lists" $ do
          prop "sfromList" $ \ comp (xs :: [Word]) ->
            sfromList xs !==! toPrimitiveVector (fromList comp xs)
          prop "sfromList" $ \(xs :: [Word]) ->
            sfromList xs !==! VP.fromList xs
          -- prop "sfromListN" $ \ n (xs :: [Word]) ->
          --   sfromListN n xs !==! VP.fromListN n xs
