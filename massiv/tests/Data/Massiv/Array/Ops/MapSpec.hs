{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Massiv.Array.Ops.MapSpec (spec) where

import           Data.Massiv.CoreArbitrary as A
import           Prelude                   as P
import           Test.Hspec
import           Test.QuickCheck

prop_zipUnzip ::
     (Arbitrary ix, CoArbitrary ix, Index ix, Show (Array D ix Int))
  => proxy ix
  -> Array D ix Int
  -> Array D ix Int
  -> Property
prop_zipUnzip _ arr1 arr2 =
  (extract' zeroIndex sz arr1, extract' zeroIndex sz arr2) === A.unzip (A.zip arr1 arr2)
  where sz = liftIndex2 min (size arr1) (size arr2)

prop_zipFlip ::
     (Arbitrary ix, CoArbitrary ix, Index ix, Show (Array D ix Int), Show (Array D ix (Int, Int)))
  => proxy ix
  -> Array D ix Int
  -> Array D ix Int
  -> Property
prop_zipFlip _ arr1 arr2 =
  A.zip arr1 arr2 ===
  A.map (\(e2, e1) -> (e1, e2)) (A.zip arr2 arr1)

prop_zipUnzip3 ::
     (Arbitrary ix, CoArbitrary ix, Index ix, Show (Array D ix Int))
  => proxy ix
  -> Array D ix Int
  -> Array D ix Int
  -> Array D ix Int
  -> Property
prop_zipUnzip3 _ arr1 arr2 arr3 =
  (extract' zeroIndex sz arr1, extract' zeroIndex sz arr2, extract' zeroIndex sz arr3) ===
  A.unzip3 (A.zip3 arr1 arr2 arr3)
  where
    sz = liftIndex2 min (liftIndex2 min (size arr1) (size arr2)) (size arr3)

prop_zipFlip3 ::
     ( Arbitrary ix
     , CoArbitrary ix
     , Index ix
     , Show (Array D ix Int)
     , Show (Array D ix (Int, Int, Int))
     )
  => proxy ix
  -> Array D ix Int
  -> Array D ix Int
  -> Array D ix Int
  -> Property
prop_zipFlip3 _ arr1 arr2 arr3 =
  A.zip3 arr1 arr2 arr3 === A.map (\(e3, e2, e1) -> (e1, e2, e3)) (A.zip3 arr3 arr2 arr1)

mapSpec ::
     ( Arbitrary ix
     , CoArbitrary ix
     , Index ix
     , Show (Array D ix Int)
     , Show (Array D ix (Int, Int))
     , Show (Array D ix (Int, Int, Int))
     )
  => proxy ix
  -> Spec
mapSpec proxy = do
  describe "Zipping" $ do
    it "zipUnzip" $ property $ prop_zipUnzip proxy
    it "zipFlip" $ property $ prop_zipFlip proxy
    it "zipUnzip3" $ property $ prop_zipUnzip3 proxy
    it "zipFlip3" $ property $ prop_zipFlip3 proxy

spec :: Spec
spec = do
  describe "Ix1" $ mapSpec (Nothing :: Maybe Ix1)
  describe "Ix2" $ mapSpec (Nothing :: Maybe Ix2)
  describe "Ix3" $ mapSpec (Nothing :: Maybe Ix3)
  describe "Ix4" $ mapSpec (Nothing :: Maybe Ix4)
