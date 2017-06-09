{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Data.Array.Massiv.ManifestSpec (spec) where

import           Data.Array.Massiv.Common
import           Data.Array.Massiv.CommonSpec (Arr (..))
import           Data.Array.Massiv.Manifest
import           Data.Array.Massiv.Mutable
import           Data.Typeable
import           Test.Hspec
import           Test.QuickCheck
import Data.Vector.Generic as VG

prop_toFromVector
  :: ( Arbitrary (Arr r ix Int)
     , VG.Vector (VRepr r) Int
     , Target r ix Int
     , Typeable (VRepr r)
     , ARepr (VRepr r) ~ r
     , Eq (Array r ix Int)
     )
  => proxy ix -> r -> Arr r ix Int -> Property
prop_toFromVector _ _ (Arr arr) =
  Just arr === (toVector arr >>= fromVector (size arr))


toFromVector :: Spec
toFromVector = do
  describe "Unboxed" $ do
    it "DIM1" $ property $ prop_toFromVector (Nothing :: Maybe DIM1) U
    it "DIM2" $ property $ prop_toFromVector (Nothing :: Maybe DIM2) U
    it "DIM3" $ property $ prop_toFromVector (Nothing :: Maybe DIM3) U
  describe "Storable" $ do
    it "DIM1" $ property $ prop_toFromVector (Nothing :: Maybe DIM1) S
    it "DIM2" $ property $ prop_toFromVector (Nothing :: Maybe DIM2) S
    it "DIM3" $ property $ prop_toFromVector (Nothing :: Maybe DIM3) S
  describe "Primitive" $ do
    it "DIM1" $ property $ prop_toFromVector (Nothing :: Maybe DIM1) P
    it "DIM2" $ property $ prop_toFromVector (Nothing :: Maybe DIM2) P
    it "DIM3" $ property $ prop_toFromVector (Nothing :: Maybe DIM3) P
  describe "Boxed" $ do
    it "DIM1" $ property $ prop_toFromVector (Nothing :: Maybe DIM1) B
    it "DIM2" $ property $ prop_toFromVector (Nothing :: Maybe DIM2) B
    it "DIM3" $ property $ prop_toFromVector (Nothing :: Maybe DIM3) B



spec :: Spec
spec = describe "toFromVector" toFromVector
