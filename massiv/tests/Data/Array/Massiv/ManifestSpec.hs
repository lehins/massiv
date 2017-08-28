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
import           Data.Vector.Generic          as VG
import           Test.Hspec
import           Test.QuickCheck

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
    it "Ix1" $ property $ prop_toFromVector (Nothing :: Maybe Ix1) U
    it "Ix2" $ property $ prop_toFromVector (Nothing :: Maybe Ix2) U
    it "Ix3" $ property $ prop_toFromVector (Nothing :: Maybe Ix3) U
  describe "Storable" $ do
    it "Ix1" $ property $ prop_toFromVector (Nothing :: Maybe Ix1) S
    it "Ix2" $ property $ prop_toFromVector (Nothing :: Maybe Ix2) S
    it "Ix3" $ property $ prop_toFromVector (Nothing :: Maybe Ix3) S
  describe "Primitive" $ do
    it "Ix1" $ property $ prop_toFromVector (Nothing :: Maybe Ix1) P
    it "Ix2" $ property $ prop_toFromVector (Nothing :: Maybe Ix2) P
    it "Ix3" $ property $ prop_toFromVector (Nothing :: Maybe Ix3) P
  describe "Boxed" $ do
    it "Ix1" $ property $ prop_toFromVector (Nothing :: Maybe Ix1) B
    it "Ix2" $ property $ prop_toFromVector (Nothing :: Maybe Ix2) B
    it "Ix3" $ property $ prop_toFromVector (Nothing :: Maybe Ix3) B



spec :: Spec
spec = describe "toFromVector" toFromVector
