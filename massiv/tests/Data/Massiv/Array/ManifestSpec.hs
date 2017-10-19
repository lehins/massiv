{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module Data.Massiv.Array.ManifestSpec (spec) where

import           Data.Massiv.CoreArbitrary
import           Data.Massiv.Array.Manifest
import           Data.Proxy
import           Data.Typeable
import qualified Data.Vector                  as VB
import qualified Data.Vector.Generic          as VG
import qualified Data.Vector.Primitive        as VP
import qualified Data.Vector.Storable         as VS
import qualified Data.Vector.Unboxed          as VU
import           Test.Hspec
import           Test.QuickCheck

prop_castToFromVector
  :: ( Arbitrary (Arr r ix Int)
     , VG.Vector (VRepr r) Int
     , Mutable r ix Int
     , Typeable (VRepr r)
     , ARepr (VRepr r) ~ r
     , Eq (Array r ix Int)
     , Show (Array r ix Int)
     )
  => proxy ix -> r -> Arr r ix Int -> Property
prop_castToFromVector _ _ (Arr arr) =
  Just arr === (castToVector arr >>= castFromVector (getComp arr) (size arr))


prop_toFromVector ::
     forall r ix v.
     ( Arbitrary (Arr r ix Int)
     , Mutable r ix Int
     , Mutable (ARepr v) ix Int
     , VRepr (ARepr v) ~ v
     , Eq (Array r ix Int)
     , VG.Vector v Int
     , Show (Array r ix Int)
     , Typeable v
     )
  => Proxy v
  -> Proxy ix
  -> r
  -> Arr r ix Int
  -> Property
prop_toFromVector _ _ _ (Arr arr) =
  arr === fromVector (getComp arr) (size arr) (toVector arr :: v Int)

toFromVectorSpec :: Spec
toFromVectorSpec  = do
  let it_prop name r = describe name $ do
        describe "Through Boxed Vector" $ do
          it "Ix1" $ property $ prop_toFromVector (Proxy :: Proxy VB.Vector) (Proxy :: Proxy Ix1) r
          it "Ix2" $ property $ prop_toFromVector (Proxy :: Proxy VB.Vector) (Proxy :: Proxy Ix2) r
        describe "Through Unboxed Vector" $ do
          it "Ix1" $ property $ prop_toFromVector (Proxy :: Proxy VU.Vector) (Proxy :: Proxy Ix1) r
          it "Ix2" $ property $ prop_toFromVector (Proxy :: Proxy VU.Vector) (Proxy :: Proxy Ix2) r
        describe "Through Primitive Vector" $ do
          it "Ix1" $ property $ prop_toFromVector (Proxy :: Proxy VP.Vector) (Proxy :: Proxy Ix1) r
          it "Ix2" $ property $ prop_toFromVector (Proxy :: Proxy VP.Vector) (Proxy :: Proxy Ix2) r
        describe "Through Storable Vector" $ do
          it "Ix1" $ property $ prop_toFromVector (Proxy :: Proxy VS.Vector) (Proxy :: Proxy Ix1) r
          it "Ix2" $ property $ prop_toFromVector (Proxy :: Proxy VS.Vector) (Proxy :: Proxy Ix2) r
  it_prop "Unboxed" U
  it_prop "Primitive" P
  it_prop "Storable" S
  it_prop "BoxedStrict" B


castToFromVectorSpec :: Spec
castToFromVectorSpec  = do
  let it_prop name r = describe name $ do
        it "Ix1" $ property $ prop_castToFromVector (Proxy :: Proxy Ix1) r
        it "Ix2" $ property $ prop_castToFromVector (Proxy :: Proxy Ix2) r
        it "Ix3" $ property $ prop_castToFromVector (Proxy :: Proxy Ix3) r
  it_prop "Unboxed" U
  it_prop "Primitive" P
  it_prop "Storable" S
  it_prop "BoxedStrict" B


spec :: Spec
spec = do
  describe "toFromVector" toFromVectorSpec
  describe "castToFromVector" castToFromVectorSpec
