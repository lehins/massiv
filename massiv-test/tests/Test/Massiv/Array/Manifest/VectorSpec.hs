{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Massiv.Array.Manifest.VectorSpec (spec) where

import Data.Massiv.Array as A
import Data.Massiv.Array.Manifest.Vector
import qualified Data.Vector as VB
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Test.Massiv.Core

prop_castToFromVector
  :: ( VG.Vector (VRepr r) Int
     , Manifest r Int
     , Typeable (VRepr r)
     , ARepr (VRepr r) ~ r
     , Eq (Array r ix Int)
     , Show (Array r ix Int)
     , Index ix
     )
  => proxy ix
  -> r
  -> ArrNE r ix Int
  -> Property
prop_castToFromVector _ _ (ArrNE arr) =
  Just arr === (castToVector arr >>= castFromVector (getComp arr) (size arr))

prop_toFromVector
  :: forall r ix v
   . ( Manifest r Int
     , Manifest (ARepr v) Int
     , VRepr (ARepr v) ~ v
     , Eq (Array r ix Int)
     , VG.Vector v Int
     , Show (Array r ix Int)
     , Typeable v
     , Load (ARepr v) ix Int
     , Load r ix Int
     )
  => Proxy v
  -> Proxy ix
  -> r
  -> ArrNE r ix Int
  -> Property
prop_toFromVector _ _ _ (ArrNE arr) =
  let comp = getComp arr
      arr' = fromVector' comp (size arr) (toVector arr :: v Int)
   in arr' === arr .&&. (getComp arr' === comp)

toFromVectorSpec :: Spec
toFromVectorSpec = do
  it_prop "Unboxed" U
  it_prop "Primitive" P
  it_prop "Storable" S
  it_prop "BoxedStrict" BL
  where
    it_prop name r =
      describe name $ do
        describe "CastToFrom" $ do
          it "Ix1" $ property $ prop_castToFromVector (Proxy :: Proxy Ix1) r
          it "Ix2" $ property $ prop_castToFromVector (Proxy :: Proxy Ix2) r
          it "Ix3" $ property $ prop_castToFromVector (Proxy :: Proxy Ix3) r
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

spec :: Spec
spec = describe "toFromVector" toFromVectorSpec
