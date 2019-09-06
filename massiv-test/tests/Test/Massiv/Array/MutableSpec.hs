{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Test.Massiv.Array.MutableSpec (spec) where

import Data.Massiv.Array
import Test.Massiv.Core
import Test.Massiv.Core.Mutable
import Test.Massiv.Array.Delayed
import Test.Massiv.Array.Mutable
import GHC.Exts

type MutableArraySpec r ix e
   = ( Show e
     , Eq e
     , Eq (Array r ix e)
     , Show (Array r ix e)
     , Eq (Array (R r) Ix1 e)
     , Show (Array (R r) Ix1 e)
     , Load (R r) ix e
     , Extract r ix e
     , Resize r ix
     , Arbitrary (Array r ix e)
     , Mutable r ix e
     , Stream r ix e
     , Construct r ix e)

type MutableSpec r e
   = ( Typeable e
     , Arbitrary e
     , CoArbitrary e
     , Function e
     , IsList (Array r Ix1 e)
     , Item (Array r Ix1 e) ~ e
     , MutableArraySpec r Ix1 e
     , MutableArraySpec r Ix2 e
     , MutableArraySpec r Ix3 e
     , MutableArraySpec r Ix4 e
     , MutableArraySpec r Ix5 e)

streamSpecR :: forall r ix e. (MutableArraySpec r ix e) => Spec
streamSpecR =
  describe "toStream/toList" $
    it "toStream" $ property (prop_toStream @r @ix @e)

specMutableR :: forall r e. MutableSpec r e => Spec
specMutableR = do
  unsafeMutableSpec @r @Ix1 @e
  unsafeMutableSpec @r @Ix2 @e
  unsafeMutableSpec @r @Ix3 @e
  unsafeMutableSpec @r @Ix4 @e
  unsafeMutableSpec @r @Ix5 @e
  mutableSpec @r @Ix1 @e
  mutableSpec @r @Ix2 @e
  mutableSpec @r @Ix3 @e
  mutableSpec @r @Ix4 @e
  streamSpecR @r @Ix1 @e
  streamSpecR @r @Ix2 @e
  streamSpecR @r @Ix3 @e
  streamSpecR @r @Ix4 @e
  describe "toStream/toList" $
    it "toStreamIsList" $ property (prop_toStreamIsList @r @e)
  --mutableSpec @r @Ix5 @e -- slows down the test suite


specUnboxedMutableR :: forall r e. MutableSpec r e => Spec
specUnboxedMutableR = do
  specMutableR @r @e
  unsafeMutableUnboxedSpec @r @Ix1 @e
  unsafeMutableUnboxedSpec @r @Ix2 @e
  unsafeMutableUnboxedSpec @r @Ix3 @e
  unsafeMutableUnboxedSpec @r @Ix4 @e
  unsafeMutableUnboxedSpec @r @Ix5 @e



spec :: Spec
spec = do
  specMutableR @B @Int
  specMutableR @N @Int
  specUnboxedMutableR @S @Int
  specUnboxedMutableR @P @Int
  specUnboxedMutableR @U @Int
  atomicIntSpec @Ix1
  atomicIntSpec @Ix2
  atomicIntSpec @Ix3
  atomicIntSpec @Ix4
  atomicIntSpec @Ix5
