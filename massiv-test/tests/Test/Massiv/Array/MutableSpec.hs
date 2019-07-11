{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Test.Massiv.Array.MutableSpec (spec) where

import Data.Massiv.Array
import Test.Massiv.Core
import Test.Massiv.Core.Mutable
import Test.Massiv.Array.Mutable

type MutableArraySpec r ix e
   = ( Eq (Array r ix e)
     , Show (Array r ix e)
     , Eq (Array (EltRepr r Ix1) Ix1 e)
     , Show (Array (EltRepr r Ix1) Ix1 e)
     , Extract r Ix1 e
     , Resize r ix
     , Arbitrary (Array r ix e)
     , Mutable r ix e
     , Construct r ix e)

type MutableSpec r e
   = ( Show e
     , Eq e
     , Typeable e
     , Arbitrary e
     , CoArbitrary e
     , Function e
     , MutableArraySpec r Ix1 e
     , MutableArraySpec r Ix2 e
     , MutableArraySpec r Ix3 e
     , MutableArraySpec r Ix4 e
     , MutableArraySpec r Ix5 e)

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
  mutableSpec @r @Ix5 @e


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
