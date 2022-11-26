{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Massiv.Array.Load (loadSpec) where

import Data.Massiv.Array as A
import Test.Massiv.Core.Common ()
import Test.Massiv.Utils as T

prop_replicate
  :: forall r ix e
   . ( Eq e
     , Show e
     , Load r ix e
     , Ragged L ix e
     )
  => Comp
  -> Sz ix
  -> e
  -> Property
prop_replicate comp sz e = propIO $ do
  computeAs B (A.replicate @r comp sz e)
    `shouldBe` computeAs B (makeArrayLinear @r comp sz (const e))

prop_makeArray
  :: forall r ix e
   . ( Eq e
     , Show e
     , Load r ix e
     , Ragged L ix e
     )
  => Comp
  -> Sz ix
  -> Fun ix e
  -> Property
prop_makeArray comp sz f = propIO $ do
  let barr = makeArray @B comp sz (applyFun f)
  computeAs B (makeArray @r comp sz (applyFun f)) `shouldBe` barr
  computeAs B (makeArrayLinear @r comp sz (applyFun f . fromLinearIndex sz)) `shouldBe` barr

loadSpec
  :: forall r ix e
   . ( Eq e
     , Show e
     , Typeable e
     , Arbitrary e
     , Function ix
     , Arbitrary ix
     , CoArbitrary ix
     , Ragged L ix e
     , Load r ix e
     )
  => Spec
loadSpec = do
  describe (("LoadSpec " ++) . showsArrayType @r @ix @e $ "") $ do
    prop "replicate" $ prop_replicate @r @ix @e
    prop "makeArray" $ prop_makeArray @r @ix @e
