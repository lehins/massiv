{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Massiv.Core.Mutable
  ( mutableSpec
  , prop_UnsafeThawFreeze
  ) where

import Data.Massiv.Array
import Data.Massiv.Array.Unsafe
import Test.Massiv.Utils


prop_UnsafeThawFreeze ::
     forall r ix e.
     (Eq (Array r ix e), Show (Array r ix e), Arbitrary (Array r ix e), Mutable r ix e)
  => Property
prop_UnsafeThawFreeze = property $ \ (arr :: Array r ix e) ->
  (unsafeThaw arr >>= unsafeFreeze (getComp arr)) `shouldReturn` arr








mutableSpec ::
     forall r ix e.
     (Eq (Array r ix e), Show (Array r ix e), Arbitrary (Array r ix e), Mutable r ix e)
  => Spec
mutableSpec = do
  describe "Mutable (Unsafe)" $ do
    it "UnsafeThawFreeze" $ prop_UnsafeThawFreeze @r @ix @e
