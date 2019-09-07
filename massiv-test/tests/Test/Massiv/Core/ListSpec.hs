{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Test.Massiv.Core.ListSpec (spec) where

import Data.Massiv.Array
import Test.Massiv.Core
import Test.Massiv.Array.Delayed


spec :: Spec
spec = do
  describe "L" $
    it "toStream" $ property (prop_toStreamIsList @L @Int)
  describe "LN" $
    it "toStream" $ property (prop_toStreamIsList @LN @Int)
