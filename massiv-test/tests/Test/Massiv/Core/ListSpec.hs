{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Test.Massiv.Core.ListSpec (spec) where

import Data.Massiv.Array
import Test.Massiv.Core
import Test.Massiv.Array.Delayed


spec :: Spec
spec = do
  describe "L" $
    it "toStream" $ property (prop_toStreamIsList @L @Int)
