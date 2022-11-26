{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Massiv.Core.ListSpec (spec) where

import Data.Massiv.Array
import Test.Massiv.Array.Delayed
import Test.Massiv.Core

spec :: Spec
spec = do
  describe "L" $ do
    prop "toStream" $ prop_toStreamIsList @L @Int
