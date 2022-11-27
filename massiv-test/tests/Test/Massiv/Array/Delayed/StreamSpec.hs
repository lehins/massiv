{-# LANGUAGE TypeApplications #-}

module Test.Massiv.Array.Delayed.StreamSpec (spec) where

import Data.Int
import Data.Massiv.Array
import Test.Massiv.Array.Delayed
import Test.Massiv.Array.Load
import Test.Massiv.Core

spec :: Spec
spec = do
  delayedStreamSpec
  loadSpec @DS @Ix1 @Int16
