{-# LANGUAGE TypeApplications #-}

module Test.Massiv.Array.Delayed.StreamSpec (spec) where

import Data.Massiv.Array
import Test.Massiv.Core
import Test.Massiv.Array.Delayed
import Test.Massiv.Array.Load
import Data.Int

spec :: Spec
spec = do
  delayedStreamSpec
  loadSpec @DS @Ix1 @Int16
