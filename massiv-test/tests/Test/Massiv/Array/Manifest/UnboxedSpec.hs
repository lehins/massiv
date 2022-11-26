{-# LANGUAGE TypeApplications #-}

module Test.Massiv.Array.Manifest.UnboxedSpec (spec) where

import Data.Massiv.Array as A
import Data.Word
import Test.Massiv.Array.MutableSpec (specUnboxedMutableR)
import Test.Massiv.Core

spec :: Spec
spec =
  describe "Unboxed" $ do
    specUnboxedMutableR @U @Word16
    specUnboxedMutableR @S @Word32
