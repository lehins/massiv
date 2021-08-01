{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Test.Massiv.Array.Delayed.WindowedSpec (spec) where

import Data.Massiv.Array.Delayed
import Data.Massiv.Array.Unsafe
import Data.Massiv.Array as A
import Test.Massiv.Core


prop_EqDelayed ::
     (Ragged L ix Int, Load DW ix Int) => Proxy ix -> ArrDW ix Int -> Property
prop_EqDelayed _ (ArrDW arrD arrDW) =
  computeAs P arrD === computeAs P arrDW

prop_EqDelayedStride ::
     (Ragged L ix Int, StrideLoad DW ix Int) => Proxy ix -> Stride ix -> ArrDW ix Int -> Property
prop_EqDelayedStride _ stride (ArrDW arrD arrDW) =
  computeWithStrideAs P stride arrD === computeWithStrideAs P stride arrDW


spec :: Spec
spec = do
  describe "Equivalency with Delayed" $ do
    it "Ix1" $ property $ prop_EqDelayed (Proxy :: Proxy Ix1)
    it "Ix2" $ property $ prop_EqDelayed (Proxy :: Proxy Ix2)
    it "Ix3" $ property $ prop_EqDelayed (Proxy :: Proxy Ix3)
    it "Ix4" $ property $ prop_EqDelayed (Proxy :: Proxy Ix4)
    it "Ix5" $ property $ prop_EqDelayed (Proxy :: Proxy Ix5)
  describe "Equivalency with Stride With Delayed" $ do
    it "Ix1" $ property $ prop_EqDelayedStride (Proxy :: Proxy Ix1)
    it "Ix2" $ property $ prop_EqDelayedStride (Proxy :: Proxy Ix2)
    it "Ix3" $ property $ prop_EqDelayedStride (Proxy :: Proxy Ix3)
    it "Ix4" $ property $ prop_EqDelayedStride (Proxy :: Proxy Ix4)
    it "Ix5" $ property $ prop_EqDelayedStride (Proxy :: Proxy Ix5)
