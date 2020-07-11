{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Test.Massiv.Array.Delayed.InterleavedSpec
  ( spec
  ) where

import Data.Massiv.Array
import Test.Massiv.Core


prop_EqDelayed ::
     (Ragged L ix Int, Load D ix Int, Load DI ix Int)
  => Array D ix Int
  -> Property
prop_EqDelayed arr = computeAs P arr === computeAs P (toInterleaved arr)


prop_Resize ::
     (Ragged L ix Int, Load D ix Int, Load DI ix Int)
  => Array DI ix Int
  -> Property
prop_Resize arr =
  computeAs P (resize' k arr) === computeAs P (resize' k arrD)
  where
    arrD = fromInterleaved arr
    k = Sz (totalElem (size arr))

spec :: Spec
spec =
  describe "Interleaved same as Delayed" $ do
    it "EqDelayed Ix1" $ property $ prop_EqDelayed @Ix1
    it "EqDelayed Ix2" $ property $ prop_EqDelayed @Ix2
    it "EqDelayed Ix3" $ property $ prop_EqDelayed @Ix3
    it "EqDelayed Ix4" $ property $ prop_EqDelayed @Ix4
    it "EqDelayed Ix5" $ property $ prop_EqDelayed @Ix5
    it "Resize Ix1" $ property $ prop_Resize @Ix1
    it "Resize Ix2" $ property $ prop_Resize @Ix2
    it "Resize Ix3" $ property $ prop_Resize @Ix3
    it "Resize Ix4" $ property $ prop_Resize @Ix4
    it "Resize Ix5" $ property $ prop_Resize @Ix5
