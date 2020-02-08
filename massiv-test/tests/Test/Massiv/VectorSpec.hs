{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Test.Massiv.VectorSpec (spec) where

import Data.Massiv.Array as A
import Data.Massiv.Vector as V
import Test.Massiv.Core
import qualified Data.Vector.Primitive as VP

infix 4 !==!

(!==!) :: (Eq e, Show e, Prim e, Load r Ix1 e) => V.Vector r e -> VP.Vector e -> Property
(!==!) arr vec = toPrimitiveVector (compute arr) === vec

prop_Generate :: Comp -> Int -> Fun Int Word -> Property
prop_Generate comp k f =
  V.generate comp (Sz k) (apply f) !==! VP.generate k (apply f)

spec :: Spec
spec = do
  describe "Vector" $ do
    describe "same-as-vector-package" $ do
      prop "generate" prop_Generate
