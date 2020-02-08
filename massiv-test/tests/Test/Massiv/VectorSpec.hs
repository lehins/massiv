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

import Control.Exception
import Data.Massiv.Array as A
import Data.Massiv.Vector as V
import Test.Massiv.Core
import qualified Data.Vector.Primitive as VP

infix 4 !==!, !!==!!

sizeException :: SizeException -> Bool
sizeException _ = True

(!==!) :: (Eq e, Show e, Prim e, Source r Ix1 e) => V.Vector r e -> VP.Vector e -> Property
(!==!) arr vec = toPrimitiveVector (computeSource arr) === vec

(!!==!!) :: (Eq e, Show e, Prim e, Source r Ix1 e) => V.Vector r e -> VP.Vector e -> Property
(!!==!!) arr vec = property $ do
  eRes <- try (pure $! vec)
  case eRes of
    Right vec' -> toPrimitiveVector (computeSource arr) `shouldBe` vec'
    Left (_exc :: ErrorCall) ->
      shouldThrow (pure $! toPrimitiveVector (computeSource arr)) sizeException


prop_generate :: Comp -> Int -> Fun Int Word -> Property
prop_generate comp k f =
  V.generate comp (Sz k) (apply f) !==! VP.generate k (apply f)

prop_take :: Int -> Array P Ix1 Word -> Property
prop_take n arr =
  V.take (Sz n) arr !==! VP.take n (toPrimitiveVector arr)

prop_drop :: Int -> Array P Ix1 Word -> Property
prop_drop n arr =
  V.drop (Sz n) arr !==! VP.drop n (toPrimitiveVector arr)

prop_splitAt :: Int -> Array P Ix1 Word -> Property
prop_splitAt n arr = (larr !==! lvec) .&&. (rarr !==! rvec)
  where
    (larr, rarr) = V.splitAt (Sz n) arr
    (lvec, rvec) = VP.splitAt n (toPrimitiveVector arr)

prop_slice' :: Int -> Sz1 -> Array P Ix1 Word -> Property
prop_slice' i n arr =
  V.slice' i n arr !!==!! VP.slice i (unSz n) (toPrimitiveVector arr)

spec :: Spec
spec = do
  describe "Vector" $ do
    describe "same-as-vector-package" $ do
      prop "generate" prop_generate
      prop "take" prop_take
      prop "drop" prop_drop
      prop "splitAt" prop_splitAt
      prop "slice" prop_slice'
