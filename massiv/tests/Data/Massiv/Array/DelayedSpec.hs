{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Massiv.Array.DelayedSpec (spec) where

import Data.Massiv.Array
import Data.Massiv.Array.Delayed
import Data.Massiv.Array.Unsafe
import Data.Massiv.CoreArbitrary as A
import Data.Proxy


downsampleArr :: Source r ix e => Stride ix -> Array r ix e -> Array D ix e
downsampleArr stride arr =
  unsafeBackpermute (strideSize stride (size arr)) (liftIndex2 (*) (unStride stride)) arr

prop_computeWithStrideEqDownsample ::
     Ragged L ix Int
  => proxy ix
  -> Stride ix
  -> Array D ix Int
  -> Property
prop_computeWithStrideEqDownsample _ stride arr =
  computeWithStride stride arr === computeAs U (downsampleArr stride arr)


prop_computeWithStrideInterleavedEqDownsample ::
     Ragged L ix Int
  => proxy ix
  -> Stride ix
  -> Array D ix Int
  -> Property
prop_computeWithStrideInterleavedEqDownsample _ stride arr =
  computeWithStride stride (toInterleaved arr) === computeAs U (downsampleArr stride arr)

prop_computeWithStrideWindowedEqDownsample ::
     (Ragged L ix Int, StrideLoad DW ix Int)
  => proxy ix
  -> Stride ix
  -> ArrIx D ix Int
  -> Property
prop_computeWithStrideWindowedEqDownsample _ stride (ArrIx arr _) =
  computeWithStride stride (insertWindow arr (Window zeroIndex (size arr) (unsafeIndex arr) Nothing)) ===
  -- Below triggers a bug in ghc-8.0 which results in a deadlock.
  -- computeWithStride stride (makeWindowedArray arr zeroIndex (size arr) (unsafeIndex arr)) ===
  computeAs U (downsampleArr stride arr)


delayedSpec ::
     (Arbitrary ix, CoArbitrary ix, StrideLoad DW ix Int, Ragged L ix Int)
  => String
  -> proxy ix
  -> Spec
delayedSpec dimName proxy =
  describe dimName $ do
    it "computeWithStrideEqDownsample" $ property $ prop_computeWithStrideEqDownsample proxy
    it "computeWithStrideInterleavedEqDownsample" $
      property $ prop_computeWithStrideInterleavedEqDownsample proxy
    it "computeWithStrideWindowedEqDownsample" $
      property $ prop_computeWithStrideWindowedEqDownsample proxy

spec :: Spec
spec = do
  delayedSpec "Ix1" (Proxy :: Proxy Ix1)
  delayedSpec "Ix2" (Proxy :: Proxy Ix2)
  delayedSpec "Ix3" (Proxy :: Proxy Ix3)
  delayedSpec "Ix4" (Proxy :: Proxy Ix4)
