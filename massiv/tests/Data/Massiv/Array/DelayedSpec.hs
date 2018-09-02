{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Massiv.Array.DelayedSpec (spec) where

import           Data.Massiv.Array.Delayed
import           Data.Massiv.Array.Unsafe
import           Data.Massiv.CoreArbitrary as A
import           Data.Proxy
import           Test.Hspec
import           Test.QuickCheck


downsampleArr :: Source r ix e => Stride ix -> Array r ix e -> Array D ix e
downsampleArr stride arr =
  makeArrayR D Seq (strideSize stride (size arr)) (unsafeIndex arr . liftIndex2 (*) strideIx)
  where
    strideIx = unStride stride

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
     (Ragged L ix Int, Load DW ix Int)
  => proxy ix
  -> Stride ix
  -> ArrIx D ix Int
  -> Property
prop_computeWithStrideWindowedEqDownsample _ stride (ArrIx arr _) =
  computeWithStride stride (makeWindowedArray arr zeroIndex (size arr) (unsafeIndex arr)) ===
  computeAs U (downsampleArr stride arr)


delayedSpec ::
     (Arbitrary ix, CoArbitrary ix, Load DW ix Int, Ragged L ix Int)
  => String
  -> proxy ix
  -> SpecWith ()
delayedSpec dimName proxy = do
  describe dimName $ do
    it "computeWithStrideEqDownsample" $ property $ prop_computeWithStrideEqDownsample proxy
    it "computeWithStrideInterleavedEqDownsample" $
      property $ prop_computeWithStrideInterleavedEqDownsample proxy
    it "computeWithStrideWindowedEqDownsample" $
      property $ prop_computeWithStrideWindowedEqDownsample proxy

spec :: Spec
spec = do
  delayedSpec "DIM1" (Proxy :: Proxy Ix1)
  delayedSpec "DIM2" (Proxy :: Proxy Ix2)
  delayedSpec "DIM3" (Proxy :: Proxy Ix3)
  delayedSpec "DIM4" (Proxy :: Proxy Ix4)
