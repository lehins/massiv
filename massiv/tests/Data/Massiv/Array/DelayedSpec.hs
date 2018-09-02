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
     (Index ix)
  => proxy ix
  -> Stride ix
  -> Array D ix Int
  -> Bool
prop_computeWithStrideEqDownsample _ stride arr =
  computeWithStride stride arr == computeAs U (downsampleArr stride arr)


prop_computeWithStrideInterleavedEqDownsample ::
     (Index ix)
  => proxy ix
  -> Stride ix
  -> Array D ix Int
  -> Bool
prop_computeWithStrideInterleavedEqDownsample _ stride arr =
  computeWithStride stride (toInterleaved arr) == computeAs U (downsampleArr stride arr)


-- prop_computeWithStrideEqDownsample ::
--      (Ragged L ix Int, Index ix) => proxy ix -> Stride ix -> Array D ix Int -> Property
-- prop_computeWithStrideEqDownsample _ stride arr =
--   computeWithStride stride arr === computeAs U downSampledArr
--   where
--     strideIx = unStride stride
--     downSampledArr =
--       makeArrayR
--         D
--         Seq
--         (strideSize stride (size arr))
--         (unsafeIndex arr . liftIndex2 (*) strideIx)

delayedSpec :: (Arbitrary ix, CoArbitrary ix, Ragged L ix Int) => String -> proxy ix -> SpecWith ()
delayedSpec dimName proxy = do
  describe dimName $ do
    it "computeWithStrideEqDownsample" $ property $ prop_computeWithStrideEqDownsample proxy
    it "computeWithStrideInterleavedEqDownsample D" $
      property $ prop_computeWithStrideInterleavedEqDownsample proxy

spec :: Spec
spec = do
  delayedSpec "DIM1" (Proxy :: Proxy Ix1)
  delayedSpec "DIM2" (Proxy :: Proxy Ix2)
  delayedSpec "DIM3" (Proxy :: Proxy Ix3)
