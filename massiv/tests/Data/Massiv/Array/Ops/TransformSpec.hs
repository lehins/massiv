{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
module Data.Massiv.Array.Ops.TransformSpec (spec) where

import           Data.Massiv.CoreArbitrary as A
import           Prelude                   as P
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Function


prop_transposeOuterInner :: Arr D Ix2 Int -> Property
prop_transposeOuterInner (Arr arr) = transposeOuter arr === transpose arr


prop_upsampleDownsample ::
     (Show (Array P ix Int), Index ix) => ArrTiny P ix Int -> Stride ix -> Int -> Property
prop_upsampleDownsample (ArrTiny arr) stride fill =
  arr === compute (downsample stride (computeAs P (upsample fill stride arr)))

prop_ExtractAppend
  :: (Show (Array P ix Int), Index ix)
  => DimIx ix -> ArrIx P ix Int -> Property
prop_ExtractAppend (DimIx dim) (ArrIx arr ix) =
  arr === compute (uncurry (append' dim) $ A.splitAt' dim (getDim' ix dim) arr)

prop_ConcatAppend
  :: (Show (Array P ix Int), Index ix)
  => DimIx ix -> Comp -> Sz ix -> NonEmptyList (Fun ix Int) -> Property
prop_ConcatAppend (DimIx dim) comp sz (NonEmpty fns) =
  foldl (\arr -> computeAs P . append' dim arr) (head arrs) (tail arrs) ===
  computeAs P (concat' dim arrs)
  where
    arrs = P.map (makeArrayR P comp sz . apply) fns

prop_AppendMappend
  :: Array D Ix1 Int -> Array D Ix1 Int -> Property
prop_AppendMappend arr1 arr2 =
  computeAs P (append' 1 arr1 arr2) === computeAs P (toLoadArray arr1 <> toLoadArray arr2)

prop_ConcatMconcat
  :: [Array D Ix1 Int] -> Property
prop_ConcatMconcat arrs =
  computeAs P (concat' 1 (empty : arrs)) === computeAs P (mconcat (fmap toLoadArray arrs))


spec :: Spec
spec = do
  it "transposeOuterInner" $ property prop_transposeOuterInner
  describe "upsampleDownsample" $ do
    it "Ix1" $ property (prop_upsampleDownsample @Ix1)
    it "Ix2" $ property (prop_upsampleDownsample @Ix2)
    it "Ix3" $ property (prop_upsampleDownsample @Ix3)
    it "Ix4" $ property (prop_upsampleDownsample @Ix4)
  describe "ExtractAppend" $ do
    it "Ix1" $ property (prop_ExtractAppend @Ix1)
    it "Ix2" $ property (prop_ExtractAppend @Ix2)
    it "Ix3" $ property (prop_ExtractAppend @Ix3)
    it "Ix4" $ property (prop_ExtractAppend @Ix4)
  describe "ConcatAppend" $ do
    it "Ix1" $ property (prop_ConcatAppend @Ix1)
    it "Ix2" $ property (prop_ConcatAppend @Ix2)
    it "Ix3" $ property (prop_ConcatAppend @Ix3)
    it "Ix4" $ property (prop_ConcatAppend @Ix4)
  describe "Monoid" $ do
    it "Ix1" $ property prop_AppendMappend
    it "Ix1" $ property prop_ConcatMconcat
