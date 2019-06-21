{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
module Data.Massiv.Array.Ops.TransformSpec (spec) where

import Data.Massiv.CoreArbitrary as A
import Data.Sequence as S
import Prelude as P
import Data.Foldable as F (foldl', toList)
import Data.Maybe

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

prop_SplitExtract
  :: (Show (Array P ix Int), Show (Array M ix Int), Index ix)
  => DimIx ix -> ArrIx P ix Int -> Positive Int -> Property
prop_SplitExtract (DimIx dim) (ArrIx arr ix) (Positive n) =
  (computeAs P <$> splitAt' dim i arr) === (left, computeAs P (append' dim center right)) .&&.
  (computeAs P splitLeft, splitRight) === (computeAs P (append' dim left center), right)
  where i = getDim' ix dim
        k = getDim' (unSz (size arr)) dim
        n' = n `mod` (k - i)
        (left, center, right) = either throw id (splitExtractM dim i (Sz n') arr)
        (splitLeft, splitRight) = splitAt' dim (i + n') arr

prop_ConcatAppend
  :: (Show (Array P ix Int), Index ix)
  => DimIx ix -> Comp -> Sz ix -> NonEmptyList (Fun ix Int) -> Property
prop_ConcatAppend (DimIx dim) comp sz (NonEmpty fns) =
  foldl1 (\arr -> computeAs P . append' dim arr) arrs ===
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
  computeAs P (concat' 1 (A.empty : arrs)) === computeAs P (mconcat (fmap toLoadArray arrs))

prop_ExtractSizeMismatch ::
     Index ix => ArrTiny P ix Int -> Positive Int -> Property
prop_ExtractSizeMismatch (ArrTiny arr) (Positive n) =
  assertExceptionIO (SizeElementsMismatchException sz sz' ==) $ resizeM sz' arr
  where
    sz = size arr
    sz' = Sz (totalElem sz + n)

spec :: Spec
spec = do
  it "transposeOuterInner" $ property prop_transposeOuterInner
  describe "upsampleDownsample" $ do
    it "Ix1" $ property (prop_upsampleDownsample @Ix1)
    it "Ix2" $ property (prop_upsampleDownsample @Ix2)
    it "Ix3" $ property (prop_upsampleDownsample @Ix3)
    it "Ix4" $ property (prop_upsampleDownsample @Ix4)
  describe "extractSizeMismatch" $ do
    it "Ix1" $ property (prop_ExtractSizeMismatch @Ix1)
    it "Ix2" $ property (prop_ExtractSizeMismatch @Ix2)
    it "Ix3" $ property (prop_ExtractSizeMismatch @Ix3)
    it "Ix4" $ property (prop_ExtractSizeMismatch @Ix4)
  describe "ExtractAppend" $ do
    it "Ix1" $ property (prop_ExtractAppend @Ix1)
    it "Ix2" $ property (prop_ExtractAppend @Ix2)
    it "Ix3" $ property (prop_ExtractAppend @Ix3)
    it "Ix4" $ property (prop_ExtractAppend @Ix4)
  describe "ExtractAppend" $ do
    it "Ix1" $ property (prop_SplitExtract @Ix1)
    it "Ix2" $ property (prop_SplitExtract @Ix2)
    it "Ix3" $ property (prop_SplitExtract @Ix3)
    it "Ix4" $ property (prop_SplitExtract @Ix4)
  describe "ConcatAppend" $ do
    it "Ix1" $ property (prop_ConcatAppend @Ix1)
    it "Ix2" $ property (prop_ConcatAppend @Ix2)
    it "Ix3" $ property (prop_ConcatAppend @Ix3)
    it "Ix4" $ property (prop_ConcatAppend @Ix4)
  describe "Monoid" $ do
    it "Ix1" $ property prop_AppendMappend
    it "Ix1" $ property prop_ConcatMconcat
  describe "Sequence" $ do
    it "ConsSnoc" $ property prop_ConsSnoc
    it "UnconsUnsnoc" $ property prop_UnconsUnsnoc
  describe "zoomWithGrid" $ do
    it "Ix1" $ property (prop_zoomWithGridStrideCompute @Ix1)
    it "Ix2" $ property (prop_zoomWithGridStrideCompute @Ix2)
    it "Ix3" $ property (prop_zoomWithGridStrideCompute @Ix3)
    it "Ix4" $ property (prop_zoomWithGridStrideCompute @Ix4)

prop_zoomWithGridStrideCompute :: (Show (Array P ix Int), Index ix) => Array D ix Int -> Stride ix -> Int -> Property
prop_zoomWithGridStrideCompute arr stride defVal =
  (computeWithStrideAs P stride' arr' ===
   A.replicate Seq (Sz (liftIndex (+ 1) $ unSz (size arr))) defVal) .&&.
  (computeWithStrideAs P stride' (extract' (pureIndex 1) sz' arr') === compute arr)
  where
    arr' = computeAs P (zoomWithGrid defVal stride arr)
    sz' = Sz (liftIndex (subtract 1) $ unSz (size arr'))
    stride' = Stride (liftIndex (+ 1) $ unStride stride)


prop_UnconsUnsnoc :: Array D Ix1 Int -> Bool -> Property
prop_UnconsUnsnoc arr unconsFirst =
  preJust $ do
    (arr', u, s) <-
      if unconsFirst
        then do
          (u, au) <- unconsM arr
          (as, s) <- unsnocM au
          pure (as, u, s)
        else do
          (as, s) <- unsnocM arr
          (u, au) <- unconsM as
          pure (au, u, s)
    pure (computeAs U (A.snoc (A.cons u (toLoadArray (computeAs U arr'))) s) === compute arr)

preJust :: Testable prop => Maybe prop -> Property
preJust m = isJust m ==> fromJust m

prop_ConsSnoc :: Array D Ix1 Int -> [SeqOp Int] -> Property
prop_ConsSnoc arr ops =
  A.toList (computeAs U (foldl' applyArraySeqOp (toLoadArray arr) ops)) ===
  F.toList (foldl' applySequenceSeqOp (S.fromList (A.toList arr)) ops)

data SeqOp e = Cons e | Snoc e deriving (Eq, Show)

instance Arbitrary e => Arbitrary (SeqOp e) where
  arbitrary = do
    e <- arbitrary
    elements [Cons e, Snoc e]

applyArraySeqOp :: Array DL Ix1 e -> SeqOp e -> Array DL Ix1 e
applyArraySeqOp arr = \case
  Cons x -> A.cons x arr
  Snoc x -> A.snoc arr x


applySequenceSeqOp :: Seq a -> SeqOp a -> Seq a
applySequenceSeqOp arr = \case
  Cons x -> x <| arr
  Snoc x -> arr |> x
