{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Test.Massiv.Array.Ops.TransformSpec (spec) where

import Data.Foldable as F (foldl', toList)
import Data.Massiv.Array as A
import Data.Maybe
import Data.Sequence as S
import Prelude as P
import Test.Massiv.Core
import Test.Massiv.Array.Delayed (stackSlices')

prop_TransposeOuterInner :: Matrix D Int -> Property
prop_TransposeOuterInner arr = transposeOuter arr === transpose arr

prop_UpsampleDownsample ::
     forall r ix e . (Eq (Array r ix e), Show (Array r ix e), Load r ix e, Manifest r e)
  => ArrTiny r ix e
  -> Stride ix
  -> e
  -> Property
prop_UpsampleDownsample (ArrTiny arr) stride fill =
  arr === compute (downsample stride (compute @r (upsample fill stride arr)))

prop_ExtractAppend ::
     forall r ix e. (Eq (Array r ix e), Show (Array r ix e), Manifest r e, Index ix)
  => DimIx ix
  -> ArrIx r ix e
  -> Property
prop_ExtractAppend (DimIx dim) (ArrIx arr ix) =
  arr === compute (uncurry (append' dim) $ A.splitAt' dim (getDim' ix dim) arr)

prop_SplitExtract ::
     forall r ix e.
     ( Eq e
     , Show e
     , Eq (Array r ix e)
     , Show (Array r ix e)
     , Source r e
     , Load r ix e
     , Manifest r e
     , Ragged L ix e
     )
  => DimIx ix
  -> ArrIx r ix e
  -> Positive Int
  -> Property
prop_SplitExtract (DimIx dim) (ArrIx arr ix) (Positive n) =
  (compute @r <$> splitAt' dim i arr) === (left, compute @r (append' dim center right)) .&&.
  (compute @r splitLeft, splitRight) === (compute @r (append' dim left center), right)
  where i = getDim' ix dim
        k = getDim' (unSz (size arr)) dim
        n' = n `mod` (k - i)
        (left, center, right) = throwEither (splitExtractM dim i (Sz n') arr)
        (splitLeft, splitRight) = splitAt' dim (i + n') arr

prop_ConcatAppend ::
     forall r ix. (Eq (Array r ix Int), Show (Array r ix Int), Load r ix Int, Manifest r Int)
  => DimIx ix
  -> Comp
  -> Sz ix
  -> NonEmptyList (Fun ix Int)
  -> Property
prop_ConcatAppend (DimIx dim) comp sz (NonEmpty fns) =
  foldl1 (\arr -> compute @r . append' dim arr) arrs ===
  compute @r (concat' dim arrs)
  where
    arrs = P.zipWith (\ f i -> makeArray @r comp sz ((+i) . apply f)) fns [0 .. ]

prop_ConcatMConcatOuterM ::
     forall r ix.
     (Eq (Array r ix Int), Show (Array r ix Int), Load r ix Int, Manifest r Int)
  => Comp
  -> Sz ix
  -> NonEmptyList (Fun ix Int)
  -> Property
prop_ConcatMConcatOuterM comp sz (NonEmpty fns) =
  property $ do
    as <- compute @r <$> concatM (dimensions sz) arrs
    as' <- compute @r <$> concatOuterM (P.map toLoadArray arrs)
    as `shouldBe` as'
  where
    arrs = P.zipWith (\ f i -> makeArray @r comp sz ((+i) . apply f)) fns [0 .. ]


prop_AppendMappend
  :: Array D Ix1 Int -> Array D Ix1 Int -> Property
prop_AppendMappend arr1 arr2 =
  computeAs P (append' 1 arr1 arr2) === computeAs P (toLoadArray arr1 <> toLoadArray arr2)

prop_ConcatMconcat
  :: [Array D Ix1 Int] -> Property
prop_ConcatMconcat arrs =
  computeAs P (concat' 1 (A.empty : arrs)) === computeAs P (mconcat (fmap toLoadArray arrs))

prop_ExtractSizeMismatch ::
     (Size r, Load r ix e, NFData (Array r Int e)) => ArrTiny r ix e -> Positive Int -> Property
prop_ExtractSizeMismatch (ArrTiny arr) (Positive n) =
  assertExceptionIO (SizeElementsMismatchException sz sz' ==) $ resizeM sz' arr
  where
    sz = size arr
    sz' = Sz (totalElem sz + n)

-- FIXME: deal with overlapping instances for slices, see #106
-- prop_stackInnerSlices ::
--      forall ix.
--      ( Index ix
--      , Index (Lower ix)
--      , Elt M ix Int ~ Array M (Lower ix) Int
--      , Elt P ix Int ~ Array M (Lower ix) Int
--      , Show (Array P ix Int)
--      )
--   => ArrNE P ix Int
--   -> Property
-- prop_stackInnerSlices (ArrNE arr) =
--   arr === compute (throwEither (stackInnerSlicesM (innerSlices arr))) .&&.
--   arr === compute (stackSlices' 1 (innerSlices arr))
prop_stackInnerSlicesIx2 :: ArrNE P Ix2 Int -> Property
prop_stackInnerSlicesIx2 (ArrNE arr) =
  arr === compute (throwEither (stackInnerSlicesM (innerSlices arr))) .&&.
  arr === compute (stackSlices' 1 (innerSlices arr))
prop_stackInnerSlicesIx3 :: ArrNE P Ix3 Int -> Property
prop_stackInnerSlicesIx3 (ArrNE arr) =
  arr === compute (throwEither (stackInnerSlicesM (innerSlices arr))) .&&.
  arr === compute (stackSlices' 1 (innerSlices arr))
prop_stackInnerSlicesIx4 :: ArrNE P Ix4 Int -> Property
prop_stackInnerSlicesIx4 (ArrNE arr) =
  arr === compute (throwEither (stackInnerSlicesM (innerSlices arr))) .&&.
  arr === compute (stackSlices' 1 (innerSlices arr))

-- prop_stackOuterSlices ::
--      forall ix.
--      ( Index ix
--      , Index (Lower ix)
--      , Elt M ix Int ~ Array M (Lower ix) Int
--      , Elt P ix Int ~ Array M (Lower ix) Int
--      , Show (Array P ix Int)
--      )
--   => ArrNE P ix Int
--   -> Property
-- prop_stackOuterSlices (ArrNE arr) =
--   arr === compute (throwEither (stackOuterSlicesM (outerSlices arr))) .&&.
--   arr === compute (stackSlices' (dimensions (Proxy :: Proxy ix)) (outerSlices arr))
prop_stackOuterSlicesIx2 :: ArrNE P Ix2 Int -> Property
prop_stackOuterSlicesIx2 (ArrNE arr) =
  arr === compute (throwEither (stackOuterSlicesM (outerSlices arr))) .&&.
  arr === compute (stackSlices' (dimensions (Proxy :: Proxy Ix2)) (outerSlices arr))
prop_stackOuterSlicesIx3 :: ArrNE P Ix3 Int -> Property
prop_stackOuterSlicesIx3 (ArrNE arr) =
  arr === compute (throwEither (stackOuterSlicesM (outerSlices arr))) .&&.
  arr === compute (stackSlices' (dimensions (Proxy :: Proxy Ix3)) (outerSlices arr))
prop_stackOuterSlicesIx4 :: ArrNE P Ix4 Int -> Property
prop_stackOuterSlicesIx4 (ArrNE arr) =
  arr === compute (throwEither (stackOuterSlicesM (outerSlices arr))) .&&.
  arr === compute (stackSlices' (dimensions (Proxy :: Proxy Ix4)) (outerSlices arr))





prop_ZoomWithGridStrideCompute ::
     forall r ix e.
     ( Eq (Array r ix e)
     , Show (Array r ix e)
     , StrideLoad r ix e
     , Manifest r e
     )
  => Array r ix e
  -> Stride ix
  -> e
  -> Property
prop_ZoomWithGridStrideCompute arr stride defVal =
  (computeWithStride @r stride' arr' ===
   compute (A.replicate @DL Seq (Sz (liftIndex (+ 1) $ unSz (size arr))) defVal)) .&&.
  (computeWithStride @r stride' (extract' (pureIndex 1) sz' arr') === compute arr)
  where
    arr' = compute @r (zoomWithGrid defVal stride arr)
    sz' = Sz (liftIndex (subtract 1) $ unSz (size arr'))
    stride' = Stride (liftIndex (+ 1) $ unStride stride)

prop_ZoomStrideCompute ::
     forall r ix e. (Eq (Array r ix e), Show (Array r ix e), StrideLoad r ix e, Manifest r e)
  => Array r ix e
  -> Stride ix
  -> Property
prop_ZoomStrideCompute arr stride = computeWithStride @r stride arr' === compute arr
  where
    arr' = compute @r (zoom stride arr)


type Transform r ix e
   = ( Show e
     , Eq e
     , Arbitrary e
     , Arbitrary ix
     , Arbitrary (Array r ix e)
     , Typeable e
     , Typeable ix
     , CoArbitrary e
     , CoArbitrary ix
     , Function e
     , Function ix
     , Eq (Array r ix e)
     , Eq (Array r ix Int)
     , Show (Array r ix e)
     , Show (Array r ix Int)
     , NFData (Array r ix e)
     , NFData (Array r Int e)
     , Load r ix e
     , Load r ix Int
     , Ragged L ix e
     , Source r e
     , StrideLoad r ix e
     , Manifest r Int
     , Manifest r e)

specTransformR ::
     forall r ix e. Transform r ix e
  => Spec
specTransformR =
  describe ("Transform (" ++ showsArrayType @r @ix @e ")") $ do
    prop "UpsampleDownsample" (prop_UpsampleDownsample @r @ix @e)
    prop "ExtractSizeMismatch" (prop_ExtractSizeMismatch @r @ix @e)
    prop "ExtractAppend" (prop_ExtractAppend @r @ix @e)
    prop "SplitExtract" (prop_SplitExtract @r @ix @e)
    prop "ConcatAppend" (prop_ConcatAppend @r @ix)
    prop "ConcatMConcatOuterM" (prop_ConcatMConcatOuterM @r @ix)
    prop "ZoomStrideCompute" (prop_ZoomStrideCompute @r @ix @e)
    prop "ZoomWithGridStrideCompute" (prop_ZoomWithGridStrideCompute @r @ix @e)

spec :: Spec
spec = do
  it "transposeOuterInner" $ property prop_TransposeOuterInner
  specTransformR @P @Ix1 @Int
  specTransformR @P @Ix2 @Int
  specTransformR @P @Ix3 @Int
  specTransformR @P @Ix4 @Int
  describe "Monoid" $ do
    it "Ix1" $ property prop_AppendMappend
    it "Ix1" $ property prop_ConcatMconcat
  describe "Sequence" $ do
    it "ConsSnoc" $ property prop_ConsSnoc
    it "UnconsUnsnoc" $ property prop_UnconsUnsnoc
  describe "slice+stack" $ do
    -- prop "Ix2 - Inner" (prop_stackInnerSlices @Ix2)
    -- prop "Ix3 - Inner" (prop_stackInnerSlices @Ix3)
    -- prop "Ix4 - Inner" (prop_stackInnerSlices @Ix4)
    -- prop "Ix2 - Outer" (prop_stackOuterSlices @Ix2)
    -- prop "Ix3 - Outer" (prop_stackOuterSlices @Ix3)
    -- prop "Ix4 - Outer" (prop_stackOuterSlices @Ix4)
    prop "Ix2 - Inner" prop_stackInnerSlicesIx2
    prop "Ix3 - Inner" prop_stackInnerSlicesIx3
    prop "Ix4 - Inner" prop_stackInnerSlicesIx4
    prop "Ix2 - Outer" prop_stackOuterSlicesIx2
    prop "Ix3 - Outer" prop_stackOuterSlicesIx3
    prop "Ix4 - Outer" prop_stackOuterSlicesIx4


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
