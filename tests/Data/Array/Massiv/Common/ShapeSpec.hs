{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Array.Massiv.Common.ShapeSpec (spec) where

import           Data.Array.Massiv
import           Data.Array.Massiv.CommonSpec (ArrIx (..))
import           Data.Array.Massiv.Common.IndexSpec (Ix (..), Sz(..))
import           Data.Array.Massiv.DelayedSpec ()
import           Data.Array.Massiv.ManifestSpec ()
import           Test.Hspec
import           Test.QuickCheck

-----------
-- Shape --
-----------

-- extract

prop_ExtractUnsafeCheck
  :: (Eq (Array r ix e), Arbitrary (Array r ix e), Shape r ix e)
  => proxy (r, ix, e) -> ix -> ix -> Array r ix e -> Property
prop_ExtractUnsafeCheck _ sIx newSz arr =
  not (isSafeIndex (size arr) sIx) || newSz > size arr ==> extract sIx newSz arr == Nothing

prop_ExtractFromToUnsafeCheck
  :: (Eq (Array r ix e), Arbitrary (Array r ix e), Shape r ix e)
  => proxy (r, ix, e) -> ix -> ix -> Array r ix e -> Property
prop_ExtractFromToUnsafeCheck _ sIx eIx arr =
  not (isSafeIndex eIx sIx) || eIx > size arr ==> extractFromTo sIx eIx arr == Nothing

prop_ExtractEqualsExtractFromTo
  :: (Eq (Array r ix e), Arbitrary (Array r ix e), Shape r ix e)
  => proxy (r, ix, e) -> Ix ix -> Array r ix e -> Bool
prop_ExtractEqualsExtractFromTo _ (Ix (Sz eIx) sIx) arr =
  extractFromTo sIx eIx arr == extract sIx (liftIndex2 (-) eIx sIx) arr




specShapeN
  :: (Eq (Array r ix e), Arbitrary (Array r ix e), Arbitrary ix, Shape r ix e)
  => proxy (r, ix, e) -> Spec
specShapeN proxy = do
  describe "extract" $ do
    it "ExtractUnsafeCheck" $ property $ prop_ExtractUnsafeCheck proxy
    it "ExtractFromToUnsafeCheck" $ property $ prop_ExtractFromToUnsafeCheck proxy
    it "ExtractEqualsExtractFromTo" $ property $ prop_ExtractEqualsExtractFromTo proxy


-----------
-- Slice --
-----------


prop_SliceRight :: (Eq (Array r (Lower ix) e), Arbitrary (Array r ix e), Slice r ix e)
  => proxy (r, ix, e) -> Int -> Array r ix e -> Bool
prop_SliceRight _ i arr = arr !?> i == arr <!?> (1, i)


prop_SliceLeft :: (Eq (Array r (Lower ix) e), Arbitrary (Array r ix e), Slice r ix e)
  => proxy (r, ix, e) -> Int -> Array r ix e -> Bool
prop_SliceLeft _ i arr = arr <!? i == arr <!?> (rank (size arr), i)


prop_SliceIndexDim2D :: ArrIx D DIM2 Int -> Bool
prop_SliceIndexDim2D (ArrIx arr ix@(i, j)) =
  val == safeIndex (arr <! j) i &&
  val == safeIndex (arr !> i) j
  where
    val = unsafeIndex arr ix


prop_SliceIndexDim2RankD :: ArrIx D DIM2 Int -> Bool
prop_SliceIndexDim2RankD (ArrIx arr ix@(i, j)) =
  val == safeIndex (arr <!> (1, i)) j &&
  val == safeIndex (arr <!> (2, j)) i
  where
    val = unsafeIndex arr ix


prop_SliceIndexDim3D :: ArrIx D DIM3 Int -> Bool
prop_SliceIndexDim3D (ArrIx arr ix@(i, j, k)) =
  val == safeIndex (arr <! k <! j) i &&
  val == safeIndex (arr !> i !> j) k &&
  val == safeIndex (arr <! k !> i) j &&
  val == safeIndex (arr !> i <! k) j
  where
    val = unsafeIndex arr ix

prop_SliceIndexDim3RankD :: ArrIx D DIM3 Int -> Bool
prop_SliceIndexDim3RankD (ArrIx arr ix@(i, j, k)) =
  val == safeIndex (arr <!> (1, i) <!> (1, j)) k &&
  val == safeIndex (arr <!> (1, i) <!> (2, k)) j &&
  val == safeIndex (arr <!> (2, j) <!> (1, i)) k &&
  val == safeIndex (arr <!> (2, j) <!> (2, k)) i &&
  val == safeIndex (arr <!> (3, k) <!> (1, i)) j &&
  val == safeIndex (arr <!> (3, k) <!> (2, j)) i
  where
    val = unsafeIndex arr ix


prop_SliceIndexDim2M :: ArrIx M DIM2 Int -> Bool
prop_SliceIndexDim2M (ArrIx arr ix@(i, j)) =
  val == arr <!> (1, i) ! j &&
  val == arr <!> (2, j) ! i
  where
    val = unsafeIndex arr ix

prop_SliceIndexDim2RankM :: ArrIx M DIM2 Int -> Bool
prop_SliceIndexDim2RankM (ArrIx arr ix@(i, j)) =
  val == arr <!> (1, i) ! j &&
  val == arr <!> (2, j) ! i
  where
    val = unsafeIndex arr ix


prop_SliceIndexDim3M :: ArrIx M DIM3 Int -> Bool
prop_SliceIndexDim3M (ArrIx arr ix@(i, j, k)) =
  val == arr <! k <! j ! i &&
  val == arr !> i !> j ! k &&
  val == arr <! k !> i ! j &&
  val == arr !> i <! k ! j
  where
    val = unsafeIndex arr ix


prop_SliceIndexDim3RankM :: ArrIx M DIM3 Int -> Bool
prop_SliceIndexDim3RankM (ArrIx arr ix@(i, j, k)) =
  val == arr <!> (1, i) <!> (1, j) ! k &&
  val == arr <!> (1, i) <!> (2, k) ! j &&
  val == arr <!> (2, j) <!> (1, i) ! k &&
  val == arr <!> (2, j) <!> (2, k) ! i &&
  val == arr <!> (3, k) <!> (1, i) ! j &&
  val == arr <!> (3, k) <!> (2, j) ! i
  where
    val = unsafeIndex arr ix


prop_SliceIndexDim4D :: ArrIx D DIM4 Int -> Bool
prop_SliceIndexDim4D (ArrIx arr ix@(i1, i2, i3, i4)) =
  val == safeIndex (arr !> i1 !> i2 !> i3) i4 &&
  val == safeIndex (arr !> i1 !> i2 <! i4) i3 &&
  val == safeIndex (arr !> i1 <! i4 <! i3) i2 &&
  val == safeIndex (arr !> i1 <! i4 !> i2) i3 &&
  val == safeIndex (arr <! i4 !> i1 !> i2) i3 &&
  val == safeIndex (arr <! i4 !> i1 <! i3) i2 &&
  val == safeIndex (arr <! i4 <! i3 <! i2) i1 &&
  val == safeIndex (arr <! i4 <! i3 !> i1) i2
  where
    val = unsafeIndex arr ix

prop_SliceIndexDim4RankD :: ArrIx D DIM4 Int -> Bool
prop_SliceIndexDim4RankD (ArrIx arr ix@(i1, i2, i3, i4)) =
  val == unsafeIndex (arr <!> (1, i1) <!> (1, i2) <!> (1, i3)) i4 &&
  val == unsafeIndex (arr <!> (1, i1) <!> (2, i3) <! i4) i2 &&
  val == unsafeIndex (arr <!> (2, i2) <!> (1, i1)) (i3, i4) &&
  val == unsafeIndex (arr <!> (3, i3) <!> (2, i2)) (i1, i4) &&
  val == unsafeIndex (arr <!> (3, i3) <!> (3, i4) !> i1) i2 &&
  val == unsafeIndex (arr <!> (4, i4) !> i1 !> i2) i3
  where
    val = unsafeIndex arr ix

prop_SliceIndexDim4M :: ArrIx M DIM4 Int -> Bool
prop_SliceIndexDim4M (ArrIx arr ix@(i1, i2, i3, i4)) =
  val == arr !> i1 !> i2 !> i3 ! i4 &&
  val == arr !> i1 !> i2 <! i4 ! i3 &&
  val == arr !> i1 <! i4 <! i3 ! i2 &&
  val == arr !> i1 <! i4 !> i2 ! i3 &&
  val == arr <! i4 !> i1 !> i2 ! i3 &&
  val == arr <! i4 !> i1 <! i3 ! i2 &&
  val == arr <! i4 <! i3 <! i2 ! i1 &&
  val == arr <! i4 <! i3 !> i1 ! i2
  where
    val = unsafeIndex arr ix


prop_SliceIndexDim4RankM :: ArrIx M DIM4 Int -> Bool
prop_SliceIndexDim4RankM (ArrIx arr ix@(i1, i2, i3, i4)) =
  val == arr <!> (1, i1) <!> (1, i2) <!> (1, i3) ! i4 &&
  val == arr <!> (1, i1) <!> (2, i3) <! i4 ! i2 &&
  val == arr <!> (2, i2) <!> (1, i1) ! (i3, i4) &&
  val == arr <!> (3, i3) <!> (2, i2) ! (i1, i4) &&
  val == arr <!> (3, i3) <!> (3, i4) !> i1 ! i2 &&
  val == arr <!> (4, i4) !> i1 !> i2 ! i3
  where
    val = unsafeIndex arr ix


specSliceN
  :: (Eq (Array r (Lower ix) e), Arbitrary (Array r ix e), Arbitrary ix, Slice r ix e)
  => proxy (r, ix, e) -> Spec
specSliceN proxy = do
  describe "Slice" $ do
    it "SliceRight" $ property $ prop_SliceRight proxy
    it "SliceLeft" $ property $ prop_SliceLeft proxy



spec :: Spec
spec = do
  describe "DIM1" $ do
    specShapeN (Nothing :: Maybe (D, DIM1, Int))
  describe "DIM2" $ do
    specShapeN (Nothing :: Maybe (D, DIM2, Int))
    specSliceN (Nothing :: Maybe (D, DIM2, Int))
    describe "SliceIndex" $ do
      it "Delayed" $ property $ prop_SliceIndexDim2D
      it "Rank - Delayed" $ property $ prop_SliceIndexDim2RankD
      it "Manifest" $ property $ prop_SliceIndexDim2M
      it "Rank - Manifest" $ property $ prop_SliceIndexDim2RankM
  describe "DIM3" $ do
    specShapeN (Nothing :: Maybe (D, DIM3, Int))
    specSliceN (Nothing :: Maybe (D, DIM3, Int))
    describe "SliceIndex" $ do
      it "Delayed" $ property $ prop_SliceIndexDim3D
      it "Rank - Delayed" $ property $ prop_SliceIndexDim3RankD
      it "Manifest" $ property $ prop_SliceIndexDim3M
      it "Rank - Manifest" $ property $ prop_SliceIndexDim3RankM
  describe "DIM4" $ do
    specShapeN (Nothing :: Maybe (D, DIM4, Int))
    specSliceN (Nothing :: Maybe (D, DIM4, Int))
    describe "SliceIndex" $ do
      it "Delayed" $ property $ prop_SliceIndexDim4D
      it "Rank - Delayed" $ property $ prop_SliceIndexDim4RankD
      it "Manifest" $ property $ prop_SliceIndexDim4M
      it "Rank - Manifest" $ property $ prop_SliceIndexDim4RankM
