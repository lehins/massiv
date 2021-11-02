{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Test.Massiv.Array.Ops.SliceSpec (spec) where

import Control.Applicative ((<|>))
import Data.Massiv.Array as A
import Test.Massiv.Core

-----------
-- Size --
-----------

prop_ExtractEqualsExtractFromTo ::
     (Source r e, Eq e, Show e, Ragged L ix e)
  => proxy (r, ix, e)
  -> SzIx ix
  -> Array r ix e
  -> Property
prop_ExtractEqualsExtractFromTo _ (SzIx (Sz eIx) sIx) arr =
  (extractFromToM sIx eIx arr <|> Nothing) === extractM sIx (Sz (liftIndex2 (-) eIx sIx)) arr


specSizeN ::
     ( HasCallStack
     , Eq e
     , Show e
     , Ragged L ix e
     , Arbitrary (Array r ix e)
     , Show (Array r ix e)
     , Source r e
     , Arbitrary ix
     )
  => proxy (r, ix, e)
  -> Spec
specSizeN proxy =
  describe "extract" $
    it "ExtractEqualsExtractFromTo" $ property $ prop_ExtractEqualsExtractFromTo proxy


-----------
-- Slice --
-----------


prop_SliceOuter ::
     ( HasCallStack
     , Source r e
     , Index ix
     , Ragged L (Lower ix) e
     , Show e
     , Eq e
     , Show (Array r (Lower ix) e)
     )
  => proxy (r, ix, e)
  -> Ix1
  -> Array r ix e
  -> Property
prop_SliceOuter _ i arr =
  expectProp $
    if isSafeIndex (fst (unconsSz (size arr))) i
    then do
      e1 <- arr !?> i
      e2 <- arr <!?> (dimensions (size arr), i)
      delay e1 `shouldBe` e2
    else do
      arr !?> i `shouldSatisfy` isNothing
      arr <!?> (dimensions (size arr), i) `shouldSatisfy` isNothing


prop_SliceInner ::
     (HasCallStack, Source r e, Index ix, Ragged L (Lower ix) e, Show e, Eq e)
  => proxy (r, ix, e)
  -> Int
  -> Array r ix e
  -> Property
prop_SliceInner _ i arr =
  expectProp $ do
    if isSafeIndex (snd (unsnocSz (size arr))) i
    then do
      e1 <- arr <!? i
      e2 <- arr <!?> (1, i)
      e1 `shouldBe` e2
    else do
      arr <!? i `shouldSatisfy` isNothing
      arr <!?> (1, i) `shouldSatisfy` isNothing


prop_SliceIndexDim2 :: (HasCallStack, Source r Int) => ArrIx r Ix2 Int -> Property
prop_SliceIndexDim2 (ArrIx arr ix@(i :. j)) =
  expectProp $ do
    val <- evaluateM arr ix
    evaluateM (arr !> i) j `shouldReturn` val
    evaluateM (arr <! j) i `shouldReturn` val
    evaluateM (arr <!> (2, i)) j `shouldReturn` val
    evaluateM (arr <!> (1, j)) i `shouldReturn` val


prop_SliceIndexDim3 :: (HasCallStack, Source r Int) => ArrIx r Ix3 Int -> Property
prop_SliceIndexDim3 (ArrIx arr ix@(i :> j :. k)) =
  expectProp $ do
    val <- evaluateM arr ix
    evaluateM (arr <! k <! j) i `shouldReturn` val
    evaluateM (arr !> i !> j) k `shouldReturn` val
    evaluateM (arr <! k !> i) j `shouldReturn` val
    evaluateM (arr !> i <! k) j `shouldReturn` val
    evaluateM (arr <!> (3, i) <!> (2, j)) k `shouldReturn` val
    evaluateM (arr <!> (3, i) <!> (1, k)) j `shouldReturn` val
    evaluateM (arr <!> (2, j) <!> (2, i)) k `shouldReturn` val
    evaluateM (arr <!> (2, j) <!> (1, k)) i `shouldReturn` val
    evaluateM (arr <!> (1, k) <!> (2, i)) j `shouldReturn` val
    evaluateM (arr <!> (1, k) <!> (1, j)) i `shouldReturn` val


prop_SliceIndexDim4 :: (HasCallStack, Source r Int) => ArrIx r Ix4 Int -> Property
prop_SliceIndexDim4 (ArrIx arr ix@(i1 :> i2 :> i3 :. i4)) =
  expectProp $ do
    val <- evaluateM arr ix
    evaluateM (arr <!> (4, i1) <!> (3, i2) <!> (2, i3)) i4 `shouldReturn` val
    evaluateM (arr <!> (4, i1) <!> (2, i3) <! i4)  i2 `shouldReturn` val
    evaluateM (arr <!> (3, i2) <!> (3, i1)) (i3 :. i4) `shouldReturn` val
    evaluateM (arr <!> (2, i3) <!> (2, i2)) (i1 :. i4) `shouldReturn` val
    evaluateM (arr <!> (2, i3) <!> (1, i4) !> i1) i2 `shouldReturn` val
    evaluateM (arr <!> (1, i4) !> i1 !> i2) i3 `shouldReturn` val

    evaluateM (arr !> i1 !> i2 !> i3) i4 `shouldReturn` val
    evaluateM (arr !> i1 !> i2 <! i4) i3 `shouldReturn` val
    evaluateM (arr !> i1 <! i4 <! i3) i2 `shouldReturn` val
    evaluateM (arr !> i1 <! i4 !> i2) i3 `shouldReturn` val
    evaluateM (arr <! i4 !> i1 !> i2) i3 `shouldReturn` val
    evaluateM (arr <! i4 !> i1 <! i3) i2 `shouldReturn` val
    evaluateM (arr <! i4 <! i3 <! i2) i1 `shouldReturn` val
    evaluateM (arr <! i4 <! i3 !> i1) i2 `shouldReturn` val




specSliceN ::
     ( HasCallStack
     , Source r e
     , Load r ix e
     , Arbitrary (Array r ix e)
     , Show (Array r ix e)
     , Ragged L (Lower ix) e
     , Show e
     , Eq e
     , Show (Array r (Lower ix) e)
     )
  => proxy (r, ix, e)
  -> Spec
specSliceN proxy =
  describe "Slice" $ do
    prop "SliceOuter" $ prop_SliceOuter proxy
    prop "SliceInner" $ prop_SliceInner proxy



spec :: Spec
spec = do
  describe "Ix1" $
    specSizeN (Nothing :: Maybe (D, Ix1, Int))
  describe "Ix2" $ do
    specSizeN (Nothing :: Maybe (D, Ix2, Int))
    specSliceN (Nothing :: Maybe (D, Ix2, Int))
    describe "SliceIndex" $ do
      prop "Delayed" $ prop_SliceIndexDim2 @D
      prop "Manifest" $ prop_SliceIndexDim2 @P
  describe "Ix3" $ do
    specSizeN (Nothing :: Maybe (D, Ix3, Int))
    specSliceN (Nothing :: Maybe (D, Ix3, Int))
    describe "SliceIndex" $ do
      prop "Delayed" $ prop_SliceIndexDim3 @D
      prop "Manifest" $ prop_SliceIndexDim3 @P
  describe "Ix4" $ do
    specSizeN (Nothing :: Maybe (D, Ix4, Int))
    specSliceN (Nothing :: Maybe (D, Ix4, Int))
    describe "SliceIndex" $ do
      prop "Delayed" $ prop_SliceIndexDim4 @D
      prop "Manifest" $ prop_SliceIndexDim4 @P
