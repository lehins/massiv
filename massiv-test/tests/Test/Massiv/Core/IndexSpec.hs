{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Test.Massiv.Core.IndexSpec (spec) where

import Data.Massiv.Array
import Data.Massiv.Array.Unsafe (Sz(SafeSz))
import Test.Massiv.Core.Index
import Data.Proxy
import Test.Hspec
import Test.QuickCheck
import Data.Typeable



specIxN ::
     forall ix.
     ( Num ix
     -- , Unbox ix -- TODO: add spec for unboxed vectors
     , Index ix
     , Bounded ix
     , Index (Lower ix)
     , Typeable ix
     , Typeable (Lower ix)
     , Arbitrary ix
     , Arbitrary (Lower ix)
     )
  => Spec
specIxN = do
  describe (showsTypeRep (typeRep (Proxy :: Proxy ix)) "") $ do
    ixSpec @ix
    ix2UpSpec @ix
    ixNumSpec @ix
  describe "Show" $
    it "prop_Show" $ property $ \ix -> ("Just (" ++ show (ix :: ix) ++ ")") === show (Just ix)
  describe "Bounded" $ do
    it "minBound" $ fromIntegral (minBound :: Int) `shouldBe` (minBound :: ix)
    it "maxBound" $ fromIntegral (maxBound :: Int) `shouldBe` (maxBound :: ix)

specIxT ::
     forall ix ix'.
     ( Typeable ix
     , Typeable (Lower ix)
     , Index ix
     , Index (Lower ix)
     , Arbitrary ix
     , Arbitrary (Lower ix)
     )
  => (ix -> ix')
  -> (ix' -> ix)
  -> Spec
specIxT fromIxT toIxT = describe (showsTypeRep (typeRep (Proxy :: Proxy ix)) "") $ do
  ixSpec @ix
  ix2UpSpec @ix
  it "toFromIx" $ property $ \ ix -> ix === toIxT (fromIxT ix)

specPatterns :: Spec
specPatterns =
  describe "Patterns" $ do
    it "Ix1" $
      property $ \i ->
        case i of
          Ix1 i' -> i' === Ix1 i
    it "Ix2" $
      property $ \i2 i1 ->
        case i2 :. i1 of
          Ix2 i2' i1' -> i2' :. i1' === Ix2 i2 i1
    it "Ix3" $
      property $ \i3 i2 i1 ->
        case i3 :> i2 :. i1 of
          Ix3 i3' i2' i1' -> i3' :> i2' :. i1' === Ix3 i3 i2 i1
    it "Ix4" $
      property $ \i4 i3 i2 i1 ->
        case i4 :> i3 :> i2 :. i1 of
          Ix4 i4' i3' i2' i1' -> i4' :> i3' :> i2' :. i1' === Ix4 i4 i3 i2 i1
    it "Ix5" $
      property $ \i5 i4 i3 i2 i1 ->
        case i5 :> i4 :> i3 :> i2 :. i1 of
          Ix5 i5' i4' i3' i2' i1' -> i5' :> i4' :> i3' :> i2' :. i1' === Ix5 i5 i4 i3 i2 i1
    it "Sz1" $
      property $ \i ->
        case Sz i of
          Sz1 i' -> SafeSz i' === Sz1 i
    it "Sz2" $
      property $ \i2 i1 ->
        case Sz (i2 :. i1) of
          Sz2 i2' i1' -> SafeSz (i2' :. i1') === Sz2 i2 i1
    it "Sz3" $
      property $ \i3 i2 i1 ->
        case Sz (i3 :> i2 :. i1) of
          Sz3 i3' i2' i1' -> SafeSz (i3' :> i2' :. i1') === Sz3 i3 i2 i1
    it "Sz4" $
      property $ \i4 i3 i2 i1 ->
        case Sz (i4 :> i3 :> i2 :. i1) of
          Sz4 i4' i3' i2' i1' -> SafeSz (i4' :> i3' :> i2' :. i1') === Sz4 i4 i3 i2 i1
    it "Sz5" $
      property $ \i5 i4 i3 i2 i1 ->
        case Sz (i5 :> i4 :> i3 :> i2 :. i1) of
          Sz5 i5' i4' i3' i2' i1' -> SafeSz (i5' :> i4' :> i3' :> i2' :. i1') === Sz5 i5 i4 i3 i2 i1


specSz ::
     forall ix.
     ( Num ix
     -- , Unbox ix -- TODO: add Unbox instance and a spec for unboxed vectors
     , Index ix
     , Typeable ix
     , Arbitrary ix
     )
  => Spec
specSz =
  describe ("Sz (" ++ showsTypeRep (typeRep (Proxy :: Proxy ix)) ")") $ do
    szSpec @ix
    szNumSpec @ix

specIx :: Spec
specIx = do
  specIx1
  specIxN @Ix2
  specIxN @Ix3
  specIxN @Ix4
  specIxN @Ix5
  describe "Dimension" $ do
    it "fromDimension" $ do
      fromDimension Dim1 `shouldBe` 1
      fromDimension Dim2 `shouldBe` 2
      fromDimension Dim3 `shouldBe` 3
      fromDimension Dim4 `shouldBe` 4
      fromDimension Dim5 `shouldBe` 5


spec :: Spec
spec = do
  specIx
  specIxT @Ix2T toIx2 fromIx2
  specIxT @Ix3T toIx3 fromIx3
  specIxT @Ix4T toIx4 fromIx4
  specIxT @Ix5T toIx5 fromIx5
  specPatterns
  specSz @Ix1
  specSz @Ix2
  specSz @Ix3
  specSz @Ix4
  specSz @Ix5
