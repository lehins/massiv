{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Test.Massiv.Core.IndexSpec (spec) where

import Control.Exception
import Control.DeepSeq
import Data.Massiv.Array
import Data.Massiv.Array.Unsafe (Sz(SafeSz))
import Test.Massiv.Core.Index
import Test.Massiv.Utils
import Test.Validity.Eq (eqSpecOnArbitrary)
import Test.Validity.Ord (ordSpecOnArbitrary)


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
     , IsIndexDimension ix (Dimensions ix)
     )
  => Spec
specIxN = do
  describe (showsTypeRep (typeRep (Proxy :: Proxy ix)) "") $ do
    ixSpec @ix
    ix2UpSpec @ix
    ixNumSpec @ix
    it "Show" $ property $ \ix -> ("Just (" ++ show (ix :: ix) ++ ")") === show (Just ix)
  describe "Bounded" $ do
    it "minBound" $ fromIntegral (minBound :: Int) `shouldBe` (minBound :: ix)
    it "maxBound" $ fromIntegral (maxBound :: Int) `shouldBe` (maxBound :: ix)
  eqSpecOnArbitrary @ix
  ordSpecOnArbitrary @ix
  describe "Stride" $ do
    it "Positive" $
      property $ \(ix :: ix) ->
        case Stride ix of
          str@(Stride ix') -> foldlIndex (\a x -> a && x > 0) True ix' .&&.
                              unStride str === liftIndex (max 1) ix
    it "Show" $ property $ \str -> ("Just (" ++ show (str :: Stride ix) ++ ")") === show (Just str)
    eqSpecOnArbitrary @(Stride ix)
    ordSpecOnArbitrary @(Stride ix)
    it "DeebpSeq" $ property $ \ (str :: Stride ix) -> rnf str `shouldBe` ()
    it "oneStride" $ unStride oneStride `shouldBe` (1 :: ix)
    it "toLinearIndexStride" $ property $ \ str (SzIx sz ix :: SzIx ix) ->
      let k = toLinearIndexStride str sz ix
          ix' = fromLinearIndex sz k
      in ix' * unStride str + liftIndex2 mod ix (unStride str) === ix
    it "strideSize" $ property $ \ (str :: Stride ix) sz ->
      let sz' = Sz (unSz sz * unStride str) in strideSize str sz' === sz
    it "strideStart" $ property $ \ (str :: Stride ix) ix ->
      let start = strideStart str ix
      in liftIndex2 mod start (unStride str) === zeroIndex .&&.
         ix <= start


specIxT ::
     forall ix ix'.
     ( Typeable ix
     , Typeable (Lower ix)
     , Index ix
     , Index (Lower ix)
     , Arbitrary ix
     , Arbitrary (Lower ix)
     , IsIndexDimension ix (Dimensions ix)
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
specSz = do
  describe ("Sz (" ++ showsTypeRep (typeRep (Proxy :: Proxy ix)) ")") $ do
    szSpec @ix
    szNumSpec @ix
    prop "throws error on negate" $ \sz ->
      sz /= zeroSz ==>
      assertException (\(ErrorCallWithLocation err loc) -> err `deepseq` loc `deepseq` True)  (negate sz)
    prop "Show" $ \sz -> ("Just (" ++ show (sz :: Sz ix) ++ ")") === show (Just sz)
  eqSpecOnArbitrary @(Sz ix)
  ordSpecOnArbitrary @(Sz ix)

specIx :: Spec
specIx = do
  specIx1
  specIxN @Ix2
  specIxN @Ix3
  specIxN @Ix4
  specIxN @Ix5
  describe "Dimension" $
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
  describe "NFData Border" $ do
    it "Fill exception" $
      assertException (ExpectedException==) (Fill (throw ExpectedException :: Int))
    it "rnf" $ property $ \ (b :: Border Int) -> rnf b `shouldBe` ()
  eqSpecOnArbitrary @(Border Int)
