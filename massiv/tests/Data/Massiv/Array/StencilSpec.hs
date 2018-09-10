{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Massiv.Array.StencilSpec (spec) where

import           Control.DeepSeq           (deepseq)
import           Data.Massiv.CoreArbitrary as A
import           Data.Maybe                (fromJust)
import           Data.Proxy
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Data.Default              (Default(def))

-- sum3x3Stencil :: Fractional a => Stencil Ix2 a a
-- sum3x3Stencil = makeConvolutionStencil (3 :. 3) (1 :. 1) $ \ get ->
--   get (-1 :. -1) 1 . get (-1 :. 0) 1 . get (-1 :. 1) 1 .
--   get ( 0 :. -1) 1 . get ( 0 :. 0) 1 . get ( 0 :. 1) 1 .
--   get ( 1 :. -1) 1 . get ( 1 :. 0) 1 . get ( 1 :. 1) 1
-- {-# INLINE sum3x3Stencil #-}


singletonStencil :: (Index ix) => (Int -> Int) -> Stencil ix Int Int
singletonStencil f = makeStencil (pureIndex 1) zeroIndex $ \ get -> fmap f (get zeroIndex)
{-# INLINE singletonStencil #-}


prop_MapSingletonStencil :: (Load DW ix Int, Manifest U ix Int) =>
                            Proxy ix -> Fun Int Int -> Border Int -> ArrP U ix Int -> Bool
prop_MapSingletonStencil _ f b (ArrP arr) =
  computeAs U (mapStencil b (singletonStencil (apply f)) arr) == computeAs U (A.map (apply f) arr)

prop_MapSingletonStencilWithStride :: (Load DW ix Int, Manifest U ix Int) =>
                                      Proxy ix -> Fun Int Int -> Border Int -> ArrP U ix Int -> Bool
prop_MapSingletonStencilWithStride _ f b (ArrP arr) =
  computeWithStride oneStride (mapStencil b (singletonStencil (apply f)) arr) ==
  computeAs U (A.map (apply f) arr)

-- Tests out of bounds stencil indexing
prop_DangerousStencil ::
     Index ix => Proxy ix -> NonZero Int -> DimIx ix -> SzIx ix -> Property
prop_DangerousStencil _ (NonZero s) (DimIx r) (SzIx (Sz sz) ix) =
  ix' `deepseq` assertSomeException $ makeStencil sz ix $ \get -> (get ix' :: Value Int)
  where
    ix' =
      liftIndex (* signum s) $
      fromJust $ do
        i <- getIndex sz r
        setIndex zeroIndex r i


stencilSpec :: Spec
stencilSpec = do
  describe "MapSingletonStencil" $ do
    it "Ix1" $ property $ prop_MapSingletonStencil (Proxy :: Proxy Ix1)
    it "Ix2" $ property $ prop_MapSingletonStencil (Proxy :: Proxy Ix2)
    it "Ix3" $ property $ prop_MapSingletonStencil (Proxy :: Proxy Ix3)
    it "Ix4" $ property $ prop_MapSingletonStencil (Proxy :: Proxy Ix4)
    it "Ix2T" $ property $ prop_MapSingletonStencil (Proxy :: Proxy Ix2T)
    it "Ix3T" $ property $ prop_MapSingletonStencil (Proxy :: Proxy Ix3T)
  describe "MapSingletonStencilWithStride" $ do
    it "Ix1" $ property $ prop_MapSingletonStencilWithStride (Proxy :: Proxy Ix1)
    it "Ix2" $ property $ prop_MapSingletonStencilWithStride (Proxy :: Proxy Ix2)
    it "Ix3" $ property $ prop_MapSingletonStencilWithStride (Proxy :: Proxy Ix3)
  describe "DangerousStencil" $ do
    it "Ix1" $ property $ prop_DangerousStencil (Proxy :: Proxy Ix1)
    it "Ix2" $ property $ prop_DangerousStencil (Proxy :: Proxy Ix2)
    it "Ix3" $ property $ prop_DangerousStencil (Proxy :: Proxy Ix3)
    it "Ix4" $ property $ prop_DangerousStencil (Proxy :: Proxy Ix4)


stencilDirection :: (Default a, Unbox a, Manifest r Ix2 a) => Ix2 -> Array r Ix2 a -> Array U Ix2 a
stencilDirection ix = computeAs U . mapStencil (Fill def) (makeStencil (3 :. 3) (1 :. 1) $ \f -> f ix)


stencilCorners ::
     (Default a, Unbox a, Manifest r Ix2 a) => Ix2 -> Ix2 -> Array r Ix2 a -> Array U Ix2 a
stencilCorners ixC ix = computeAs U . mapStencil (Fill def) (makeStencil (3 :. 3) ixC $ \f -> f ix)

spec :: Spec
spec =
  describe "Stencil" $ do
    stencilSpec
    let arr = [[1, 2, 3], [4, 5, 6], [7, 8, 9]] :: Array U Ix2 Int
    describe "Unit tests Ix2" $ do
      it "Direction Left" $
        stencilDirection (0 :. 1) arr `shouldBe` [[2, 3, 0], [5, 6, 0], [8, 9, 0]]
      it "Direction Right" $
        stencilDirection (0 :. -1) arr `shouldBe` [[0, 1, 2], [0, 4, 5], [0, 7, 8]]
      it "Direction Down" $
        stencilDirection (1 :. 0) arr `shouldBe` [[4, 5, 6], [7, 8, 9], [0, 0, 0]]
      it "Direction Up" $
        stencilDirection (-1 :. 0) arr `shouldBe` [[0, 0, 0], [1, 2, 3], [4, 5, 6]]
      it "Direction Left/Top Corner" $
        stencilCorners (0 :. 0) (2 :. 2) arr `shouldBe` [[9, 0, 0], [0, 0, 0], [0, 0, 0]]
      it "Direction Right/Top Corner" $
        stencilCorners (0 :. 2) (2 :. -2) arr `shouldBe` [[0, 0, 7], [0, 0, 0], [0, 0, 0]]
      it "Direction Right/Bottom Corner" $
        stencilCorners (2 :. 2) (-2 :. -2) arr `shouldBe` [[0, 0, 0], [0, 0, 0], [0, 0, 1]]
      it "Direction Left/Bottom Corner" $
        stencilCorners (2 :. 0) (-2 :. 2) arr `shouldBe` [[0, 0, 0], [0, 0, 0], [3, 0, 0]]
    describe "mapStencil with stride" $ do
      let kernel = [[-1, 0, 1], [0, 1, 0], [-1, 0, 1]] :: Array U Ix2 Int
          stencil = makeConvolutionStencilFromKernel kernel
          stride = Stride 2
      it "map stencil with stride on small array" $
        let strideArr = mapStencil (Fill 0) stencil arr
         in computeWithStrideAs U stride strideArr `shouldBe` [[-4, 8],[2, 14]]
      it "map stencil with stride on larger array" $
        let largeArr = makeArrayR U Seq (5 :. 5) (succ . toLinearIndex (5 :. 5))
            strideArr = mapStencil (Fill 0) stencil largeArr
         in computeWithStrideAs U stride strideArr `shouldBe` [[-6, 1, 14], [-13, 9, 43], [4, 21, 44]]
