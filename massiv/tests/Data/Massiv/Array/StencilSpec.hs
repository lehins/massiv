{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Massiv.Array.StencilSpec (spec) where

import           Control.DeepSeq           (deepseq)
import           Data.Massiv.Array.Stencil
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


singletonStencil :: (Num ix, Index ix) => (Int -> Int) -> Stencil ix Int Int
singletonStencil f = makeStencil 1 0 $ \ get -> fmap f (get zeroIndex)
{-# INLINE singletonStencil #-}


prop_MapSingletonStencil :: (Load DW ix Int, Manifest U ix Int, Num ix) =>
                            Proxy ix -> Fun Int Int -> Border Int -> ArrP U ix Int -> Bool
prop_MapSingletonStencil _ f b (ArrP arr) =
  computeAs U (mapStencil b (singletonStencil (apply f)) arr) == computeAs U (A.map (apply f) arr)

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
  describe "DangerousStencil" $ do
    it "Ix1" $ property $ prop_DangerousStencil (Proxy :: Proxy Ix1)
    it "Ix2" $ property $ prop_DangerousStencil (Proxy :: Proxy Ix2)
    it "Ix3" $ property $ prop_DangerousStencil (Proxy :: Proxy Ix3)
    it "Ix4" $ property $ prop_DangerousStencil (Proxy :: Proxy Ix4)
--   describe "Storable" $ do
--     it "Ix1" $ property $ prop_toFromVector (Nothing :: Maybe Ix1) S
--     it "Ix2" $ property $ prop_toFromVector (Nothing :: Maybe Ix2) S
--     it "Ix3" $ property $ prop_toFromVector (Nothing :: Maybe Ix3) S
--   describe "Primitive" $ do
--     it "Ix1" $ property $ prop_toFromVector (Nothing :: Maybe Ix1) P
--     it "Ix2" $ property $ prop_toFromVector (Nothing :: Maybe Ix2) P
--     it "Ix3" $ property $ prop_toFromVector (Nothing :: Maybe Ix3) P
--   describe "Boxed" $ do
--     it "Ix1" $ property $ prop_toFromVector (Nothing :: Maybe Ix1) B
--     it "Ix2" $ property $ prop_toFromVector (Nothing :: Maybe Ix2) B
--     it "Ix3" $ property $ prop_toFromVector (Nothing :: Maybe Ix3) B


stencilDirection :: (Default a, Unbox a, Manifest r Ix2 a) => Ix2 -> Array r Ix2 a -> Array U Ix2 a
stencilDirection ix = computeAs U . mapStencil (Fill def) (makeStencil (3 :. 3) (1 :. 1) $ \f -> f ix)


stencilCorners ::
     (Default a, Unbox a, Manifest r Ix2 a) => Ix2 -> Ix2 -> Array r Ix2 a -> Array U Ix2 a
stencilCorners ixC ix = computeAs U . mapStencil (Fill def) (makeStencil (3 :. 3) ixC $ \f -> f ix)

spec :: Spec
spec = do
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
    describe "reformDW" $ do
      it "map stencil with stride on small array" $
        let kernel = [[-1, 0, 1], [0, 1, 0], [-1, 0, 1]] :: Array U Ix2 Int
            stencil = makeConvolutionStencilFromKernel kernel
            stride = 2
            strideArr = mapStride stride (1 :. 1) $ mapStencil (Fill 0) stencil arr
         in computeAs U strideArr `shouldBe` [[-4]]
      it "map stencil with stride on larger array" $
        let kernel = [[-1, 0, 1], [0, 1, 0], [-1, 0, 1]] :: Array U Ix2 Int
            stencil = makeConvolutionStencilFromKernel kernel
            stride = 2
            largeArr =
              fromLists' Par [[5 * n + 1 .. 5 * (n + 1)] | n <- [0 .. 4]] :: Array U Ix2 Int
            stencilledArr = mapStencil (Fill 0) stencil largeArr
            strideArr = mapStride stride (2 :. 2) stencilledArr
         in do computeAs U strideArr `shouldBe` [[-6, 1], [-13, 9]]
      it "resize DWArray resulting from mapStencil" $
        let kernel = [[-1, 0, 1], [0, 1, 0], [-1, 0, 1]] :: Array U Ix2 Int
            stencil = makeConvolutionStencilFromKernel kernel
            si = -1 :. -1
            result = reformDW (subtract si) (+ si) (5 :. 5) $ mapStencil (Fill 0) stencil arr
            expectation =
                  [ [ -1, -2, -2,  2,  3]
                  , [ -4, -4,  0,  8,  6]
                  , [ -8, -6,  1, 16, 12]
                  , [ -4,  2,  6, 14,  6]
                  , [ -7, -8, -2,  8,  9]
                  ]
         in computeAs U result `shouldBe` expectation

mapStride :: Int -> Ix2 -> Array DW Ix2 e -> Array DW Ix2 e
mapStride stride sz =
  let toOldIndex = liftIndex (* stride)
      ceilingDivStride a = ceiling $ (fromIntegral a :: Double) / fromIntegral stride
      toNewIndex = liftIndex ceilingDivStride
   in reformDW toNewIndex toOldIndex sz
