{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Massiv.Array.StencilSpec (spec) where

import Control.DeepSeq (deepseq)
import Data.Default (Default(def))
import Data.Massiv.Array as A
import Test.Massiv.Core

-- sum3x3Stencil :: Fractional a => Stencil Ix2 a a
-- sum3x3Stencil = makeConvolutionStencil (3 :. 3) (1 :. 1) $ \ get ->
--   get (-1 :. -1) 1 . get (-1 :. 0) 1 . get (-1 :. 1) 1 .
--   get ( 0 :. -1) 1 . get ( 0 :. 0) 1 . get ( 0 :. 1) 1 .
--   get ( 1 :. -1) 1 . get ( 1 :. 0) 1 . get ( 1 :. 1) 1
-- {-# INLINE sum3x3Stencil #-}


singletonStencil :: (Index ix) => (Int -> Int) -> Stencil ix Int Int
singletonStencil f =
  makeStencil oneSz zeroIndex $ \ get -> fmap f (get zeroIndex)
{-# INLINE singletonStencil #-}


prop_MapSingletonStencil :: (Load DW ix Int, Manifest U ix Int, Show (Array U ix Int)) =>
                            Proxy ix -> Fun Int Int -> Border Int -> ArrNE U ix Int -> Property
prop_MapSingletonStencil _ f b (ArrNE arr) =
  computeAs U (mapStencil b (singletonStencil (apply f)) arr) === computeAs U (A.map (apply f) arr)

prop_MapZeroStencil :: (Load DW ix Int, Manifest U ix Int, Show (Array U ix Int)) =>
                       Proxy ix -> Int -> Array U ix Int -> Property
prop_MapZeroStencil _ e arr =
  mappedSame === arrAllSame .&&. appliedAllSame === arrAllSame
  where
    mappedSame =
      computeAs U $
      mapStencil (Fill (error "Should not have accessed elements outside")) zeroStencil arr
    appliedAllSame = computeAs U (applyStencil zeroStencil arr)
    zeroStencil = makeStencil zeroSz zeroIndex $ \_get -> pure e
    arrAllSame = A.replicate Seq (size arr) e


prop_MapSingletonStencilWithStride ::
     (StrideLoad DW ix Int, Manifest U ix Int, Show (Array U ix Int))
  => Proxy ix
  -> Fun Int Int
  -> Border Int
  -> ArrNE U ix Int
  -> Property
prop_MapSingletonStencilWithStride _ f b (ArrNE arr) =
  computeWithStride oneStride (mapStencil b (singletonStencil (apply f)) arr) ===
  computeAs U (A.map (apply f) arr)

-- Tests out of bounds stencil indexing
prop_DangerousStencil ::
     Index ix => Proxy ix -> NonZero Int -> DimIx ix -> SzIx ix -> Property
prop_DangerousStencil _ (NonZero s) (DimIx r) (SzIx sz ix) =
  ix' `deepseq` assertSomeException $ makeStencil sz ix $ \get -> get ix' :: Value Int
  where
    ix' = liftIndex (* signum s) (setDim' zeroIndex r (getDim' (unSz sz) r))


stencilSpec :: Spec
stencilSpec = do
  describe "MapSingletonStencil" $ do
    it "Ix1" $ property $ prop_MapSingletonStencil (Proxy :: Proxy Ix1)
    it "Ix2" $ property $ prop_MapSingletonStencil (Proxy :: Proxy Ix2)
    it "Ix3" $ property $ prop_MapSingletonStencil (Proxy :: Proxy Ix3)
    it "Ix4" $ property $ prop_MapSingletonStencil (Proxy :: Proxy Ix4)
    it "Ix2T" $ property $ prop_MapSingletonStencil (Proxy :: Proxy Ix2T)
  describe "MapZeroStencil" $ do
    it "Ix1" $ property $ prop_MapZeroStencil (Proxy :: Proxy Ix1)
    it "Ix2" $ property $ prop_MapZeroStencil (Proxy :: Proxy Ix2)
    it "Ix3" $ property $ prop_MapZeroStencil (Proxy :: Proxy Ix3)
    it "Ix4" $ property $ prop_MapZeroStencil (Proxy :: Proxy Ix4)
    it "Ix2T" $ property $ prop_MapZeroStencil (Proxy :: Proxy Ix2T)
  describe "MapSingletonStencilWithStride" $ do
    it "Ix1" $ property $ prop_MapSingletonStencilWithStride (Proxy :: Proxy Ix1)
    it "Ix2" $ property $ prop_MapSingletonStencilWithStride (Proxy :: Proxy Ix2)
    it "Ix3" $ property $ prop_MapSingletonStencilWithStride (Proxy :: Proxy Ix3)
  describe "DangerousStencil" $ do
    it "Ix1" $ property $ prop_DangerousStencil (Proxy :: Proxy Ix1)
    it "Ix2" $ property $ prop_DangerousStencil (Proxy :: Proxy Ix2)
    it "Ix3" $ property $ prop_DangerousStencil (Proxy :: Proxy Ix3)
    it "Ix4" $ property $ prop_DangerousStencil (Proxy :: Proxy Ix4)


stencilDirection :: Ix2 -> Array U Ix2 Int -> Array U Ix2 Int
stencilDirection ix = computeAs U . mapStencil (Fill def) (makeStencil (Sz 3) (1 :. 1) $ \f -> f ix)


stencilCorners :: Ix2 -> Ix2 -> Array U Ix2 Int -> Array U Ix2 Int
stencilCorners ixC ix = computeAs U . mapStencil (Fill def) (makeStencil (Sz 3) ixC $ \f -> f ix)


stencilConvolution :: Spec
stencilConvolution = do
  let xs3 :: Array U Ix1 Int
      xs3 = [1, 2, 3]
      xs3f f = f (-1) 1 . f 0 2 . f 1 3
      xs4 :: Array U Ix1 Int
      xs4 = [1, 2, 3, 4]
      xs4f f = f (-2) 1 . f (-1) 2 . f 0 3 . f 1 4
      ys :: Array U Ix1 Int
      ys = [1, 2, 3, 4, 5]
      ysConvXs3 = [4, 10, 16, 22, 22]
      ysConvXs4 = [10, 20, 30, 34, 31]
      ysCorrXs3 = [8, 14, 20, 26, 14]
      ysCorrXs4 = [11, 20, 30, 40, 26]
      ysConvXs4' = [4, 10, 20, 30, 34]
      ysCorrXs4' = [20, 30, 40, 26, 14]
      xs4f' f = f (-1) 1 . f 0 2 . f 1 3 . f 2 4
      mapStencil1 :: Stencil Ix1 Int Int -> Array U Ix1 Int -> Array U Ix1 Int
      mapStencil1 s = computeAs U . mapStencil (Fill 0) s
      mapStencil2 :: Stencil Ix2 Int Int -> Array U Ix2 Int -> Array U Ix2 Int
      mapStencil2 s = computeAs U . mapStencil (Fill 0) s
      applyStencil1 :: Stencil Ix1 Int Int -> Array U Ix1 Int -> Array U Ix1 Int
      applyStencil1 s = computeAs U . applyStencil s
  describe "makeConvolutionStencilFromKernel" $ do
    it "1x3 map" $ mapStencil1 (makeConvolutionStencilFromKernel xs3) ys `shouldBe` ysConvXs3
    it "1x4 map" $ mapStencil1 (makeConvolutionStencilFromKernel xs4) ys `shouldBe` ysConvXs4
    it "1x3 apply" $
      applyStencil1 (makeConvolutionStencilFromKernel xs3) ys `shouldBe`
      compute (extract' 1 3 ysConvXs3)
    it "1x4 apply" $
      applyStencil1 (makeConvolutionStencilFromKernel xs4) ys `shouldBe`
      compute (extract' 1 2 ysConvXs4)
  describe "makeCorrelationStencilFromKernel" $ do
    it "1x3 map" $ mapStencil1 (makeCorrelationStencilFromKernel xs3) ys `shouldBe` ysCorrXs3
    it "1x4 map" $ mapStencil1 (makeCorrelationStencilFromKernel xs4) ys `shouldBe` ysCorrXs4
  describe "makeConvolutionStencil" $ do
    it "1x3" $ mapStencil1 (makeConvolutionStencil (Sz1 3) 1 xs3f) ys `shouldBe` ysConvXs3
    it "1x4" $ mapStencil1 (makeConvolutionStencil (Sz1 4) 2 xs4f) ys `shouldBe` ysConvXs4
    it "1x4" $ mapStencil1 (makeConvolutionStencil (Sz1 4) 1 xs4f') ys `shouldBe` ysConvXs4'
  describe "makeCorrelationStencil" $ do
    it "1x3" $ mapStencil1 (makeCorrelationStencil (Sz1 3) 1 xs3f) ys `shouldBe` ysCorrXs3
    it "1x4" $ mapStencil1 (makeCorrelationStencil (Sz1 4) 2 xs4f) ys `shouldBe` ysCorrXs4
    it "1x4" $ mapStencil1 (makeCorrelationStencil (Sz1 4) 1 xs4f') ys `shouldBe` ysCorrXs4'
  describe "makeConvolutionStencil == makeConvolutionStencilFromKernel" $ do
    it "Sobel Horizontal" $
      property $ \(arr :: Array U Ix2 Int) ->
        mapStencil2 (makeConvolutionStencil (Sz 3) 1 sobelX) arr ===
        mapStencil2 (makeConvolutionStencilFromKernel sobelKernelX) arr
    it "1x3" $
      property $ \(arr :: Array U Ix1 Int) ->
        mapStencil1 (makeConvolutionStencil (Sz1 3) 1 xs3f) arr ===
        mapStencil1 (makeConvolutionStencilFromKernel xs3) arr
    it "1x4" $
      property $ \(arr :: Array U Ix1 Int) ->
        mapStencil1 (makeConvolutionStencil (Sz1 4) 2 xs4f) arr ===
        mapStencil1 (makeConvolutionStencilFromKernel xs4) arr
  describe "makeCorrelationStencil == makeCorrelationStencilFromKernel" $ do
    it "Sobel Horizontal" $
      property $ \(arr :: Array U Ix2 Int) ->
        mapStencil2 (makeCorrelationStencil (Sz 3) 1 sobelX) arr ===
        mapStencil2 (makeCorrelationStencilFromKernel sobelKernelX) arr
    it "1x3" $
      property $ \(arr :: Array U Ix1 Int) ->
        mapStencil1 (makeCorrelationStencil (Sz1 3) 1 xs3f) arr ===
        mapStencil1 (makeCorrelationStencilFromKernel xs3) arr
    it "1x4" $
      property $ \(arr :: Array U Ix1 Int) ->
        mapStencil1 (makeCorrelationStencil (Sz1 4) 2 xs4f) arr ===
        mapStencil1 (makeCorrelationStencilFromKernel xs4) arr
  describe "makeConvolutionStencil == makeCorrelationStencil . rotate180" $ do
    it "Sobel Horizontal" $
      property $ \(arr :: Array U Ix2 Int) ->
        mapStencil2 (makeConvolutionStencilFromKernel sobelKernelX) arr ===
        mapStencil2 (makeCorrelationStencilFromKernel (rotate180 sobelKernelX)) arr
    it "1x3" $
      property $ \(arr :: Array U Ix1 Int) ->
        mapStencil1 (makeConvolutionStencilFromKernel xs3) arr ===
        mapStencil1 (makeCorrelationStencilFromKernel (rotate180 xs3)) arr
    it "1x5" $
      property $ \(arr :: Array U Ix1 Int) ->
        mapStencil1 (makeConvolutionStencilFromKernel ys) arr ===
        mapStencil1 (makeCorrelationStencilFromKernel (rotate180 ys)) arr

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
    describe "mapStencil with stride" $ do
      let kernel = [[-1, 0, 1], [0, 1, 0], [-1, 0, 1]] :: Array U Ix2 Int
          stencil = makeConvolutionStencilFromKernel kernel
          stride = Stride 2
      it "map stencil with stride on small array" $
        let strideArr = mapStencil (Fill 0) stencil arr
         in computeWithStrideAs U stride strideArr `shouldBe` [[-4, 8], [2, 14]]
      it "map stencil with stride on larger array" $
        let largeArr = makeArrayR U Seq (Sz 5) (succ . toLinearIndex (Sz 5))
            strideArr = mapStencil (Fill 0) stencil largeArr
         in computeWithStrideAs U stride strideArr `shouldBe`
            [[-6, 1, 14], [-13, 9, 43], [4, 21, 44]]
  stencilConvolution

sobelX :: Num e => (Ix2 -> e -> e -> e) -> e -> e
sobelX f = f (-1 :. -1) (-1) . f (-1 :. 1) 1 .
           f ( 0 :. -1) (-2) . f ( 0 :. 1) 2 .
           f ( 1 :. -1) (-1) . f ( 1 :. 1) 1

sobelKernelX :: Array U Ix2 Int
sobelKernelX = [ [-1, 0, 1]
               , [-2, 0, 2]
               , [-1, 0, 1] ]

rotate180 :: (Num ix, Index ix) => Array U ix Int -> Array U ix Int
rotate180 = computeAs U . transform' (\sz -> (sz, sz)) (\(Sz sz) f ix -> f (sz - 1 - ix))
