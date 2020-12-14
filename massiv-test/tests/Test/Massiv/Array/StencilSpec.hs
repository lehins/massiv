{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Massiv.Array.StencilSpec (spec) where

import Prelude as P
import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe as A
import Test.Massiv.Core

avg3x3Stencil :: Fractional a => Stencil Ix2 a a
avg3x3Stencil = (/9) <$> makeConvolutionStencil (Sz 3) (1 :. 1) $ \ get ->
  get (-1 :. -1) 1 . get (-1 :. 0) 1 . get (-1 :. 1) 1 .
  get ( 0 :. -1) 1 . get ( 0 :. 0) 1 . get ( 0 :. 1) 1 .
  get ( 1 :. -1) 1 . get ( 1 :. 0) 1 . get ( 1 :. 1) 1


singletonStencil :: (Index ix) => (Int -> Int) -> Stencil ix Int Int
singletonStencil f =
  makeStencil oneSz zeroIndex $ \ get -> fmap f (get zeroIndex)


prop_MapSingletonStencil ::
     (Load DW ix Int, Manifest U ix Int, Show (Array U ix Int))
  => Proxy ix
  -> Fun Int Int
  -> Border Int
  -> ArrNE U ix Int
  -> Property
prop_MapSingletonStencil _ f b (ArrNE arr) =
  computeAs U (mapStencil b (singletonStencil (apply f)) arr) === computeAs U (A.map (apply f) arr)

prop_ApplyZeroStencil ::
     (Load DW ix Int, Show (Array U ix Int), Manifest U ix Int)
  => Proxy ix
  -> Int
  -> Array U ix Int
  -> Property
prop_ApplyZeroStencil _ e arr =
  computeAs U (applyStencil noPadding zeroStencil arr) === makeArray Seq (size arr) (const e)
  where
    zeroStencil = makeStencil zeroSz zeroIndex $ \_get -> pure e


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
     forall ix. Load DW ix Int
  => Proxy ix
  -> DimIx ix
  -> SzIx ix
  -> Property
prop_DangerousStencil _ (DimIx r) (SzIx sz center) =
  assertException validateException arr
  where
    stencil = makeStencil sz center $ \get -> get ix' :: Value Int
    arr = computeAs P (mapStencil Edge stencil (makeArray Seq sz (const 0) :: Array P ix Int))
    ix' = liftIndex2 (-)
          (setDim' zeroIndex r (getDim' (unSz sz) r))
          (setDim' zeroIndex r (getDim' center r))
    validateException = \case
      IndexOutOfBoundsException _ _ -> pure ()
      exc -> expectationFailure $ "Unexpected exception: " <> show exc


instance Index ix => Show (Stencil ix a b) where
  show stencil =
    "Stencil " ++ show (getStencilSize stencil) ++ " " ++ show (getStencilCenter stencil)


prop_MapEqApplyStencil ::
     (Show (Array P ix Int), StrideLoad DW ix Int)
  => Stride ix
  -> SzTiny ix
  -> Border Int
  -> Array P ix Int
  -> Property
prop_MapEqApplyStencil stride (SzTiny sz) b arr =
  forAll (elements (P.zip [0 ..] (toList $ A.map (\(n, _, _) -> n) stencils))) $ \(i, _) ->
    let (_, stencil, g) = stencils ! i
     in computeAs P (unsafeMapStencil b sz zeroIndex (const g) arr) ===
        computeAs P (applyStencil (samePadding stencil b) stencil arr) .&&.
        computeWithStrideAs P stride (unsafeMapStencil b sz zeroIndex (const g) arr) ===
        computeWithStrideAs P stride (applyStencil (samePadding stencil b) stencil arr)
  where
    stencils = mkCommonStencils sz

mkCommonStencils ::
     (Bounded a, Num a, Ord a, Index ix)
  => Sz ix
  -> Array B Ix1 (String, Stencil ix a a, (ix -> a) -> a)
mkCommonStencils sz =
  fromList
    Seq
    [ (name, stencil sz, \get -> foldlS f acc0 $ fmap get (zeroIndex ..: unSz sz))
    | (name, stencil, f, acc0) <-
        [ ("maxStencil", maxStencil, max, minBound)
        , ("minStencil", minStencil, min, maxBound)
        , ("sumStencil", sumStencil, (+), 0)
        , ("productStencil", productStencil, (*), 1)
        ]
    ]

prop_FoldrStencil :: Load DW ix (Array DL Ix1 Int) => ArrNE P ix Int -> Property
prop_FoldrStencil (ArrNE arr) =
  computeAs P (computeAs B folded ! zeroIndex) === A.fromList Seq (foldrS (:) [] arr)
  where
    folded = applyStencil noPadding (foldrStencil cons A.empty (size arr)) arr

stencilSpec :: Spec
stencilSpec = do
  describe "MapSingletonStencil" $ do
    it "Ix1" $ property $ prop_MapSingletonStencil (Proxy :: Proxy Ix1)
    it "Ix2" $ property $ prop_MapSingletonStencil (Proxy :: Proxy Ix2)
    it "Ix3" $ property $ prop_MapSingletonStencil (Proxy :: Proxy Ix3)
    it "Ix4" $ property $ prop_MapSingletonStencil (Proxy :: Proxy Ix4)
    it "Ix2T" $ property $ prop_MapSingletonStencil (Proxy :: Proxy Ix2T)
  describe "MapSingletonStencilWithStride" $ do
    it "Ix1" $ property $ prop_MapSingletonStencilWithStride (Proxy :: Proxy Ix1)
    it "Ix2" $ property $ prop_MapSingletonStencilWithStride (Proxy :: Proxy Ix2)
    it "Ix3" $ property $ prop_MapSingletonStencilWithStride (Proxy :: Proxy Ix3)
  describe "ApplyZeroStencil" $ do
    it "Ix1" $ property $ prop_ApplyZeroStencil (Proxy :: Proxy Ix1)
    it "Ix2" $ property $ prop_ApplyZeroStencil (Proxy :: Proxy Ix2)
    it "Ix3" $ property $ prop_ApplyZeroStencil (Proxy :: Proxy Ix3)
    it "Ix4" $ property $ prop_ApplyZeroStencil (Proxy :: Proxy Ix4)
    it "Ix2T" $ property $ prop_ApplyZeroStencil (Proxy :: Proxy Ix2T)
  describe "DangerousStencil" $ do
    it "Ix1" $ property $ prop_DangerousStencil (Proxy :: Proxy Ix1)
    it "Ix2" $ property $ prop_DangerousStencil (Proxy :: Proxy Ix2)
    it "Ix3" $ property $ prop_DangerousStencil (Proxy :: Proxy Ix3)
    it "Ix4" $ property $ prop_DangerousStencil (Proxy :: Proxy Ix4)
  describe "MapEqApplyStencil" $ do
    it "Ix1" $ property $ prop_MapEqApplyStencil @Ix1
    it "Ix2" $ property $ prop_MapEqApplyStencil @Ix2
    it "Ix3" $ property $ prop_MapEqApplyStencil @Ix3
    it "Ix4" $ property $ prop_MapEqApplyStencil @Ix4
  describe "FoldrStencil" $ do
    it "Ix1" $ property $ prop_FoldrStencil @Ix1
    it "Ix2" $ property $ prop_FoldrStencil @Ix2
    it "Ix3" $ property $ prop_FoldrStencil @Ix3
    it "Ix4" $ property $ prop_FoldrStencil @Ix4
  describe "Simple" $ do
    it "sumStencil" $ property $ \ (arr :: Array B Ix2 Rational) border ->
      computeAs N (mapStencil border avg3x3Stencil arr) ===
      computeAs N (applyStencil (Padding 1 1 border) (avgStencil (Sz 3)) arr)
    it "sameSizeAndCenter" $ property $ \ (SzIx sz ix) ->
      let stencil = makeStencil sz ix ($ Ix1 0) :: Stencil Ix1 Int Int
      in getStencilSize stencil === sz .&&. getStencilCenter stencil === ix

stencilDirection :: Ix2 -> Array U Ix2 Int -> Array U Ix2 Int
stencilDirection ix = computeAs U . mapStencil (Fill 0) (makeStencil (Sz 3) (1 :. 1) $ \f -> f ix)


stencilCorners :: Ix2 -> Ix2 -> Array U Ix2 Int -> Array U Ix2 Int
stencilCorners ixC ix = computeAs U . mapStencil (Fill 0) (makeStencil (Sz 3) ixC $ \f -> f ix)


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
      applyStencil1 s = computeAs U . applyStencil noPadding s
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
