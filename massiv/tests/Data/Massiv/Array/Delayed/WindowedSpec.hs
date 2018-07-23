{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
module Data.Massiv.Array.Delayed.WindowedSpec (spec) where

import           Data.Massiv.Array.Delayed
import           Data.Massiv.Array.Unsafe
import           Data.Massiv.CoreArbitrary as A
import           Data.Proxy
import           Data.Typeable
import           Test.Hspec
import           Test.QuickCheck


data ArrDW ix e = ArrDW (Array D ix e) (Array DW ix e)

instance (Show ix, Index ix, Show (Array D ix e), Show (Array DW ix e)) => Show (ArrDW ix e) where
  show (ArrDW d dw) =
    "Delayed: \n" ++
    show d ++
    "\nCorresponding Windowed: \n" ++
    show dw ++
    "\nStride: (" ++ show (getStride dw) ++ ")" ++ "\nComputed: (" ++ show (getComp dw) ++ ")"

instance (Arbitrary ix, CoArbitrary ix, Index ix, Arbitrary e, Typeable e) =>
         Arbitrary (ArrDW ix e) where
  arbitrary = do
    ArrTiny (arr :: Array D ix e) <- arbitrary
    let sz = size arr
    (wix, wsz) <- if totalElem sz == 0
      then pure (zeroIndex, zeroIndex)
      else do
        wix <- flip (liftIndex2 mod) sz <$> arbitrary
        wsz <- flip (liftIndex2 mod) (liftIndex2 (-) sz wix) <$> arbitrary
        pure (wix, wsz)
    stride <- liftIndex abs <$> arbitrary
    return $ ArrDW arr (setStride stride $ makeWindowedArray arr wix wsz (unsafeIndex arr))

prop_EqDelayed ::
     (Ragged L ix Int, Load DW ix Int) => Proxy ix -> ArrDW ix Int -> Property
prop_EqDelayed _ (ArrDW arrD arrDW) =
  computeAs P arrDW === computeAs P (backpermute sz ixMap arrD)
  where
    stride = getStride arrDW
    sz = liftIndex2 div (size arrD) stride
    ixMap = liftIndex2 (*) stride


spec :: Spec
spec = do
  describe "Equivalency with Delayed" $ do
    it "Ix1" $ property $ prop_EqDelayed (Proxy :: Proxy Ix1)
    it "Ix2" $ property $ prop_EqDelayed (Proxy :: Proxy Ix2)
--     it "Ix3" $ property $ prop_MapSingletonStencil (Proxy :: Proxy Ix3)
--     it "Ix4" $ property $ prop_MapSingletonStencil (Proxy :: Proxy Ix4)
--   describe "DangerousStencil" $ do
--     it "Ix1" $ property $ prop_DangerousStencil (Proxy :: Proxy Ix1)
--     it "Ix2" $ property $ prop_DangerousStencil (Proxy :: Proxy Ix2)
--     it "Ix3" $ property $ prop_DangerousStencil (Proxy :: Proxy Ix3)
--     it "Ix4" $ property $ prop_DangerousStencil (Proxy :: Proxy Ix4)


-- stencilDirection :: (Default a, Unbox a, Manifest r Ix2 a) => Ix2 -> Array r Ix2 a -> Array U Ix2 a
-- stencilDirection ix = computeAs U . mapStencil (Fill def) (makeStencil (3 :. 3) (1 :. 1) $ \f -> f ix)


-- stencilCorners ::
--      (Default a, Unbox a, Manifest r Ix2 a) => Ix2 -> Ix2 -> Array r Ix2 a -> Array U Ix2 a
-- stencilCorners ixC ix = computeAs U . mapStencil (Fill def) (makeStencil (3 :. 3) ixC $ \f -> f ix)

--   describe "Stencil" $ do
--     stencilSpec
--     let arr = [[1, 2, 3], [4, 5, 6], [7, 8, 9]] :: Array U Ix2 Int
--     describe "Unit tests Ix2" $ do
--       it "Direction Left" $
--         stencilDirection (0 :. 1) arr `shouldBe` [[2, 3, 0], [5, 6, 0], [8, 9, 0]]
--       it "Direction Right" $
--         stencilDirection (0 :. -1) arr `shouldBe` [[0, 1, 2], [0, 4, 5], [0, 7, 8]]
--       it "Direction Down" $
--         stencilDirection (1 :. 0) arr `shouldBe` [[4, 5, 6], [7, 8, 9], [0, 0, 0]]
--       it "Direction Up" $
--         stencilDirection (-1 :. 0) arr `shouldBe` [[0, 0, 0], [1, 2, 3], [4, 5, 6]]
--       it "Direction Left/Top Corner" $
--         stencilCorners (0 :. 0) (2 :. 2) arr `shouldBe` [[9, 0, 0], [0, 0, 0], [0, 0, 0]]
--       it "Direction Right/Top Corner" $
--         stencilCorners (0 :. 2) (2 :. -2) arr `shouldBe` [[0, 0, 7], [0, 0, 0], [0, 0, 0]]
--       it "Direction Right/Bottom Corner" $
--         stencilCorners (2 :. 2) (-2 :. -2) arr `shouldBe` [[0, 0, 0], [0, 0, 0], [0, 0, 1]]
--       it "Direction Left/Bottom Corner" $
--         stencilCorners (2 :. 0) (-2 :. 2) arr `shouldBe` [[0, 0, 0], [0, 0, 0], [3, 0, 0]]
--     describe "mapStencilStride" $ do
--       it "map stencil with stride on small array" $
--         let kernel = [[-1, 0, 1], [0, 1, 0], [-1, 0, 1]] :: Array U Ix2 Int
--             stencil = makeConvolutionStencilFromKernel kernel
--             stride = 2
--             strideArr = mapStencilStride (Fill 0) stride stencil arr
--          in computeAs U strideArr `shouldBe` [[-4, 8],[2, 14]]
--       it "map stencil with stride on larger array" $
--         let kernel = [[-1, 0, 1], [0, 1, 0], [-1, 0, 1]] :: Array U Ix2 Int
--             stencil = makeConvolutionStencilFromKernel kernel
--             stride = 2
--             largeArr = makeArrayR U Seq (5 :. 5) (succ . toLinearIndex (5 :. 5))
--             strideArr = mapStencilStride (Fill 0) stride stencil largeArr
--          in do computeAs U strideArr `shouldBe` [[-6, 1, 14], [-13, 9, 43], [4, 21, 44]]
--     describe "reformDW" $ do
--       it "map stencil with stride on small array" $
--         let kernel = [[-1, 0, 1], [0, 1, 0], [-1, 0, 1]] :: Array U Ix2 Int
--             stencil = makeConvolutionStencilFromKernel kernel
--             stride = 2
--             strideArr = mapStride stride (1 :. 1) $ mapStencil (Fill 0) stencil arr
--          in computeAs U strideArr `shouldBe` [[-4]]
--       it "map stencil with stride on larger array" $
--         let kernel = [[-1, 0, 1], [0, 1, 0], [-1, 0, 1]] :: Array U Ix2 Int
--             stencil = makeConvolutionStencilFromKernel kernel
--             stride = 2
--             largeArr = makeArrayR U Seq (5 :. 5) (succ . toLinearIndex (5 :. 5))
--             stencilledArr = mapStencil (Fill 0) stencil largeArr
--             strideArr = mapStride stride (2 :. 2) stencilledArr
--          in do computeAs U strideArr `shouldBe` [[-6, 1], [-13, 9]]
--       it "resize DWArray resulting from mapStencil" $
--         let kernel = [[-1, 0, 1], [0, 1, 0], [-1, 0, 1]] :: Array U Ix2 Int
--             stencil = makeConvolutionStencilFromKernel kernel
--             result = unsafeBackpermuteDW (+1) (subtract 1) (5 :. 5) $ mapStencil (Fill 0) stencil arr
--             expectation =
--                   [ [ -1, -2, -2,  2,  3]
--                   , [ -4, -4,  0,  8,  6]
--                   , [ -8, -6,  1, 16, 12]
--                   , [ -4,  2,  6, 14,  6]
--                   , [ -7, -8, -2,  8,  9]
--                   ]
--          in computeAs U result `shouldBe` expectation

-- mapStride :: Index ix => Int -> ix -> Array DW ix e -> Array DW ix e
-- mapStride stride sz =
--   let toOldIndex = liftIndex (* stride)
--       ceilingDivStride a = ceiling $ (fromIntegral a :: Double) / fromIntegral stride
--       toNewIndex = liftIndex ceilingDivStride
--    in unsafeBackpermuteDW toNewIndex toOldIndex sz
