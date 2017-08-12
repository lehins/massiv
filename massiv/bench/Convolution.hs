{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import           CommonMassiv
import           CommonRepa
import           Criterion.Main
import           Data.Array.Massiv         as M hiding ((:.), Z)
import           Data.Array.Massiv.Stencil as M
import           Data.Array.Repa           as R
import           Data.Default

-- -- | Repa stencil base Kirsch W horizontal convolution
-- kirschWR
--   :: (R.Source r e, Num e) => R.Array r R.DIM2 e
--      -> R.Array PC5 R.DIM2 e
-- kirschWR = mapStencil2 BoundClamp stencil
--   where stencil = makeStencil2 3 3
--                   (\ix -> case ix of
--                       Z :. -1 :. -1 -> Just 5
--                       Z :. -1 :.  0 -> Just (-3)
--                       Z :. -1 :.  1 -> Just (-3)
--                       Z :.  0 :. -1 -> Just 5
--                       Z :.  0 :.  1 -> Just (-3)
--                       Z :.  1 :. -1 -> Just 5
--                       Z :.  1 :.  0 -> Just (-3)
--                       Z :.  1 :.  1 -> Just (-3)
--                       _             -> Nothing)


-- validate :: (Int, Int) -> (R.Array R.U R.DIM2 Int, R.Array R.U R.DIM2 Int)
-- validate (m, n) = (sR, sVR)
--   where
--     arrCR = R.computeUnboxedS (arrRLight (m, n) :: R.Array R.D R.DIM2 Int)
--     sR = R.computeUnboxedS . sobelXR $ arrCR
--     arrCU = M.computeUnboxedS (arrMLight (m, n))
--     mArr = M.computeS $ mapStencil (sobelStencilX Edge) arrCU
--     --arrVU = VC.makeVUArray (m, n) lightF :: VC.VUArray Int
--     --mArr = VC.applyFilter (VC.sobelFilter VC.Horizontal Edge) arrVU
--     sVR = R.fromUnboxed (Z :. m :. n) $ M.toVectorUnboxed mArr
 
-- validateOperator :: (Int, Int) -> (R.Array R.U R.DIM2 Double, R.Array R.U R.DIM2 Double)
-- validateOperator (m, n) = (sR, sVR)
--   where
--     arrCR = R.computeUnboxedS (arrRLight (m, n) :: R.Array R.D R.DIM2 Double)
--     sR = sobelOperatorR' arrCR
--     mArr = sobelOperator' Edge arrCU
--     sVR = R.fromUnboxed (Z :. m :. n) $ M.toVectorUnboxed mArr

sobelOperatorR' :: R.Source r Double =>
                  R.Array r R.DIM2 Double -> R.Array R.U R.DIM2 Double
sobelOperatorR' arr =
  R.computeUnboxedS $ R.map sqrt $ R.zipWith (+) arrX2 arrY2 where
    !arrX2 = R.map (^ (2 :: Int)) $ R.computeUnboxedS (sobelXR arr)
    !arrY2 = R.map (^ (2 :: Int)) $ R.computeUnboxedS (sobelYR arr)

sobelOperatorR :: R.Source r Double =>
                  R.Array r R.DIM2 Double -> R.Array R.U R.DIM2 Double
sobelOperatorR arr =
  R.computeUnboxedS $ R.smap sqrt $ R.szipWith (+) arrX2 arrY2 where
    !arrX2 = R.smap (^ (2 :: Int)) $ sobelXR arr
    !arrY2 = R.smap (^ (2 :: Int)) $ sobelYR arr



sobelStencilX :: Num e => Border e -> Stencil Ix2T e e
sobelStencilX b = mkConvolutionStencil b (3, 3) (1, 1) accum where
  accum f =
     f (-1, -1)   1  .
     f ( 0, -1)   2  .
     f ( 1, -1)   1  .
     f (-1,  1) (-1) .
     f ( 0,  1) (-2) .
     f ( 1,  1) (-1)
  {-# INLINE accum #-}
{-# INLINE sobelStencilX #-}


sobelStencilY :: Num e => Border e -> Stencil Ix2T e e
sobelStencilY b = mkConvolutionStencil b (3, 3) (1, 1) accum where
  accum f =
     f (-1, -1)   1  .
     f (-1,  0)   2  .
     f (-1,  1)   1  .
     f ( 1, -1) (-1) .
     f ( 1,  0) (-2) .
     f ( 1,  1) (-1)
  {-# INLINE accum #-}
{-# INLINE sobelStencilY #-}

-- sobelOperator :: (Default b, Floating b) => Border b -> Stencil Ix2T b b
-- sobelOperator b = fmap sqrt ((+) <$> sX <*> sY) where
--   !sX = fmap (^ (2 :: Int)) (sobelStencilX b)
--   !sY = fmap (^ (2 :: Int)) (sobelStencilY b)
-- {-# INLINE sobelOperator #-}

sobelOperator :: (Default b, Floating b) => Border b -> Stencil Ix2T b b
sobelOperator b = sqrt <$> ((+) <$> sX <*> sY) where
  !sX = (^ (2 :: Int)) <$> sobelStencilX b
  !sY = (^ (2 :: Int)) <$> sobelStencilY b
{-# INLINE sobelOperator #-}


sobelOperator'
  :: (Manifest r Ix2T Double)
  => Border Double -> M.Array r Ix2T Double -> M.Array M.U Ix2T Double
sobelOperator' b arr = M.computeAs M.U $ M.map sqrt $ M.zipWith (+) arrX2 arrY2
  where
    !arrX2 = M.map (^ (2 :: Int)) $ M.computeAs M.U (mapStencil sX arr)
    !arrY2 = M.map (^ (2 :: Int)) $ M.computeAs M.U (mapStencil sY arr)
    !sX = sobelStencilX b
    !sY = sobelStencilY b

sobelOperator'' :: (Default b, Floating b) => Border b -> Stencil Ix2T b b
sobelOperator'' b = sqrt (sX + sY) where
  !sX = (^ (2 :: Int)) <$> sobelStencilX b
  !sY = (^ (2 :: Int)) <$> sobelStencilY b
{-# INLINE sobelOperator'' #-}



-- M.computeUnboxedS (mapStencil (sobelStencilX Edge) (M.computeUnboxedS (arrM (43, 45)))) == M.computeUnboxedS (mapStencil (sobelXSIMD Edge) (M.computeUnboxedS (arrM (43,45))))

main :: IO ()
main = do
  let !sz = (1600, 1200)
      !arrCR = R.computeUnboxedS (arrR sz)
      -- !kirschW = kirschWStencil
      -- !kirschW' = kirschWStencil'
      arrUM :: M.Array M.U Ix2T Double
      !arrUM = M.computeAs M.U (makeArrayR D Seq sz lightFD)
      !sobel = sobelStencilX Edge
      !sobelOp = sobelOperator Edge
      !sobelOp' = sobelOperator' Edge
      !sobelOp'' = sobelOperator'' Edge
      -- !kirschW = kirschWStencil Edge
  defaultMain
    [ bgroup
        "Sobel Horizontal"
        [ bench "Massiv mapStencil" $
          whnf (M.computeAs M.U . mapStencil sobel) arrUM
        , bench "Repa Sobel" $ whnf (R.computeUnboxedS . sobelXR) arrCR
        ]
    , bgroup
        "Sobel Operator"
        [ bench "Massiv stencil operator" $
          whnf (M.computeAs M.U . mapStencil sobelOp) arrUM
        , bench "Massiv stencil operator local" $
          whnf (M.computeAs M.U . mapStencil sobelOp'') arrUM
        , bench "Massiv unfused" $ whnf sobelOp' arrUM
        , bench "Repa fused" $ whnf sobelOperatorR arrCR
        , bench "Repa unfused" $ whnf sobelOperatorR' arrCR
        ]
    -- , bgroup
    --     "KirschW Horizontal"
    --     [ bench "Massiv mapStencil" $
    --       whnf (M.computeUnboxedS . mapStencil kirschW) arrUM
    --     , bench "Repa KirschW" $ whnf (R.computeUnboxedS . kirschWR) arrCR
    --     --, bench "repa R Agorithms" $ whnfIO kirschWRAlg
    --     ]
    ]
