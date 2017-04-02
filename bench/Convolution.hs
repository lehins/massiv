{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Main where

import           Compute
import           Criterion.Main
import           Data.Array.Massiv                  as M
import           Data.Array.Massiv.Compute          as M
import           Data.Array.Massiv.Manifest.Unboxed as M
import           Data.Array.Massiv.Stencil          as M
import           Data.Array.Massiv.Stencil          as M
import           Data.Array.Repa                    as R
import qualified VectorConvolve                     as VC


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


validate :: (Int, Int) -> (R.Array R.U R.DIM2 Int, R.Array R.U R.DIM2 Int)
validate (m, n) = (sR, sVR)
  where
    arrCR = R.computeUnboxedS (arrRLight (m, n) :: R.Array R.D R.DIM2 Int)
    sR = R.computeUnboxedS . sobelXR $ arrCR
    arrCU = M.computeUnboxedS (arrMLight (m, n))
    mArr = M.computeS $ mapStencil (sobelStencilX Edge) arrCU
    --arrVU = VC.makeVUArray (m, n) lightF :: VC.VUArray Int
    --mArr = VC.applyFilter (VC.sobelFilter VC.Horizontal Edge) arrVU
    sVR = R.fromUnboxed (Z :. m :. n) $ M.toVectorUnboxed mArr

validateOperator :: (Int, Int) -> (R.Array R.U R.DIM2 Double, R.Array R.U R.DIM2 Double)
validateOperator (m, n) = (sR, sVR)
  where
    arrCR = R.computeUnboxedS (arrRLight (m, n) :: R.Array R.D R.DIM2 Double)
    sR = sobelOperatorR' arrCR
    arrCU = M.computeUnboxedS (arrMLight (m, n))
    mArr = M.computeS $ mapStencil (sobelOperator Edge) arrCU
    --arrVU = VC.makeVUArray (m, n) lightF :: VC.VUArray Int
    --mArr = VC.applyFilter (VC.sobelFilter VC.Horizontal Edge) arrVU
    sVR = R.fromUnboxed (Z :. m :. n) $ M.toVectorUnboxed mArr

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


sobelOperator'
  :: (Manifest r M.DIM2 Double)
  => Border Double -> M.Array r M.DIM2 Double -> M.Array M.U M.DIM2 Double
sobelOperator' b arr = M.computeUnboxedS $ M.map sqrt $ M.zipWith (+) arrX2 arrY2
  where
    !arrX2 = M.map (^ (2 :: Int)) $ M.computeUnboxedS (mapStencil sX arr)
    !arrY2 = M.map (^ (2 :: Int)) $ M.computeUnboxedS (mapStencil sY arr)
    !sX = sobelStencilX b
    !sY = sobelStencilY b


main :: IO ()
main = do
  let !sz = (1502, 602)
      !arrCR = R.computeUnboxedS (arrR sz)
      !arrCU = M.computeUnboxedS (arrM sz)
      !arrCM = M.computeManifestS U (arrM sz)
      -- !kirschW = kirschWStencil
      -- !kirschW' = kirschWStencil'
      !sobelHVC = VC.sobelFilter VC.Horizontal Edge
      arrVU :: VC.VUArray Double
      !arrVU = VC.makeVUArray sz lightF
      arrUM :: M.Array M.U M.DIM2 Double
      !arrUM = M.computeUnboxedS (arrM sz)
      !sobel = sobelStencilX Edge
      !sobelOp = sobelOperator Edge
      !sobelOp' = sobelOperator' Edge
      !kirschW = kirschWStencil Edge
  defaultMain
    [ bgroup
        "Sobel Horizontal"
        [ bench "Massiv mapStencil" $
          whnf (M.computeUnboxedS . mapStencil sobel) arrUM
        , bench "VectorConvolve" $ whnf (VC.applyFilter sobelHVC) arrVU
        , bench "Repa Sobel" $ whnf (R.computeUnboxedS . sobelXR) arrCR
        ]
    , bgroup
        "Sobel Operator"
        [ bench "Massiv stencil operator" $
          whnf (M.computeUnboxedS . mapStencil sobelOp) arrUM
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
