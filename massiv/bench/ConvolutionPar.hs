{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Main where

import           CommonMassiv
import           CommonRepa
import           Criterion.Main
import           Data.Array.Massiv                   as M hiding ((:.), Z)
import           Data.Functor.Identity
-- import           Data.Array.Massiv.Compute             as M
import           Data.Array.Massiv.Stencil           as M
-- import           Data.Array.Massiv.Stencil.Convolution as M
import           Data.Array.Repa                     as R
import           Data.Array.Repa.Algorithms.Convolve as R

-- import           Data.Array.Repa.Repr.Unboxed          as R


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
--     arrCU = M.computeUnboxedS (arrMLight (m, n))
--     mArr = M.computeS $ mapStencil (sobelOperator Edge) arrCU
--     --arrVU = VC.makeVUArray (m, n) lightF :: VC.VUArray Int
--     --mArr = VC.applyFilter (VC.sobelFilter VC.Horizontal Edge) arrVU
--     sVR = R.fromUnboxed (Z :. m :. n) $ M.toVectorUnboxed mArr

sobelOperatorR' :: R.Source r Double =>
                  R.Array r R.DIM2 Double -> Identity (R.Array R.U R.DIM2 Double)
sobelOperatorR' !arr = do
  arrX <- R.computeUnboxedP (sobelXR arr)
  arrY <- R.computeUnboxedP (sobelYR arr)
  R.computeUnboxedP $
    R.map sqrt $
    R.zipWith (+) (R.map (^ (2 :: Int)) arrX) (R.map (^ (2 :: Int)) arrY)
-- INLINE or NOINLINE neither should be specified, both slow down the operator

sobelOperatorR :: R.Source r Double =>
                  R.Array r R.DIM2 Double -> Identity (R.Array R.U R.DIM2 Double)
sobelOperatorR !arr =
  R.computeUnboxedP $ R.smap sqrt $ R.szipWith (+) arrX2 arrY2 where
    !arrX2 = R.smap (^ (2 :: Int)) $ sobelXR arr
    !arrY2 = R.smap (^ (2 :: Int)) $ sobelYR arr


sobelOperator'
  :: (Manifest r M.DIM2 Double)
  => Border Double -> M.Array r M.DIM2 Double -> M.Array M.U M.DIM2 Double
sobelOperator' !b !arr =
  M.computeAs M.U $
    M.map sqrt $
    M.zipWith (+) (M.map (^ (2 :: Int)) arrX) (M.map (^ (2 :: Int)) arrY)
  where
    !arrX = M.computeAs M.U (mapStencil (sobelStencilX b) arr)
    !arrY = M.computeAs M.U (mapStencil (sobelStencilY b) arr)
{-# INLINE sobelOperator' #-}

main :: IO ()
main = do
  let !sz = (1600, 1200)
      !arrCR = R.computeUnboxedS (arrR sz)
      -- !kirschW = kirschWStencil
      -- !kirschW' = kirschWStencil'
      !arrCM = M.computeAs M.U (arrM Par sz)
      -- !arrCM' = M.computeAs M.U (M.transpose arrCM)
      !sobel = sobelStencilX Edge
      !sobelKern = sobelKernelStencilX Edge
      !sobelOp = sobelOperator Edge
      !sobelOp' = sobelOperator' Edge
      -- !kirschW = kirschWStencil Edge
      !sobelKernelR = R.fromListUnboxed (Z :. 3 :. 3) [-1, 0, 1, -2, 0, 2, -1, 0, 1]
      sobelKernRP = convolveOutP outClamp sobelKernelR
  defaultMain
    [ bgroup
        "Sobel Horizontal"
        [ bench "Massiv mapStencil" $
          whnf (M.computeAs M.U . mapStencil sobel) arrCM
        , bench "Repa Sobel" $ whnf (runIdentity . R.computeUnboxedP . sobelXR) arrCR
        ]
    , bgroup
        "Sobel Kernel Horizontal"
        [ bench "Massiv mapStencil" $
          whnf (M.computeAs M.U . mapStencil sobelKern) arrCM
        , bench "repa R Agorithms" $ whnfIO (sobelKernRP arrCR)
        ]
    , bgroup
        "Sobel Operator"
        [ bench "Massiv stencil operator" $
          whnf (M.computeAs M.U . mapStencil sobelOp) arrCM
        --, bench "Massiv unfused" $ whnfIO (sobelOp' arrUM)
        , bench "Repa fused" $ whnf (runIdentity . sobelOperatorR) arrCR
        --, bench "Repa unfused" $ whnfIO (sobelOperatorR' arrCR)
        ]
    , bgroup
        "Sobel Unfused Operator"
        [ bench "Massiv" $ whnf sobelOp' arrCM
        , bench "Repa" $ whnf (runIdentity . sobelOperatorR') arrCR
        ]
    , bgroup
        "Fuse"
        [ bench "Array Massiv" $
          whnf (M.computeAs M.U . M.map (+ 25) . arrM Par) sz
        , bench "Array Repa" $
          whnf (runIdentity . R.computeUnboxedP . R.map (+ 25) . arrR) sz
        ]
    -- , bgroup
    --     "KirschW Horizontal"
    --     [ bench "Massiv mapStencil" $
    --       whnf (M.computeUnboxedS . mapStencil kirschW) arrUM
    --     , bench "Repa KirschW" $ whnf (R.computeUnboxedS . kirschWR) arrCR
    --     --, bench "repa R Agorithms" $ whnfIO kirschWRAlg
    --     ]
    ]
