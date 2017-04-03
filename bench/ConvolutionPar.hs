{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Main where

import           Compute
import           Criterion.Main
import           Data.Array.Massiv                     as M
import           Data.Array.Massiv.Compute             as M
import           Data.Array.Massiv.Manifest.Unboxed    as M
import           Data.Array.Massiv.Stencil             as M
-- import           Data.Array.Massiv.Stencil.Convolution as M
import           Data.Array.Repa                       as R
import           Data.Array.Repa.Algorithms.Convolve   as R
import           Data.Array.Repa.Eval                  as R
import           Data.Array.Repa.Repr.Unboxed          as R


forceP
  :: (R.Load r1 sh e, Unbox e, Monad m)
  => R.Array r1 sh e -> m (R.Array R.U sh e)
forceP !arr = do
    forcedArr <- R.computeUnboxedP arr
    forcedArr `deepSeqArray` return forcedArr


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
                  R.Array r R.DIM2 Double -> IO (R.Array R.U R.DIM2 Double)
sobelOperatorR' arr = do
  arrX <- forceP (sobelXR arr)
  arrY <- forceP (sobelYR arr)
  forceP $
    R.map sqrt $
    R.zipWith (+) (R.map (^ (2 :: Int)) arrX) (R.map (^ (2 :: Int)) arrY)

sobelOperatorR :: R.Source r Double =>
                  R.Array r R.DIM2 Double -> IO (R.Array R.U R.DIM2 Double)
sobelOperatorR arr =
  forceP $ R.smap sqrt $ R.szipWith (+) arrX2 arrY2 where
    !arrX2 = R.smap (^ (2 :: Int)) $ sobelXR arr
    !arrY2 = R.smap (^ (2 :: Int)) $ sobelYR arr


sobelOperator'
  :: (Manifest r M.DIM2 Double)
  => Border Double -> M.Array r M.DIM2 Double -> IO (M.Array M.U M.DIM2 Double)
sobelOperator' b arr = do
  arrX <- M.computeUnboxedP (mapStencil (sobelStencilX b) arr)
  arrY <- M.computeUnboxedP (mapStencil (sobelStencilY b) arr)
  M.computeUnboxedP $
    M.map sqrt $
    M.zipWith (+) (M.map (^ (2 :: Int)) arrX) (M.map (^ (2 :: Int)) arrY)

main :: IO ()
main = do
  let !sz = (2502, 2602)
      !arrCR = R.computeUnboxedS (arrR sz)
      !arrCU = M.computeUnboxedS (arrM sz)
      !arrCM = M.computeManifestS U (arrM sz)
      -- !kirschW = kirschWStencil
      -- !kirschW' = kirschWStencil'
      arrUM :: M.Array M.U M.DIM2 Double
      !arrUM = M.computeUnboxedS (arrM sz)
      !sobel = sobelStencilX Edge
      !sobelKern = sobelKernelStencilX Edge
      !sobelOp = sobelOperator Edge
      !sobelOp' = sobelOperator' Edge
      !kirschW = kirschWStencil Edge
      !sobelKernelR = R.fromListUnboxed (Z :. 3 :. 3) [-1, 0, 1, -2, 0, 2, -1, 0, 1]
      sobelKernRP = convolveOutP outClamp sobelKernelR
  defaultMain
    [ bgroup
        "Sobel Horizontal"
        [ bench "Massiv mapStencil" $
          whnfIO (M.computeUnboxedP . mapStencil sobel $ arrUM)
        , bench "Massiv mapStencil UNSAFE" $
          whnf (M.unsafeComputeUnboxedP . mapStencil sobel) arrUM
        , bench "Repa Sobel" $ whnfIO (forceP (sobelXR arrCR))
        ]
    , bgroup
        "Sobel Horizontal Kernel"
        [ bench "Massiv mapStencil" $
          whnfIO (M.computeUnboxedP . mapStencil sobelKern $ arrUM)
        , bench "repa R Agorithms" $ whnfIO (sobelKernRP arrCR)
        ]
    , bgroup
        "Sobel Operator"
        [ bench "Massiv stencil operator" $
          whnfIO (M.computeUnboxedP . mapStencil sobelOp $ arrUM)
        , bench "Massiv stencil operator UNSAFE" $
          whnf (M.unsafeComputeUnboxedP . mapStencil sobelOp) arrUM
        , bench "Massiv unfused" $ whnfIO (sobelOp' arrUM)
        , bench "Repa fused" $ whnfIO (sobelOperatorR arrCR)
        , bench "Repa unfused" $ whnfIO (sobelOperatorR' arrCR)
        ]
    -- , bgroup
    --     "KirschW Horizontal"
    --     [ bench "Massiv mapStencil" $
    --       whnf (M.computeUnboxedS . mapStencil kirschW) arrUM
    --     , bench "Repa KirschW" $ whnf (R.computeUnboxedS . kirschWR) arrCR
    --     --, bench "repa R Agorithms" $ whnfIO kirschWRAlg
    --     ]
    ]
