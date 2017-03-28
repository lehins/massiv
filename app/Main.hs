{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Main where


import Compute
import qualified VectorConvolve as VC
import           Data.Array.Massiv                  as M
import           Data.Array.Massiv.Convolution
import           Data.Array.Repa                     as R




repaSobel :: (Int, Int) -> IO (R.Array U R.DIM2 Double)
repaSobel sz = do
  let !sobel = R.computeUnboxedS $ sobelGxR $ arrR sz
  print (extent sobel)
  return sobel
{-# INLINE repaSobel #-}


massivSobel :: (Int, Int) -> IO (M.Array M.M M.DIM2 Double)
massivSobel sz = do
  arrMC <- M.computeUnboxedIO $ arrM sz
  let !sobelH = sobelStencil' Horizontal
  let !sobel = M.computeUnboxedS $ mapStencil2D sobelH arrMC
  print sobel
  return sobel



vectorSobel :: (Int, Int) -> IO (VC.VUArray Double)
vectorSobel sz = do
  let arrVU :: VC.VUArray Double
      !arrVU = VC.makeVUArray sz lightF
      !sobelHVC = VC.sobelFilter VC.Horizontal 0
  let !sobel = VC.applyFilter sobelHVC arrVU
  print sobel
  return sobel


main :: IO ()
main = do
  --_ <- massivSobel (1600, 1600)
  _ <- vectorSobel (1600, 1600)
  --_ <- repaSobel (1600, 1600)
  return ()

