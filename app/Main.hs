{-# LANGUAGE FlexibleContexts      #-}
module Main where


--import           Compute
--import qualified VectorConvolve as VC
import           Data.Array.Massiv                  as M
import           Data.Array.Massiv.Ops.Map          as O
-- import           Data.Array.Massiv.Manifest.Unboxed

--import           Data.Array.Massiv.Stencil

-- import           Data.Array.Repa                     as R


arrMLight :: (Int, Int) -> M.Array M.D M.DIM2 Double
--arrMLight !arrSz = makeArray2D arrSz $ \(i, j) -> fromIntegral (i + j)
arrMLight arrSz = unsafeMakeArray Seq arrSz $ \(i, j) -> fromIntegral (i + j)
{-# INLINE arrMLight #-}


magnitude :: (Source r M.DIM2 Double)
          => M.Array r M.DIM2 Double -> M.Array D M.DIM2 Double
magnitude arr = O.zipWith (+) arrX2 arrY2 -- (M.map (*2) arr) (M.map (*2) arr)
  where
    arrX2 = M.map (*2) arr :: M.Array D M.DIM2 Double
    arrY2 = M.map (*2) arr :: M.Array D M.DIM2 Double
    -- !arrX2 = M.map (^ (2 :: Int)) $ M.computeUnboxedS (mapStencil sX arr)
    -- !arrY2 = M.map (^ (2 :: Int)) $ M.computeUnboxedS (mapStencil sY arr)
    -- !sX = sobelStencilX b
    -- !sY = sobelStencilY b
--{-# INLINE sobelOperator' #-}

-- magnitude :: (Source r M.DIM2 Double)
--           => M.Array r M.DIM2 Double -> M.Array M.U M.DIM2 Double
-- magnitude arr = M.computeUnboxedS $ M.map sqrt $ M.zipWith (+) arrX2 arrY2
--   where
--     !arrX2 = M.map (^ (2 :: Int)) $ arr
--     !arrY2 = M.map (^ (2 :: Int)) $ arr

-- repaSobel :: (Int, Int) -> IO (R.Array R.U R.DIM2 Double)
-- repaSobel sz = do
--   let !arrCR = R.computeUnboxedS (arrR sz)
--   let !sobel = R.computeUnboxedS $ sobelGxR arrCR
--   print (extent sobel)
--   return sobel


-- massivSobel :: (Int, Int) -> IO (M.Array M.M M.DIM2 Double)
-- massivSobel sz = do
--   --let !(VC.VUArray _ v) = VC.makeVUArray sz lightF :: VC.VUArray Double
--   let !arrMC = M.computeUnboxedS (arrM sz)
--   let !sobelH = correlate Edge arrMC
--   let !sobel = M.computeUnboxedS sobelH
--   print sobel
--   return sobel


-- unboxSobel :: (Int, Int) -> IO (M.Array M.M M.DIM2 Double)
-- unboxSobel sz = do
--   let !(VC.VUArray _ v) = VC.makeVUArray sz lightF :: VC.VUArray Double
--       !arrUM = UArray sz v
--       !sobelHVC = correlateU Edge arrUM
--   let !sobel = M.computeUnboxedS sobelHVC
--   print sobel
--   return sobel

main :: IO ()
main = do
  --_ <- massivSobel (16000, 16000)
  --_ <- unboxSobel (16000, 16000)
  --_ <- repaSobel (16000, 16000)
  -- let arrU = computeUnboxedS $ makeArray1D 1518500250 succ
  -- let arr = toManifest arrU
  -- let res = foldlS (+) 0 arr
  -- let arrU = computeUnboxedS $ makeArray1D 1518500250 succ
  -- let arr = arrU `seq` toManifest arrU
  -- let res = arr `seq` foldlS (+) 0 arr
  let arrCM = magnitude $ arrMLight (1600, 1200)
  print arrCM


