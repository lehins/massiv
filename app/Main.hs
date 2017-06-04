{-# LANGUAGE FlexibleContexts      #-}
module Main where


--import           Compute
--import qualified VectorConvolve as VC
import           Data.Array.Massiv                  as M

-- import           Data.Array.Massiv.Manifest.Unboxed

--import           Data.Array.Massiv.Stencil

-- import           Data.Array.Repa                     as R


arrMLight :: (Int, Int) -> M.Array U DIM2 Double
arrMLight arrSz = smakeArray Seq arrSz $ \(i, j) -> fromIntegral (i + j)
{-# INLINE arrMLight #-}


magnitude :: Array U DIM2 Double -> Array U DIM2 Double
magnitude arr = szipWith (+) (smap (*2) arr) (smap (*2) arr)
  where
    -- arrX2 = smap (*2) arr
    -- arrY2 = smap (*2) arr
    -- !arrX2 = M.map (^ (2 :: Int)) $ M.computeUnboxedS (mapStencil sX arr)
    -- !arrY2 = M.map (^ (2 :: Int)) $ M.computeUnboxedS (mapStencil sY arr)
    -- !sX = sobelStencilX b
    -- !sY = sobelStencilY b
{-# INLINE magnitude #-}

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
  -- let arr = arrU `seq` toManifest arrU
  -- let res = arr `seq` foldlS (+) 0 arr
  let arrCM = stranspose $ magnitude $ arrMLight (1600, 1200)
  print arrCM
  -- >>> :m + Data.IORef
  -- >>> var <- newIORef 0 :: IO (IORef Int)
  -- >>> forM_ (range 0 1000) $ \ i -> modifyIORef' var (+i)
-- >>> readIORef var
  -- let arr = computeAs U $ makeArray1D 1518500250 id
  -- var <- newIORef 0 :: IO (IORef Int)
  -- forM_ arr $ \ i -> modifyIORef' var (+i)
  -- res <- readIORef var
  -- let arr = computeAs U $ makeArray1D 1518500250 id
  -- let res = M.sum arr
  --putStrLn $ "Result is: " ++ show res


  -- let arr = computeAs U $ setComp Par $ makeArray1D 1518500250 id
  -- let res = M.sum arr
  -- putStrLn $ "Result is: " ++ show res
