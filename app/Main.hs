module Main where


--import           Compute
--import qualified VectorConvolve as VC
import           Data.Array.Massiv                  as M
import           Data.Array.Massiv.Manifest.Unboxed

-- import           Data.Array.Massiv.Stencil

-- import           Data.Array.Repa                     as R




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
  --let a = M.computeUnboxedP $ arrM (1600, 1201)
  --let arr = makeArray1D 3037000500 succ
  let arrU = computeAs U $ makeArray1D Seq 1518500250 succ
  let arr = toManifest arrU
  let res = foldlS (+) 0 arr
  -- let arrU = computeUnboxedS $ makeArray1D 1518500250 succ
  -- let arr = arrU `seq` toManifest arrU
  -- let res = arr `seq` foldlS (+) 0 arr
  print res


