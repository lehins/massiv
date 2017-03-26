{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Main where

-- import qualified Data.Vector.Unboxed             as VU
import           Data.Array.Massiv                  as M
-- import           Data.Array.Massiv.Convolution
-- import           Data.Array.Massiv.Windowed
import           Data.Array.Massiv.Manifest.Unboxed as M
-- import           Data.Array.Repa                    as R



import           Data.Time.Clock.POSIX
import           Prelude                            as P


main :: IO ()
main = do
  t0 <- getPOSIXTime
  -- let makeArrR :: (Int, Int) -> R.Array R.D R.DIM2 Double
  --     makeArrR !(m, n) =
  --       fromFunction
  --         (Z :. m :. n)
  --         (\(Z :. i :. j) -> fromIntegral (min i j `div` (1 + max i j)))
  --let arrM :: M.Array M M.DIM2 Double
  arrM <-
        M.computeUnboxedPIO $
        makeArray2D
          (32560, 42560)
          (\ !(i, j) -> (fromIntegral ((max i j) `div` (1 + min i j))))
  --arrR <- return $ R.computeUnboxedS $ makeArrR (215600, 215600)
  print (arrM :: M.Array M M.DIM2 Double)
  print (arrM ! 1000 ! 12000)
  t1 <- getPOSIXTime
  print (t1 - t0)
  -- threadDelay 1000000
  -- t1' <- getPOSIXTime
  -- let !sobel = M.computeUnboxedP $ sobelHorizontal arrM
  -- print sobel
  -- t2 <- getPOSIXTime
  -- print (t2 - t1')
