{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
module Main where


--import           Compute
--import qualified VectorConvolve as VC
import           Data.Array.Massiv                  as M
import           Data.Array.Massiv.Delayed.SIMD.Prim
import Data.Int

lightF :: Num b => (Int, Int) -> b
lightF !(i, j) =
  fromIntegral
    (round (sin (fromIntegral (i ^ (2 :: Int) + j ^ (2 :: Int)) :: Float)) :: Int)
{-# INLINE lightF #-}

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

main :: IO ()
main = do
  let arrCM = arrMLight (1600, 1200)
  let arr :: Array P DIM2 Int32
      arr = computeAs P $ M.map round arrCM
  arrP <- computeInt32 $ delaySIMD32 arr
  --print $ toList2D arrP
  print (arrP == arr)
