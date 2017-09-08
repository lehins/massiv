{-# LANGUAGE FlexibleContexts      #-}
module Main where


--import           CommonMassiv
--import qualified VectorConvolve as VC
import           Data.Array.Massiv                  as M

-- lightF :: Num b => (Int, Int) -> b
-- lightF !(i, j) =
--   fromIntegral
--     (round (sin (fromIntegral (i ^ (2 :: Int) + j ^ (2 :: Int)) :: Float)) :: Int)
-- {-# INLINE lightF #-}

arrMLight :: Ix2 -> Massiv Ix2 Double
arrMLight arrSz = makeMassiv Seq arrSz $ \(i :. j) -> fromIntegral (i + j)
{-# INLINE arrMLight #-}


-- magnitude :: Array U DIM2 Double -> Array U DIM2 Double
-- magnitude arr = szipWith (+) (smap (*2) arr) (smap (*2) arr)
--   where
--     -- arrX2 = smap (*2) arr
--     -- arrY2 = smap (*2) arr
--     -- !arrX2 = M.map (^ (2 :: Int)) $ M.computeUnboxedS (mapStencil sX arr)
--     -- !arrY2 = M.map (^ (2 :: Int)) $ M.computeUnboxedS (mapStencil sY arr)
--     -- !sX = sobelStencilX b
--     -- !sY = sobelStencilY b
-- {-# INLINE magnitude #-}

main :: IO ()
main = do
  let a = arrMLight (10 :. 20)
      b = arrMLight (30 :. 20)
  print $ M.zipWith (+) a (M.map (*2) b) !> 4 ! 2
