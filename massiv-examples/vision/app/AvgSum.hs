{-# LANGUAGE FlexibleContexts #-}

module Main where


import Data.Default
import Data.Massiv.Array as A
import Data.Massiv.Array.IO


arrLightIx2 :: Comp -> Ix2 -> Array D Ix2 Double
arrLightIx2 comp arrSz = makeArray comp (Sz arrSz) lightFunc
    where lightFunc (i :. j) = sin (fromIntegral (i ^ (2 :: Int) + j ^ (2 :: Int)) :: Double)
{-# INLINE arrLightIx2 #-}


average3x3Filter :: (Default a, Fractional a) => Stencil Ix2 a a
average3x3Filter = makeStencil (Sz (3 :. 3)) (1 :. 1) $ \ get ->
  (  get (-1 :. -1) + get (-1 :. 0) + get (-1 :. 1) +
     get ( 0 :. -1) + get ( 0 :. 0) + get ( 0 :. 1) +
     get ( 1 :. -1) + get ( 1 :. 0) + get ( 1 :. 1)   ) / 9
{-# INLINE average3x3Filter #-}


sum3x3Filter :: Fractional a => Stencil Ix2 a a
sum3x3Filter = makeConvolutionStencil (Sz (3 :. 3)) (1 :. 1) $ \ get ->
  get (-1 :. -1) 1 . get (-1 :. 0) 1 . get (-1 :. 1) 1 .
  get ( 0 :. -1) 1 . get ( 0 :. 0) 1 . get ( 0 :. 1) 1 .
  get ( 1 :. -1) 1 . get ( 1 :. 0) 1 . get ( 1 :. 1) 1
{-# INLINE sum3x3Filter #-}



main :: IO ()
main = do
  let arr = computeAs S $ arrLightIx2 Par (600 :. 800)
      toImage ::
           (Functor (Array r Ix2), Load r Ix2 (Pixel (Y' SRGB) Word8))
        => Array r Ix2 Double
        -> Image S (Y' SRGB) Word8
      toImage = computeAs S . fmap (PixelY' . toWord8)
      lightPath = "files/light.png"
      lightAvgPath = "files/light_avg.png"
      lightSumPath = "files/light_sum.png"
  writeImage lightPath $ toImage $ delay arr
  putStrLn $ "written: " ++ lightPath
  writeImage lightAvgPath $ toImage $ mapStencil Edge average3x3Filter arr
  putStrLn $ "written: " ++ lightAvgPath
  writeImage lightSumPath $ toImage $ mapStencil Edge sum3x3Filter arr
  putStrLn $ "written: " ++ lightSumPath
