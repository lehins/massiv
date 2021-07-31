{-# LANGUAGE FlexibleContexts #-}

module Main where


import Data.Default
import Data.Massiv.Array as A
import Data.Massiv.Array.IO


arrLightIx2 :: Comp -> Ix2 -> Array D Ix2 Double
arrLightIx2 comp arrSz = makeArray comp (Sz arrSz) lightFunc
    where lightFunc (i :. j) = sin (fromIntegral (i ^ (2 :: Int) + j ^ (2 :: Int)) :: Double)
{-# INLINE arrLightIx2 #-}


main :: IO ()
main = do
  let arr = computeAs S $ arrLightIx2 Par (600 :. 800)
      toImage ::
           (Functor (Array r Ix2), Load r Ix2 (Pixel (Y' SRGB) Word8))
        => Array r Ix2 Double
        -> Image S (Y' SRGB) Word8
      toImage = computeAs S . fmap (PixelY' . toWord8)
      lightPath = "files/light.png"
      lightImage = toImage $ delay arr
      lightAvgPath = "files/light_avg.png"
      lightAvgImage = toImage $ mapStencil Edge (avgStencil 3) arr
      lightSumPath = "files/light_sum.png"
      lightSumImage = toImage $ mapStencil Edge (sumStencil 3) arr
  writeImage lightPath lightImage
  putStrLn $ "written: " ++ lightPath
  writeImage lightAvgPath lightAvgImage
  putStrLn $ "written: " ++ lightAvgPath
  writeImage lightSumPath lightSumImage
  putStrLn $ "written: " ++ lightSumPath
  displayImageUsing defaultViewer True . computeAs S
    =<< concatM 1 [lightAvgImage, lightImage, lightSumImage]
