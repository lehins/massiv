module Main where

import           Data.Massiv.Array
import           Data.Massiv.Array.IO
import           Examples.Convolution
import           Graphics.ColorSpace


main :: IO ()
main = do
  let arr = arrLightIx2 Par (600 :. 800)
      img = computeAs S $ fmap PixelY arr
  writeImage "files/light.png" img
  putStrLn "written: files/light.png"
  writeImage "files/light_avg.png" $ computeAs S $ mapStencil Edge average3x3Filter img
  putStrLn "written: files/light_avg.png"
