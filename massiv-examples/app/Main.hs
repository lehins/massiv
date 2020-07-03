module Main where

import Data.Massiv.Array
import Data.Massiv.Array.IO
import Examples.Convolution
import qualified Graphics.ColorModel as CM

main :: IO ()
main = do
  let arr = arrLightIx2 Par (600 :. 800)
      -- Without the extra CM.toPixel8 the writeImage failed with:
      --   ConvertError "Format PNG cannot be encoded as <Image S Y Double>"
      img = computeAs S $ fmap CM.toPixel8 $ fmap CM.PixelY arr
  writeImage "files/light.png" img
  putStrLn "written: files/light.png"
  -- But this is now failing because:
  --   No instance for (Fractional Word8) arising from a use of ‘average3x3Filter’
  -- writeImage "files/light_avg.png" $ computeAs S $ mapStencil Edge average3x3Filter img
  -- putStrLn "written: files/light_avg.png"
