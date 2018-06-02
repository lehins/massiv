{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BinaryLiterals #-}
module Main where

import Control.Concurrent
import Data.Bits
import Data.Massiv.Array
import Data.Massiv.Array.Delayed
import Data.Massiv.Array.IO
import Data.Massiv.Array.Unsafe
import Examples.Convolution
import Examples.PixelGrid
import Graphics.ColorSpace


paintThreads ::
     (Load r Ix2 (Pixel RGB Word8), Construct r Ix2 (Pixel RGB Word8))
  => r
  -> Ix2
  -> IO (Image S RGB Word8)
paintThreads r sz = do
  mArr <- unsafeNew sz
  loadP [] (makeArrayR r Par sz (const 0)) (unsafeLinearRead mArr) $ \ i _ -> do
    (cInt, True) <- myThreadId >>= threadCapability
    let c = fromIntegral cInt
    unsafeLinearWrite mArr i (255 * PixelRGB (c .&. 0b100) (c .&. 0b010) (c .&. 0b001))
  unsafeFreeze Par mArr

main :: IO ()
main = do
  caps <- paintThreads D (50 :. 80)
  --displayImageUsing defaultViewer True $ pixelGrid 10 caps
  writeImage "D.png" $ pixelGrid 10 caps
  caps <- paintThreads DI (50 :. 80)
  --displayImageUsing defaultViewer True $ pixelGrid 10 caps
  writeImage "DI.png" $ pixelGrid 10 caps
  -- let arr = arrLightIx2 Par (600 :. 800)
  --     img = computeAs S $ fmap PixelY arr
  -- writeImage "files/light.png" img
  -- putStrLn "written: files/light.png"
  -- writeImage "files/light_avg.png" $ computeAs S $ mapStencil (average3x3Filter Edge) img
  -- putStrLn "written: files/light_avg.png"
