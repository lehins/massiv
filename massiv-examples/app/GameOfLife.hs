{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists  #-}
module Main where

import           Control.Concurrent
import           Data.Massiv.Array                 as A
import           Data.Word
import           Graphics.ColorSpace
import           Graphics.UI.GLUT                  as G

lifeRules :: Word8 -> Word8 -> Word8
lifeRules 0 3 = 1
lifeRules 1 2 = 1
lifeRules 1 3 = 1
lifeRules _ _ = 0

lifeStencil :: Stencil Ix2 Word8 Word8
lifeStencil = makeStencil Wrap (3 :. 3) (1 :. 1) $ \ get ->
  lifeRules <$> get (0 :. 0) <*>
  (get (-1 :. -1) + get (-1 :. 0) + get (-1 :. 1) +
   get ( 0 :. -1)         +         get ( 0 :. 1) +
   get ( 1 :. -1) + get ( 1 :. 0) + get ( 1 :. 1))

life :: Array S Ix2 Word8 -> Array S Ix2 Word8
life = compute . A.mapStencil lifeStencil

initLife :: Ix2 -> Array S Ix2 Word8 -> Array S Ix2 Word8
initLife sz arr =
  compute $
  makeWindowedArray (makeArrayR D Seq sz (const 0)) ix0 (size arr) (index' arr . subtract ix0)
  where
    ix0 = liftIndex (`div` 2) (sz - size arr)


blinker :: Array S Ix2 Word8
blinker = [ [0, 1, 0]
          , [0, 1, 0]
          , [0, 1, 0] ]


glider :: Array S Ix2 Word8
glider = [ [0, 1, 0]
         , [0, 0, 1]
         , [1, 1, 1] ]

-- | Scale the array, negate values and create an image with a grid.
pixelGrid :: Word8 -> Array S Ix2 Word8 -> Array D Ix2 Word8
pixelGrid k8 arr = A.transpose $ A.traverse sz' getNewPx arr
  where
    k = succ $ fromIntegral k8
    (m :. n) = size arr
    sz' = (1 + m * k :. 1 + n * k)
    getNewPx getPx (i :. j) =
            if i `mod` k == 0 || j `mod` k == 0
              then 128
              else ((1 - getPx ((i - 1) `div` k :. (j - 1) `div` k)) * 255)

sizeFromIx2 :: Ix2 -> G.Size
sizeFromIx2 (m :. n) = Size (fromIntegral n) (fromIntegral m)

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  w <- createWindow "Game of Life"
  rowAlignment Unpack $= 1
  displayCallback $= display
  mainLoop

drawLife :: Array S Ix2 Word8 -> IO ()
drawLife arr = do
  let grid = compute $ pixelGrid 10 arr
  A.unsafeWithPtr grid $ \ptr ->
    drawPixels (sizeFromIx2 (size grid)) (PixelData Luminance UnsignedByte ptr)

display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  gameOfLife (initLife (27 :. 27) glider)

gameOfLife :: Array S Ix2 Word8 -> IO ()
gameOfLife arr = do
  drawLife arr
  flush
  addTimerCallback 20 $ gameOfLife (life arr)



