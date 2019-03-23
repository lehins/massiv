{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Control.Monad
import Data.Massiv.Array as A hiding (windowSize)
import Data.Massiv.Array.Unsafe as A
import Data.Word
import Graphics.UI.GLUT as G
import System.Exit (ExitCode(..), exitSuccess, exitWith)
import Text.Read (readMaybe)

lifeRules :: Word8 -> Word8 -> Word8
lifeRules 0 3 = 1
lifeRules 1 2 = 1
lifeRules 1 3 = 1
lifeRules _ _ = 0

lifeStencil :: Stencil Ix2 Word8 Word8
lifeStencil = makeStencil (Sz (3 :. 3)) (1 :. 1) $ \ f ->
  lifeRules <$> f (0 :. 0) <*>
  (f (-1 :. -1) + f (-1 :. 0) + f (-1 :. 1) +
   f ( 0 :. -1) +               f ( 0 :. 1) +
   f ( 1 :. -1) + f ( 1 :. 0) + f ( 1 :. 1))

life :: Array S Ix2 Word8 -> Array S Ix2 Word8
life = compute . A.mapStencil Wrap lifeStencil

initLife :: Sz2 -> Array S Ix2 Word8 -> Array S Ix2 Word8
initLife sz arr =
  compute $
  insertWindow (A.replicate Par sz 0) (Window ix0 (size arr) (index' arr . subtract ix0) Nothing)
  where
    ix0 = liftIndex (`div` 2) (unSz (sz - size arr))


blinker :: Array S Ix2 Word8
blinker = [ [0, 1, 0]
          , [0, 1, 0]
          , [0, 1, 0] ]


glider :: Array S Ix2 Word8
glider = [ [0, 1, 0]
         , [0, 0, 1]
         , [1, 1, 1] ]

inf2 :: Array S Ix2 Word8
inf2 = [ [1, 1, 1, 0, 1]
       , [1, 0, 0, 0, 0]
       , [0, 0, 0, 1, 1]
       , [0, 1, 1, 0, 1]
       , [1, 0, 1, 0, 1] ]

-- | Scale the array, negate values and create an image with a grid.
pixelGrid :: Int -> Array S Ix2 Word8 -> Array D Ix2 Word8
pixelGrid k8 arr = A.makeArray (getComp arr) sz' getNewElt
  where
    k = succ k8
    Sz (n :. m) = size arr
    sz' = Sz (1 + m * k :. 1 + n * k)
    getNewElt (j :. i) =
      if i `mod` k == 0 || j `mod` k == 0
        then 128
        else (1 - A.unsafeIndex arr ((i - 1) `div` k :. (j - 1) `div` k)) * 255

sizeFromSz2 :: Sz2 -> G.Size
sizeFromSz2 (Sz2 m n) = Size (fromIntegral n) (fromIntegral m)

main :: IO ()
main = do
  let helpTxt =
        "Usage:\n\
                \    life [WIDTH HEIGHT] [SCALE]\n\
                \ * WIDTH - number of cells horizontally (default 100)\n\
                \ * HEIGHT - number of cells vertically (default 70)\n\
                \ * SCALE - scaling factor, or how many pixels one cell should take on a screen\n"
  (_progName, args) <- getArgsAndInitialize
  when (args == ["--help"]) $ putStrLn helpTxt >> exitSuccess
  (m, n, s) <- case fmap readMaybe args of
    [Just m, Just n, Just s]
      | m > 0 && n > 0 && s > 0 -> return (m, n, s)
    [Just m, Just n]
      | m > 0 && n > 0 -> return (m, n, 10)
    [] -> return (100, 70, 10)
    _ -> do
      putStrLn "Invalid arguments."
      putStrLn helpTxt
      exitWith $ ExitFailure 1
  _w <- createWindow "Game of Life"
  startGameOfLife (Sz2 m n) s
  mainLoop


startGameOfLife :: Sz2 -> Int -> IO ()
startGameOfLife sz s = do
  rowAlignment Unpack $= 1
  let iLife = initLife sz inf2
      wSz = size (pixelGrid s iLife)
  G.windowSize $= sizeFromSz2 wSz
  mArr <- new wSz
  displayCallback $= clear [ColorBuffer]
  drawLife s mArr iLife
  runGameOfLife s mArr iLife


drawLife :: Int -> MArray RealWorld S Ix2 Word8 -> Array S Ix2 Word8 -> IO ()
drawLife s mArr arr = do
  computeInto mArr $ pixelGrid s arr
  A.withPtr mArr $ \ptr ->
    drawPixels (sizeFromSz2 (msize mArr)) (PixelData Luminance UnsignedByte ptr)


drawLifeStep :: Int -> MArray RealWorld S Ix2 Word8 -> Array D Ix2 (Word8, Word8) -> IO ()
drawLifeStep s mArr arr = do
  imapM_ updateCellLife arr
  A.withPtr mArr $ \ptr ->
    drawPixels (sizeFromSz2 (msize mArr)) (PixelData Luminance UnsignedByte ptr)
  where
    k = s + 1
    updateCellLife (i :. j) (prev, next) =
      when (prev /= next) $ do
        let ixArr = makeArrayR D Seq (Sz2 s s) $ \(jc :. ic) -> (1 + jc + j * k) :. (1 + ic + i * k)
            nVal = (1 - next) * 255
        A.forM_ ixArr $ \ix -> write mArr ix nVal

runGameOfLife :: Int -> MArray RealWorld S Ix2 Word8 -> Array S Ix2 Word8 -> IO ()
runGameOfLife s mArr = go
  where
    go arr = do
      let nextLife = life arr
      drawLifeStep s mArr $ A.zip arr nextLife
      flush
      addTimerCallback 10 $ go nextLife
