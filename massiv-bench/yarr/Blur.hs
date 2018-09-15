{-# LANGUAGE
    FlexibleContexts, ScopedTypeVariables, BangPatterns,
    QuasiQuotes #-}

module Main (main) where

import System.Environment
import Data.Word

import Control.Monad (when)
import Data.Massiv.Array as A hiding (L, loadS)
import Data.Massiv.Array.Mutable as A
import Data.Massiv.Array.IO as A
import Graphics.ColorSpace
import System.Directory

import Data.Yarr as Y
import Data.Yarr.Shape as S
import Data.Yarr.Convolution
import Data.Yarr.IO.Image as Y
import Data.Yarr.Benchmarking
import Data.Yarr.Utils.FixedVector as V
import Data.Yarr.Utils.Primitive as P

blur :: UArray F L Dim2 Int -> UArray CV CVL Dim2 Float
{-# INLINE blur #-}
blur arr =
    let convolved =
            dConvolveLinearDim2WithStaticStencil
                [dim2St| 2   4   5   4   2
                         4   9  12   9   4
                         5  12  15  12   5
                         4   9  12   9   4
                         2   4   5   4   2 |]
                arr
    in dmap ((/ 159) . fromIntegral) convolved

truncate' f = fromIntegral (truncate f :: Int)

main :: IO ()
main = do
    [imageFile] <- getArgs
    anyImage <- Y.readImage imageFile

    (image :: UArray (SE F) L Dim2 (VecList N3 Int)) <-
        Y.compute (loadS S.fill) $
            mapElems fromIntegral $ readRGBVectors anyImage

    let delayedBlurred = mapElems truncate' $ unsafeMapSlices blur image

    (blurred :: UArray F L Dim2 (VecList N3 Word8)) <- Y.new (extent image)

    benchSlices "seq slice-wise blur" 10 (extent image)
                (loadS S.fill) delayedBlurred blurred

    let db' = dzip construct (slices delayedBlurred)
    bench "Yarr   seq blur" 10 (extent image) $ loadS S.fill db' blurred

    let {-# INLINE ffill #-}
        ffill = S.unrolledFill n2 P.touch
    bench "Yarr   par blur" 10 (extent image) $
        loadSlicesP ffill caps delayedBlurred blurred

    removeFileIfExists ("t-blurred-" ++ imageFile)
    Y.writeImage ("t-blurred-" ++ imageFile) (RGB blurred)

    benchMassivBlur imageFile


blurFilter :: Stencil Ix2 (Pixel RGB Word) (Pixel RGB Float)
blurFilter = (/ 40545) . fmap fromIntegral <$> makeConvolutionStencil (5 :. 5) (2 :. 2) ( \ f ->
    f (-2 :. -2) 2 . f (-2 :. -1) 4  . f (-2 :. 0) 5  . f (-2 :. 1) 4  . f (-2 :. 2) 2 .
    f (-1 :. -2) 4 . f (-1 :. -1) 9  . f (-1 :. 0) 12 . f (-1 :. 1) 9  . f (-1 :. 2) 4 .
    f ( 0 :. -2) 5 . f ( 0 :. -1) 12 . f ( 0 :. 0) 15 . f ( 0 :. 1) 12 . f ( 0 :. 2) 5 .
    f ( 1 :. -2) 4 . f ( 1 :. -1) 9  . f ( 1 :. 0) 12 . f ( 1 :. 1) 9  . f ( 1 :. 2) 4 .
    f ( 2 :. -2) 2 . f ( 2 :. -1) 4  . f ( 2 :. 0) 5  . f ( 2 :. 1) 4  . f ( 2 :. 2) 2)
{-# INLINE blurFilter #-}


benchMassivBlur :: FilePath -> IO ()
benchMassivBlur fileName = do
  img8 <- A.readImageAuto fileName :: IO (A.Image A.S RGB Word8)
  let imgS = A.computeAs S $ setComp Seq $ A.map (fmap fromIntegral) img8
      blurredS = mapStencil Edge blurFilter imgS
      imgP = setComp Par imgS
      blurredP = mapStencil Edge blurFilter imgP
  imgM <- A.new (A.size img8)
  bench "Massiv Seq blur" 10 (fromIx2 (A.size imgS)) (computeInto imgM blurredS)
  bench "Massiv Par blur" 10 (fromIx2 (A.size imgP)) (computeInto imgM blurredP)
  imgBlurred <- A.freeze Par imgM :: IO (A.Image A.S RGB Float)
  removeFileIfExists ("tm-blurred-" ++ fileName)
  A.writeImage ("tm-blurred-" ++ fileName) $ A.computeAs S $ A.map toWord8 imgBlurred


removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fileName = doesFileExist fileName >>= (`when` removeFile fileName)
