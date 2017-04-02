{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Criterion.Main
import           Compute
import           Prelude                            as P
import           Data.Array.Massiv                  as M
import           Data.Array.Massiv.Manifest.Unboxed as M
import           Data.Array.Repa                    as R
import           Data.Array.Repa.Eval               as R
import           Data.Array.Repa.Repr.Unboxed       as R



forceP
  :: (R.Load r1 sh e, R.Unbox e, Monad m)
  => R.Array r1 sh e -> m (R.Array R.U sh e)
forceP !arr = do
    forcedArr <- R.computeUnboxedP arr
    forcedArr `deepSeqArray` return forcedArr

main :: IO ()
main = do
  let !sz = (1600, 1200 :: Int)
  defaultMain
    [ bgroup
        "Load"
        [ bgroup
            "Light"
            [ bench "Array Massiv" $ whnfIO (M.computeUnboxedP $ arrM sz)
            , bench "Array Repa" $ whnfIO (forceP $ arrR sz)
            ]
        , bgroup
            "Heavy"
            [ bench "Array Massiv" $ whnfIO (M.computeUnboxedP $ arrM' sz)
            , bench "Array Repa" $ whnfIO (forceP $ arrR' sz)
            ]
        , bgroup
            "Windowed"
            [ bench "Array Massiv IO" $
              whnfIO (M.computeUnboxedP $ arrWindowedM sz)
            , bench "Array Repa" $ whnfIO (forceP $ arrWindowedR sz)
            ]
        ]
    , bgroup
        "Fuse"
        [ bench "Array Massiv IO" $
          whnfIO (M.computeUnboxedP $ mapA (+ 25) $ arrM sz)
        , bench "Array Repa" $ whnfIO (forceP $ R.map (+ 25) $ arrR sz)
        ]
    ]

-- main :: IO ()
-- main = do
--   let !sz = (1600, 1200) :: M.DIM2
--   let arrR :: (Int, Int) -> R.Array R.D R.DIM2 Double
--       arrR !(m, n) =
--         fromFunction
--           (Z :. m :. n)
--           (\(Z :. i :. j) -> fromIntegral (min i j `div` (1 + max i j)))
--   let arrM :: (Int, Int) -> M.Array M.D M.DIM2 Double
--       arrM !arrSz =
--         makeArray2D
--           arrSz
--           (\ !(i, j) -> fromIntegral (min i j `div` (1 + max i j)))
--   defaultMain
--     [ bgroup
--         "Load"
--         [ bench "Array Massiv" $ whnf (M.computeUnboxedP . arrM) sz
--         , bench "Array Massiv IO" $ whnfIO (M.computeUnboxedPIO (arrM sz))
--         , bench "Array Repa" $ whnfIO (forceP (arrR sz))
--         ]
--     , bgroup
--         "Fuse"
--         [ bench "Array Massiv" $
--           whnf (M.computeUnboxedP . mapA (+ 25) . arrM) sz
--         , bench "Array Massiv IO" $
--           whnfIO (M.computeUnboxedPIO . mapA (+ 25) . arrM $ sz)
--         , bench "Array Repa" $ whnfIO (forceP . R.map (+ 25) . arrR $ sz)
--         ]
--     ]
