{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Compute
import           Criterion.Main
import           Data.Array.Massiv                     as M
import           Data.Array.Massiv.Delayed.Interleaved as M
import           Data.Array.Massiv.Manifest.Unboxed    as M
import           Data.Array.Repa                       as R
-- import           Data.Foldable                         as F
import           Data.Functor.Identity
import           Prelude                               as P

main :: IO ()
main = do
  let !sz = (1600, 1200) :: M.DIM2
  defaultMain
    [ bgroup
        "Load"
        [ bgroup
            "Light"
            [ bench "Array Massiv" $ whnf (M.computeUnboxedP . arrM) sz
            , bench "Array Massiv ID" $ whnf (M.computeUnboxedP . toInterleaved . arrM) sz
            , bench "Array Repa" $ whnf (runIdentity . R.computeUnboxedP .  arrR) sz
            ]
        , bgroup
            "Heavy"
            [ bench "Array Massiv" $ whnf (M.computeUnboxedP . arrM') sz
            , bench "Array Massiv ID" $ whnf (M.computeUnboxedP . toInterleaved . arrM') sz
            , bench "Array Repa" $ whnf (runIdentity . R.computeUnboxedP . arrR') sz
            ]
        , bgroup
            "Windowed"
            [ bench "Array Massiv" $
              whnf (M.computeUnboxedP . arrWindowedM) sz
            , bench "Array Repa" $ whnf (runIdentity . R.computeUnboxedP .  arrWindowedR) sz
            ]
        ]
    , bgroup
        "Fold"
        [ -- bench "Array Massiv Seq" $
        --   whnf (foldl' (+) 0 . arrM) sz
        -- , 
          bench "Array Massiv Par" $
          whnf (M.sumP . arrM) sz
        , bench "Array Repa" $ whnf (runIdentity . sumAllP . arrR) sz
        ]
    , bgroup
        "Fuse"
        [ bench "Array Massiv" $
          whnf (M.computeUnboxedP . M.map (+ 25) . arrM) sz
        , bench "Array Repa" $ whnf (runIdentity . R.computeUnboxedP .  R.map (+ 25) . arrR) sz
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
