{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Compute
import           Criterion.Main
import           Data.Array.Massiv                     as M
import           Data.Array.Massiv.Delayed.Interleaved as M

import           Data.Array.Repa                       as R
import           Data.Functor.Identity
import           Prelude                               as P


main :: IO ()
main = do
  let !sz = (1600, 1200) :: M.DIM2
      nestedArrM !(m, n) = makeArray1D m (arr !>)
        where
          !arr = arrM (m, n)
      {-# INLINE nestedArrM #-}
  defaultMain
    [ bgroup
        "Load"
        [ bgroup
            "Light"
            [ bench "Array Massiv" $ whnf (M.computeUnboxedP . arrM) sz
            , bench "Array Massiv ID" $
              whnf (M.computeUnboxedP . toInterleaved . arrM) sz
            , bench "Array Repa" $
              whnf (runIdentity . R.computeUnboxedP . arrR) sz
            ]
        , bgroup
            "Heavy"
            [ bench "Array Massiv" $ whnf (M.computeUnboxedP . arrM') sz
            , bench "Array Massiv ID" $
              whnf (M.computeUnboxedP . toInterleaved . arrM') sz
            , bench "Array Repa" $
              whnf (runIdentity . R.computeUnboxedP . arrR') sz
            ]
        , bgroup
            "Windowed"
            [ bench "Array Massiv" $ whnf (M.computeUnboxedP . arrWindowedM) sz
            , bench "Array Repa" $
              whnf (runIdentity . R.computeUnboxedP . arrWindowedR) sz
            ]
        ]
    , bgroup
        "Fold"
        [ -- bench "Array Massiv Rows" $ nfIO (M.foldlP' (+) 0 (+) 0 . arrM $ sz)
        -- ,
          bench "Array Massiv" $ whnf (M.sumP . arrM) sz
        , bench "Array Massiv Nested" $
          whnf (M.sumP . (M.map M.sumP) . nestedArrM) sz
        , bench "Array Repa" $ whnf (runIdentity . sumAllP . arrR) sz
        ]
    , bgroup
        "toList"
        [ bench "Array Massiv 2D Seq" $ nf (M.toListS2D . arrM) sz
    --     , bench "Array Massiv 2D toListP'" $ nf (M.toListP' . arrM) sz
    --     , bench "Array Massiv 2D'" $ nfIO (M.toListP2D' . arrM $ sz)
        , bench "Array Massiv 2D" $ nfIO (M.toListP2D . arrM $ sz)
        ]
    , bgroup
        "Fuse"
        [ bench "Array Massiv" $
          whnf (M.computeUnboxedP . M.map (+ 25) . arrM) sz
        , bench "Array Repa" $
          whnf (runIdentity . R.computeUnboxedP . R.map (+ 25) . arrR) sz
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
