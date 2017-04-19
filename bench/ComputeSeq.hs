{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Compute
import           Criterion.Main
import           Data.Array.Massiv                  as M
import           Data.Array.Massiv.Manifest.Unboxed as M
import           Data.Array.Repa                    as R
import qualified Data.Vector.Unboxed                as VU
import           Prelude                            as P


main :: IO ()
main = do
  let !sz = (1600, 1200) :: M.DIM2
  let !ixM = (1000, 999)
      !ixR = (Z :. 1000 :. 999)
      !ix1D = toLinearIndex sz ixM
  let !arrCM = M.computeUnboxedS $ arrM sz
      !arrCR = R.computeUnboxedS $ arrR sz
      !vecCU = vecU sz
  defaultMain
    [ bgroup
        "Indexing"
        [ bgroup
            "Unsafe"
            [ bench "Massiv 2D" $ whnf (M.unsafeIndex arrCM) ixM
            , bench "Repa 2D" $ whnf (R.unsafeIndex arrCR) ixR
            , bench "Vector 1D" $ whnf (VU.unsafeIndex vecCU) ix1D
            ]
        , bgroup
            "Safe"
            [ bench "Massiv 2D: maybeIndex" $
              whnf (maybe (error "impossible") id . M.maybeIndex arrCM) ixM
            , bench "Massiv 2D: index" $ whnf (M.index arrCM) ixM
            , bench "Massiv 2D: (!)" $ whnf (\ !(i, j) -> (toManifest arrCM) M.<! i M.! j) ixM
            , bench "Massiv 2D: (<!>)" $
              whnf (\ !(i, j) -> (toManifest arrCM) M.<!> (1, i) M.! j) ixM
            , bench "Repa 2D" $ whnf (R.index arrCR) ixR
            , bench "Vector 1D" $ whnf (vecCU VU.!) ix1D
            ]
        , bgroup
            "Linear Unsafe"
            [ bench "Massiv 2D" $ whnf (M.unsafeLinearIndex arrCM) ix1D
            , bench "Repa 2D" $ whnf (R.unsafeLinearIndex arrCR) ix1D
            , bench "Vector 1D" $ whnf (VU.unsafeIndex vecCU) ix1D
            ]
        ]
    , bgroup
        "Load"
        [ bgroup
            "Light"
            [ bench "Array Massiv" $ whnf (M.computeUnboxedS . arrM) sz
            , bench "Array Repa" $ whnf (R.computeUnboxedS . arrR) sz
            , bench "Vector Unboxed" $ whnf vecU sz
            ]
        , bgroup
            "Heavy"
            [ bench "Array Massiv" $ whnf (M.computeUnboxedS . arrM') sz
            , bench "Array Repa" $ whnf (R.computeUnboxedS . arrR') sz
            , bench "Vector Unboxed" $ whnf vecU' sz
            ]
        , bgroup
            "Windowed"
            [ bench "Array Massiv" $ whnf (M.computeUnboxedS . arrWindowedM) sz
            , bench "Array Repa" $ whnf (R.computeUnboxedS . arrWindowedR) sz
            ]
        ]
    , bgroup
        "Fuse"
        [ bgroup
            "map"
            [ bench "Array Massiv" $
              whnf (M.computeUnboxedS . M.map (+ 25) . arrM) sz
            , bench "Array Repa" $
              whnf (R.computeUnboxedS . R.map (+ 25) . arrR) sz
            , bench "Vector Unboxed" $ whnf (VU.map (+ 25) . vecU) sz
            ]
        ]
    , bgroup
        "Append"
        [ bgroup
            "append"
            [ bench "Array Massiv" $
              whnf
                (\sz' ->
                   M.computeUnboxedS $ M.append' 1 (arrM sz') (arrM sz'))
                sz
            , bench "Array Repa" $
              whnf
                (\sz' -> R.computeUnboxedS $ R.append (arrR sz') (arrR sz'))
                sz
            , bench "Vector Unboxed" $
              whnf (\sz' -> (vecU sz') VU.++ (vecU sz')) sz
            ]
        ]
    ]
