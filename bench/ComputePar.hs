{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Compute
import           Criterion.Main
import           Data.Array.Massiv                     as M
import           Data.Array.Massiv.Delayed.Interleaved as M
import           Data.Array.Massiv.Numeric
import           Data.Array.Repa                       as R
import           Data.Array.Repa.Algorithms.Matrix     as R (transpose2S)
import           Data.Functor.Identity
import           Prelude                               as P

main :: IO ()
main = do
  let !sz = (1600, 1200) :: M.DIM2
      !ixR = (Z :. fst sz :. snd sz)
  let !szs = (600, 200) :: M.DIM2
  let !szf = (120, 1200) :: M.DIM2
  let !arrCM = M.computeUnboxedS $ arrM sz
      !arrCMs = M.computeUnboxedS $ arrM szs
      !arrCMs' = M.computeUnboxedS (M.transpose arrCMs)
      !arrCRs = R.computeUnboxedS $ arrR szs
      !arrCRs' = R.transpose2S arrCRs
      !ls1D = toListS1D arrCM
      !ls2D = toListS2D arrCM
      nestedArrM !(m, n) = makeArray1D m (arr !>)
        where
          !arr = arrM (m, n)
      {-# INLINE nestedArrM #-}
  arrCRs' `R.deepSeqArray` defaultMain
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
        [ bench "Array Massiv" $ whnf (M.sumP . arrM) sz
        , bench "Array Massiv Nested" $
          whnf (M.sumP . (M.map M.sumP) . nestedArrM) sz
        , bench "Array Repa" $ whnf (runIdentity . sumAllP . arrR) sz
        ]
    , bgroup
        "toList"
        [ bench "Array Massiv 2D Seq" $ nf (M.toListS2D . arrM) szf
    --     , bench "Array Massiv 2D toListP'" $ nf (M.toListP' . arrM) sz
    --     , bench "Array Massiv 2D'" $ nfIO (M.toListP2D' . arrM $ sz)
        , bench "Array Massiv 2D" $ nfIO (M.toListP2D . arrM $ szf)
        ]
    , bgroup
        "fromList"
        [ bench "Array Massiv" $ whnf (M.fromListAsP2D M.U) ls2D
        , bench "Array Massiv Seq" $ whnf (M.fromListAsS2D M.U) ls2D
        , bench "Array Repa" $ whnf (R.fromListUnboxed ixR) ls1D
        ]
    , bgroup
        "Fuse"
        [ bench "Array Massiv" $
          whnf (M.computeUnboxedP . M.map (+ 25) . arrM) sz
        , bench "Array Repa" $
          whnf (runIdentity . R.computeUnboxedP . R.map (+ 25) . arrR) sz
        ]
    , bgroup
        "Matrix Multiplication"
        [ bench "Array Massiv" $ whnf (M.computeUnboxedP . (arrCMs' |*|)) arrCMs
        , bench "Array Repa" $ whnf (runIdentity . mmultP arrCRs') arrCRs
        ]
    ]

