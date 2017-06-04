{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Compute

import           Criterion.Main
import           Data.Array.Massiv                 as M
import           Data.Array.Massiv.Numeric
import           Data.Array.Repa                   as R
import           Data.Array.Repa.Algorithms.Matrix as R
import           Data.Foldable
import qualified Data.Vector.Unboxed               as VU
import           Prelude                           as P


sarrM :: (Int, Int) -> M.Array M.U M.DIM2 Double
sarrM arrSz = smakeArray Seq arrSz lightF
{-# INLINE sarrM #-}


main :: IO ()
main = do
  let !sz = (1600, 1200) :: M.DIM2
      !szR = (Z :. fst sz :. snd sz)
  let !szs = (600, 200) :: M.DIM2
  let !ixM = (612, 211)
      !ixR = (Z :. fst ixM :. snd ixM)
      !ix1D = toLinearIndex sz ixM
  let !arrCM = computeAs U $ arrM sz
      !arrCMM = toManifest arrCM
      !arrCR = R.computeUnboxedS $ arrR sz
      !vecCU = vecU sz
      !ls1D = toListS1D arrCM
      !ls2D = toListS2D arrCM
  let !arrCMs = computeAs U $ arrM szs
      !arrCMs' = computeAs U (M.transpose arrCMs)
      !arrCRs = R.computeUnboxedS $ arrR szs
      !arrCRs' = R.transpose2S arrCRs
  defaultMain
    [ bgroup
        "Indexing"
        [ bgroup
            "Unsafe"
            [ bench "Massiv 2D" $ whnf (M.unsafeIndex arrCM) ixM
            , bench "Vector 1D" $ whnf (VU.unsafeIndex vecCU) ix1D
            , bench "Repa 2D" $ whnf (R.unsafeIndex arrCR) ixR
            ]
        , bgroup
            "Safe"
            [ bench "Massiv 2D: maybeIndex" $
              whnf (maybe (error "impossible") id . M.maybeIndex arrCM) ixM
            , bench "Massiv 2D: index" $ whnf (M.index arrCM) ixM
            , bench "Massiv 2D: (!) . (<!)" $
              whnf (\(i, j) -> (toManifest arrCM) <! j M.! i) ixM
            , bench "Massiv 2D: (!) . (!>)" $
              whnf (\(i, j) -> (toManifest arrCM) !> i M.! j) ixM
            , bench "Massiv 2D: (!) . (<!>)" $
              whnf (\(i, j) -> (toManifest arrCM) <!> (1, i) M.! j) ixM
            , bench "Massiv 2D: (!) . (<!>)" $
              whnf (\(i, j) -> (toManifest arrCM) <!> (2, j) M.! i) ixM
            , bench "Vector 1D" $ whnf (vecCU VU.!) ix1D
            , bench "Repa 2D" $ whnf (R.index arrCR) ixR
            ]
        , bgroup
            "Linear Unsafe"
            [ bench "Massiv 2D" $ whnf (M.unsafeLinearIndex arrCM) ix1D
            , bench "Vector 1D" $ whnf (VU.unsafeIndex vecCU) ix1D
            , bench "Repa 2D" $ whnf (R.unsafeLinearIndex arrCR) ix1D
            ]
        ]
    , bgroup
        "Load"
        [ bgroup
            "Light Unboxed"
            [ bench "Massiv" $ whnf (M.computeAs U . arrM) sz
            , bench "Vector" $ whnf vecU sz
            , bench "Repa" $ whnf (R.computeUnboxedS . arrR) sz
            ]
        , bgroup
            "Heavy Unboxed"
            [ bench "Massiv" $ whnf (computeAs U . arrM') sz
            , bench "Vector" $ whnf vecU' sz
            , bench "Repa" $ whnf (R.computeUnboxedS . arrR') sz
            ]
        , bgroup
            "Windowed"
            [ bench "Massiv" $ whnf (computeAs U . arrWindowedM) sz
            , bench "Repa" $ whnf (R.computeUnboxedS . arrWindowedR) sz
            ]
        ]
    , bgroup
        "Fold"
        [ bgroup
            "Left"
            [ bench "Array Massiv Delayed" $ whnf (foldl' (+) 0 . arrM) sz
            , bench "Vector Unboxed" $ whnf (VU.foldl' (+) 0 . vecU) sz
            , bench "Array Repa" $ whnf (R.foldAllS (+) 0 . arrR) sz
            ]
        , bgroup
            "Right"
            [ bench "Array Massiv Delayed" $ whnf (foldr' (+) 0 . arrM) sz
            , bench "Vector Unboxed" $ whnf (VU.foldr' (+) 0 . vecU) sz
            , bench "Array Repa" $ whnf (R.foldAllS (+) 0 . arrR) sz
            ]
        , bgroup
            "Sum"
            [ bench "Array Massiv Delayed" $ whnf (M.sum . arrM) sz
            , bench "Vector Unboxed" $ whnf (VU.sum . vecU) sz
            , bench "Array Repa" $ whnf (R.sumAllS . arrR) sz
            ]
        , bgroup
            "Computed"
            [ bench "Array Massiv Unboxed Left Fold" $ whnf (foldlS (+) 0) arrCM
            --, bench "Array Massiv Unboxed Left Fold'" $ whnf (foldlS' (+) 0) arrCM
            , bench "Array Massiv Unboxed Right Fold" $
              whnf (foldrS (+) 0) arrCM
            --, bench "Array Massiv Unboxed Right Fold'" $ whnf (foldrS' (+) 0) arrCM
            , bench "Array Massiv Manifest Left Fold" $
              whnf (foldlS (+) 0) arrCMM
            --, bench "Array Massiv Manifest Left Fold'" $ whnf (foldlS' (+) 0) arrCMM
            , bench "Array Massiv Manifest Right Fold" $
              whnf (foldrS (+) 0) arrCMM
            --, bench "Array Massiv Manifest Right Fold'" $ whnf (foldrS' (+) 0) arrCMM
            , bench "Vector Unboxed Left Strict" $ whnf (VU.foldl' (+) 0) vecCU
            , bench "Vector Unboxed Right Strict" $ whnf (VU.foldr' (+) 0) vecCU
            , bench "Array Repa FoldAll" $ whnf (R.foldAllS (+) 0) arrCR
            ]
        ]
    , bgroup
        "toList"
        [ bench "Array Massiv" $ nf (M.toListS1D . arrM) sz
        , bench "Array Massiv 2D" $ nf (M.toListS2D . arrM) sz
        , bench "Array vector" $ whnf (VU.toList . vecU) sz
        , bench "Array Repa" $ nf (R.toList . arrR) sz
        ]
    , bgroup
        "fromList"
        [ bench "Array Massiv" $ whnf (M.fromListAsS2D M.U) ls2D
        , bench "Array Repa" $ whnf (R.fromListUnboxed szR) ls1D
        , bench "Array Vector" $ whnf VU.fromList ls1D
        ]
    , bgroup
        "Fuse"
        [ bgroup
            "map"
            [ bench "Array Massiv" $
              whnf (M.smap (+ 25) . sarrM) sz
            , bench "Array Massiv" $
              whnf (computeAs U . fmap (+ 25) . arrM) sz
            , bench "Vector Unboxed" $ whnf (VU.map (+ 25) . vecU) sz
            , bench "Array Repa" $
              whnf (R.computeUnboxedS . R.map (+ 25) . arrR) sz
            ]
        ]
    , bgroup
        "Append"
        [ bgroup
            "append"
            [ bench "Array Massiv" $
              whnf
                (\sz' -> computeAs U $ M.append' 1 (arrM sz') (arrM sz'))
                sz
            , bench "Vector Unboxed" $
              whnf (\sz' -> (vecU sz') VU.++ (vecU sz')) sz
            , bench "Array Repa" $
              whnf
                (\sz' -> R.computeUnboxedS $ R.append (arrR sz') (arrR sz'))
                sz
            ]
        ]
    , bgroup
        "Matrix Multiplication"
        [ bench "Array Massiv" $ whnf (computeAs U . (arrCMs' |*|)) arrCMs
        , bench "Array Repa" $ whnf (R.mmultS arrCRs') arrCRs
        ]
    ]
