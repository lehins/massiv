{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           CommonMassiv
import           CommonRepa
import           Criterion.Main
import           Data.Array.Massiv                 as M
import           Data.Array.Massiv.Common.Ix       as M
import           Data.Array.Massiv.Numeric
import           Data.Array.Repa                   as R
import           Data.Array.Repa.Algorithms.Matrix as R
import           Data.Foldable
import qualified Data.Vector.Unboxed               as VU
import           Prelude                           as P
import Data.Int

sarrM :: (Int, Int) -> M.Array M.U M.DIM2 Double
sarrM arrSz = smakeArray Seq arrSz lightF
{-# INLINE sarrM #-}

toIx2 :: (Int, Int) -> Ix2
toIx2 (i, j) = i M.:. j

toIxArr
  :: M.Source r (Int, Int) e
  => M.Array r (Int, Int) e -> M.Array M.D Ix2 e
toIxArr arr = makeArray (getComp arr) (toIx2 (M.size arr)) (\(i M.:. j) -> M.unsafeIndex arr (i, j))

main :: IO ()
main = do
  let !sz = (1600, 1200) :: M.DIM2
      !szR = (R.Z R.:. fst sz R.:. snd sz)
  let !szs = (600, 200) :: M.DIM2
  let !ixM = (612, 211)
      !ixR = (R.Z R.:. fst ixM R.:. snd ixM)
      !ix1D = toLinearIndex sz ixM
  let !arrCM = computeAs U $ arrM Seq sz
      !arrCM' = computeAs U $ toIxArr $ arrM Seq sz
      -- !arrInt32 =
      --   computeAs
      --     P
      --     (M.map (round . (* 100)) (arrM Seq sz) :: M.Array M.D M.DIM2 Int32)
      !arrDouble = computeAs P $ arrM Seq sz
      !arrCMM = toManifest arrCM
      !arrCR = R.computeUnboxedS $ arrR sz
      !vecCU = vecU sz
      !ls1D = toList1D arrCM
      !ls2D = toList2D arrCM
  let !arrCMs = computeAs U $ arrM Seq szs
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
            [ bench "Massiv" $ whnf (M.computeAs U . arrM Seq) sz
            , bench "Vector" $ whnf vecU sz
            , bench "Repa" $ whnf (R.computeUnboxedS . arrR) sz
            ]
        , bgroup
            "Heavy Unboxed"
            [ bench "Massiv" $ whnf (computeAs U . arrM' Seq) sz
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
            [ bench "Array Massiv Delayed" $ whnf (foldl' (+) 0 . arrM Seq) sz
            , bench "Vector Unboxed" $ whnf (VU.foldl' (+) 0 . vecU) sz
            , bench "Array Repa" $ whnf (R.foldAllS (+) 0 . arrR) sz
            ]
        , bgroup
            "Right"
            [ bench "Array Massiv Delayed" $ whnf (foldr' (+) 0 . arrM Seq) sz
            , bench "Vector Unboxed" $ whnf (VU.foldr' (+) 0 . vecU) sz
            , bench "Array Repa" $ whnf (R.foldAllS (+) 0 . arrR) sz
            ]
        , bgroup
            "Sum"
            [ bench "Array Massiv Delayed" $ whnf (M.sum . arrM Seq) sz
            , bench "Vector Unboxed" $ whnf (VU.sum . vecU) sz
            , bench "Array Repa" $ whnf (R.sumAllS . arrR) sz
            ]
        , bgroup
            "Computed"
            [ bench "Array Massiv Unboxed Left Fold" $ whnf (foldlS (+) 0) arrCM
            --, bench "Array Massiv Unboxed Left Fold'" $ whnf (foldlS' (+) 0) arrCM
            , bench "Array Massiv Primitive Left Fold" $ whnf (foldlS (+) 0) arrDouble
            -- , bench "Array Massiv Unboxed Right Fold" $
            --   whnf (foldrS (+) 0) arrCM
            -- --, bench "Array Massiv Unboxed Right Fold'" $ whnf (foldrS' (+) 0) arrCM
            -- , bench "Array Massiv Manifest Left Fold" $
            --   whnf (foldlS (+) 0) arrCMM
            -- --, bench "Array Massiv Manifest Left Fold'" $ whnf (foldlS' (+) 0) arrCMM
            -- , bench "Array Massiv Manifest Right Fold" $
            --   whnf (foldrS (+) 0) arrCMM
            -- --, bench "Array Massiv Manifest Right Fold'" $ whnf (foldrS' (+) 0) arrCMM
            , bench "Vector Unboxed Left Strict" $ whnf (VU.foldl' (+) 0) vecCU
            -- , bench "Vector Unboxed Right Strict" $ whnf (VU.foldr' (+) 0) vecCU
            , bench "Array Repa FoldAll" $ whnf (R.foldAllS (+) 0) arrCR
            ]
        ]
    , bgroup
        "toList"
        [ bench "Array Massiv" $ nf (M.toList1D . arrM Seq) sz
        , bench "Array Massiv 2D" $ nf (M.toList2D . arrM Seq) sz
        , bench "Array vector" $ whnf (VU.toList . vecU) sz
        , bench "Array Repa" $ nf (R.toList . arrR) sz
        ]
    , bgroup
        "fromList"
        [ bench "Array Massiv 1D" $ whnf (M.fromListAs1D M.U Seq) ls1D
        , bench "Array Massiv 2D" $ whnf (M.fromListAs2D M.U Seq) ls2D
        , bench "Array Repa" $ whnf (R.fromListUnboxed szR) ls1D
        , bench "Array Vector" $ whnf VU.fromList ls1D
        ]
    , bgroup
        "Fuse"
        [ bgroup
            "map"
            [ bench "Array Massiv" $ whnf (M.smap (+ 25) . sarrM) sz
            , bench "Array Massiv" $
              whnf (computeAs U . fmap (+ 25) . arrM Seq) sz
            , bench "Vector Unboxed" $ whnf (VU.map (+ 25) . vecU) sz
            , bench "Array Repa" $
              whnf (R.computeUnboxedS . R.map (+ 25) . arrR) sz
            ]
        , bgroup
            "transpose"
            [ bench "Array Massiv" $
              whnf (M.computeAs U . M.transpose . arrM Seq) sz
            , bench "Array Repa" $
              whnf (R.computeUnboxedS . R.transpose . arrR) sz
            ]
        ]
    , bgroup
        "Transform"
        [ bgroup
            "transpose"
            [ bench "Array Massiv" $
              whnf (M.computeAs U . M.transposeInner) arrCM
            , bench "Array Massiv" $
              whnf (M.computeAs U . M.transposeOuter) arrCM'
            , bench "Array Repa" $ whnf (R.computeUnboxedS . R.transpose) arrCR
            ]
        , bgroup
            "map"
            [ bench "Array Massiv" $ whnf (M.computeAs U . M.map (+ 15)) arrCM
            , bench "Array Massiv" $ whnf (M.computeAs U . M.map (+ 15)) arrCM'
            , bench "Array Repa" $ whnf (R.computeUnboxedS . R.map (+ 15)) arrCR
            ]
        ]
    , bgroup
        "Append"
        [ bgroup
            "append"
            [ bench "Array Massiv" $
              whnf
                (\sz' -> computeAs U $ M.append' 1 (arrM Seq sz') (arrM Seq sz'))
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
