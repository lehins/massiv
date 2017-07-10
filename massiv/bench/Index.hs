{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where



import           Criterion.Main
import           Data.Array.Massiv                 as M
import           Data.Array.Repa                   as R
-- import           Data.Array.Massiv.Common.Ix       as M
import           Prelude                           as P


class ExtraShape sh where
  liftShape :: (Int -> Int) -> sh -> sh

instance ExtraShape R.Z where

  liftShape _ R.Z = R.Z
  {-# INLINE [1] liftShape #-}

instance ExtraShape sh => ExtraShape (sh R.:. Int) where

  liftShape f (sh R.:. i) = liftShape f sh R.:. f i
  {-# INLINE [1] liftShape #-}


main :: IO ()
main = do
  let i2 = (200 M.:. 300, 20 M.:. 17)
      t2 = ((200, 300), (20, 17)) :: (M.DIM2, M.DIM2)
      r2 = (R.Z R.:. 200 R.:. 300, R.Z R.:. 20 R.:. 17) :: (R.DIM2, R.DIM2)
      i3 = (100 :> 200 M.:. 300, 10 :> 20 M.:. 17)
      t3 = ((100, 200, 300), (10, 20, 17)) :: (M.DIM3, M.DIM3)
      r3 = (R.Z R.:. 100 R.:. 200 R.:. 300, R.Z R.:. 10 R.:. 20 R.:. 17) :: (R.DIM3, R.DIM3)
      i4 = (100 :> 200 :> 300 M.:. 40, 10 :> 20 :> 17 M.:. 34)
      t4 = ((100, 200, 300, 40), (10, 20, 17, 34)) :: (M.DIM4, M.DIM4)
      r4 = (R.Z R.:. 100 R.:. 200 R.:. 300 R.:. 40, R.Z R.:. 10 R.:. 20 R.:. 17 R.:. 34) :: (R.DIM4, R.DIM4)
  defaultMain [ makeGroup "DIM2" i2 t2 r2
              , makeGroup "DIM3" i3 t3 r3
              , makeGroup "DIM4" i4 t4 r4]

makeGroup
  :: forall ix1 ix2 sh. (Index (Lower ix1), Index ix1, Index (Lower ix2), Index ix2, R.Shape sh, ExtraShape sh) =>
     String
     -> (ix1, ix1) -> (ix2, ix2) -> (sh, sh) -> Benchmark
makeGroup groupName !(sz1, i1) !(sz2, i2) !(sz3, i3) =
  bgroup
    groupName
    [ bgroup
        "Regular"
        [ bgroup
            "toLinearIndex"
            [ bench "Ix" $ whnf (toLinearIndex sz1) i1
            , bench "Tuple" $ whnf (toLinearIndex sz2) i2
            , bench "Repa" $ whnf (toIndex sz3) i3
            ]
        , bgroup
            "toFromLinearIndex"
            [ bench "Ix" $ whnf (fromLinearIndex sz1 . toLinearIndex sz1) i1
            , bench "Tuple" $ nf (fromLinearIndex sz2 . toLinearIndex sz2) i2
            , bench "Repa" $ whnf (fromIndex sz3 . toIndex sz3) i3
            ]
        , bgroup
            "totalElem"
            [ bench "Ix" $ whnf totalElem i1
            , bench "Tuple" $ whnf totalElem i2
            , bench "Repa" $ whnf R.size i3
            ]
        , bgroup
            "isSafeIndex"
            [ bench "Ix" $ whnf (isSafeIndex sz1) i1
            , bench "Tuple" $ whnf (isSafeIndex sz2) i2
            , bench "Repa" $ nf (R.inShapeRange R.zeroDim sz3) i3
            ]
        , bgroup
            "unconsConsDim"
            [ bench "Ix" $ whnf ((consDim 8 . snd . unconsDim) :: ix1 -> ix1) i1
            , bench "Tuple" $
              nf ((consDim 8 . snd . unconsDim) :: ix2 -> ix2) i2
            ]
        , bgroup
            "unsnocSnocDim"
            [ bench "Ix" $
              whnf (((`snocDim` 8) . fst . unsnocDim) :: ix1 -> ix1) i1
            , bench "Tuple" $
              nf (((`snocDim` 8) . fst . unsnocDim) :: ix2 -> ix2) i2
            ]
        , bgroup
            "liftIndex"
            [ bench "Ix" $ whnf (liftIndex succ) i1
            , bench "Tuple" $ nf (liftIndex succ) i2
            , bench "Repa" $ whnf (liftShape succ) i3
            ]
        , bgroup
            "liftIndex2"
            [ bench "Ix" $ whnf (liftIndex2 (+) sz1) i1
            , bench "Tuple" $ nf (liftIndex2 (+) sz2) i2
            , bench "Repa" $ whnf (R.addDim sz3) i3
            ]
        , bgroup
            "getIndex - 1"
            [ bench "Ix" $ whnf (`getIndex` 1) i1
            , bench "Tuple" $ whnf (`getIndex` 1) i2
            ]
        , bgroup
            "getIndex - rank"
            [ bench "Ix" $ whnf (`getIndex` M.rank i1) i1
            , bench "Tuple" $ whnf (`getIndex` M.rank i2) i2
            ]
        , bgroup
            "setIndex - 1"
            [ bench "Ix" $ whnf (setIndex i1 1) 8
            , bench "Tuple" $ whnf (setIndex i2 2) 8
            ]
        , bgroup
            "setIndex - rank"
            [ bench "Ix" $ whnf (setIndex i1 (M.rank i1)) 8
            , bench "Tuple" $ whnf (setIndex i2 (M.rank i2)) 8
            ]
        , bgroup
            "dropIndex - 1"
            [ bench "Ix" $ whnf (`getIndex` 1) i1
            , bench "Tuple" $ whnf (`getIndex` 1) i2
            ]
        , bgroup
            "dropIndex - rank"
            [ bench "Ix" $ whnf (`dropIndex` M.rank i1) i1
            , bench "Tuple" $ whnf (`dropIndex` M.rank i2) i2
            ]
        ]
    , bgroup
        "Expensive"
        [ bgroup
            "iterM"
            [ bench "Ix" $
              whnf
                (\ix ->
                   iterM ix sz1 1 (<) 0 (\i acc -> Just (totalElem i + acc)))
                i1
            , bench "Tuple" $
              whnf
                (\ix ->
                   iterM ix sz2 1 (<) 0 (\i acc -> Just (totalElem i + acc)))
                i2
            ]
        ]
    ]
