{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Main where



import           Criterion.Main
import           Data.Array.Massiv.Common         as M
import           Data.Array.Repa                  as R
import           Data.Array.Repa.Specialised.Dim2 as R
import           Prelude                          as P


class Shape sh => ExtraShape sh where
  type ShLower sh :: *
  liftShape :: (Int -> Int) -> sh -> sh
  liftShape2 :: (Int -> Int -> Int) -> sh -> sh -> sh
  unsnocShape :: sh -> (ShLower sh, Int)
  snocShape :: ShLower sh -> Int -> sh


instance ExtraShape R.Z where
  type ShLower Z = Z
  liftShape _ _ = R.Z
  {-# INLINE [1] liftShape #-}
  liftShape2 _ _ _ = R.Z
  {-# INLINE [1] liftShape2 #-}
  unsnocShape = undefined
  snocShape = undefined

instance ExtraShape sh => ExtraShape (sh :. Int) where
  type ShLower (sh :. Int) = sh

  liftShape f (sh R.:. i) = liftShape f sh R.:. f i
  {-# INLINE [1] liftShape #-}
  liftShape2 f (sh1 R.:. i1) (sh2 R.:. i2) = liftShape2 f sh1 sh2 R.:. f i1 i2
  {-# INLINE [1] liftShape2 #-}
  unsnocShape (ix R.:. i) = (ix, i)
  {-# INLINE [1] unsnocShape #-}
  snocShape ix i = (ix R.:. i)
  {-# INLINE [1] snocShape #-}

main :: IO ()
main = do
  let i2 = (200 M.:. 300, 20 M.:. 17) :: (Ix2, Ix2)
      t2 = ((200, 300), (20, 17)) :: (Ix2T, Ix2T)
      r2 = (R.Z R.:. 200 R.:. 300, R.Z R.:. 20 R.:. 17) :: (R.DIM2, R.DIM2)
      i3 = (100 :> 200 M.:. 300, 10 :> 20 M.:. 17) :: (Ix3, Ix3)
      t3 = ((100, 200, 300), (10, 20, 17)) :: (Ix3T, Ix3T)
      r3 =
        (R.Z R.:. 100 R.:. 200 R.:. 300, R.Z R.:. 10 R.:. 20 R.:. 17) :: ( R.DIM3
                                                                         , R.DIM3)
      i4 = (100 :> 200 :> 300 M.:. 40, 10 :> 20 :> 17 M.:. 34) :: (Ix4, Ix4)
      t4 = ((100, 200, 300, 40), (10, 20, 17, 34)) :: (Ix4T, Ix4T)
      r4 =
        ( R.Z R.:. 100 R.:. 200 R.:. 300 R.:. 40
        , R.Z R.:. 90 R.:. 190 R.:. 290 R.:. 34) :: (R.DIM4, R.DIM4)
      ixs2 = (i2, t2, r2)
      ixs3 = (i3, t3, r3)
      ixs4 = (i4, t4, r4)
  defaultMain
    [ bgroup
        "Regular"
        [ bgroup
            "toLinearIndex"
            [ toLinearIndexGroup "DIM2" ixs2
            , toLinearIndexGroup "DIM3" ixs3
            , toLinearIndexGroup "DIM4" ixs4
            ]
        , bgroup
            "fromLinearIndex"
            [ fromLinearIndexGroup "DIM2" ixs2
            , fromLinearIndexGroup "DIM3" ixs3
            , fromLinearIndexGroup "DIM4" ixs4
            ]
        , bgroup
            "toFromLinearIndex"
            [ toFromLinearIndexGroup "DIM2" ixs2
            , toFromLinearIndexGroup "DIM3" ixs3
            , toFromLinearIndexGroup "DIM4" ixs4
            ]
        , bgroup
            "totalElem"
            [ totalElemGroup "DIM2" ixs2
            , totalElemGroup "DIM3" ixs3
            , totalElemGroup "DIM4" ixs4
            ]
        , bgroup
            "isSafeIndex"
            [ isSafeIndexGroup "DIM2" ixs2
            , isSafeIndexGroup "DIM3" ixs3
            , isSafeIndexGroup "DIM4" ixs4
            ]
        , bgroup
            "handleBorderIndex"
            [ handleBorderIndexGroup "DIM2" ixs2
            , bench "DIM2/Repa" $ whnf (R.clampToBorder2 (fst r2)) (snd r2)
            , handleBorderIndexGroup "DIM3" ixs3
            , handleBorderIndexGroup "DIM4" ixs4
            ]
        , bgroup
            "unconsConsDim"
            [ unconsConsDimGroup "DIM2" ixs2
            , unconsConsDimGroup "DIM3" ixs3
            , unconsConsDimGroup "DIM4" ixs4
            ]
        , bgroup
            "unsnocSnocDim"
            [ unsnocSnocDimGroup "DIM2" ixs2
            , unsnocSnocDimGroup "DIM3" ixs3
            , unsnocSnocDimGroup "DIM4" ixs4
            ]
        , bgroup
            "liftIndex"
            [ liftIndexGroup "DIM2" ixs2
            , liftIndexGroup "DIM3" ixs3
            , liftIndexGroup "DIM4" ixs4
            ]
        , bgroup
            "liftIndex2"
            [ liftIndex2Group "DIM2" ixs2
            , liftIndex2Group "DIM3" ixs3
            , liftIndex2Group "DIM4" ixs4
            ]
        , bgroup
            "getIndex"
            [ getIndexGroup "DIM2" ixs2
            , getIndexGroup "DIM3" ixs3
            , getIndexGroup "DIM4" ixs4
            ]
        , bgroup
            "setIndex"
            [ setIndexGroup "DIM2" ixs2
            , setIndexGroup "DIM3" ixs3
            , setIndexGroup "DIM4" ixs4
            ]
        , bgroup
            "dropDim"
            [ dropDimGroup "DIM2" ixs2
            , dropDimGroup "DIM3" ixs3
            , dropDimGroup "DIM4" ixs4
            ]
        ]
    , bgroup
        "Expensive"
        [ bgroup
            "iter"
            [ iterGroup "DIM2" ixs2
            , iterGroup "DIM3" ixs3
            , iterGroup "DIM4" ixs4
            ]
        , bgroup
            "iterM"
            [ iterMGroup "DIM2" ixs2
            , iterMGroup "DIM3" ixs3
            , iterMGroup "DIM4" ixs4
            ]
        ]
    ]



toLinearIndexGroup :: (Index ix1, Index ix2, R.Shape sh) =>
  String -> ((ix1, ix1), (ix2, ix2), (sh, sh)) -> Benchmark
toLinearIndexGroup groupName !((sz1, i1), (sz2, i2), (sz3, i3)) =
  bgroup
    groupName
    [ bench "Ix" $ whnf (toLinearIndex sz1) i1
    , bench "Tuple" $ whnf (toLinearIndex sz2) i2
    , bench "Repa" $ whnf (toIndex sz3) i3
    ]


fromLinearIndexGroup :: (Index ix1, Index ix2, R.Shape sh) =>
  String -> ((ix1, ix1), (ix2, ix2), (sh, sh)) -> Benchmark
fromLinearIndexGroup groupName !((sz1, _), (sz2, _), (sz3, _)) =
  bgroup
    groupName
    [ bench "Ix" $ whnf (fromLinearIndex sz1) 100
    , bench "Tuple" $ nf (fromLinearIndex sz2) 100
    , bench "Repa" $ whnf (fromIndex sz3) 100
    ]


toFromLinearIndexGroup :: (Index ix1, Index ix2, R.Shape sh) =>
  String -> ((ix1, ix1), (ix2, ix2), (sh, sh)) -> Benchmark
toFromLinearIndexGroup groupName !((sz1, i1), (sz2, i2), (sz3, i3)) =
  bgroup
    groupName
    [ bench "Ix" $ whnf (fromLinearIndex sz1 . toLinearIndex sz1) i1
    , bench "Tuple" $ nf (fromLinearIndex sz2 . toLinearIndex sz2) i2
    , bench "Repa" $ whnf (fromIndex sz3 . toIndex sz3) i3
    ]


totalElemGroup :: (Index ix1, Index ix2, R.Shape sh) =>
  String -> ((ix1, ix1), (ix2, ix2), (sh, sh)) -> Benchmark
totalElemGroup groupName !((_, i1), (_, i2), (_, i3)) =
  bgroup
    groupName
    [ bench "Ix" $ whnf totalElem i1
    , bench "Tuple" $ whnf totalElem i2
    , bench "Repa" $ whnf R.size i3
    ]



isSafeIndexGroup :: (Index ix1, Index ix2, R.Shape sh) =>
  String -> ((ix1, ix1), (ix2, ix2), (sh, sh)) -> Benchmark
isSafeIndexGroup groupName !((sz1, i1), (sz2, i2), (sz3, i3)) =
  bgroup
    groupName
    [ bench "Ix" $ whnf (isSafeIndex sz1) i1
    , bench "Tuple" $ whnf (isSafeIndex sz2) i2
    , bench "Repa" $ whnf (R.inShapeRange R.zeroDim sz3) i3
    ]



handleBorderIndexGroup :: (Index ix1, Index ix2) =>
  String -> ((ix1, ix1), (ix2, ix2), (sh, sh)) -> Benchmark
handleBorderIndexGroup groupName !((sz1, i1), (sz2, i2), _) =
  bgroup
    groupName
    [ bench "Ix" $ whnf (handleBorderIndex Edge sz1 id) i1
    , bench "Tuple" $ nf (handleBorderIndex Edge sz2 id) i2
    ]


unconsConsDimGroup :: forall ix1 ix2 a. (Index (Lower ix1), Index ix1, Index (Lower ix2), Index ix2)
  => String
  -> ((ix1, ix1), (ix2, ix2), a)
  -> Benchmark
unconsConsDimGroup groupName !((_, i1), (_, i2), _) =
  bgroup
    groupName
    [ bench "Ix" $ whnf ((consDim 8 . snd . unconsDim) :: ix1 -> ix1) i1
    , bench "Tuple" $ nf ((consDim 8 . snd . unconsDim) :: ix2 -> ix2) i2
    ]


unsnocSnocDimGroup ::
     forall ix1 ix2 sh.
     ( Index (Lower ix1)
     , Index ix1
     , Index (Lower ix2)
     , Index ix2
     , R.Shape sh
     , ExtraShape sh
     )
  => String
  -> ((ix1, ix1), (ix2, ix2), (sh, sh))
  -> Benchmark
unsnocSnocDimGroup groupName !((_, i1), (_, i2), (_, i3)) =
  bgroup
    groupName
    [ bench "Ix" $ whnf (((`snocDim` 8) . fst . unsnocDim) :: ix1 -> ix1) i1
    , bench "Tuple" $ nf (((`snocDim` 8) . fst . unsnocDim) :: ix2 -> ix2) i2
    , bench "Repa" $ whnf (((`snocShape` 8) . fst . unsnocShape) :: sh -> sh) i3
    ]


liftIndexGroup :: (Index ix1, Index ix2, ExtraShape sh) =>
  String -> ((ix1, ix1), (ix2, ix2), (sh, sh)) -> Benchmark
liftIndexGroup groupName !((_, i1), (_, i2), (_, i3)) =
  bgroup
    groupName
    [ bench "Ix" $ whnf (liftIndex succ) i1
    , bench "Tuple" $ nf (liftIndex succ) i2
    , bench "Repa" $ whnf (liftShape succ) i3
    ]



liftIndex2Group :: (Index ix1, Index ix2, ExtraShape sh) =>
  String -> ((ix1, ix1), (ix2, ix2), (sh, sh)) -> Benchmark
liftIndex2Group groupName !((sz1, i1), (sz2, i2), (sz3, i3)) =
  bgroup
    groupName
    [ bench "Ix" $ whnf (liftIndex2 (+) sz1) i1
    , bench "Tuple" $ nf (liftIndex2 (+) sz2) i2
    , bench "Repa" $ whnf (liftShape2 (+) sz3) i3
    , bench "Repa Add" $ whnf (addDim sz3) i3
    ]



getIndexGroup :: (Index ix1, Index ix2) =>
  String -> ((ix1, ix1), (ix2, ix2), a) -> Benchmark
getIndexGroup groupName !((_, i1), (_, i2), _) =
  bgroup
    groupName
    [ bgroup
        "(rank)"
        [ bench "Ix" $ whnf (`getIndex` M.rank i1) i1
        , bench "Tuple" $ whnf (`getIndex` M.rank i2) i2
        ]
    , bgroup
        "(1)"
        [ bench "Ix" $ whnf (`getIndex` 1) i1
        , bench "Tuple" $ whnf (`getIndex` 1) i2
        ]
    ]


setIndexGroup :: (Index ix1, Index ix2) =>
  String -> ((ix1, ix1), (ix2, ix2), a) -> Benchmark
setIndexGroup groupName !((_, i1), (_, i2), _) =
  bgroup
    groupName
    [ bgroup
        "(rank)"
        [ bench "Ix" $ whnf (setIndex i1 (M.rank i1)) 8
        , bench "Tuple" $ nf (setIndex i2 (M.rank i2)) 8
        ]
    , bgroup
        "(1)"
        [ bench "Ix" $ whnf (`setIndex` 1) i1
        , bench "Tuple" $ whnf (`setIndex` 1) i2
        ]
    ]


dropDimGroup :: (Index ix1, Index (Lower ix2), Index ix2) =>
  String -> ((ix1, ix1), (ix2, ix2), a) -> Benchmark
dropDimGroup groupName !((_, i1), (_, i2), _) =
  bgroup
    groupName
    [ bgroup
        "(rank)"
        [ bench "Ix" $ whnf (`dropDim` M.rank i1) i1
        , bench "Tuple" $ nf (`dropDim` M.rank i2) i2
        ]
    , bgroup
        "(1)"
        [ bench "Ix" $ whnf (`dropDim` 1) i1
        , bench "Tuple" $ nf (`dropDim` 1) i2
        ]
    ]



iterGroup :: (Index ix1, Index ix2) =>
  String -> ((ix1, ix1), (ix2, ix2), a) -> Benchmark
iterGroup groupName !((sz1, i1), (sz2, i2), _) =
  bgroup
    groupName
    [ bench "Ix" $
      nf (\ix -> iter ix sz1 1 (<) 0 (\i acc -> totalElem i + acc)) i1
    , bench "Tuple" $
      nf (\ix -> iter ix sz2 1 (<) 0 (\i acc -> totalElem i + acc)) i2
    ]


iterMGroup :: (Index ix1, Index ix2) =>
  String -> ((ix1, ix1), (ix2, ix2), a) -> Benchmark
iterMGroup groupName !((sz1, i1), (sz2, i2), _) =
  bgroup
    groupName
    [ bench "Ix" $
      nfIO $ iterM i1 sz1 1 (<) 0 (\i acc -> return (totalElem i + acc))
    , bench "Tuple" $
      nfIO $ iterM i2 sz2 1 (<) 0 (\i acc -> return (totalElem i + acc))
    ]

