{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Criterion.Main
import Data.Massiv.Array as A
-- import Data.Massiv.Array.Unsafe as A
import Data.Massiv.Bench as A
import Data.Monoid
import Prelude as P

main :: IO ()
main = do
  let !sz = Sz2 1600 1200
      !array = arrRLightIx2 D Seq sz
      !array3 =
        makeArrayR D Seq (Sz3 40 40 1200) (\(i :> j :. k) -> lightFunc (i * j) k)
      !array4 =
        makeArrayR D Seq (Sz4 40 40 40 30) (\(i :> j :> k :. l) -> lightFunc (i * j) (k * l))
      !array5 =
        makeArrayR
          D
          Seq
          (Sz5 40 40 4 10 30)
          ( \(i :> j :> k :> l :. m) ->
              lightFunc (i * j) (k * l * m)
          )
      !arrayP = arrRLightIx2 P Seq sz
  defaultMain
    [ bgroup
        "Sum (P)"
        [ bgroup "1D" $ benchSum (flatten arrayP)
        , bgroup "2D" $ benchSum arrayP
        , bgroup "3D" $ benchSum (resize' (Sz3 40 40 1200) arrayP)
        , bgroup "4D" $ benchSum (resize' (Sz4 40 40 40 30) arrayP)
        , bgroup "5D" $ benchSum (resize' (Sz5 40 40 4 10 30) arrayP)
        ]
    , bgroup
        "Sum (D)"
        [ bgroup "1D" $ benchSum (flatten array)
        , bgroup "2D" $ benchSum array
        , bgroup "3D" $ benchSum array3
        , bgroup "3D (resize)" $ benchSum (resize' (Sz3 40 40 1200) array)
        , bgroup "4D" $ benchSum array4
        , bgroup "4D (resize)" $ benchSum (resize' (Sz4 40 40 40 30) array)
        , bgroup "5D" $ benchSum array5
        , bgroup "5D (resize)" $ benchSum (resize' (Sz5 40 40 4 10 30) array)
        ]
    , bgroup
        "Nested (2D)"
        [ env (return arrayP) $ \arr ->
            bgroup
              "Seq"
              [ bench "foldlS . foldlWithin Dim2" $
                  whnf (A.foldlS (+) 0 . foldlWithin Dim2 (+) 0) arr
              , bench "foldlS . foldlInner" $ whnf (A.foldlS (+) 0 . foldlInner (+) 0) arr
              , bench "sum" $ whnf A.sum arr
              , bench "foldMono" $ whnf (getSum . foldMono Sum) arr
              ]
        , env (return (setComp Par arrayP)) $ \arr ->
            bgroup
              "Par"
              [ bench "foldlP . foldlWithin Dim2" $
                  whnfIO (A.foldlP (+) 0 (+) 0 $ foldlWithin Dim2 (+) 0 arr)
              , bench "foldlP . foldlInner" $ whnfIO (A.foldlP (+) 0 (+) 0 $ foldlInner (+) 0 arr)
              , bench "sum" $ whnf A.sum arr
              , bench "foldMono" $ whnf (getSum . foldMono Sum) arr
              ]
        ]
    ]

benchSum
  :: (Index ix, Num a, Stream r ix a, Source r a)
  => Array r ix a
  -> [Benchmark]
benchSum arr =
  [ bgroup
      "Seq"
      [ bench "foldlS" $ whnf (A.foldlS (+) 0) arr
      , bench "sfoldl" $ whnf (A.sfoldl (+) 0) arr
      , bench "foldrS" $ whnf (A.foldrS (+) 0) arr
      , bench "ifoldlS" $ whnf (A.ifoldlS (\acc !_ x -> acc + x) 0) arr
      , bench "sifoldl" $ whnf (A.sifoldl (\acc !_ x -> acc + x) 0) arr
      , bench "ifoldrS" $ whnf (A.ifoldrS (\ !_ acc x -> acc + x) 0) arr
      ]
  , bgroup
      "Par"
      [ bench "foldlP" $ whnfIO (A.foldlP (+) 0 (+) 0 (setComp Par arr))
      , bench "foldrP" $ whnfIO (A.foldrP (+) 0 (+) 0 (setComp Par arr))
      , bench "ifoldlP" $ whnfIO (A.ifoldlP (\acc !_ x -> acc + x) 0 (+) 0 (setComp Par arr))
      , bench "ifoldrP" $ whnfIO (A.ifoldrP (\ !_ acc x -> acc + x) 0 (+) 0 (setComp Par arr))
      ]
  ]
