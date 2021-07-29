{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Criterion.Main
import Data.Massiv.Array as A
-- import Data.Massiv.Array.Manifest.Vector as A
-- import Data.Massiv.Bench as A
import qualified Data.Vector.Primitive as VP
-- import Data.Primitive.ByteArray
--import Data.Primitive.PrimArray as Prim
import System.Random.Stateful

main :: IO ()
main = do
  let (stdGen1, _stdGen2) = split $ mkStdGen 2021
      sz2d = Sz2 500 5000
      arr2d = compute (uniformArray stdGen1 Seq sz2d) :: Array P Ix2 Int
      sz3d = Sz3 500 100 50
      --arr3d = compute (uniformArray stdGen1 Seq sz3d) :: Array P Ix3 Int
  arr3d <- resizeM sz3d arr2d
  defaultMain
    [ bgroup
        "fromLists (Seq)"
        [ env (return $ toLists3 arr3d) $ \xs ->
            bench "Array P Ix3" $ nfIO (A.fromListsM @P @Ix3 Seq xs)
        , env (return $ toLists2 arr2d) $ \xs ->
            bench "Array P Ix2" $ nfIO (A.fromListsM @P @Ix2 Seq xs)
        , env (return $ toList arr3d) $ \xs ->
            bench "Array P Ix1" $ nfIO (A.fromListsM @P @Ix1 Seq xs)
        , env (return $ toList arr3d) $ \xs ->
            bench "VP.Vector" $ nf VP.fromList xs
        ]
    , bgroup
        "fromLists (Par)"
        [ env (return $ toLists3 arr3d) $ \xs ->
            bench "Array P Ix3" $ nfIO (A.fromListsM @P @Ix3 Par xs)
        , env (return $ toLists2 arr2d) $ \xs ->
            bench "Array P Ix2" $ nfIO (A.fromListsM @P @Ix2 Par xs)
        , env (return $ toList arr3d) $ \xs ->
            bench "Array P Ix1" $ nfIO (A.fromListsM @P @Ix1 Par xs)
        ]
    ]
