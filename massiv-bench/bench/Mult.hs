{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Criterion.Main
import Data.Massiv.Array as A
import Data.Massiv.Bench.Matrix


multArrsAlt :: Array P Ix2 Double -> Array P Ix2 Double -> Array P Ix2 Double
multArrsAlt arr1 arr2
  | n1 /= m2 =
    error $
    "(|*|): Inner array dimensions must agree, but received: " ++
    show (size arr1) ++ " and " ++ show (size arr2)
  | otherwise =
    makeArrayR P (getComp arr1 <> getComp arr2) (Sz (m1 :. n2)) $ \(i :. j) ->
      foldlS (+) 0 (A.zipWith (*) (arr1 !> i) (arr2' !> j))
  where
    Sz2 m1 n1 = size arr1
    Sz2 m2 n2 = size arr2
    arr2' = computeAs U $ A.transpose arr2


main :: IO ()
main = do
  defaultMain
    [ let MxM {..} = randomMxM
      in bench "multArrsAlt (baseline)" $ whnf (multArrsAlt aMxM) bMxM
    , benchMxM (randomMxM :: MxM P Double)
    , benchVxM (randomVxM :: VxM P Double)
    -- , let VxM {..} = randomVxM -- sanity check: compare with VxM with MxM
    --   in benchMxM (MxM (resize' (Sz2 1 600) aVxM) bVxM :: MxM P Double)
    , benchMxV (randomMxV :: MxV P Double)
    -- , let MxV {..} = randomMxV -- sanity check: compare with MxV with MxM
    --   in benchMxM (MxM aMxV (resize' (Sz2 600 1) bMxV) :: MxM P Double)
    ]
