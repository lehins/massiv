{-# LANGUAGE BangPatterns     #-}
module Main where

import           Criterion.Main
import           Data.Massiv.Array as A
import           Data.Massiv.Array.Unsafe as A
import           Data.Massiv.Bench as A
import           Prelude           as P



multArrs :: (Unbox e, Num e) => Array U Ix2 e -> Array U Ix2 e -> Array U Ix2 e
multArrs arr1 arr2
  | n1 /= m2 =
    error $
    "(|*|): Inner array dimensions must agree, but received: " ++
    show (size arr1) ++ " and " ++ show (size arr2)
  | otherwise = compute $
    makeArrayR D (getComp arr1 <> getComp arr2) (m1 :. n2) $ \(i :. j) ->
      A.foldlS (+) 0 (A.zipWith (*) (unsafeOuterSlice arr1 i) (unsafeOuterSlice arr2' j))
  where
    (m1 :. n1) = size arr1
    (m2 :. n2) = size arr2
    arr2' = computeAs U $ transpose arr2
{-# INLINE multArrs #-}


main :: IO ()
main = do
  let !sz = 600 :. 200
      arr = arrRLightIx2 U Seq sz
  defaultMain
    [ env (return (computeAs U (transpose arr))) $ \arr' ->
        bgroup
          "Mult"
          [ bgroup
              "Seq"
              [ bench "(|*|)" $ whnf (A.computeAs U . (|*|) (setComp Seq arr)) arr'
              , bench "mmultArrs" $ whnf (multArrs (setComp Seq arr)) arr'
              ]
          , bgroup
              "Par"
              [ bench "(|*|)" $ whnf (A.computeAs U . (|*|) (setComp Par arr)) arr'
              , bench "mmultArrs" $ whnf (multArrs (setComp Par arr)) arr'
              ]
          ]
    ]
