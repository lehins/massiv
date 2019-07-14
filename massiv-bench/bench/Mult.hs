{-# LANGUAGE BangPatterns #-}
module Main where

import Criterion.Main
import Data.Massiv.Array as A
import Data.Massiv.Array.SIMD
import Data.Massiv.Array.Manifest.Vector as A
import Data.Massiv.Array.Unsafe as A
import Data.Massiv.Bench as A
import Prelude as P hiding ((<>))
import Data.Semigroup

import Statistics.Matrix as S
import Statistics.Matrix.Fast as SF


multArrsAlt :: Array U Ix2 Double -> Array U Ix2 Double -> Array U Ix2 Double
multArrsAlt arr1 arr2
  | n1 /= m2 =
    error $
    "(|*|): Inner array dimensions must agree, but received: " ++
    show (size arr1) ++ " and " ++ show (size arr2)
  | otherwise = compute $
    makeArrayR D (getComp arr1 <> getComp arr2) (Sz (m1 :. n2)) $ \(i :. j) ->
      A.foldlS (+) 0 (A.zipWith (*) (unsafeOuterSlice arr1 i) (unsafeOuterSlice arr2' j))
  where
    Sz2 m1 n1 = size arr1
    Sz2 m2 n2 = size arr2
    arr2' = computeAs U $ A.transpose arr2
{-# INLINE multArrsAlt #-}


main :: IO ()
main = do
  let !sz@(Sz2 m n) = Sz2 600 600
      !arr = arrRLightIx2 U Seq sz
      !arr2 = computeAs U arr
      !mat = S.Matrix m n $ A.toVector arr
      !arrV = arrRLightIx2 V Seq sz
      !arrV2 = computeAs V arrV
  defaultMain
    [ env (return (computeAs U (A.transpose arr), S.transpose mat)) $ \ ~(arr', mat') ->
        bgroup
          "Mult"
          [ bgroup
              "Seq"
              [ bench "(|*|)" $ whnf (setComp Seq arr |*|) arr'
              , bench "multiplyTranspose" $
                whnf (computeAs U . multiplyTransposed (setComp Seq arr)) arr2
              , bench "multiplyTransposeSIMD" $
                whnf (computeAs P . multiplyTransposedSIMD arrV) arrV2
              , bench "multArrsAlt" $ whnf (multArrsAlt (setComp Seq arr)) arr'
              , bench "multiply (dense-linear-algebra)" $ whnf (SF.multiply mat) mat'
              ]
          , bgroup
              "Par"
              [ bench "(|*|)" $ whnf (setComp Par arr |*|) arr'
              , bench "fused (|*|)" $ whnf ((setComp Par arr |*|) . A.transpose) arr
              , bench "multiplyTranspose" $
                whnf (computeAs U . multiplyTransposed (setComp Par arr)) arr2
              , bench "multiplyTransposeSIMD" $
                whnf (computeAs P . multiplyTransposedSIMD (setComp Par arrV)) arrV2
              , bench "multArrsAlt" $ whnf (multArrsAlt (setComp Par arr)) arr'
              ]
          ]
    ]
