{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns     #-}
module Main where

import Bench.Common (lightFunc)
import Bench.Massiv.Array as M

import Criterion.Main
import Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.Native as A
import Data.Massiv.Core.Scheduler
import Data.Functor.Identity

accArrLightDIM2 :: (Int, Int) -> A.Array A.DIM2 Double
accArrLightDIM2 (m, n) = A.fromFunction (A.Z A.:. m A.:. n) (\ (A.Z A.:. i A.:. j) -> lightFunc i j)


accArrLightDIM2' :: (Int, Int) -> Acc (A.Array A.DIM2 Double)
accArrLightDIM2' (m, n) =
  generate (lift (A.Z A.:. m A.:. n)) (\(unlift -> A.Z A.:. i A.:. j) -> lightFuncExp i j)


lightFuncExp :: Exp Int -> Exp Int -> Exp Double
lightFuncExp i j =
  A.sin (A.fromIntegral (i A.^ (2 :: Exp Int) + j A.^ (2 :: Exp Int)) :: Exp Double)

sumAccArr :: (A.Elt e, A.Shape t, Prelude.Num (Exp e)) => A.Array t e -> e
sumAccArr = (`indexArray` A.Z) . run . A.sum . A.flatten . use


sumAccArr' :: (A.Elt e, A.Shape t, Prelude.Num (Exp e)) => Acc (A.Array t e) -> e
sumAccArr' = (`indexArray` A.Z) . run . A.sum . A.flatten


-- sumAccArr'' :: (A.Elt e, A.Shape t, Prelude.Num (Exp e)) => A.Array t e -> e
-- sumAccArr'' = (`indexArray` A.Z) . runN (A.sum . A.flatten) . use


main :: IO ()
main = do
  let t2 = (1600, 1200) :: (Int, Int)
  defaultMain
    [ bgroup
        "Computed"
        [ env (return (accArrLightDIM2 t2)) (bench "Accelerate DIM2" . whnf sumAccArr)
        , env
            (return (computeAs P (arrDLightIx2 Par (tupleToIx2 t2))))
            (bench "Massiv P Ix2" . whnf M.sum)
        ]
    ]
