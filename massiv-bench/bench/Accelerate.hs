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
sumAccArr = (`indexArray` A.Z) . runN (A.sum . A.flatten)

type Image a            = A.Array DIM2 a

-- Gradients in the x- and y- directions
--
gradientX :: Acc (Image Double) -> Acc (Image Double)
gradientX = stencil grad A.clamp
  where
    grad :: Stencil3x3 Double -> Exp Double
    grad ((u, _, x)
         ,(v, _, y)
         ,(w, _, z)) = x + (2*y) + z - u - (2*v) - w

gradientY :: Acc (Image Double) -> Acc (Image Double)
gradientY = stencil grad A.clamp
  where
    grad :: Stencil3x3 Double -> Exp Double
    grad ((x, y, z)
         ,(_, _, _)
         ,(u, v, w)) = x + (2*y) + z - u - (2*v) - w


main :: IO ()
main = do
  let t2 = (1600, 1200) :: (Int, Int)
  singleThreadGang <- createTarget [0] unbalancedParIO
  defaultMain
    [ bgroup
        "Computed"
        [ env (return (accArrLightDIM2 t2)) (bench "Accelerate DIM2" . whnf sumAccArr)
        , env
            (return (computeAs P (arrDLightIx2 Par (tupleToIx2 t2))))
            (bench "Massiv P Ix2" . whnf M.sum)
        ]
    , bgroup
        "Sobel"
        [ bgroup
            "Horizontal"
            [ env
                (return (accArrLightDIM2 t2))
                (bench "Accelerate DIM2" . whnf (runN . gradientX . use))
            , env
                (return (computeAs P (arrDLightIx2 Seq (tupleToIx2 t2))))
                (bench "Array Seq Ix2 P" . whnf (computeAs P . M.mapStencil (sobelX Edge)))
            , env
                (return (computeAs P (arrDLightIx2 Par (tupleToIx2 t2))))
                (bench "Array Par Ix2 P" . whnf (computeAs P . M.mapStencil (sobelX Edge)))
            ]
        -- , bgroup
        --     "Vertical"
        --     [ env
        --         (return (accArrLightDIM2 t2))
        --         (bench "Accelerate DIM2" . whnf (runN . gradientY . use))
        --     , env
        --         (return (computeAs P (arrDLightIx2 Par (tupleToIx2 t2))))
        --         (bench "Array Ix2 P" . whnf (computeAs P . M.mapStencil (sobelY Edge)))
        --     ]
        ]
    , bgroup
        "HorizontalSobel"
        [ bgroup
            "Seq"
            [ env
                (return (accArrLightDIM2 t2))
                (bench "Accelerate DIM2" . whnf (runNWith singleThreadGang . gradientX . use))
            , env
                (return (computeAs P (arrDLightIx2 Seq (tupleToIx2 t2))))
                (bench "Array Seq Ix2 P" . whnf (computeAs P . M.mapStencil (sobelX Edge)))
            ]
        , bgroup
            "Par"
            [ env
                (return (accArrLightDIM2 t2))
                (bench "Accelerate DIM2" . whnf (runN . gradientX . use))
            , env
                (return (computeAs P (arrDLightIx2 Par (tupleToIx2 t2))))
                (bench "Array Par Ix2 P" . whnf (computeAs P . M.mapStencil (sobelX Edge)))
            ]
        ]
    ]
