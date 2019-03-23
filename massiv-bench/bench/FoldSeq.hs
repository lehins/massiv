{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Bench.Massiv.Array as A
import Bench.Repa
import Bench.Vector
import Control.Concurrent
import Criterion.Main
import Data.Array.Repa as R
import Data.Functor.Identity
import Data.Massiv.Array.Unsafe as A
import Data.Monoid
import qualified Data.Vector.Unboxed as VU
import GHC.Exts as GHC
import Prelude as P

main :: IO ()
main = do
  let t2 = (1600, 1200) :: (Int, Int)
  -- a <- unsafeGenerateM Par 25 (\i -> putStr (show i P.++ ": ") >> (myThreadId >>= print) >> myThreadId) :: IO (A.Array L Int ThreadId)
  defaultMain
    [ bgroup
        "Uncomputed"
        [ bgroup
            "foldLeft"
            [ env
                (return (tupleToIx2 t2))
                (bench "Array Ix2 U" . whnf (A.foldlS (+) 0 . arrDLightIx2 Seq))
            , env (return t2) (bench "Vector U" . whnf (VU.foldl' (+) 0 . vecLight2))
            , env
                (return (tupleToSh2 t2))
                (bench "Repa DIM2 U" . whnf (R.foldAllS (+) 0 . arrDLightSh2))
            ]
        , bgroup
            "foldRight"
            [ env
                (return (tupleToIx2 t2))
                (bench "Array Ix2 U" . whnf (A.foldrS (+) 0 . arrDLightIx2 Seq))
            , env (return t2) (bench "Vector U" . whnf (VU.foldr' (+) 0 . vecLight2))
            , env
                (return (tupleToSh2 t2))
                (bench "Repa DIM2 U" . whnf (R.foldAllS (+) 0 . arrDLightSh2))
            ]
        ]
    , bgroup
        "Computed"
        [ bgroup
            "foldLeft"
            [ env
                (return (computeAs U (arrDLightIx2 Seq (tupleToIx2 t2))))
                (bench "Array Ix2 U" . whnf (A.foldlS (+) 0))
            , env
                (return (computeAs B (arrDLightIx2 Seq (tupleToIx2 t2))))
                (bench "Array Ix2 B" . whnf (A.foldlS (+) 0))
            , env (return (vecLight2 t2)) (bench "Vector U" . whnf (VU.foldl' (+) 0))
            , env
                (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
                (bench "Repa DIM2 U" . whnf (R.foldAllS (+) 0))
            ]
        , bgroup
            "foldRight"
            [ env
                (return (computeAs U (arrDLightIx2 Seq (tupleToIx2 t2))))
                (bench "Array Ix2 U" . whnf (A.foldrS (+) 0))
            , env (return (vecLight2 t2)) (bench "Vector U" . whnf (VU.foldr' (+) 0))
            , env
                (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
                (bench "Repa DIM2 U" . whnf (R.foldAllS (+) 0))
            ]
        ]
    , bgroup
        "toList"
        [ bgroup
            "Sequential"
            [ env
                (return (computeAs U (arrDLightIx2 Seq (tupleToIx2 t2))))
                (bench "Array Ix2 U (A.toList)" . nf A.toList)
            , env
                (return (computeAs U (arrDLightIx2 Seq (tupleToIx2 t2))))
                (bench "Array Ix2 U (GHC.toList)" . nf GHC.toList)
            , env
                (return (computeAs U (arrDLightIx2 Seq (tupleToIx2 t2))))
                (bench "Array Ix2 U (A.toLists2)" . nf A.toLists2)
            , env
                (return (computeAs U (arrDLightIx2 Seq (tupleToIx2 t2))))
                (bench "Array Ix2 U (A.toList)" . nf A.toList)
            , env (return (vecLight2 t2)) (bench "Vector U" . nf VU.toList)
            , env
                (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
                (bench "Repa DIM2 U" . nf R.toList)
            ]
        ]
    , bgroup
        "Sum"
        [ bgroup
            "Sequential"
            [ env
                (return (computeAs U (arrDLightIx2 Seq (tupleToIx2 t2))))
                (bench "Array Ix2 U" . whnf A.sum)
            , env
                (return (computeAs U (arrDLightIx2 Seq (tupleToIx2 t2))))
                (bench "Array Ix2 U (+)" . whnf (A.foldlS (+) 0))
            , env
                (return (computeAs U (arrDLightIx2 Seq (tupleToIx2 t2))))
                (bench "Array Ix2 U monoid" . whnf (getSum . foldMono Sum))
            , env (return (vecLight2 t2)) (bench "Vector U" . whnf VU.sum)
            , env
                (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
                (bench "Repa DIM2 U" . whnf R.sumAllS)
            ]
        , bgroup
            "Parallel"
            [ env
                (return (computeAs U (arrDLightIx2 Par (tupleToIx2 t2))))
                (bench "Array U Ix2" . whnf A.sum)
            , env
                (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
                (bench "Repa DIM2 U" . whnf (runIdentity . R.sumAllP))
            ]
        ]
    -- , bgroup
    --     "mapM"
    --     [ bgroup
    --         "Massiv"
    --         [ env
    --             (return (computeAs P (arrDLightIx2 Seq (tupleToIx2 t2))))
    --             (bench "Array Ix2 P Seq" . whnf (A.mapM A.P Just))
    --         , env
    --             (return (computeAs U (arrDLightIx2 Seq (tupleToIx2 t2))))
    --             (bench "Array Ix2 U Seq" . whnf (A.mapM A.U Just))
    --         , env
    --             (return (computeAs S (arrDLightIx2 Seq (tupleToIx2 t2))))
    --             (bench "Array Ix2 S Seq" . whnf (A.mapM A.S Just))
    --         , env
    --             (return (computeAs N (arrDLightIx2 Seq (tupleToIx2 t2))))
    --             (bench "Array Ix2 N Seq" . whnf (A.mapM A.N Just))
    --         ]
    --     , env (return (vecLight2 t2)) (bench "Vector U" . whnf (VU.mapM Just))
    --     ]
    ]
