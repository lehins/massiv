{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           CommonMassiv

import           Criterion.Main
import           Data.Array.Massiv                 as M
import           Data.Array.Massiv.Common.Ix       as M
import           Data.Array.Massiv.Numeric
import           Data.Array.Massiv.Delayed.SIMD.Prim as M
import           Data.Array.Massiv.Delayed.SIMD.Prim1 as P1
import           Data.Array.Massiv.Delayed.SIMD.Prim2 as P2
import           Data.Array.Massiv.Delayed.SIMD.Prim3 as P3
import           Data.Foldable
import qualified Data.Vector.Unboxed               as VU
import           Prelude                           as P
import Data.Int

sarrM :: (Int, Int) -> Array U DIM2 Double
sarrM arrSz = smakeArray Seq arrSz lightF
{-# INLINE sarrM #-}

toIx2 :: (Int, Int) -> Ix2
toIx2 (i, j) = i :. j

toIxArr
  :: Source r (Int, Int) e
  => Array r (Int, Int) e -> Array D Ix2 e
toIxArr arr = makeArray (getComp arr) (toIx2 (size arr)) (\(i :. j) -> unsafeIndex arr (i, j))

main :: IO ()
main = do
  let !sz = (1601, 1201) :: DIM2
      !arrCM = computeAs U $ arrM Seq sz
      !arrCM' = computeAs U $ toIxArr $ arrM Seq sz
      -- !arrInt32 =
      --   computeAs
      --     P
      --     (map (round . (* 100)) (arrM Seq sz) :: Array D DIM2 Int32)
      !arrDouble = computeAs P $ arrM Seq sz
      -- !arrSIMD = delaySIMD arrInt32
      -- !arrSIMD' = delaySIMD' arrInt32
      -- !arrSIMDd = delaySIMD arrDouble
      !arrSIMDd' = delaySIMD' arrDouble
      !arrSIMDd = P1.delaySIMD arrDouble
      !arrSIMD2 = P2.delaySIMD arrDouble
      !arrSIMD3 = P3.delaySIMD arrDouble
  let corRes = foldlS (+) 0 (M.zipWith (+) arrCM arrCM)
      testRes = P1.sumSIMD (arrSIMDd + arrSIMDd)
      testRes2 = P2.sumSIMD (arrSIMD2 + arrSIMD2)
      testRes3 = P3.sumSIMD (arrSIMD3 + arrSIMD3)
  print corRes
  print testRes
  print testRes2
  print testRes3
  defaultMain
    [ bgroup
        "Fold"
        [ bgroup
            "Sum"
            [ bench "Array Massiv Unboxed Left Fold" $ whnf (foldlS (+) 0) arrCM
            --, bench "Array Massiv Unboxed Left Fold'" $ whnf (foldlS' (+) 0) arrCM
            , bench "Array Massiv Primitive Left Fold" $
              whnf (foldlS (+) 0) arrDouble
            , bench "Array Massiv SIMD" $ whnf P1.sumSIMD arrSIMDd
            , bench "Array Massiv SIMD2" $ whnf P2.sumSIMD arrSIMD2
            , bench "Array Massiv SIMD'" $ whnf sumSIMD' arrSIMDd'
            ]
        ]
    , bgroup
        "SIMD"
        [ -- bgroup
        --     "plus Int32"
        --     [ bench "Array Massiv Normal" $
        --       whnf (computeAs U . (zipWith (+) arrInt32)) arrInt32
        --     , bench "Array Massiv SIMD" $ nfIO (computeSIMD (arrSIMD + arrSIMD))
        --     , bench "Array Massiv SIMD'" $
        --       nfIO (computeSIMD' (arrSIMD' + arrSIMD'))
        --     ]
        -- , bgroup
        --     "sum Int32"
        --     [ bench "Array Massiv Normal" $ whnf sum arrInt32
        --     , bench "Array Massiv SIMD'" $ whnf sumSIMD' arrSIMD'
        --     ]
        -- , bgroup
        --     "sum Double"
        --     [ bench "Array Massiv Normal" $ whnf sum arrDouble
        --     , bench "Array Massiv SIMD'" $ whnf sumSIMD' arrSIMDd'
        --     , bench "Array Massiv Unboxed Left Fold" $ whnf (foldlS (+) 0) arrCM
        --     , bench "Vector Unboxed Left Strict" $ whnf (VU.foldl' (+) 0) vecCU
        --     ]
        -- , 
          bgroup
            "plus Double"
            [ bench "Array Massiv Normal" $
              whnf (computeAs P . (M.zipWith (+) arrDouble)) arrDouble
            , bench "Array Massiv SIMD1" $
              whnf (P1.computeSIMD . (arrSIMDd +)) arrSIMDd
            , bench "Array Massiv SIMD2" $
              whnf (P2.computeSIMD . (arrSIMD2 +)) arrSIMD2
            , bench "Array Massiv SIMD3" $
              whnf (P3.computeSIMD . (arrSIMD3 +)) arrSIMD3
            , bench "Array Massiv SIMD'" $
              whnf (computeSIMD' . (arrSIMDd' +)) arrSIMDd'
            ]
        , bgroup
            "plus sum Double"
            [ bench "Array Massiv Normal" $
              whnf (M.sum . (M.zipWith (+) arrDouble)) arrDouble
            , bench "Array Massiv SIMD" $
              whnf (\ x -> P1.sumSIMD (x + x)) arrSIMDd
            , bench "Array Massiv SIMD2" $
              whnf (P2.sumSIMD . (arrSIMD2 +)) arrSIMD2
            , bench "Array Massiv SIMD3" $
              whnf (P3.sumSIMD . (arrSIMD3 +)) arrSIMD3
            , bench "Array Massiv SIMD" $
              whnf (P1.sumSIMD2 arrSIMDd) arrSIMDd
            , bench "Array Massiv SIMD compute" $
              whnf (P1.sumArr . P1.computeSIMD . (arrSIMDd +)) arrSIMDd
            , bench "Array Massiv SIMD'" $ whnf (sumSIMD' . (arrSIMDd' +)) arrSIMDd'
            ]
        -- , bgroup
        --     "times-plus"
        --     [ bench "Array Massiv Normal" $
        --       whnf
        --         (computeAs P . (zipWith (*) arrInt32 . zipWith (+) arrInt32))
        --         arrInt32
        --     , bench "Array Massiv SIMD" $
        --       nfIO (computeSIMD (arrSIMD * (arrSIMD + arrSIMD)))
        --     ]
        -- , bgroup
        --     "abs"
        --     [ bench "Array Massiv" $ whnf (computeAs P . map abs) arrInt32
        --     , bench "Array Massiv" $ nfIO (computeSIMD (abs arrSIMD))
        --     , bench "Array Massiv" $ nfIO (computeSIMD' (abs arrSIMD'))
        --     ]
        -- , bgroup
        --     "signum"
        --     [ bench "Array Massiv" $ whnf (computeAs P . map signum) arrInt32
        --     , bench "Array Massiv" $ nfIO (computeSIMD (signum arrSIMD))
        --     , bench "Array Massiv" $ nfIO (computeSIMD' (signum arrSIMD'))
        --     ]
        ]
    ]
