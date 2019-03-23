{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Main where

--import           Bench
import Bench.Massiv.Array as A
-- import           Bench.Massiv.Auto                      as M hiding (tupleToIx2)
--import           Data.Massiv.Array.Manifest  as A
import Bench.Repa
import Bench.Vector
import Criterion.Main
import Data.Array.Repa as R
import Data.Functor.Identity
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU
import GHC.Exts as GHC
import Prelude as P


main :: IO ()
main = do
  let t2 = (1600, 1200) :: (Int, Int)
      ls = toLists2 (arrDLightIx2 Seq (tupleToIx2 t2))
  defaultMain
    [ bgroup
        "makeArray"
        [ env
            (return (tupleToIx2 t2))
            (bench "Massiv Ix2 D -> U" . whnf (computeAs U . arrDLightIx2 Seq))
        , env
            (return (tupleToIx2 t2))
            (bench "Massiv Ix2 U" . whnf (\sz -> makeArrayR U Seq sz lightFuncIx2))
        -- , env
        --     (return (tupleToIx2 t2))
        --     (bench "Massiv Ix2 U Par" . whnf (\sz -> makeArrayR U Par sz lightFuncIx2))
        , env
            (return (tupleToIx2 t2))
            (bench "Massiv Ix2 P" . whnf (\sz -> makeArrayR P Seq sz lightFuncIx2))
        , env
            (return (tupleToIx2 t2))
            (bench "Massiv Ix2 S" . whnf (\sz -> makeArrayR S Seq sz lightFuncIx2))
        , env
            (return (tupleToIx2 t2))
            (bench "Massiv Ix2 B" . nf (\sz -> makeArrayR B Seq sz lightFuncIx2))
        , env (return t2) (bench "Vector U" . whnf vecLight2)
        , env
            (return (tupleToSh2 t2))
            (bench "Repa DIM2 U" . whnf (R.computeUnboxedS . arrDLightSh2))
        -- , env
        --     (return (tupleToSh2 t2))
        --     (bench "Repa DIM2 U Par" . whnf (runIdentity . R.computeUnboxedP . arrDLightSh2))
        ]
    , bgroup
        "copy"
        [ env
            (return (makeArrayR U Par (tupleToIx2 t2) lightFuncIx2))
            (bench "Massiv Ix2 U Par" . whnf clone)
        , env
            (return (makeArrayR U Seq (tupleToIx2 t2) lightFuncIx2))
            (bench "Massiv Ix2 U Seq" . whnf clone)
        , env
            (return (R.computeUnboxedS $ arrDLightSh2 $ tupleToSh2 t2))
            (bench "Repa DIM2 U Par" .
             whnf ((runIdentity . copyP) :: R.Array R.U DIM2 Double -> R.Array R.U DIM2 Double))
        , env
            (return (R.computeUnboxedS $ arrDLightSh2 $ tupleToSh2 t2))
            (bench "Repa DIM2 U Seq" .
             whnf (copyS :: R.Array R.U DIM2 Double -> R.Array R.U DIM2 Double))
        , env
            (return (vecLight2 t2))
            (\v ->
               bench "Vector U" $
               nfIO $ MVU.unsafeNew (VU.length v) >>= (\mv -> VU.copy mv v >> VU.unsafeFreeze mv))
        ]
    , env (return (concat ls)) $ \xs ->
        bgroup
          "fromList[]"
          [ bench "GHC.Exts.fromList :: [Double] -> VU.Vector Double" $
            whnf (GHC.fromList :: [Double] -> Vector Double) xs
          , bench "Massiv.Array.Manifest.fromList :: [Double] -> A.Array A.U Ix1 Double" $
            whnf (A.fromList Seq :: [Double] -> A.Array A.U Ix1 Double) xs
          , bench "GHC.Exts.fromList :: [Double] -> A.Array A.U Ix1 Double" $
            whnf (GHC.fromList :: [Double] -> A.Array A.U Ix1 Double) xs
          , bench "Repa.fromListUnboxed :: [Double] -> R.Array R.U DIM1 Double" $
            whnf (R.fromListUnboxed (Z R.:. totalElem t2)) xs
          ]
    , env (return ls) $ \xs ->
        bgroup
          "fromList[[]]"
          [ bench "Massiv.Array.Manifest.fromList Seq :: [[Double]] -> A.Array U Ix2 Double" $
            whnf (A.fromLists' Seq :: [[Double]] -> A.Array A.U Ix2 Double) xs
          , bench "Massiv.Array.Manifest.fromList Par :: [[Double]] -> A.Array U Ix2 Double" $
            whnf (A.fromLists' Par :: [[Double]] -> A.Array A.U Ix2 Double) xs
          ]
    ]
