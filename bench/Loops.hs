{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

-- import           Control.Monad.ST
import           Criterion.Main
import           Data.Array.Massiv.Common
import           Data.IORef
-- import           Data.STRef
-- import qualified Data.Vector.Unboxed                as VU
import           Prelude                  as P

accumIO :: IORef Int -> Int -> (Int, Int) -> IO ()
accumIO accRef !_k !(i, j) = do
  modifyIORef' accRef (+ (i + j))

tailCount :: Int -> Int
tailCount !n = go 0
  where
    go !i
      | i < n = go (i + 1)
      | otherwise = i
--{-# INLINE tailCount #-}

iterInt
  :: (Int, Int, Int, Int, Int)
     -> (Int, Int, Int, Int, Int)
     -> t
     -> ((Int, Int, Int, Int, Int) -> t -> t)
     -> t
iterInt !(i0, i1, i2, i3, i4) !(j0, j1, j2, j3, j4) !acc f =
    loop i0 (< j0) (+ 1) acc $ \ !k0 !acc0 ->
      loop i1 (< j1) (+ 1) acc0 $ \ !k1 !acc1 ->
        loop i2 (< j2) (+ 1) acc1 $ \ !k2 !acc2 ->
          loop i3 (< j3) (+ 1) acc2 $ \ !k3 !acc3 ->
            loop i4 (< j4) (+ 1) acc3 $ \ !k4 -> f (k0, k1, k2, k3, k4)
{-# INLINE iterInt #-}


countElems :: Index ix => ix -> Int
countElems sz = iter zeroIndex sz 1 0 (\ _ !acc -> 1 + acc)
-- {-# INLINE countElems #-}

countElemsInt :: (Int, Int, Int, Int, Int) -> Int
countElemsInt sz = iterInt zeroIndex sz 0 (\ _ !acc -> 1 + acc)
  --loop 0 (<sz) (+1) 0 (\ _ !acc -> 1 + acc)
-- {-# INLINE countElemsInt #-}

main :: IO ()
main = do
  let n = 10
  let dim2 = liftIndex (n +) zeroIndex :: DIM2
      dim3 = liftIndex (n +) zeroIndex :: DIM3
      dim4 = liftIndex (n +) zeroIndex :: DIM4
      dim5 = liftIndex (n +) zeroIndex :: DIM5
  defaultMain
    [ bgroup
        "iter"
        [ bench ("DIM2 " ++ show dim2) $ whnf countElems dim2
        , bench ("DIM2 TailCount " ++ show (n ^ (2 :: Int))) $
          whnf tailCount (n ^ (2 :: Int))
        , bench ("DIM3 " ++ show dim3) $ whnf countElems dim3
        , bench ("DIM3 TailCount " ++ show (n ^ (3 :: Int))) $
          whnf tailCount (n ^ (3 :: Int))
        , bench ("DIM4 " ++ show dim4) $ whnf countElems dim4
        , bench ("DIM4 TailCount " ++ show (n ^ (4 :: Int))) $
          whnf tailCount (n ^ (4 :: Int))
        , bench ("DIM5 " ++ show dim5) $ whnf countElems dim5
        , bench ("DIM5 local loop" ++ show dim5) $ whnf countElemsInt dim5
        , bench ("DIM5 TailCount " ++ show (n ^ (5 :: Int))) $
          whnf tailCount (n ^ (5 :: Int))
        , bench ("DIM5 loop 1: " ++ show (n ^ (5 :: Int))) $
          whnf (\ !x -> loop 0 (< x) (+ 1) (0 :: Int) (\_ !acc -> 1 + acc)) (n ^ (5 :: Int))
        , bench ("DIM5 loop 2: " ++ show (n ^ (5 :: Int))) $
          whnf countElems (n ^ (5 :: Int))
        ]
        -- [ bench "fromLinear: 0 to k" $ whnfIO $ do
        --     ref <- newIORef 0
        --     loopM_ 0 (< n) (+ 1) $ \ !j ->
        --       loopM_ 0 (< m) (+ 1) $ \ !i ->
        --          accumIO ref (n * i + j) (i, j)
        -- , bench "fromLinear: 0 to k" $ whnfIO $ do
        --     ref <- newIORef 0
        --     iterateWithLinearM_ RowMajor sz (0, 0) sz (accumIO ref)
        -- , bench "smart ST: 0 to k" $ whnfIO $ stToIO $ do
        --     ref <- newSTRef 0
        --     iterateWithLinearST_ sz 0 (uncurry (*) sz) (accumST ref)
        -- , bench "smart: 0 to k" $ whnfIO $ do
        --     ref <- newIORef 0
        --     iterateWithLinearIO_ sz 0 (uncurry (*) sz) (accumIO ref)
                 -- , bench "toLinear: 0 to k" $ whnfIO $ do
        --     ref <- newIORef 0
        --     let !end = (uncurry (*) sz)
        --     loopM_ 0 (< end) (+ 1) $ \ !k ->
        --       accumIO ref k (fromLinearIndex sz k)
        -- , bench "quotRem: 0 to k" $ whnfIO $ do
        --     ref <- newIORef 0
        --     loopM_ 0 (< (uncurry (*) sz)) (+ 1) $ \ !k ->
        --       accumIO ref k (quotRemInt k n)
        -- ]
    -- ,
    --   bgroup
    --     "iterateWithLinearIO_"
    --     [ bench "smart ST: 521 to (k - 523)" $ whnfIO $ stToIO $ do
    --         ref <- newSTRef 0
    --         iterateWithLinearST_ sz 521 (uncurry (*) sz - 523) (accumST ref)
    --     , bench "smart IO: 521 to (k - 523)" $ whnfIO $ do
    --         ref <- newIORef 0
    --         iterateWithLinearIO_ sz 521 (uncurry (*) sz - 523) (accumIO ref)
    --     , bench "fromLinear: 521 to (k - 523)" $ whnfIO $ do
    --         ref <- newIORef 0
    --         loopM_ 521 (< (uncurry (*) sz - 523)) (+ 1) $ \ !k ->
    --           accumIO ref k (fromLinearIndex sz k)
    --     , bench "quotRem: 521 to (k - 523)" $ whnfIO $ do
    --         ref <- newIORef 0
    --         loopM_ 521 (< (uncurry (*) sz - 523)) (+ 1) $ \ !k ->
    --           accumIO ref k (quotRemInt k n)
    --     ]
    -- ,
      -- bgroup
      --   "iterateWithLinearIO_"
      --   [ bench "smart: 521 to (k - 523)" $ whnfIO $ do
      --       ref <- newIORef 0
      --       iterateWithLinearIO_ sz 521 (uncurry (*) sz - 523) (accumIO ref)
      --   , bench "fromLinear: 521 to (k - 523)" $ whnfIO $ do
      --       ref <- newIORef 0
      --       loopM_ 521 (< (uncurry (*) sz - 523)) (+ 1) $ \ !k ->
      --         accumIO ref k (fromLinearIndex sz k)
      --   , bench "quotRem: 521 to (k - 523)" $ whnfIO $ do
      --       ref <- newIORef 0
      --       loopM_ 521 (< (uncurry (*) sz - 523)) (+ 1) $ \ !k ->
      --         accumIO ref k (quotRemInt k n)
      --   ]
    ]
