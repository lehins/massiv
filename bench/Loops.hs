{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Control.Monad.ST
import           Criterion.Main
import           Data.Array.Massiv.Common
import           Data.IORef
import           Data.STRef
-- import qualified Data.Vector.Unboxed                as VU
import           Prelude                  as P

accumIO :: IORef Int -> Int -> (Int, Int) -> IO ()
accumIO accRef !_k !(i, j) = do
  modifyIORef' accRef (+ (i + j))

accumST :: STRef s Int -> Int -> (Int, Int) -> ST s ()
accumST accRef !k !(i, j) = do
  let a = (fromIntegral k)**2 * sqrt ((fromIntegral i)**2 + (fromIntegral j)**2) :: Double
  modifySTRef' accRef (+ round a)


main :: IO ()
main = do
  let !sz@(m, n) = (1600, 1200) :: (Int, Int)
  defaultMain
    [ bgroup
        "iterateWithLinearIO_"
        [ bench "fromLinear: 0 to k" $ whnfIO $ do
            ref <- newIORef 0
            loopM_ 0 (< n) (+ 1) $ \ !j ->
              loopM_ 0 (< m) (+ 1) $ \ !i ->
                 accumIO ref (n * i + j) (i, j)
        , bench "fromLinear: 0 to k" $ whnfIO $ do
            ref <- newIORef 0
            iterateWithLinearM_ RowMajor sz (0, 0) sz (accumIO ref)
        , bench "smart ST: 0 to k" $ whnfIO $ stToIO $ do
            ref <- newSTRef 0
            iterateWithLinearST_ sz 0 (uncurry (*) sz) (accumST ref)
        , bench "smart: 0 to k" $ whnfIO $ do
            ref <- newIORef 0
            iterateWithLinearIO_ sz 0 (uncurry (*) sz) (accumIO ref)
                 -- , bench "toLinear: 0 to k" $ whnfIO $ do
        --     ref <- newIORef 0
        --     let !end = (uncurry (*) sz)
        --     loopM_ 0 (< end) (+ 1) $ \ !k ->
        --       accumIO ref k (fromLinearIndex sz k)
        -- , bench "quotRem: 0 to k" $ whnfIO $ do
        --     ref <- newIORef 0
        --     loopM_ 0 (< (uncurry (*) sz)) (+ 1) $ \ !k ->
        --       accumIO ref k (quotRemInt k n)
        ]
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
