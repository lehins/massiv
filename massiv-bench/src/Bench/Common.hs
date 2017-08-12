{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Bench.Common where


lightFunc :: Int -> Int -> Double
lightFunc !i !j =
  sin (fromIntegral (i ^ (2 :: Int) + j ^ (2 :: Int)) :: Double)
{-# INLINE lightFunc #-}

heavyFunc :: Int -> Int -> Double
heavyFunc !i !j =
  sin (sqrt (sqrt ((fromIntegral i) ** 2 + (fromIntegral j) ** 2)))
{-# INLINE heavyFunc #-}
