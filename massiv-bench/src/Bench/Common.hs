{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Bench.Common where


lightFunc :: Num b => Int -> Int -> b
lightFunc !i !j =
  fromIntegral
    (round (sin (fromIntegral (i ^ (2 :: Int) + j ^ (2 :: Int)) :: Float)) :: Int)
{-# INLINE lightFunc #-}

heavyFunc :: Floating a => Int -> Int -> a
heavyFunc !i !j =
  sin (sqrt (sqrt ((fromIntegral i) ** 2 + (fromIntegral j) ** 2)))
{-# INLINE heavyFunc #-}
