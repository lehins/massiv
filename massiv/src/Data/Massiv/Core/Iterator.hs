{-# LANGUAGE BangPatterns               #-}
-- |
-- Module      : Data.Massiv.Core.Iterator
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Core.Iterator
  ( loop
  , loopM
  , loopM_
  ) where


-- | Efficient loop with an accumulator
loop :: Int -> (Int -> Bool) -> (Int -> Int) -> a -> (Int -> a -> a) -> a
loop !init' condition increment !initAcc f = go init' initAcc where
  go !step !acc =
    case condition step of
      False -> acc
      True  -> go (increment step) (f step acc)
{-# INLINE loop #-}


-- | Very efficient monadic loop with an accumulator
loopM :: Monad m => Int -> (Int -> Bool) -> (Int -> Int) -> a -> (Int -> a -> m a) -> m a
loopM !init' condition increment !initAcc f = go init' initAcc where
  go !step !acc =
    case condition step of
      False -> return acc
      True  -> f step acc >>= go (increment step)
{-# INLINE loopM #-}


-- | Efficient monadic loop. Result of each iteration is discarded.
loopM_ :: Monad m => Int -> (Int -> Bool) -> (Int -> Int) -> (Int -> m a) -> m ()
loopM_ !init' condition increment f = go init' where
  go !step =
    case condition step of
      False -> return ()
      True  -> f step >> go (increment step)
{-# INLINE loopM_ #-}
