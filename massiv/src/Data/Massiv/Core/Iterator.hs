{-# LANGUAGE BangPatterns               #-}
-- |
-- Module      : Data.Massiv.Core.Iterator
-- Copyright   : (c) Alexey Kuleshevich 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Core.Iterator
  ( loop
  , loopM
  , loopM_
  , loopDeepM
  , splitLinearly
  , splitLinearlyWith_
  ) where


-- | Efficient loop with an accumulator
loop :: Int -> (Int -> Bool) -> (Int -> Int) -> a -> (Int -> a -> a) -> a
loop !init' condition increment !initAcc f = go init' initAcc
  where
    go !step !acc =
      case condition step of
        False -> acc
        True -> go (increment step) (f step acc)
{-# INLINE loop #-}


-- | Very efficient monadic loop with an accumulator
loopM :: Monad m => Int -> (Int -> Bool) -> (Int -> Int) -> a -> (Int -> a -> m a) -> m a
loopM !init' condition increment !initAcc f = go init' initAcc
  where
    go !step !acc =
      case condition step of
        False -> return acc
        True -> f step acc >>= go (increment step)
{-# INLINE loopM #-}


-- | Efficient monadic loop. Result of each iteration is discarded.
loopM_ :: Monad m => Int -> (Int -> Bool) -> (Int -> Int) -> (Int -> m a) -> m ()
loopM_ !init' condition increment f = go init'
  where
    go !step =
      case condition step of
        False -> return ()
        True -> f step >> go (increment step)
{-# INLINE loopM_ #-}


-- | Less efficient monadic loop with an accumulator that reverses the direction of action
-- application
loopDeepM :: Monad m => Int -> (Int -> Bool) -> (Int -> Int) -> a -> (Int -> a -> m a) -> m a
loopDeepM !init' condition increment !initAcc f = go init' initAcc
  where
    go !step !acc =
      case condition step of
        False -> return acc
        True -> go (increment step) acc >>= f step
{-# INLINE loopDeepM #-}



splitLinearly :: Int -> Int -> (Int -> Int -> a) -> a
splitLinearly numChunks totalLength action = action chunkLength slackStart
  where
    !chunkLength = totalLength `quot` numChunks
    !slackStart = chunkLength * numChunks
{-# INLINE splitLinearly #-}


splitLinearlyWith_ :: Monad m => Int -> (m () -> m a) -> Int -> (Int -> b) -> (Int -> b -> m ()) -> m a
splitLinearlyWith_ numChunks with totalLength index write =
  splitLinearly numChunks totalLength  $ \chunkLength slackStart -> do
    loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
      with $ loopM_ start (< (start + chunkLength)) (+ 1) $ \ !k -> write k (index k)
    with $ loopM_ slackStart (< totalLength) (+ 1) $ \ !k -> write k (index k)
{-# INLINE splitLinearlyWith_ #-}
