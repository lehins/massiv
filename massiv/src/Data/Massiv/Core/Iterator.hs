{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Data.Massiv.Core.Iterator
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
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
  , splitLinearlyWithM_
  ) where


-- | Efficient loop with an accumulator
loop :: Int -> (Int -> Bool) -> (Int -> Int) -> a -> (Int -> a -> a) -> a
loop !init' condition increment !initAcc f = go init' initAcc
  where
    go !step !acc =
      case condition step of
        False -> acc
        True  -> go (increment step) (f step acc)
{-# INLINE loop #-}


-- | Very efficient monadic loop with an accumulator
--
-- >>> loopM 1 (< 20) (+ 2) [] (\i a -> Just (i:a))
-- Just [19,17,15,13,11,9,7,5,3,1]
--
-- @since 0.1.0
loopM :: Monad m => Int -> (Int -> Bool) -> (Int -> Int) -> a -> (Int -> a -> m a) -> m a
loopM !init' condition increment !initAcc f = go init' initAcc
  where
    go !step !acc =
      case condition step of
        False -> return acc
        True  -> f step acc >>= go (increment step)
{-# INLINE loopM #-}


-- | Efficient monadic loop. Result of each iteration is discarded.
loopM_ :: Monad m => Int -> (Int -> Bool) -> (Int -> Int) -> (Int -> m a) -> m ()
loopM_ !init' condition increment f = go init'
  where
    go !step =
      case condition step of
        False -> return ()
        True  -> f step >> go (increment step)
{-# INLINE loopM_ #-}


-- | Similar to `loopM`, but slightly less efficient monadic loop with an accumulator that reverses
-- the direction of action application. eg:
--
-- >>> loopDeepM 1 (< 20) (+ 2) [] (\i a -> Just (i:a))
-- Just [1,3,5,7,9,11,13,15,17,19]
--
-- Equivalent to:
--
-- >>> loopM 19 (>= 1) (subtract 2) [] (\i a -> Just (i:a))
-- Just [1,3,5,7,9,11,13,15,17,19]
--
-- @since 0.1.0
loopDeepM :: Monad m => Int -> (Int -> Bool) -> (Int -> Int) -> a -> (Int -> a -> m a) -> m a
loopDeepM !init' condition increment !initAcc f = go init' initAcc
  where
    go !step !acc =
      case condition step of
        False -> return acc
        True  -> go (increment step) acc >>= f step
{-# INLINE loopDeepM #-}


-- | Divide length in chunks and apply a function to the computed results
--
-- @since 0.2.1
splitLinearly :: Int -- ^ Number of chunks
              -> Int -- ^ Total length
              -> (Int -> Int -> a) -- ^ Function that accepts a chunk length and slack start index
              -> a
splitLinearly numChunks totalLength action = action chunkLength slackStart
  where
    !chunkLength = totalLength `quot` numChunks
    !slackStart = chunkLength * numChunks
{-# INLINE splitLinearly #-}


-- | Interator that can be used to split computation amongst different workers. For monadic
-- generator see `splitLinearlyWithM_`.
--
-- @since 0.2.1
splitLinearlyWith_ ::
     Monad m => Int -> (m () -> m ()) -> Int -> (Int -> b) -> (Int -> b -> m ()) -> m ()
splitLinearlyWith_ numChunks with totalLength index =
  splitLinearlyWithM_ numChunks with totalLength (pure . index)
{-# INLINE splitLinearlyWith_ #-}


-- | Interator that can be used to split computation jobs
--
-- @since 0.2.6.0
splitLinearlyWithM_ ::
     Monad m => Int -> (m () -> m ()) -> Int -> (Int -> m b) -> (Int -> b -> m c) -> m ()
splitLinearlyWithM_ numChunks with totalLength make write =
  splitLinearly numChunks totalLength  $ \chunkLength slackStart -> do
    loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
      with $ loopM_ start (< (start + chunkLength)) (+ 1) $ \ !k -> make k >>= write k
    with $ loopM_ slackStart (< totalLength) (+ 1) $ \ !k -> make k >>= write k
{-# INLINE splitLinearlyWithM_ #-}




