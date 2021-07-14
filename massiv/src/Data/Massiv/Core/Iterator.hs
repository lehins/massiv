{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Data.Massiv.Core.Iterator
-- Copyright   : (c) Alexey Kuleshevich 2018-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Core.Iterator
  ( loop
  , loopA_
  , loopM
  , loopM_
  , loopDeepM
  , splitLinearly
  , splitLinearlyM_
  , splitLinearlyWith_
  , splitLinearlyWithM_
  , splitLinearlyWithStartAtM_
  , splitLinearlyWithStatefulM_
  ) where

import Primal.Monad
import Control.Scheduler


-- | Efficient loop with an accumulator
--
-- @since 0.1.0
loop :: Int -> (Int -> Bool) -> (Int -> Int) -> a -> (Int -> a -> a) -> a
loop !init' condition increment !initAcc f = go init' initAcc
  where
    go !step !acc
      | condition step = go (increment step) (f step acc)
      | otherwise = acc
{-# INLINE loop #-}


-- | Efficient monadic loop with an accumulator
--
-- >>> loopM 1 (< 20) (+ 2) [] (\i a -> Just (i:a))
-- Just [19,17,15,13,11,9,7,5,3,1]
--
-- @since 0.1.0
loopM :: Monad m => Int -> (Int -> Bool) -> (Int -> Int) -> a -> (Int -> a -> m a) -> m a
loopM !init' condition increment !initAcc f = go init' initAcc
  where
    go !step !acc
      | condition step = f step acc >>= go (increment step)
      | otherwise = return acc
{-# INLINE loopM #-}


-- | Efficient monadic loop. Result of each iteration is discarded.
--
-- @since 0.1.0
loopM_ :: Monad m => Int -> (Int -> Bool) -> (Int -> Int) -> (Int -> m a) -> m ()
loopM_ !init' condition increment f = go init'
  where
    go !step
      | condition step = f step >> go (increment step)
      | otherwise = pure ()
{-# INLINE loopM_ #-}

-- | Similar to `loopM_` axcept the action accepts not only the value for current step,
-- but also for the next one as well.
--
-- @since 0.5.7
loopNextM_ :: Monad m => Int -> (Int -> Bool) -> (Int -> Int) -> (Int -> Int -> m a) -> m ()
loopNextM_ !init' condition increment f = go init'
  where
    go step =
      when (condition step) $
      let !next = increment step
       in f step next >> go next
{-# INLINE loopNextM_ #-}


-- | Efficient Applicative loop. Result of each iteration is discarded.
--
-- @since 0.3.0
loopA_ :: Applicative f => Int -> (Int -> Bool) -> (Int -> Int) -> (Int -> f a) -> f ()
loopA_ !init' condition increment f = go init'
  where
    go !step
      | condition step = f step *> go (increment step)
      | otherwise = pure ()
{-# INLINE loopA_ #-}


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
    go !step !acc
      | condition step = go (increment step) acc >>= f step
      | otherwise = return acc
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

-- | Iterator that expects an action that accepts starting linear index as well as the ending
--
-- @since 0.5.7
splitLinearlyM_ ::
     UnliftPrimal s m => Scheduler s () -> Int -> (Int -> Int -> m ()) -> m ()
splitLinearlyM_ scheduler totalLength action =
  splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
    loopNextM_ 0 (< slackStart) (+ chunkLength) $ \ start next ->
      scheduleWork_ scheduler $ action start next
    when (slackStart < totalLength) $
      scheduleWork_ scheduler $ action slackStart totalLength
{-# INLINE splitLinearlyM_ #-}

-- | Iterator that can be used to split computation amongst different workers. For monadic
-- generator see `splitLinearlyWithM_`.
--
-- @since 0.2.1
splitLinearlyWith_ ::
     UnliftPrimal s m => Scheduler s () -> Int -> (Int -> b) -> (Int -> b -> m ()) -> m ()
splitLinearlyWith_ scheduler totalLength index =
  splitLinearlyWithM_ scheduler totalLength (pure . index)
{-# INLINE splitLinearlyWith_ #-}


-- | Iterator that can be used to split computation jobs
--
-- @since 0.2.6
splitLinearlyWithM_ ::
     UnliftPrimal s m => Scheduler s () -> Int -> (Int -> m b) -> (Int -> b -> m c) -> m ()
splitLinearlyWithM_ scheduler totalLength make write =
  splitLinearlyM_ scheduler totalLength go
  where
    go start end = loopM_ start (< end) (+ 1) $ \ k -> make k >>= write k
    {-# INLINE go #-}
{-# INLINE splitLinearlyWithM_ #-}


-- | Iterator that can be used to split computation jobs
--
-- @since 0.3.0
splitLinearlyWithStartAtM_ ::
     UnliftPrimal s m => Scheduler s () -> Int -> Int -> (Int -> m b) -> (Int -> b -> m c) -> m ()
splitLinearlyWithStartAtM_ scheduler startAt totalLength make write =
  splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
    loopM_ startAt (< (slackStart + startAt)) (+ chunkLength) $ \ !start ->
      scheduleWork_ scheduler $
      loopM_ start (< (start + chunkLength)) (+ 1) $ \ !k -> make k >>= write k
    when (slackStart < totalLength) $
      scheduleWork_ scheduler $
        loopM_ (slackStart + startAt) (< (totalLength + startAt)) (+ 1) $ \ !k -> make k >>= write k
{-# INLINE splitLinearlyWithStartAtM_ #-}



-- | Iterator that can be used to split computation jobs, while using a stateful scheduler.
--
-- @since 0.3.4
splitLinearlyWithStatefulM_ ::
     UnliftPrimal RW m
  => SchedulerWS ws ()
  -> Int -- ^ Total linear length
  -> (Int -> ws -> m b) -- ^ Element producing action
  -> (Int -> b -> m c) -- ^ Element storing action
  -> m ()
splitLinearlyWithStatefulM_ schedulerWS totalLength make store =
  let nWorkers = numWorkers (unwrapSchedulerWS schedulerWS)
   in splitLinearly nWorkers totalLength $ \chunkLength slackStart -> do
        loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
          scheduleWorkState_ schedulerWS $ \s ->
            loopM_ start (< (start + chunkLength)) (+ 1) $ \ !k ->
              make k s >>= store k
        scheduleWorkState_ schedulerWS $ \s ->
          loopM_ slackStart (< totalLength) (+ 1) $ \ !k ->
            make k s >>= store k
{-# INLINE splitLinearlyWithStatefulM_ #-}
