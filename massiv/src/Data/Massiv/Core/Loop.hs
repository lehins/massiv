{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Massiv.Core.Loop
-- Copyright   : (c) Alexey Kuleshevich 2018-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Core.Loop
  ( loop
  , loopF
  , nextMaybeF
  , loopA
  , loopA_
  , loopM
  , loopM_
  , iloopM
  , iloopA_
  , loopNextM
  , loopNextA_
  , loopDeepM
  , splitLinearly
  , splitLinearlyM
  , splitLinearlyM_
  , splitLinearlyWith_
  , splitLinearlyWithM_
  , splitLinearlyWithStartAtM_
  , splitLinearlyWithStatefulM_
  , iterLinearST_
  , iterLinearAccST_
  , iterLinearAccST
  , splitNumChunks
  , stepStartAdjust
  -- * Experimental
  , splitWorkWithFactorST
  , scheduleMassivWork
  , withMassivScheduler_
  ) where

import Control.Monad (void, when)
import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import Control.Monad.Primitive
import Control.Monad.ST (ST)
import Control.Scheduler (Comp(..), Scheduler, SchedulerWS,
                          numWorkers, scheduleWork, scheduleWorkState_,
                          scheduleWork_, trivialScheduler_, unwrapSchedulerWS,
                          withScheduler_)
import Control.Scheduler.Global (globalScheduler, withGlobalScheduler_)
import Data.Coerce
import Data.Functor.Identity

-- | Efficient loop with an accumulator
--
-- @since 0.1.0
loop :: Int -> (Int -> Bool) -> (Int -> Int) -> a -> (Int -> a -> a) -> a
loop initial condition increment initAcc f =
  runIdentity (loopM initial condition increment initAcc (coerce f))
{-# INLINE loop #-}

-- | Efficient monadic loop with an accumulator
--
-- >>> loopM 1 (< 20) (+ 2) [] (\i a -> Just (i:a))
-- Just [19,17,15,13,11,9,7,5,3,1]
--
-- @since 0.1.0
loopM :: Monad m => Int -> (Int -> Bool) -> (Int -> Int) -> a -> (Int -> a -> m a) -> m a
loopM !initial condition increment !initAcc f =
  go initial initAcc
  where
    go !step !acc
      | condition step = f step acc >>= go (increment step)
      | otherwise = pure acc
{-# INLINE loopM #-}


-- | Efficient monadic loop with an accumulator and extra linear index incremented by 1.
--
-- >>> iloopM 100 1 (< 20) (+ 2) [] (\i ix a -> Just ((i, ix) : a))
-- Just [(109,19),(108,17),(107,15),(106,13),(105,11),(104,9),(103,7),(102,5),(101,3),(100,1)]
--
-- @since 1.0.2
iloopM ::
     Monad m => Int -> Int -> (Int -> Bool) -> (Int -> Int) -> a -> (Int -> Int -> a -> m a) -> m a
iloopM !istart !initIx condition increment !initAcc f = go istart initIx initAcc
  where
    go !i !step !acc
      | condition step = f i step acc >>= go (i + 1) (increment step)
      | otherwise = pure acc
{-# INLINE iloopM #-}

-- | Efficient monadic loop. Result of each iteration is discarded.
--
-- @since 0.1.0
loopM_ :: Monad m => Int -> (Int -> Bool) -> (Int -> Int) -> (Int -> m a) -> m ()
loopM_ !initial condition increment f = go initial
  where
    go !step
      | condition step = f step >> go (increment step)
      | otherwise = pure ()
  --loopF initial condition increment (pure ()) (\i ma -> f i >> ma)
{-# INLINE loopM_ #-}
{-# DEPRECATED loopM_ "In favor of `loopA_`" #-}

-- | Efficient monadic loop with extra linear index incremented by 1.
--
-- >>> iloopA_ 100 1 (< 10) (+ 2) (\i ix -> print (i, ix))
-- (100,1)
-- (101,3)
-- (102,5)
-- (103,7)
-- (104,9)
--
-- @since 1.0.2
iloopA_ ::
     Applicative f => Int -> Int -> (Int -> Bool) -> (Int -> Int) -> (Int -> Int -> f a) -> f ()
iloopA_ !istart !initIx condition increment f = go istart initIx
  where
    go !i !step
      | condition step = f i step *> go (i + 1) (increment step)
      | otherwise = pure ()
{-# INLINE iloopA_ #-}

-- | Similar to `loopM_` except the action accepts not only the value for current step,
-- but also for the next one as well.
--
-- @since 1.0.2
loopNextA_ :: Applicative f => Int -> (Int -> Bool) -> (Int -> Int) -> (Int -> Int -> f a) -> f ()
loopNextA_ !initial condition increment f = go initial
  where
    go !step
      | condition step =
        let !next = increment step
        in f step next *> go next
      | otherwise = pure ()
{-# INLINE loopNextA_ #-}

-- | Similar to `loopM_` except the action accepts not only the value for current step,
-- but also for the next one as well.
--
-- @since 1.0.2
loopNextM :: Monad m => Int -> (Int -> Bool) -> (Int -> Int) -> a -> (Int -> Int -> a -> m a) -> m a
loopNextM !initial condition increment !initAcc f = go initial initAcc
  where
    go !step !acc
      | condition step =
        let !next = increment step
        in f step next acc >>= go next
      | otherwise = pure acc
{-# INLINE loopNextM #-}

-- | Efficient Applicative loop. Result of each iteration is discarded.
--
-- > loopA_ initial cond incr f === loopA initial cond incr (pure ()) (\i -> id <$ f i)
--
-- @since 1.0.2
loopA_ :: Applicative f => Int -> (Int -> Bool) -> (Int -> Int) -> (Int -> f a) -> f ()
loopA_ !initial condition increment f =
  loopF initial condition increment (pure ()) (\i ma -> f i *> ma)
{-# INLINE loopA_ #-}

-- | Applicative loop. Use monadic `loopM` when possible, since it will be more efficient.
--
-- @since 0.3.0
loopA :: Applicative f => Int -> (Int -> Bool) -> (Int -> Int) -> f b -> (Int -> f (b -> b)) -> f b
loopA !initial condition increment lastAction f =
  loopF initial condition increment lastAction (\i ma -> f i <*> ma)
{-# INLINE loopA #-}


loopF :: Int -> (Int -> Bool) -> (Int -> Int) -> f a -> (Int -> f a -> f a) -> f a
loopF !initial condition increment lastAction f = go initial
  where
    go !step
      | condition step = f step (go (increment step))
      | otherwise = lastAction
{-# INLINE loopF #-}


nextMaybeF :: Int -> (Int -> Bool) -> (Int -> Int) -> (Maybe Int -> f a) -> f a
nextMaybeF !cur condition increment f =
  let !i = increment cur
  in f $! if condition i then Just i else Nothing
{-# INLINE nextMaybeF #-}

-- | Similar to `loopM`, but way less efficient monadic loop with an accumulator that reverses
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
loopDeepM !initial condition increment !initAcc f =
  loopF initial condition increment (pure initAcc) (\i ma -> ma >>= f i)
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
     MonadPrimBase s m => Scheduler s () -> Int -> (Int -> Int -> m ()) -> m ()
splitLinearlyM_ scheduler totalLength action =
  splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
    loopNextA_ 0 (< slackStart) (+ chunkLength) $ \ start next ->
      scheduleWork_ scheduler $ action start next
    when (slackStart < totalLength) $
      scheduleWork_ scheduler $ action slackStart totalLength
{-# INLINE splitLinearlyM_ #-}

-- | Iterator that expects an action that accepts starting linear index as well as the ending
--
-- @since 1.0.2
splitLinearlyM ::
     MonadPrimBase s m => Scheduler s a -> Int -> (Int -> Int -> m a) -> m ()
splitLinearlyM scheduler totalLength action =
  splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
    loopNextA_ 0 (< slackStart) (+ chunkLength) $ \ start next ->
      scheduleWork scheduler (action start next)
    when (slackStart < totalLength) $
      scheduleWork scheduler (action slackStart totalLength)
{-# INLINE splitLinearlyM #-}

-- | Iterator that can be used to split computation amongst different workers. For monadic
-- generator see `splitLinearlyWithM_`.
--
-- @since 0.2.1
splitLinearlyWith_ ::
     MonadPrimBase s m => Scheduler s () -> Int -> (Int -> b) -> (Int -> b -> m ()) -> m ()
splitLinearlyWith_ scheduler totalLength index =
  splitLinearlyWithM_ scheduler totalLength (pure . index)
{-# INLINE splitLinearlyWith_ #-}


-- | Iterator that can be used to split computation jobs
--
-- @since 0.2.6
splitLinearlyWithM_ ::
     MonadPrimBase s m => Scheduler s () -> Int -> (Int -> m b) -> (Int -> b -> m c) -> m ()
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
     MonadPrimBase s m => Scheduler s () -> Int -> Int -> (Int -> m b) -> (Int -> b -> m c) -> m ()
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
     MonadUnliftIO m
  => SchedulerWS ws ()
  -> Int -- ^ Total linear length
  -> (Int -> ws -> m b) -- ^ Element producing action
  -> (Int -> b -> m c) -- ^ Element storing action
  -> m ()
splitLinearlyWithStatefulM_ schedulerWS totalLength make store =
  let nWorkers = numWorkers (unwrapSchedulerWS schedulerWS)
   in withRunInIO $ \run ->
      splitLinearly nWorkers totalLength $ \chunkLength slackStart -> do
        loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
          scheduleWorkState_ schedulerWS $ \s ->
            loopM_ start (< (start + chunkLength)) (+ 1) $ \ !k ->
              run (make k s >>= store k)
        scheduleWorkState_ schedulerWS $ \s ->
          loopM_ slackStart (< totalLength) (+ 1) $ \ !k ->
            run (make k s >>= store k)
{-# INLINE splitLinearlyWithStatefulM_ #-}


-- | This is a major helper function for fair splitting and parallelization of
-- work with ability to use some arbitrary accumulator and splittable seed
--
-- @since 1.0.2
splitWorkWithFactorST ::
     Int -- ^ Multiplying factor to be applied to number of workers for number
         -- of jobs to schedule. Higher the factor, more jobs will be
         -- scheduled. Only positive values are valid.
  -> Scheduler s a
  -> Int -- ^ Starting index
  -> Int -- ^ Stepping value. Can be negative, but must not be zero.
  -> Int -- ^ Total number of steps to be taken
  -> b -- ^ Initial value for an accumulator
  -> (b -> ST s (b, b)) -- ^ An action to split accumulator for multiple threads
  -> (Int -> Int -> Int -> Int -> b -> ST s a)
  -- ^ A job to be scheduled. Accepts:
  --
  -- * Chunk index start
  -- * Chunk length
  -- * Chunk start index adjusted for supplied start and stepping value
  -- * Chunk stop index adjusted for supplied start and stepping value
  -> ST s b
splitWorkWithFactorST fact scheduler start step totalLength initAcc splitAcc action = do
  let !(chunkLength, slackStart) = splitNumChunks fact (numWorkers scheduler) totalLength
  slackAcc <-
    loopM 0 (< slackStart) (+ chunkLength) initAcc $ \ !chunkStart !acc -> do
      (accCur, accNext) <- splitAcc acc
      scheduleMassivWork scheduler $ do
        let !chunkStartAdj = start + chunkStart * step
            !chunkStopAdj = chunkStartAdj + chunkLength * step
        action chunkStart chunkLength chunkStartAdj chunkStopAdj accCur
      pure accNext
  let !slackLength = totalLength - slackStart
  if slackLength > 0
    then do
      (curAcc, nextAcc) <- splitAcc slackAcc
      scheduleMassivWork scheduler $ do
        let !slackStartAdj = start + slackStart * step
            !slackStopAdj = slackStartAdj + slackLength * step
        action slackStart slackLength slackStartAdj slackStopAdj curAcc
      pure nextAcc
    else pure slackAcc
{-# INLINE splitWorkWithFactorST #-}

-- | Linear iterator that supports multiplying factor
--
-- @since 1.0.2
iterLinearST_ ::
     Int
  -> Scheduler s ()
  -> Int
  -> Int
  -> Int
  -> (Int -> ST s a)
  -> ST s ()
iterLinearST_ fact scheduler start step n action = do
  let totalLength = (n - start) `quot` step
  splitWorkWithFactorST fact scheduler start step totalLength () (\_ -> pure ((), ()))
    $ \ _ _ chunkStartAdj chunkStopAdj _ ->
    loopA_ chunkStartAdj (< chunkStopAdj) (+ step) action
{-# INLINE iterLinearST_ #-}

-- | Linear iterator that supports multiplying factor and accumulator, but the results are discarded.
--
-- @since 1.0.2
iterLinearAccST_ ::
     Int
  -> Scheduler s ()
  -> Int
  -> Int
  -> Int
  -> a
  -> (a -> ST s (a, a))
  -> (Int -> a -> ST s a)
  -> ST s ()
iterLinearAccST_ fact scheduler start step n initAcc splitAcc action = do
  let totalLength = (n - start) `quot` step
  void $ splitWorkWithFactorST fact scheduler start step totalLength initAcc splitAcc
    $ \ _ _ chunkStartAdj chunkStopAdj accCur ->
    void $ loopM chunkStartAdj (< chunkStopAdj) (+ step) accCur action
{-# INLINE iterLinearAccST_ #-}

-- | Linear iterator that supports multiplying factor and accumulator. Results
-- of actions are stored in the scheduler.
--
-- @since 1.0.2
iterLinearAccST ::
     Int
  -> Scheduler s a
  -> Int
  -> Int -- ^ Step. Must be non-zero
  -> Int
  -> a
  -> (a -> ST s (a, a))
  -> (Int -> a -> ST s a)
  -> ST s a
iterLinearAccST fact scheduler start step n initAcc splitAcc action = do
  let totalLength = (n - start) `quot` step
  splitWorkWithFactorST fact scheduler start step totalLength initAcc splitAcc
    $ \ _ _ chunkStartAdj chunkStopAdj accCur ->
    loopM chunkStartAdj (< chunkStopAdj) (+ step) accCur action
{-# INLINE iterLinearAccST #-}


-- | Helper for figuring out the chunk length and slack start
splitNumChunks :: Int -> Int -> Int -> (Int, Int)
splitNumChunks fact nw totalLength =
  let maxNumChunks = nw * max 1 fact
      !numChunks
        | nw == 1 || totalLength <= 0 = 1 -- Optimize for Seq and avoid `quot` by 0.
        | totalLength <= nw = totalLength
        | totalLength >= maxNumChunks = maxNumChunks
        | otherwise = nw
      !chunkLength = totalLength `quot` numChunks
      !slackStart = chunkLength * numChunks
  in (chunkLength, slackStart)


-- | Helper for adjusting stride of a chunk
stepStartAdjust :: Int -> Int -> Int
stepStartAdjust step ix = ix + ((step - (ix `mod` step)) `mod` step)
{-# INLINE stepStartAdjust #-}


-- | Internal version of a `scheduleWork` that will be replaced by
-- `scheduleWork_` by the compiler whenever action produces `()`
scheduleMassivWork :: PrimBase m => Scheduler (PrimState m) a -> m a -> m ()
scheduleMassivWork = scheduleWork
{-# INLINE[0] scheduleMassivWork #-}

{-# RULES
"scheduleWork/scheduleWork_/ST" forall (scheduler :: Scheduler s ()) (action :: ST s ()) . scheduleMassivWork scheduler action = scheduleWork_ scheduler action
"scheduleWork/scheduleWork_/IO" forall (scheduler :: Scheduler RealWorld ()) (action :: IO ()) . scheduleMassivWork scheduler action = scheduleWork_ scheduler action
 #-}

-- | Selects an optimal scheduler for the supplied strategy, but it works only in `IO`
--
-- @since 1.0.0
withMassivScheduler_ :: Comp -> (Scheduler RealWorld () -> IO ()) -> IO ()
withMassivScheduler_ comp f =
  case comp of
    Par -> withGlobalScheduler_ globalScheduler f
    Seq -> f trivialScheduler_
    _   -> withScheduler_ comp f
{-# INLINE withMassivScheduler_ #-}
