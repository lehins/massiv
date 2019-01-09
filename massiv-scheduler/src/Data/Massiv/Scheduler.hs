{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Massiv.Scheduler
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Scheduler
  ( Scheduler
  , Comp(..)
  , pattern Par
  , numWorkers
  , scheduleWork
  , scheduleWork_
  , withScheduler
  , mapConcurrently
  -- , withScheduler'
  -- , withScheduler_
  -- , divideWork
  -- , divideWork_
  , traverse_
  ) where

import           Control.Concurrent
import           Control.Monad
import           Data.Atomics                      (atomicModifyIORefCAS,
                                                    atomicModifyIORefCAS_)
import           Data.Foldable                     as F (foldl')
import           Data.IORef
import           Data.Massiv.Scheduler.Computation
import           Data.Massiv.Scheduler.Queue

data Scheduler a = Scheduler
  { sNumWorkers        :: {-# UNPACK #-} !Int
  , sWorkersCounterRef :: !(IORef Int)
  , sJQueue            :: !(JQueue a)
  , sJobsCountRef      :: !(IORef Int)
  }

numWorkers :: Scheduler a -> Int
numWorkers = sNumWorkers


-- | This is a faster traverse than if `mapM_` is used.
traverse_ :: (Foldable t, Applicative f) => (a -> f ()) -> t a -> f ()
traverse_ f = F.foldl' (\c a -> c *> f a) (pure ())

mapConcurrently :: Foldable t => Comp -> (a -> IO b) -> t a -> IO [b]
mapConcurrently comp f ls = withScheduler comp $ \s -> traverse_ (scheduleWork s . f) ls

-- | Nested scheduling is absolutely fins as long as there is no other concurrent delayed
-- submissions going on
scheduleWork :: Scheduler a -> IO a -> IO ()
scheduleWork = scheduleWorkInternal mkJob

scheduleWork_ :: Scheduler a -> IO () -> IO ()
scheduleWork_ = scheduleWorkInternal (return . Job_)

scheduleWorkInternal :: (IO b -> IO (Job a)) -> Scheduler a -> IO b -> IO ()
scheduleWorkInternal mkJob' Scheduler {sJQueue, sJobsCountRef, sNumWorkers} action = do
  atomicModifyIORefCAS_ sJobsCountRef (+ 1)
  job <-
    mkJob' $ do
      res <- action
      dropCounterOnZero sJobsCountRef $
        traverse_ (pushJQueue sJQueue) $ replicate sNumWorkers Retire
      return res
  pushJQueue sJQueue job

dropCounterOnZero :: IORef Int -> IO () -> IO ()
dropCounterOnZero counterRef onZero = do
  jc <-
    atomicModifyIORefCAS
      counterRef
      (\ !i' ->
         let !i = i' - 1
          in (i, i))
  when (jc == 0) onZero



runWorker :: JQueue a
          -> IO () -- ^ Action to run upon retirement
          -> IO ()
runWorker jQueue onRetire = go
  where
    go =
      popJQueue jQueue >>= \case
        Just job -> job >> go
        Nothing -> onRetire


withScheduler :: Comp -- ^ Computation strategy
              -> (Scheduler a -> IO b)
              -- ^ Action that will be scheduling all the work.
              -> IO [a]
withScheduler comp submitWork = do
  sNumWorkers <-
    case comp of
      Seq       -> return 1
      Par       -> getNumCapabilities
      ParOn wss -> return $ length wss
  sWorkersCounterRef <- newIORef sNumWorkers
  sJQueue <- newJQueue
  sJobsCountRef <- newIORef 0
  workDoneMVar <- newEmptyMVar
  let scheduler = Scheduler {..}
      onRetire = dropCounterOnZero sWorkersCounterRef $ putMVar workDoneMVar ()
  _ <- submitWork scheduler
  -- Ensure that at least something gets scheduled
  jc <- readIORef sJobsCountRef
  when (jc == 0) $ scheduleWork_ scheduler (pure ())
  case comp of
    Seq -> runWorker sJQueue onRetire
    ParOn ws' -> do
      let ws =
            if null ws'
              then [1 .. sNumWorkers]
              else ws'
      _tids <- forM ws $ \w -> forkOn w $ runWorker sJQueue onRetire
      return ()
  readMVar workDoneMVar
  flushResults sJQueue
