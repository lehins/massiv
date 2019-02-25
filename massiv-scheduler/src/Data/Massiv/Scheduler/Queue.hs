{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE NamedFieldPuns #-}
-- |
-- Module      : Data.Massiv.Scheduler.Queue
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Scheduler.Queue
  ( -- * Queue
    -- ** Pure queue
    Queue
  , emptyQueue
  , pushQueue
  , popQueue
  -- ** Job queue
  , Job(Retire, Job_)
  , mkJob
  , JQueue
  , newJQueue
  , pushJQueue
  , popJQueue
  , flushResults
  -- * Tools
  -- , isDeadlock
  ) where

import           Control.Concurrent.MVar
import           Control.Monad           (join, void)
import           Data.Atomics            (atomicModifyIORefCAS)
import           Data.IORef
import           Control.Monad.IO.Unlift


-- | Pure functional Okasaki queue with total length
data Queue a = Queue { qQueue :: ![a]
                     , qStack :: ![a]
                     }

emptyQueue :: Queue a
emptyQueue = Queue [] []

pushQueue :: Queue a -> a -> Queue a
pushQueue queue@Queue {qStack} x = queue {qStack = x : qStack}

popQueue :: Queue a -> Maybe (a, Queue a)
popQueue queue@Queue {qQueue, qStack} =
  case qQueue of
    x:xs -> Just (x, queue {qQueue = xs})
    [] ->
      case reverse qStack of
        []   -> Nothing
        y:ys -> Just (y, Queue {qQueue = ys, qStack = []})

data Job m a
  = Job !(IORef a) !(m a)
  | Job_ !(m ())
  | Retire


mkJob :: MonadIO m => m a -> m (Job m a)
mkJob action = do
  resRef <- liftIO $ newIORef $ error "mkJob: result is uncomputed"
  return $!
    Job resRef $ do
      res <- action
      liftIO $ writeIORef resRef res
      return res


newtype JQueue m a =
  JQueue (IORef (Queue (Job m a), [IORef a], MVar ()))


newJQueue :: MonadIO m => m (JQueue m a)
newJQueue = do
  newBaton <- liftIO $ newEmptyMVar
  queueRef <- liftIO $ newIORef (emptyQueue, [], newBaton)
  return $ JQueue queueRef


pushJQueue :: MonadIO m => JQueue m a -> Job m a -> m ()
pushJQueue (JQueue jQueueRef) job = do
  newBaton <- liftIO $ newEmptyMVar
  join $
    liftIO $ atomicModifyIORefCAS
      jQueueRef
      (\(queue, resRefs, baton) ->
         ( ( pushQueue queue job
           , case job of
               Job resRef _ -> resRef : resRefs
               _            -> resRefs
           , newBaton)
         , liftIO $ putMVar baton ()))


popJQueue :: MonadIO m => JQueue m a -> m (Maybe (m ()))
popJQueue (JQueue jQueueRef) = liftIO $ inner
  where
    inner =
      join $
      atomicModifyIORefCAS jQueueRef $ \jQueue@(queue, resRefs, baton) ->
        case popQueue queue of
          Nothing -> (jQueue, readMVar baton >> inner)
          Just (job, newQueue) ->
            ( (newQueue, resRefs, baton)
            , case job of
                Job _ action -> return $ Just (void action)
                Job_ action_ -> return $ Just action_
                Retire -> return Nothing)

flushResults :: MonadIO m => JQueue m a -> m [a]
flushResults (JQueue jQueueRef) =
  liftIO $ do
    resRefs <-
      atomicModifyIORefCAS jQueueRef $ \(queue, resRefs, baton) -> ((queue, [], baton), resRefs)
    mapM readIORef $ reverse resRefs
