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

-- import Control.Concurrent
-- import Say
-- import GHC.Stack (HasCallStack, CallStack, getCallStack, callStack, SrcLoc(..))
-- import System.Timeout

-- _TIMEOUT :: Int
-- _TIMEOUT = 1000000

-- _DEBUG :: Bool
-- _DEBUG = True

-- -- @since 0.0.0.0
-- sayStack :: CallStack -> IO ()
-- sayStack cs =
--   sayString $
--   case reverse $ getCallStack cs of
--     [] -> "<no call stack found>"
--     (_desc, loc):_ ->
--       srcLocFile loc <> ":" <> show (srcLocStartLine loc) <> ":" <> show (srcLocStartCol loc)


-- isDeadlock :: HasCallStack => IO a -> IO a
-- isDeadlock io =
--   if _DEBUG && False
--     then timeout _TIMEOUT io >>= maybe (sayStack callStack >> error "Deadlock") return
--     else io



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

data Job a
  = Job !(IORef a)
        !(IO a)
  | Job_ !(IO ())
  | Retire


mkJob :: IO a -> IO (Job a)
mkJob action = do
  resRef <- newIORef $ error "mkJob: result is uncomputed"
  return $!
    Job resRef $ do
      res <- action
      writeIORef resRef res
      return res


newtype JQueue a =
  JQueue (IORef (Queue (Job a), [IORef a], MVar ()))


newJQueue :: IO (JQueue a)
newJQueue = do
  newBaton <- newEmptyMVar
  queueRef <- newIORef (emptyQueue, [], newBaton)
  return $ JQueue queueRef


pushJQueue :: JQueue a -> Job a -> IO ()
pushJQueue (JQueue jQueueRef) job = do
  newBaton <- newEmptyMVar
  join $
    atomicModifyIORefCAS
      jQueueRef
      (\(queue, resRefs, baton) ->
         ( ( pushQueue queue job
           , case job of
               Job resRef _ -> resRef : resRefs
               _            -> resRefs
           , newBaton)
         , putMVar baton ()))


popJQueue :: JQueue a -> IO (Maybe (IO ()))
popJQueue (JQueue jQueueRef) = inner
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
                Retire       -> return Nothing)

flushResults :: JQueue a -> IO [a]
flushResults (JQueue jQueueRef) = do
  resRefs <-
    atomicModifyIORefCAS jQueueRef $ \(queue, resRefs, baton) -> ((queue, [], baton), resRefs)
  mapM readIORef $ reverse resRefs
