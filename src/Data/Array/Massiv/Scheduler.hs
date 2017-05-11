{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards           #-}
-- |
-- Module      : Data.Array.Massiv.Scheduler
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Scheduler
  ( Scheduler
  , SchedulerRetired(..)
  , makeScheduler
  , numWorkers
  , JobRequest(..)
  , JobResult(..)
  , submitRequest
  , collectResults
  , waitTillDone
  , splitWork
  , splitWork_
  ) where

import           Control.Concurrent             (ThreadId, forkOn,
                                                 getNumCapabilities, killThread)
import           Control.Concurrent.STM.TChan   (TChan, isEmptyTChan, newTChan,
                                                 newTChanIO, readTChan,
                                                 tryReadTChan, writeTChan)
import           Control.Concurrent.STM.TVar    (TVar, modifyTVar', newTVar,
                                                 newTVarIO, readTVar, writeTVar)
import           Control.Exception.Safe         (Exception, SomeException,
                                                 catchAny, throw, throwM)
import           Control.Monad                  (unless, void, when)
import           Control.Monad.STM              (STM, atomically)
import           Data.Array.Massiv.Common.Index
import           Data.Either                    (isLeft)
import           System.IO.Unsafe               (unsafePerformIO)

data Job = Job (Int -> IO ())
         | Retire


data SchedulerRetired = SchedulerRetired deriving (Eq, Show)

instance Exception SchedulerRetired

data Scheduler a = Scheduler
  { resultsChan       :: TChan (Either SomeException (JobResult a))
  , jobsSubmittedVar  :: TVar Int
  , jobsFinishedVar   :: TVar Int
  , workers           :: Workers
  , numWorkers        :: !Int
  , retiredVar        :: TVar Bool
  , isGlobalScheduler :: Bool
  }

data JobResult a = JobResult { jobResultId :: Int
                             , jobResult   :: !a }


data JobRequest a = JobRequest { jobRequestId     :: Int
                               , jobRequestAction :: IO a }


data Workers = Workers { workerIds :: [Int]
                       , workerThreadIds :: [ThreadId]
                       , workersJobQueue  :: TChan Job }

-- | Create a `Scheduler` that can be used to submit `JobRequest`s and collect
-- work done by the workers using `collectResults`.
makeScheduler :: [Int] -> IO (Scheduler a)
makeScheduler wIds = do
  isGlobalScheduler <-
    if null wIds
      then atomically $ do
             hasGlobalScheduler <- readTVar hasGlobalSchedulerVar
             unless hasGlobalScheduler $ writeTVar hasGlobalSchedulerVar True
             return $ not hasGlobalScheduler
      else return False
  workers <-
    if isGlobalScheduler
      then atomically $ readTVar globalWorkersVar
      else hireWorkers wIds
  let numWorkers = length $ workerIds workers
  atomically $ do
    resultsChan <- newTChan
    jobsSubmittedVar <- newTVar 0
    jobsFinishedVar <- newTVar 0
    retiredVar <- newTVar False
    return $ Scheduler {..}


-- | Clear out outstanding jobs in the queue
clearJobQueue :: Scheduler a -> STM ()
clearJobQueue scheduler@(Scheduler {..}) = do
  mJob <- tryReadTChan (workersJobQueue workers)
  case mJob of
    Just _ -> do
      modifyTVar' jobsFinishedVar (+ 1)
      clearJobQueue scheduler
    Nothing -> return ()


-- | Submit a `JobRequest`, which will get executed as soon as there is an
-- available worker. Thows `SchedulerRetired`
submitRequest :: Scheduler a -> JobRequest a -> IO ()
submitRequest Scheduler {..} JobRequest {..} = do
  atomically $ do
    isRetired <- readTVar retiredVar
    when isRetired $ throwM SchedulerRetired
    modifyTVar' jobsSubmittedVar (+ 1)
    writeTChan (workersJobQueue workers) $
      Job $ \_wid -> do
        eResult <- catchAny (Right <$> jobRequestAction) (return . Left)
        atomically $ do
          modifyTVar' jobsFinishedVar (+ 1)
          writeTChan resultsChan (JobResult jobRequestId <$> eResult)


-- | Block current thread and wait for all `JobRequest`s to get processed. Use a
-- supplied function to collect all of the results produced by submitted
-- jobs. If any job throws an exception, the whole scheduler is retired, all
-- jobs are immediately cancelled and the exception is re-thrown. `Scheduler` is
-- retired after results are collected and can not be used again, doing so will
-- result in a synchronous exception `SchedulerRetired`.
collectResults :: Scheduler a -> (JobResult a -> b -> b) -> b -> IO b
collectResults scheduler@(Scheduler {..}) f initAcc = do
  jobsSubmitted <- atomically $ do
    isRetired <- readTVar retiredVar
    when isRetired $ throwM SchedulerRetired
    readTVar jobsSubmittedVar
  if jobsSubmitted == 0
    then return initAcc
    else collect initAcc
  where
    collect !acc = do
      (eJRes, stop) <-
        atomically $ do
          eJRes <- readTChan resultsChan
          -- prevent any new jobs from starting in case of an exception
          when (isLeft eJRes) $ do
            clearJobQueue scheduler
            writeTVar retiredVar True
          resEmpty <- isEmptyTChan resultsChan
          if resEmpty
            then do
              jobsSubmitted <- readTVar jobsSubmittedVar
              jobsFinished <- readTVar jobsFinishedVar
              return (eJRes, jobsSubmitted == jobsFinished)
            else return (eJRes, False)
      case eJRes of
        Right jRes ->
          if stop
            then do
              atomically $ do
                writeTVar retiredVar True
                if isGlobalScheduler
                  then writeTVar hasGlobalSchedulerVar False
                  else loopM_ 0 (< numWorkers) (+ 1) $ \ !_ ->
                         writeTChan (workersJobQueue workers) Retire
              return $! f jRes acc
            else collect $ f jRes acc
        Left exc -> do
          mapM_ killThread (workerThreadIds workers)
          -- kill all workers. Recreate the workers only if those that were
          -- killed were primary workers.
          when isGlobalScheduler $ do
            globalWorkers <- hireWorkers []
            atomically $ do
              writeTVar hasGlobalSchedulerVar False
              writeTVar globalWorkersVar globalWorkers
          throw exc


-- | Block current thread and wait for the `Scheduler` to process all submitted
-- `JobRequest`s. It is a call to `collectResults`, which discards the results,
-- so all specifics apply here as well.
waitTillDone :: Scheduler a -> IO ()
waitTillDone scheduler = collectResults scheduler (const id) ()



splitWork :: Index ix
          => [Int] -> ix -> (Scheduler a -> Int -> Int -> Int -> IO b) -> IO [JobResult a]
splitWork wIds !sz submitWork
  | totalElem sz == 0 = return []
  | otherwise = do
    scheduler <- makeScheduler wIds
    let !totalLength = totalElem sz
        !chunkLength = totalLength `quot` numWorkers scheduler
        !slackStart = chunkLength * numWorkers scheduler
    void $ submitWork scheduler chunkLength totalLength slackStart
    collectResults scheduler (:) []

splitWork_ :: Index ix
          => [Int] -> ix -> (Scheduler a -> Int -> Int -> Int -> IO b) -> IO ()
splitWork_ wIds sz = void . splitWork wIds sz

hasGlobalSchedulerVar :: TVar Bool
hasGlobalSchedulerVar = unsafePerformIO $ newTVarIO False
{-# NOINLINE hasGlobalSchedulerVar #-}


globalWorkersVar :: TVar Workers
globalWorkersVar = unsafePerformIO $ hireWorkers [] >>= newTVarIO
{-# NOINLINE globalWorkersVar #-}


hireWorkers :: [Int] -> IO Workers
hireWorkers wIds = do
  workerIds <-
    if null wIds
      then do
        wNum <- getNumCapabilities
        return [0 .. wNum-1]
      else return wIds
  workersJobQueue <- newTChanIO
  workerThreadIds <- startWorkers workersJobQueue workerIds
  return Workers {..}


runWorker :: TChan Job -> Int -> IO ()
runWorker jQueue wid = do
  job <- atomically $ readTChan jQueue
  case job of
    Job action -> do
      action wid
      runWorker jQueue wid
    Retire -> return ()


startWorkers :: TChan Job -> [Int] -> IO [ThreadId]
startWorkers jQueue = mapM (\ !wId -> forkOn wId (runWorker jQueue wId))

