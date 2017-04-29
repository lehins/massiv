{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
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
  , makeScheduler
  , numWorkers
  , JobRequest(..)
  , JobResult(..)
  , submitRequest
  , collectResults
  , waitTillDone
  , splitWork
  ) where

import           Control.Concurrent             (forkOn, getNumCapabilities)
import           Control.Concurrent.STM.TChan   (TChan, isEmptyTChan,
                                                 newTChan, newTChanIO, readTChan,
                                                 writeTChan)
import           Control.Concurrent.STM.TVar    (TVar, modifyTVar', newTVar, newTVarIO,
                                                 readTVar, writeTVar)
import           Control.Monad                  (void, when, unless)
import           Control.Monad.Catch            (Exception, throwM)
import           Control.Monad.STM              (atomically)
import           Data.Array.Massiv.Common.Index
import           System.IO.Unsafe               (unsafePerformIO)

data Job = Job (Int -> IO ())
         | Retire


data SchedulerRetired = SchedulerRetired deriving Show

instance Exception SchedulerRetired

data Scheduler a = Scheduler
  { resultsChan  :: TChan (JobResult a)
  , jobCountVar :: TVar Int
  , jobQueue     :: TChan Job
  , numWorkers   :: !Int
  , retiredVar   :: TVar Bool
  , isGlobalScheduler :: Bool
  }

data JobResult a = JobResult { jobResultId :: !Int
                             , jobResult   :: !a }


data JobRequest a = JobRequest { jobRequestId     :: !Int
                               , jobRequestAction :: IO a }



makeScheduler :: IO (Scheduler a)
makeScheduler = do
  numWorkers <- getNumWorkers
  isGlobalScheduler <- atomically $ do
    hasGlobalScheduler <- readTVar hasGlobalSchedulerVar
    unless hasGlobalScheduler $ writeTVar hasGlobalSchedulerVar True
    return $ not hasGlobalScheduler
  jobQueue <- if isGlobalScheduler
              then return globalJobQueue
              else makeJobQueue numWorkers
  atomically $ do
    resultsChan <- newTChan
    jobCountVar <- newTVar 0
    retiredVar <- newTVar False
    return $ Scheduler {..}


submitRequest :: Scheduler a -> JobRequest a -> IO ()
submitRequest (Scheduler {..}) (JobRequest {..}) = do
  atomically $ do
    isRetired <- readTVar retiredVar
    when isRetired $ throwM SchedulerRetired
    modifyTVar' jobCountVar (+ 1)
    writeTChan jobQueue $
      Job $ \_wid -> do
        result <- jobRequestAction
        atomically $ do
          modifyTVar' jobCountVar (subtract 1)
          writeTChan resultsChan (JobResult jobRequestId result)


collectResults :: Scheduler a -> (JobResult a -> b -> b) -> b -> IO b
collectResults (Scheduler {..}) f !initAcc = collect initAcc
  where
    collect !acc = do
      (jRes, stop) <-
        atomically $ do
          isRetired <- readTVar retiredVar
          when isRetired $ throwM SchedulerRetired
          jRes <- readTChan resultsChan
          resEmpty <- isEmptyTChan resultsChan
          if resEmpty
            then do
              jCount <- readTVar jobCountVar
              when (jCount == 0) $ do
                writeTVar retiredVar True
                if isGlobalScheduler
                  then writeTVar hasGlobalSchedulerVar False
                  else loopM_ 0 (< numWorkers) (+ 1) $ \ !_ ->
                         writeTChan jobQueue Retire
              return (jRes, jCount == 0)
            else return (jRes, False)
      if stop
        then return $! f jRes acc
        else collect $! f jRes acc



waitTillDone :: Scheduler a -> IO ()
waitTillDone scheduler = collectResults scheduler (const id) ()



splitWork :: Index ix
          => ix -> (Scheduler a -> Int -> Int -> Int -> IO b) -> IO [JobResult a]
splitWork !sz submitWork
  | totalElem sz == 0 = return []
  | otherwise = do
    scheduler <- makeScheduler
    let !totalLength = totalElem sz
        !chunkLength = totalLength `quot` numWorkers scheduler
        !slackStart = chunkLength * numWorkers scheduler
    void $ submitWork scheduler chunkLength totalLength slackStart
    collectResults scheduler (:) []


hasGlobalSchedulerVar :: TVar Bool
hasGlobalSchedulerVar = unsafePerformIO $ newTVarIO False
{-# NOINLINE hasGlobalSchedulerVar #-}


globalJobQueue :: TChan Job
globalJobQueue = unsafePerformIO (getNumWorkers >>= makeJobQueue)
{-# NOINLINE globalJobQueue #-}


makeJobQueue :: Int -> IO (TChan Job)
makeJobQueue nWorkers = do
  jQueue <- newTChanIO
  startWorkers jQueue nWorkers
  return jQueue


getNumWorkers :: IO Int
getNumWorkers = getNumCapabilities


runWorker :: TChan Job -> Int -> IO ()
runWorker jQueue wid = do
  job <- atomically $ readTChan jQueue
  case job of
    Job action -> do
      action wid
      runWorker jQueue wid
    Retire -> return ()


startWorkers :: TChan Job -> Int -> IO ()
startWorkers jQueue nWorkers =
  loopM_ 0 (< nWorkers) (+ 1) $ \ !wid -> forkOn wid (runWorker jQueue wid)

