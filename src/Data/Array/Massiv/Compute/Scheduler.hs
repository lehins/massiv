{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Data.Array.Massiv.Manifest.Scheduler
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Compute.Scheduler
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
                                                 newTChanIO, readTChan,
                                                 writeTChan)
import           Control.Concurrent.STM.TVar    (TVar, modifyTVar', newTVarIO,
                                                 readTVar)
import           Control.Monad                  (void)
import           Control.Monad.STM              (atomically)
import           Data.Array.Massiv.Common.Index
import           System.IO.Unsafe               (unsafePerformIO)


-- initWorkers :: IO ()
-- initWorkers = jobQueue `seq` return ()

data Job = Job (Int -> IO ())


data Scheduler a = Scheduler
  { _resultsChan  :: !(TChan (JobResult a))
  , _jobsCountVar :: !(TVar Int)
  , numWorkers    :: !Int
  }

data JobResult a = JobResult { jobResultId :: !Int
                             , jobResult   :: !a }


data JobRequest a = JobRequest { jobRequestId     :: !Int
                               , jobRequestAction :: IO a }



makeScheduler :: IO (Scheduler a)
makeScheduler = Scheduler <$> newTChanIO <*> newTVarIO 0 <*> getNumWorkers


submitRequest :: Scheduler a -> JobRequest a -> IO ()
submitRequest (Scheduler rChan jVar _) (JobRequest jId jAction) = do
  atomically $ do
    modifyTVar' jVar (+ 1)
    writeTChan jobQueue $
      Job $ \_wid -> do
        result <- jAction
        atomically $ do
          modifyTVar' jVar (subtract 1)
          writeTChan rChan (JobResult jId result)


collectResults :: Scheduler a -> (JobResult a -> b -> b) -> b -> IO b
collectResults (Scheduler rChan jVar _) f !initAcc = collect initAcc
  where
    collect !acc = do
      (jRes, stop) <-
        atomically $ do
          jRes <- readTChan rChan
          resEmpty <- isEmptyTChan rChan
          if resEmpty
            then do
              jCount <- readTVar jVar
              return (jRes, jCount == 0)
            else return (jRes, False)
      if stop
        then return $! f jRes acc
        else collect $! f jRes acc



waitTillDone :: Scheduler a -> IO ()
waitTillDone scheduler = collectResults scheduler (const id) ()



splitWork
  :: Index ix
  => ix -> (Scheduler a -> Int -> Int -> Int -> IO b) -> IO [JobResult a]
splitWork !sz submitWork = do
  scheduler <- makeScheduler
  let !totalLength = totalElem sz
      !chunkLength = totalLength `quot` numWorkers scheduler
      !slackStart = chunkLength * numWorkers scheduler
  void $ submitWork scheduler chunkLength totalLength slackStart
  collectResults scheduler (:) []
{-# INLINE splitWork #-}



getNumWorkers :: IO Int
getNumWorkers = getNumCapabilities


jobQueue :: TChan Job
jobQueue =
  unsafePerformIO $ do
    nWorkers <- getNumWorkers
    jQueue <- newTChanIO
    startWorkers jQueue nWorkers
    return jQueue
{-# NOINLINE jobQueue #-}


runWorker :: TChan Job -> Int -> IO ()
runWorker jQueue wid = do
  Job action <- atomically $ readTChan jQueue
  action wid
  runWorker jQueue wid


startWorkers :: TChan Job -> Int -> IO ()
startWorkers jQueue nWorkers =
  loopM_ 0 (< nWorkers) (+ 1) $ \ !wid -> forkOn wid (runWorker jQueue wid)

