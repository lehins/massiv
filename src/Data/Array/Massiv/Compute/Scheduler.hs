{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Data.Array.Massiv.Manifest.Scheduler
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Compute.Scheduler where

import Data.Array.Massiv.Common.Index
import           Control.Concurrent
import           System.IO.Unsafe   (unsafePerformIO)
-- import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM
-- import Control.Concurrent.STM.TBQueue -- (TBQueue, newTBQueueIO, writeTBQueue)


data Job = Job (Int -> IO ())


data Scheduler a = Scheduler
  { resultsChan :: !(TChan a)
  , jobsCountVar :: TVar Int
  }


data JobRequest a = JobRequest { jobAction :: IO a }


submitRequest :: Scheduler a -> JobRequest a -> IO ()
submitRequest (Scheduler rChan jVar) (JobRequest jAction) = do
  atomically $ do
    modifyTVar' jVar (+ 1)
    writeTChan jobQueue $
      Job $ \_wid -> do
        result <- jAction
        atomically $ do
          modifyTVar' jVar (subtract 1)
          writeTChan rChan result


collectResults :: Scheduler a -> (b -> a -> b) -> b -> IO b
collectResults (Scheduler rChan jVar) f !initAcc = collect initAcc
  where
    collect !acc = do
      (res, stop) <-
        atomically $ do
          res <- readTChan rChan
          resEmpty <- isEmptyTChan rChan
          if resEmpty
            then do
              jCount <- readTVar jVar
              return (res, jCount == 0)
            else return (res, False)
      if stop
        then return $! f acc res
        else collect $! f acc res



waitTillDone :: Scheduler a -> IO ()
waitTillDone scheduler = collectResults scheduler (\ _ _ -> ()) ()

makeScheduler :: IO (Scheduler a)
makeScheduler = Scheduler <$> newTChanIO <*> newTVarIO 0


data Worker = Worker { workerId :: !Int } deriving Show


getNumWorkers :: IO Int
getNumWorkers = getNumCapabilities


jobQueue :: TChan Job
jobQueue =
  unsafePerformIO $ do
    numWorkers <- getNumWorkers
    jQueue <- newTChanIO
    startWorkers jQueue numWorkers
    return jQueue
{-# NOINLINE jobQueue #-}


runWorker :: TChan Job -> Int -> IO ()
runWorker jQueue wid = do
  Job action <- atomically $ readTChan jQueue
  action wid
  runWorker jQueue wid

startWorkers :: TChan Job -> Int -> IO ()
startWorkers jQueue numWorkers =
  loopM_ 0 (< numWorkers) (+ 1) $ \ !wid -> forkOn wid (runWorker jQueue wid)



-- computeIO :: Int -- ^ Number of workers to engage. (should be in range from 1 to
--                  -- `getNumCapabilities`)
--           -> (Int -> IO a)
--           -> IO [(Int, a)]
-- computeIO n f = do
