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
  , SchedulerException(..)
  , WorkerException
  , fromWorkerException
  , makeScheduler
  , numWorkers
  , JobRequest(..)
  , JobResult
  , jobResultId
  , jobResult
  , submitRequest
  , collectResults
  , waitTillDone
  , splitWork
  , splitWork_
  , withScheduler
  , withScheduler_
  , divideWork
  ) where

import           Control.Applicative
import           Control.Concurrent             (ThreadId, forkOnWithUnmask, forkOn,
                                                 getNumCapabilities, killThread, myThreadId)
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TQueue  (TQueue, isEmptyTQueue,
                                                 newTQueue, newTQueueIO,
                                                 readTQueue, tryReadTQueue,
                                                 writeTQueue)
import           Control.Concurrent.STM.TVar    (TVar, modifyTVar', newTVar,
                                                 newTVarIO, readTVar, writeTVar)
import           Control.Exception.Base         (mask_, mask, uninterruptibleMask, uninterruptibleMask_)
import           Control.Exception.Safe         (Exception (..), SomeException,
                                                 bracket, catchAsync, isAsyncException,
                                                 throwIO, onException, withException)
import           Control.Monad                  (unless, void, when, foldM)
import           Control.Monad.STM              (STM, atomically, throwSTM)
import           Data.Array.Massiv.Common.Index
import           System.IO.Unsafe               (unsafePerformIO)
import Control.Concurrent.Async


data Job = Job (Int -> IO ())
         | Retire


data SchedulerException = SchedulerRetired deriving (Eq, Show)

instance Exception SchedulerException

data WorkerException = forall e. Exception e => WorkerDied e

instance Show WorkerException where
  show (WorkerDied exc) = "WorkerDied: " ++ show exc

instance Exception WorkerException


-- | A way to check which asyncronous exception was the cause of a worker's death.
fromWorkerException :: Exception e => WorkerException -> Maybe e
fromWorkerException (WorkerDied exc) = fromException $ toException exc


data Scheduler a = Scheduler
  { resultsChan       :: TQueue (JobResult a)
  , jobsSubmittedVar  :: TVar Int
  , jobsFinishedVar   :: TVar Int
  , workers           :: Workers
  , numWorkers        :: !Int
  , retiredVar        :: TVar Bool
  , isGlobalScheduler :: Bool
  }

data JobResult a = JobResult { jobResultId :: Int
                             , jobResult   :: !a }


data JobRequest a = JobRequest { jobRequestAction :: IO a }


data Workers = Workers { workerIds        :: [Int]
                       , workerThreadIds  :: [Async ()]
                       , workersJobQueue  :: TQueue Job
                       , workersException :: TMVar SomeException }

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
    resultsChan <- newTQueue
    jobsSubmittedVar <- newTVar 0
    jobsFinishedVar <- newTVar 0
    retiredVar <- newTVar False
    return $ Scheduler {..}



-- | Create a `Scheduler` that can be used to submit `JobRequest`s and collect
-- work done by the workers using `collectResults`.
withScheduler :: [Int] -> (JobResult a -> b -> b) -> b -> (Scheduler a -> IO c) -> IO b
withScheduler wIds collector acc submitAction = do
  let getIsGlobalScheduler = return False
        -- if null wIds
        --   then atomically $ do
        --          hasGlobalScheduler <- readTVar hasGlobalSchedulerVar
        --          unless hasGlobalScheduler $
        --            writeTVar hasGlobalSchedulerVar True
        --          return $ not hasGlobalScheduler
        --   else return False
  let getWorkers isGlobalScheduler
        | isGlobalScheduler = atomically $ readTVar globalWorkersVar
        | otherwise = hireWorkers wIds
  bracket
    (do isGlobalScheduler <- getIsGlobalScheduler
        workers <- getWorkers isGlobalScheduler
        return (isGlobalScheduler, workers)
    )
    (\(_, workers) -> return ()) --mapM_ cancel (workerThreadIds workers))
    (\(isGlobalScheduler, workers) -> do
       let numWorkers = length $ workerIds workers
       scheduler <-
         atomically $ do
           resultsChan <- newTQueue
           jobsSubmittedVar <- newTVar 0
           jobsFinishedVar <- newTVar 0
           retiredVar <- newTVar False
           return $ Scheduler {..}
       _ <- submitAction scheduler
       collectJobResults scheduler collector acc)
  -- mask_ $ do
  --   isGlobalScheduler <- getIsGlobalScheduler
  --   workers <- getWorkers isGlobalScheduler
  --   res <-
  --     onException
  --       (do let numWorkers = length $ workerIds workers
  --           scheduler <-
  --                atomically $ do
  --                  resultsChan <- newTQueue
  --                  jobsSubmittedVar <- newTVar 0
  --                  jobsFinishedVar <- newTVar 0
  --                  retiredVar <- newTVar False
  --                  return $ Scheduler {..}
  --           _ <- submitAction scheduler
  --           collectJobResults scheduler collector acc)
  --       (do mapM_ cancel (workerThreadIds workers))
  --          -- kill all workers. Recreate the workers only if killed ones were the
  --          -- global ones.
  --           -- when isGlobalScheduler $ do
  --           --   globalWorkers <- hireWorkers []
  --           --   atomically $ do
  --           --     writeTVar globalWorkersVar globalWorkers
  --           --     writeTVar hasGlobalSchedulerVar False)
  --   -- atomically $
  --   --   if isGlobalScheduler
  --   --     then writeTVar hasGlobalSchedulerVar False
  --   --     else mapM_
  --   --            (\_ -> writeTQueue (workersJobQueue workers) Retire)
  --   --            (workerThreadIds workers)
  --   return res
  -- isGlobalScheduler <- getIsGlobalScheduler
  -- workers <- getWorkers isGlobalScheduler
  -- let numWorkers = length $ workerIds workers
  -- scheduler <-
  --    atomically $ do
  --                  resultsChan <- newTQueue
  --                  jobsSubmittedVar <- newTVar 0
  --                  jobsFinishedVar <- newTVar 0
  --                  retiredVar <- newTVar False
  --                  return $ Scheduler {..}
  -- _ <- submitAction scheduler
  -- collectJobResults scheduler collector acc
  -- atomically $
  --     if isGlobalScheduler
  --       then writeTVar hasGlobalSchedulerVar False
  --       else mapM_
  --              (\_ -> writeTQueue (workersJobQueue workers) Retire)
  --              (workerThreadIds workers)
  -- return res

withScheduler_ :: [Int] -> (Scheduler a -> IO ()) -> IO ()
withScheduler_ wIds = withScheduler wIds (const id) ()


divideWork :: Index ix
          => [Int] -> ix -> (Scheduler a -> Int -> Int -> Int -> IO b) -> IO [JobResult a]
divideWork wIds sz submitWork
  | totalElem sz == 0 = return []
  | otherwise = do
    withScheduler wIds (:) [] $ \ scheduler -> do
      let !totalLength = totalElem sz
          !chunkLength = totalLength `quot` numWorkers scheduler
          !slackStart = chunkLength * numWorkers scheduler
      submitWork scheduler chunkLength totalLength slackStart


-- | Block current thread and wait for all `JobRequest`s to get processed. Use a
-- supplied function to collect all of the results produced by submitted
-- jobs. If a job throws an exception, the whole scheduler is retired, all
-- jobs are immediately cancelled and the exception is re-thrown in the main
-- thread. Same thing happens if a worker dies because of an asynchronous
-- exception, but with a `WorkerException` being thrown in a main
-- thread. `Scheduler` is also retired as soon as all of the results are
-- collected, after that it can not be used again, thus doing so will result in
-- a `SchedulerRetired` exception.
collectJobResults :: Scheduler a -> (JobResult a -> b -> b) -> b -> IO b
collectJobResults (Scheduler {..}) f !initAcc = do
  jobsSubmitted <- atomically $ readTVar jobsSubmittedVar
  if jobsSubmitted == 0
    then return initAcc
    else collect initAcc
  where
    collect !acc = do
      (res, stop) <-
        atomically $ do
          eRes <-
            (Right <$> readTQueue resultsChan) <|>
            (Left <$> readTMVar (workersException workers))
          case eRes of
            Right res -> do
              resEmpty <- isEmptyTQueue resultsChan
              if resEmpty
                then do
                  jobsSubmitted <- readTVar jobsSubmittedVar
                  jobsFinished <- readTVar jobsFinishedVar
                  return (res, jobsSubmitted == jobsFinished)
                else return (res, False)
            Left exc -> throwSTM exc
            -- $
                -- if isAsyncException exc
                --   then toException $ WorkerDied exc
                --   else exc
      if stop
        then return $! f res acc
        else collect $! f res acc



-- | Clear out outstanding jobs in the queue
clearJobQueue :: Scheduler a -> STM ()
clearJobQueue scheduler@(Scheduler {..}) = do
  mJob <- tryReadTQueue (workersJobQueue workers)
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
    when isRetired $ throwSTM SchedulerRetired
    jId <- readTVar jobsSubmittedVar
    writeTVar jobsSubmittedVar (jId + 1)
    writeTQueue (workersJobQueue workers) $
      Job $ \_wid -> do
        result <- jobRequestAction
        atomically $ do
          modifyTVar' jobsFinishedVar (+ 1)
          writeTQueue resultsChan (JobResult jId result)


-- | Block current thread and wait for all `JobRequest`s to get processed. Use a
-- supplied function to collect all of the results produced by submitted
-- jobs. If a job throws an exception, the whole scheduler is retired, all
-- jobs are immediately cancelled and the exception is re-thrown in the main
-- thread. Same thing happens if a worker dies because of an asynchronous
-- exception, but with a `WorkerException` being thrown in a main
-- thread. `Scheduler` is also retired as soon as all of the results are
-- collected, after that it can not be used again, thus doing so will result in
-- a `SchedulerRetired` exception.
collectResults :: Scheduler a -> (JobResult a -> b -> b) -> b -> IO b
collectResults scheduler@(Scheduler {..}) f !initAcc = do
  jobsSubmitted <-
    atomically $ do
      isRetired <- readTVar retiredVar
      when isRetired $ throwSTM SchedulerRetired
      readTVar jobsSubmittedVar
  if jobsSubmitted == 0
    then return initAcc
    else collect initAcc
  where
    collect !acc = do
      eResStop <-
        atomically $ do
          eRes <-
            (Right <$> readTQueue resultsChan) <|>
            (Left <$> readTMVar (workersException workers))
          case eRes of
            Right res -> do
              resEmpty <- isEmptyTQueue resultsChan
              if resEmpty
                then do
                  jobsSubmitted <- readTVar jobsSubmittedVar
                  jobsFinished <- readTVar jobsFinishedVar
                  let stop = jobsSubmitted == jobsFinished
                  when stop $ do
                    writeTVar retiredVar True
                    if isGlobalScheduler
                      then writeTVar hasGlobalSchedulerVar False
                      else loopM_ 0 (< numWorkers) (+ 1) $ \ !_ ->
                             writeTQueue (workersJobQueue workers) Retire
                  return $ Right (res, stop)
                else return $ Right (res, False)
            Left exc -> do
              writeTVar retiredVar True
              clearJobQueue scheduler
              return $ Left exc
      case eResStop of
        Right (res, stop) ->
          if stop
            then return $! f res acc
            else collect $! f res acc
        Left exc -> do
          mapM_ cancel (workerThreadIds workers)
          -- kill all workers. Recreate the workers only if killed ones were the
          -- global ones.
          when isGlobalScheduler $ do
            globalWorkers <- hireWorkers []
            atomically $ do
              writeTVar hasGlobalSchedulerVar False
              writeTVar globalWorkersVar globalWorkers
          -- We don't want to re-throw async exceptions in the main thread, so
          -- we have to wrap them in a `WorkerDied` exception.
          throwIO $
            if isAsyncException exc
              then toException $ WorkerDied exc
              else exc


-- | Block current thread and wait for the `Scheduler` to process all submitted
-- `JobRequest`s. It is a call to `collectResults`, which discards the results,
-- so all specifics apply here as well.
waitTillDone :: Scheduler a -> IO ()
waitTillDone scheduler = collectResults scheduler (const id) ()


splitWork :: Index ix
          => [Int] -> ix -> (Scheduler a -> Int -> Int -> Int -> IO b) -> IO [JobResult a]
splitWork = divideWork
-- splitWork wIds sz submitWork
--   | totalElem sz == 0 = return []
--   | otherwise = do
--     scheduler <- makeScheduler wIds
--     let !totalLength = totalElem sz
--         !chunkLength = totalLength `quot` numWorkers scheduler
--         !slackStart = chunkLength * numWorkers scheduler
--     void $ submitWork scheduler chunkLength totalLength slackStart
--     collectResults scheduler (:) []


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
  workersJobQueue <- newTQueueIO
  workersException <- newEmptyTMVarIO
  workerThreadIds <- startWorkers workersException workersJobQueue workerIds
  return Workers {..}


runWorker :: TQueue Job -> Int -> IO ()
runWorker jQueue wid = do
  job <- atomically $ readTQueue jQueue
  case job of
    Job action -> do
      action wid
      runWorker jQueue wid
    Retire -> return ()


startWorkers :: TMVar SomeException -> TQueue Job -> [Int] -> IO [Async ()]
startWorkers _wExcMVar jQueue (wId1:wIds) = do
  a <- asyncOn wId1 $ runWorker jQueue wId1
  link a
  foldM (\acc@(prevA:_) wId -> do
            curA <- asyncOn wId $ runWorker jQueue wId
            link2 prevA curA
            return (curA:acc)
        ) [a] wIds

  -- mapM
  --   (\ !wId -> do
       -- mask_ $
       -- asyncOnWithUnmask wId $ \unmask -> do
       --   withException
       --     (unmask $ runWorker jQueue wId)
       --     (\exc -> atomically (tryPutTMVar wExcMVar exc)))

       -- uninterruptibleMask_ $
       -- asyncOnWithUnmask wId $ \unmask ->
       --   withException
       --     (unmask $ runWorker jQueue wId)
       --     (atomically . tryPutTMVar wExcMVar))
         -- asyncOn wId $
         -- withException
         --   (runWorker jQueue wId)
         --   (atomically . tryPutTMVar wExcMVar))

       -- uninterruptibleMask_ $
       -- asyncOnWithUnmask wId $ \unmask ->
       --   catchAsync (unmask (runWorker jQueue wId)) $ \exc -> do
       --     _ <- atomically (tryPutTMVar wExcMVar exc)
       --     throwIO exc)
