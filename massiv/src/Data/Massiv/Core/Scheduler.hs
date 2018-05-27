{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
-- |
-- Module      : Data.Massiv.Core.Scheduler
-- Copyright   : (c) Alexey Kuleshevich 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Core.Scheduler
  ( Scheduler
  , numWorkers
  , scheduleWork
  , withScheduler
  , withScheduler'
  , withScheduler_
  , divideWork
  , divideWork_
  ) where

import           Control.Concurrent           (ThreadId, forkOnWithUnmask,
                                               getNumCapabilities, killThread)
import           Control.Concurrent.MVar
import           Control.DeepSeq
import           Control.Exception            (SomeException, catch, mask,
                                               mask_, throwIO, try,
                                               uninterruptibleMask_)
import           Control.Monad                (forM)
import           Control.Monad.Primitive      (RealWorld)
import           Data.IORef                   (IORef, atomicModifyIORef',
                                               newIORef, readIORef)
import           Data.Massiv.Core.Index.Class (Index (totalElem))
import           Data.Massiv.Core.Iterator    (loop)
import           Data.Primitive.Array         (Array, MutableArray, indexArray,
                                               newArray, unsafeFreezeArray,
                                               writeArray)
import           System.IO.Unsafe             (unsafePerformIO)
import           System.Mem.Weak

data Job = Job (IO ())
         | Retire

data Scheduler a = Scheduler
  { jobsCountIORef :: !(IORef Int)
  , jobQueueMVar   :: !(MVar [Job])
  , resultsMVar    :: !(MVar (MutableArray RealWorld a))
  , workers        :: !Workers
  , numWorkers     :: {-# UNPACK #-} !Int
  }


data Workers = Workers { workerThreadIds :: ![ThreadId]
                       , workerJobDone   :: !(MVar (Maybe SomeException))
                       , workerJobQueue  :: !(MVar [Job])
                       }


-- | Helper function that allows scheduling work to be done in parallel. Use
-- `withScheduler` to be able to get to a `Scheduler`.
scheduleWork :: Scheduler a -- ^ Scheduler to use
             -> IO a -- ^ Action to hand of to a worker
             -> IO ()
scheduleWork Scheduler {..} jobAction = do
  modifyMVar_ jobQueueMVar $ \jobs -> do
    jix <- atomicModifyIORef' jobsCountIORef $ \jc -> (jc + 1, jc)
    let job =
          Job $ do
            jobResult <- jobAction
            withMVar resultsMVar $ \resArray -> do
              writeArray resArray jix jobResult
              putMVar (workerJobDone workers) Nothing
    return (job : jobs)


uninitialized :: a
uninitialized = error "Data.Array.Massiv.Scheduler: uncomputed job result"


-- | Execute some action that needs a resource. Perform different cleanup actions depending if
-- thataction resulted in an error or was successful. Sort of like `bracket` and `bracketOnError`
-- with info about exception combined.
bracketWithException :: forall a b c d .
  IO a -- ^ Acquire resource
  -> (a -> IO b) -- ^ Run after successfull execution
  -> (SomeException -> a -> IO c) -- ^ Run if execution resulted in exception.
  -> (a -> IO d) -- ^ Execute an action that actually needs that resource.
  -> IO d
bracketWithException before afterSuccess afterError thing = mask $ \restore -> do
  x <- before
  eRes <- try $ restore (thing x)
  case eRes of
    Left (exc :: SomeException) -> do
      _ :: Either SomeException c <- try $ uninterruptibleMask_ $ afterError exc x
      throwIO exc
    Right y -> do
      _ <- uninterruptibleMask_ $ afterSuccess x
      return y

-- | Run arbitrary computations in parallel. A pool of workers is initialized, unless Worker
-- Stations list is empty and a global worker pool is currently available. All of those workers will
-- be stealing work that you can schedule using `scheduleWork`. The order in which work is scheduled
-- will be the same as the order of the resuts of those computations, stored withing the resulting
-- array. Size of the array, which is also the first element in the returned tuple, will match the
-- number of times `scheduleWork` has been invoked. This function blocks until all of the submitted
-- jobs has finished or one of them resulted in an exception, which will be re-thrown here.
--
-- __Important__: In order to get work done truly in parallel, program needs to be compiled with
-- @-threaded@ GHC flag and executed with @+RTS -N@.
--
withScheduler :: [Int] -- ^ Worker Stations, i.e. capabilities. Empty list will
                       -- result in utilization of all available capabilities.
              -> (Scheduler a -> IO b) -- ^ Action that will be scheduling all
                                       -- the work.
              -> IO (Int, Array a)
withScheduler wss submitJobs = do
  jobsCountIORef <- newIORef 0
  jobQueueMVar <- newMVar []
  resultsMVar <- newEmptyMVar
  bracketWithException
    (do mWeakWorkers <-
          if null wss
            then tryTakeMVar globalWorkersMVar
            else return Nothing
        mGlobalWorkers <- maybe (return Nothing) deRefWeak mWeakWorkers
        let toWorkers w = return (mWeakWorkers, w)
        maybe (hireWorkers wss >>= toWorkers) toWorkers mGlobalWorkers)
    (\(mWeakWorkers, workers) -> do
       case mWeakWorkers of
         Nothing ->
           putMVar (workerJobQueue workers) $
           replicate (length (workerThreadIds workers)) Retire
         Just weak -> putMVar globalWorkersMVar weak)
    (\_ (mWeakWorkers, workers) -> do
       case mWeakWorkers of
         Nothing -> mapM_ killThread (workerThreadIds workers)
         Just weakWorkers -> do
           finalize weakWorkers
           newWeakWorkers <- hireWeakWorkers globalWorkersMVar
           putMVar globalWorkersMVar newWeakWorkers)
    (\(_, workers) -> do
       let scheduler =
             Scheduler {numWorkers = length $ workerThreadIds workers, ..}
       _ <- submitJobs scheduler
       jobCount <- readIORef jobsCountIORef
       marr <- newArray jobCount uninitialized
       putMVar resultsMVar marr
       jobQueue <- takeMVar jobQueueMVar
       putMVar (workerJobQueue workers) $ reverse jobQueue
       waitTillDone scheduler
       arr <- unsafeFreezeArray marr
       return (jobCount, arr))


-- | Just like `withScheduler`, but returns computed results in a list, instead
-- of an array.
withScheduler' :: [Int] -> (Scheduler a -> IO b) -> IO [a]
withScheduler' wss submitJobs = do
  (jc, arr) <- withScheduler wss submitJobs
  return $
    loop (jc - 1) (>= 0) (subtract 1) [] $ \i acc -> indexArray arr i : acc


-- | Just like `withScheduler`, but discards the results.
withScheduler_ :: [Int] -> (Scheduler a -> IO b) -> IO ()
withScheduler_ wss submitJobs = withScheduler wss submitJobs >> return ()


-- | Same as `divideWork`, but discard the result.
divideWork_ :: Index ix
            => [Int] -> ix -> (Scheduler a -> Int -> Int -> Int -> IO b) -> IO ()
divideWork_ wss sz submit = divideWork wss sz submit >> return ()


-- | Linearly (row-major first) and equally divide work among available workers. Submit function
-- will receive a `Scheduler`, length of each chunk, total number of elements, as well as where
-- chunks end and slack begins. Slack work will get picked up by the first worker, that has finished
-- working on his chunk. Returns list with results in the same order that work was submitted
divideWork :: Index ix
           => [Int] -- ^ Worker Stations (capabilities)
           -> ix -- ^ Size
           -> (Scheduler a -> Int -> Int -> Int -> IO b) -- ^ Submit function
           -> IO [a]
divideWork wss sz submit
  | totalElem sz == 0 = return []
  | otherwise = do
    withScheduler' wss $ \scheduler -> do
      let !totalLength = totalElem sz
          !chunkLength = totalLength `quot` numWorkers scheduler
          !slackStart = chunkLength * numWorkers scheduler
      submit scheduler chunkLength totalLength slackStart

-- | Wait till workers finished with all submitted jobs, but raise an exception if either of them
-- has died. Raised exception is the same one that was the cause of worker's death.
waitTillDone :: Scheduler a -> IO ()
waitTillDone (Scheduler {..}) = readIORef jobsCountIORef >>= waitTill 0
  where
    waitTill jobsDone jobsCount
      | jobsDone == jobsCount = return ()
      | otherwise = do
          mExc <- takeMVar (workerJobDone workers)
          case mExc of
            Just exc -> throwIO exc
            Nothing  -> waitTill (jobsDone + 1) jobsCount


-- | Worker can either be doing work, waiting for a job, or going into retirement. Temp workers are
-- rarely in waiting state, unless there is simply not enough work for all workers in the
-- pool. Unlike temp workers, global workers do spend quite a bit of time waiting for work and they
-- are never retired, but ruthlessly killed.
runWorker :: MVar [Job] -> IO ()
runWorker jobsMVar = do
  jobs <- takeMVar jobsMVar
  case jobs of
    (Job job:rest) -> putMVar jobsMVar rest >> job >> runWorker jobsMVar
    (Retire:rest)  -> putMVar jobsMVar rest
    []             -> runWorker jobsMVar


-- | Used whenever a pool of new workers is needed. If list is empty all capabilities are utilized,
-- otherwise each element in the list will be an argument to `forkOn`.
hireWorkers :: [Int] -> IO Workers
hireWorkers wss = do
  wss' <-
    if null wss
      then do
        wNum <- getNumCapabilities
        return [0 .. wNum - 1]
      else return wss
  workerJobQueue <- newEmptyMVar
  workerJobDone <- newEmptyMVar
  workerThreadIds <-
    forM wss' $ \ws ->
      mask_ $
      forkOnWithUnmask ws $ \unmask -> do
        catch
          (unmask $ runWorker workerJobQueue)
          (unmask . putMVar workerJobDone . Just)
  workerThreadIds `deepseq` return Workers {..}

-- | Global workers are the most utilized ones, therefore they are rarily restarted, in particular,
-- only in case when one of them dies of an exception. Weak reference is used so workers don't
-- continue running after MVar has been cleaned up by the GC. Each global worker has his own
-- station, i.e. global workers always span all available capabilities.
globalWorkersMVar :: MVar (Weak Workers)
globalWorkersMVar = unsafePerformIO $ do
  workersMVar <- newEmptyMVar
  weakWorkers <- hireWeakWorkers workersMVar
  putMVar workersMVar weakWorkers
  return workersMVar
{-# NOINLINE globalWorkersMVar #-}


-- | Hire workers under weak pointers. Finalizer will kill all the workers. These will be used as
-- global workers
hireWeakWorkers :: key -> IO (Weak Workers)
hireWeakWorkers k = do
  workers <- hireWorkers []
  mkWeak k workers (Just (mapM_ killThread (workerThreadIds workers)))
