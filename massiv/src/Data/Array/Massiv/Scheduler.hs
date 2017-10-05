{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
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
  , numWorkers
  , withScheduler
  , withScheduler'
  , withScheduler_
  , scheduleWork
  , divideWork
  , divideWork_
  ) where

import           Control.Concurrent             (ThreadId, forkOnWithUnmask,
                                                 getNumCapabilities, killThread,
                                                 myThreadId)
import           Control.Concurrent.MVar
import           Control.DeepSeq
import           Control.Exception              (BlockedIndefinitelyOnMVar (..),
                                                 SomeException,
                                                 catch, mask, mask_, throwIO,
                                                 try, uninterruptibleMask_)
import           Control.Monad                  (forM, when)
import           Control.Monad.Primitive        (RealWorld)
import           Data.Array.Massiv.Common.Index (Index (totalElem), loop)
import           Data.IORef                     (IORef, atomicModifyIORef',
                                                 newIORef, readIORef)
import           Data.Primitive.Array           (Array, MutableArray,
                                                 indexArray, newArray,
                                                 unsafeFreezeArray, writeArray)
import           Say
import           System.IO.Unsafe               (unsafePerformIO)
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
                       , workerJobDone   :: !(MVar (Maybe (ThreadId, SomeException)))
                       , workerJobQueue  :: !(MVar [Job])
                       }


-- | Submit a `JobRequest`, which will get executed as soon as there is an
-- available worker. Thows `SchedulerRetired`
scheduleWork :: Scheduler a -> IO a -> IO ()
scheduleWork Scheduler {..} jobAction = do
  d "scheduleWork.init"
  modifyMVar_ jobQueueMVar $ \jobs -> do
    d "scheduleWork.modifyQueue"
    jix <- atomicModifyIORef' jobsCountIORef $ \jc -> (jc + 1, jc)
    let job =
          Job $ do
            d "scheduleWork.Job.before"
            jobResult <- jobAction
            d "scheduleWork.Job.afterJob"
            withMVar resultsMVar $ \resArray -> do
              writeArray resArray jix jobResult
              d "scheduleWork.Job.wroteResult"
              retryCount "scedulerWork.Job(put).workerJobDone" $
                putMVar (workerJobDone workers) Nothing
              d "scheduleWork.jobDone"
    return (job : jobs)


uninitialized :: a
uninitialized = error "Data.Array.Massiv.Scheduler: uncomputed job result"


-- | Execute some action that needs a resource. Perform different cleanup
-- actions depending if thataction resulted in an error or was successful. Sort
-- of like `bracket` and `bracketOnError` with info about exception combined.
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
      _ :: Either SomeException b <- try $ uninterruptibleMask_ $ afterSuccess x
      return y


withScheduler :: [Int] -> (Scheduler a -> IO b) -> IO (Int, Array a)
withScheduler wIds submitJobs = do
  jobsCountIORef <- newIORef 0
  jobQueueMVar <- newMVar []
  resultsMVar <- newEmptyMVar
  d "withScheduler.init"
  bracketWithException
    (do mWeakWorkers <-
          if null wIds
            then tryTakeMVar globalWorkersMVar
            else return Nothing
        mGlobalWorkers <- maybe (return Nothing) deRefWeak mWeakWorkers
        let toWorkers w = return (mWeakWorkers, w)
        maybe (hireWorkers wIds >>= toWorkers) toWorkers mGlobalWorkers)
    (\(mWeakWorkers, workers) -> do
       case mWeakWorkers of
         Nothing ->
           retryCount "withScheduler (put).workerJobQueue" $
           putMVar (workerJobQueue workers) $
           replicate (length (workerThreadIds workers)) Retire
         Just weak ->
           retryCount "withScheduler(put).globalWorkersMVar" $
           putMVar globalWorkersMVar weak)
    (\exc (mWeakWorkers, workers) -> do
       d $ "withScheduler.gotException: " ++ show (exc :: SomeException)
       case mWeakWorkers of
         Nothing -> mapM_ killThread (workerThreadIds workers)
         Just weakWorkers -> do
           finalize weakWorkers
           newWeakWorkers <- hireWeakWorkers globalWorkersMVar
           putMVar globalWorkersMVar newWeakWorkers)
    (\(_, workers) -> do
       d "withScheduler.hiredWorkers"
       let scheduler =
             Scheduler {numWorkers = length $ workerThreadIds workers, ..}
       _ <- submitJobs scheduler
       jobCount <- readIORef jobsCountIORef
       marr <- newArray jobCount uninitialized
       putMVar resultsMVar marr
       d "withScheduler.waiting"
       jobQueue <-
         retryCount "withScheduler (take).jobQueueMVar" $ takeMVar jobQueueMVar
       retryCount "withScheduler (put).workerJobQueue" $
         putMVar (workerJobQueue workers) $ reverse jobQueue
       waitTillDone scheduler
       arr <- unsafeFreezeArray marr
       return (jobCount, arr))


-- | Just like `withScheduler`, but returns computed results in a list, instead
-- of an array.
withScheduler' :: [Int] -> (Scheduler a -> IO b) -> IO [a]
withScheduler' wIds submitJobs = do
  (jc, arr) <- withScheduler wIds submitJobs
  return $
    loop (jc - 1) (>= 0) (subtract 1) [] $ \i acc -> indexArray arr i : acc


-- | Just like `withScheduler`, but discards the results.
withScheduler_ :: [Int] -> (Scheduler a -> IO b) -> IO ()
withScheduler_ wIds submitJobs = withScheduler wIds submitJobs >> return ()


divideWork_ :: Index ix
            => [Int] -> ix -> (Scheduler a -> Int -> Int -> Int -> IO b) -> IO ()
divideWork_ wIds sz submit = divideWork wIds sz submit >> return ()


divideWork :: Index ix
          => [Int] -> ix -> (Scheduler a -> Int -> Int -> Int -> IO b) -> IO [a]
divideWork wIds sz submit
  | totalElem sz == 0 = return []
  | otherwise = do
    withScheduler' wIds $ \scheduler -> do
      let !totalLength = totalElem sz
          !chunkLength = totalLength `quot` numWorkers scheduler
          !slackStart = chunkLength * numWorkers scheduler
      submit scheduler chunkLength totalLength slackStart


waitTillDone :: Scheduler a -> IO ()
waitTillDone (Scheduler {..}) = readIORef jobsCountIORef >>= waitTill 0
  where
    waitTill jobsDone jobsCount
      | jobsDone == jobsCount = return ()
      | otherwise = do
          mExc <- retryCount "waitTillDone.jobDoneMVar" (takeMVar (workerJobDone workers))
          case mExc of
            Just (tid, exc) -> do
              d $ "waitTillDone.gotException (from " ++ show tid ++ "): " ++ show exc
              throwIO exc
            Nothing -> waitTill (jobsDone + 1) jobsCount

debug :: Bool
debug = False

d :: [Char] -> IO ()
d str = when debug $ do
  tid <- myThreadId
  sayString $ "ThreadId: " ++ show tid ++ " is doing " ++ str


retryCount :: [Char] -> IO a -> IO a
retryCount loc action =
  if debug
    then loop' (10 :: Int)
    else action
  where
    loop' 0 = action
    loop' cnt =
      action `catch` \BlockedIndefinitelyOnMVar -> do
        d $ "retryCount." ++ loc
        loop' (cnt - 1)

runWorker :: MVar [Job] -> IO ()
runWorker jobsMVar = do
  jobs <- retryCount "runWorker.jobsMVar" (takeMVar jobsMVar)
  case jobs of
    (Job job:rest) -> do
      d "runWorker.Job" >>
        retryCount "runWorker.jobsMVar (Job)" (putMVar jobsMVar rest) >>
        job >>
        runWorker jobsMVar
    (Retire:rest) ->
      d "runWorker.Retire" >>
      retryCount "runWorker.jobsMVar (Retire)" (putMVar jobsMVar rest) >>
      return ()
    [] -> d "runWorker.Empty" >> runWorker jobsMVar

hireWorkers :: [Int] -> IO Workers
hireWorkers wIds = do
  workerIds <-
    if null wIds
      then do
        wNum <- getNumCapabilities
        return [0 .. wNum - 1]
      else return wIds
  workerJobQueue <- newEmptyMVar
  workerJobDone <- newEmptyMVar
  workerThreadIds <-
    forM workerIds $ \wId ->
      mask_ $
      forkOnWithUnmask wId $ \unmask -> do
        catch
          (unmask (runWorker workerJobQueue))
          (\exc -> do
             d $ "hireWorkers.gotException: " ++ show exc
             tid <- myThreadId
             retryCount ("hireWorkers.workerJobDone: " ++ show exc) $
               unmask $ putMVar workerJobDone (Just (tid, exc))
             d $ "hireWorkers.storedException: " ++ show exc)
  workerThreadIds `deepseq` return Workers {..}


globalWorkersMVar :: MVar (Weak Workers)
globalWorkersMVar = unsafePerformIO $ do
  workersMVar <- newEmptyMVar
  weakWorkers <- hireWeakWorkers workersMVar
  retryCount "globalWorkersMVar" $ putMVar workersMVar weakWorkers
  return workersMVar
{-# NOINLINE globalWorkersMVar #-}


hireWeakWorkers :: key -> IO (Weak Workers)
hireWeakWorkers k = do
  workers <- hireWorkers []
  mkWeak k workers (Just (mapM_ killThread (workerThreadIds workers)))

-- startWorkers :: MVar SomeException -> MVar [Job] -> [Int] -> IO Workers
-- startWorkers workerException workerJobQueue workerIds = do
--   workerThreadIds <-
--     mapM
--       (\wId ->
--          uninterruptibleMask_ $
--          forkOnWithUnmask wId $ \unmask ->
--            withException (unmask (runWorker workerJobQueue)) $ \exc -> do
--              _ <- tryPutMVar workerException exc
--              throwIO exc) workerIds
--   return Workers {..}

--        a <-
--          asyncOn wId $
--          withException
--            (runWorker jQueue wId)
--            (\exc -> do
--               suc <- atomically $ tryPutTMVar wExcMVar exc
--               when suc $ putStrLn $ "Will attempt to kill: " ++ show tid)
--        --s <- newStablePtr $ asyncThreadId a
--        return (undefined, a))
--     wIds
--   -- a <- asyncOn wId1 $ runWorker jQueue wId1
--   -- link a
--   -- foldM (\acc@(prevA:_) wId -> do
--   --           curA <- asyncOn wId $ runWorker jQueue wId
--   --           link2 prevA curA
--   --           return (curA:acc)
--   --       ) [a] wIds
--   -- mapM
--   --   (\ !wId -> do
--        -- mask_ $
--        -- asyncOnWithUnmask wId $ \unmask -> do
--        --   withException
--        --     (unmask $ runWorker jQueue wId)
--        --     (\exc -> atomically (tryPutTMVar wExcMVar exc)))
--        -- uninterruptibleMask_ $
--        -- asyncOnWithUnmask wId $ \unmask ->
--        --   withException
--        --     (unmask $ runWorker jQueue wId)
--        --     (atomically . tryPutTMVar wExcMVar))
--          -- asyncOn wId $
--          -- withException
--          --   (runWorker jQueue wId)
--          --   (atomically . tryPutTMVar wExcMVar))
--        -- uninterruptibleMask_ $
--        -- asyncOnWithUnmask wId $ \unmask ->
--        --   catchAsync (unmask (runWorker jQueue wId)) $ \exc -> do
--        --     _ <- atomically (tryPutTMVar wExcMVar exc)
--        --     throwIO exc)




-- -- | Create a `Scheduler` that can be used to submit `JobRequest`s and collect
-- -- work done by the workers using `collectResults`.
-- withScheduler :: [Int] -> (JobResult a -> b -> b) -> b -> (Scheduler a -> IO c) -> IO b
-- withScheduler wIds collector acc submitAction = do
--   bracketOnError'
--     (hireWorkers wIds)
--     (\exc workers -> do
--        mapM_ (cancel . snd) (workerThreadIds workers)
--         --mapM_ (freeStablePtr . fst) (workerThreadIds workers)
--        if isAsyncException exc
--          then do
--            tid <- myThreadId
--            putStrLn $ "Thread killed by another: " ++ show tid
--            return $ toException $ WorkerDied exc
--          else return exc)
--     (\workers -> do
--        let numWorkers = length $ workerIds workers
--        let isGlobalScheduler = False
--        scheduler <-
--          atomically $ do
--            resultsQueue <- newTQueue
--            jobsSubmittedVar <- newTVar 0
--            jobsFinishedVar <- newTVar 0
--            retiredVar <- newTVar False
--            return $ Scheduler {..}
--        _ <- submitAction scheduler
--        res <- collectJobResults scheduler collector acc
--        atomically $
--          loopM_ 0 (< numWorkers) (+ 1) $ \ !_ ->
--            writeTQueue (workersJobQueue workers) Retire
--        return res)
--   -- mask_ $ do
--   --   isGlobalScheduler <- getIsGlobalScheduler
--   --   workers <- getWorkers isGlobalScheduler
--   --   res <-
--   --     onException
--   --       (do let numWorkers = length $ workerIds workers
--   --           scheduler <-
--   --                atomically $ do
--   --                  resultsQueue <- newTQueue
--   --                  jobsSubmittedVar <- newTVar 0
--   --                  jobsFinishedVar <- newTVar 0
--   --                  retiredVar <- newTVar False
--   --                  return $ Scheduler {..}
--   --           _ <- submitAction scheduler
--   --           collectJobResults scheduler collector acc)
--   --       (do mapM_ cancel (workerThreadIds workers))
--   --          -- kill all workers. Recreate the workers only if killed ones were the
--   --          -- global ones.
--   --           -- when isGlobalScheduler $ do
--   --           --   globalWorkers <- hireWorkers []
--   --           --   atomically $ do
--   --           --     writeTVar globalWorkersVar globalWorkers
--   --           --     writeTVar hasGlobalSchedulerVar False)
--   --   -- atomically $
--   --   --   if isGlobalScheduler
--   --   --     then writeTVar hasGlobalSchedulerVar False
--   --   --     else mapM_
--   --   --            (\_ -> writeTQueue (workersJobQueue workers) Retire)
--   --   --            (workerThreadIds workers)
--   --   return res
--   -- isGlobalScheduler <- getIsGlobalScheduler
--   -- workers <- getWorkers isGlobalScheduler
--   -- let numWorkers = length $ workerIds workers
--   -- scheduler <-
--   --    atomically $ do
--   --                  resultsQueue <- newTQueue
--   --                  jobsSubmittedVar <- newTVar 0
--   --                  jobsFinishedVar <- newTVar 0
--   --                  retiredVar <- newTVar False
--   --                  return $ Scheduler {..}
--   -- _ <- submitAction scheduler
--   -- collectJobResults scheduler collector acc
--   -- atomically $
--   --     if isGlobalScheduler
--   --       then writeTVar hasGlobalSchedulerVar False
--   --       else mapM_
--   --              (\_ -> writeTQueue (workersJobQueue workers) Retire)
--   --              (workerThreadIds workers)
--   -- return res

-- withScheduler_ :: [Int] -> (Scheduler a -> IO ()) -> IO ()
-- withScheduler_ wIds = withScheduler wIds (const id) ()



-- -- | Block current thread and wait for all `JobRequest`s to get processed. Use a
-- -- supplied function to collect all of the results produced by submitted
-- -- jobs. If a job throws an exception, the whole scheduler is retired, all
-- -- jobs are immediately cancelled and the exception is re-thrown in the main
-- -- thread. Same thing happens if a worker dies because of an asynchronous
-- -- exception, but with a `WorkerException` being thrown in a main
-- -- thread. `Scheduler` is also retired as soon as all of the results are
-- -- collected, after that it can not be used again, thus doing so will result in
-- -- a `SchedulerRetired` exception.
-- collectJobResults :: Scheduler a -> (JobResult a -> b -> b) -> b -> IO b
-- collectJobResults (Scheduler {..}) f !initAcc = do
--   jobsSubmitted <- atomically $ readTVar jobsSubmittedVar
--   if jobsSubmitted == 0
--     then return initAcc
--     else collect initAcc
--   where
--     collect !acc = do
--       (res, stop) <-
--         atomically $ do
--           res <-
--             orElse
--               (readTQueue resultsQueue)
--               (readTMVar (workersException workers) >>= throwSTM)
--           jobsSubmitted <- readTVar jobsSubmittedVar
--           jobsFinished <- readTVar jobsFinishedVar
--           return (res, jobsSubmitted == jobsFinished)
--       if stop
--         then return $! f res acc
--         else collect $! f res acc



-- -- | Clear out outstanding jobs in the queue
-- clearJobQueue :: Scheduler a -> STM ()
-- clearJobQueue scheduler@(Scheduler {..}) = do
--   mJob <- tryReadTQueue (workersJobQueue workers)
--   case mJob of
--     Just _ -> do
--       modifyTVar' jobsFinishedVar (+ 1)
--       clearJobQueue scheduler
--     Nothing -> return ()


-- -- | Submit a `JobRequest`, which will get executed as soon as there is an
-- -- available worker. Thows `SchedulerRetired`
-- submitRequest :: Scheduler a -> JobRequest a -> IO ()
-- submitRequest Scheduler {..} JobRequest {..} = do
--   atomically $ do
--     -- isRetired <- readTVar retiredVar
--     -- when isRetired $ throwSTM SchedulerRetired
--     jId <- readTVar jobsSubmittedVar
--     writeTVar jobsSubmittedVar (jId + 1)
--     writeTQueue (workersJobQueue workers) $
--       Job $ \_wid -> do
--         result <- jobRequestAction
--         atomically $ do
--           modifyTVar' jobsFinishedVar (+ 1)
--           writeTQueue resultsQueue (JobResult jId result)


-- -- | Block current thread and wait for all `JobRequest`s to get processed. Use a
-- -- supplied function to collect all of the results produced by submitted
-- -- jobs. If a job throws an exception, the whole scheduler is retired, all
-- -- jobs are immediately cancelled and the exception is re-thrown in the main
-- -- thread. Same thing happens if a worker dies because of an asynchronous
-- -- exception, but with a `WorkerException` being thrown in a main
-- -- thread. `Scheduler` is also retired as soon as all of the results are
-- -- collected, after that it can not be used again, thus doing so will result in
-- -- a `SchedulerRetired` exception.
-- collectResults :: Scheduler a -> (JobResult a -> b -> b) -> b -> IO b
-- collectResults scheduler@(Scheduler {..}) f !initAcc = do
--   jobsSubmitted <-
--     atomically $ do
--       isRetired <- readTVar retiredVar
--       when isRetired $ throwSTM SchedulerRetired
--       readTVar jobsSubmittedVar
--   if jobsSubmitted == 0
--     then return initAcc
--     else collect initAcc
--   where
--     collect !acc = do
--       eResStop <-
--         atomically $ do
--           eRes <-
--             (Right <$> readTQueue resultsQueue) <|>
--             (Left <$> readTMVar (workersException workers))
--           case eRes of
--             Right res -> do
--               resEmpty <- isEmptyTQueue resultsQueue
--               if resEmpty
--                 then do
--                   jobsSubmitted <- readTVar jobsSubmittedVar
--                   jobsFinished <- readTVar jobsFinishedVar
--                   let stop = jobsSubmitted == jobsFinished
--                   when stop $ do
--                     writeTVar retiredVar True
--                     if isGlobalScheduler
--                       then writeTVar hasGlobalSchedulerVar False
--                       else loopM_ 0 (< numWorkers) (+ 1) $ \ !_ ->
--                              writeTQueue (workersJobQueue workers) Retire
--                   return $ Right (res, stop)
--                 else return $ Right (res, False)
--             Left exc -> do
--               writeTVar retiredVar True
--               clearJobQueue scheduler
--               return $ Left exc
--       case eResStop of
--         Right (res, stop) ->
--           if stop
--             then return $! f res acc
--             else collect $! f res acc
--         Left exc -> do
--           mapM_ (cancel . snd) (workerThreadIds workers)
--           -- kill all workers. Recreate the workers only if killed ones were the
--           -- global ones.
--           when isGlobalScheduler $ do
--             globalWorkers <- hireWorkers []
--             atomically $ do
--               writeTVar hasGlobalSchedulerVar False
--               writeTVar globalWorkersVar globalWorkers
--           -- We don't want to re-throw async exceptions in the main thread, so
--           -- we have to wrap them in a `WorkerDied` exception.
--           throwIO $
--             if isAsyncException exc
--               then toException $ WorkerDied exc
--               else exc


-- -- | Block current thread and wait for the `Scheduler` to process all submitted
-- -- `JobRequest`s. It is a call to `collectResults`, which discards the results,
-- -- so all specifics apply here as well.
-- waitTillDone :: Scheduler a -> IO ()
-- waitTillDone scheduler = collectResults scheduler (const id) ()


-- splitWork :: Index ix
--           => [Int] -> ix -> (Scheduler a -> Int -> Int -> Int -> IO b) -> IO [JobResult a]
-- splitWork = divideWork
-- -- splitWork wIds sz submitWork
-- --   | totalElem sz == 0 = return []
-- --   | otherwise = do
-- --     scheduler <- makeScheduler wIds
-- --     let !totalLength = totalElem sz
-- --         !chunkLength = totalLength `quot` numWorkers scheduler
-- --         !slackStart = chunkLength * numWorkers scheduler
-- --     void $ submitWork scheduler chunkLength totalLength slackStart
-- --     collectResults scheduler (:) []


-- splitWork_ :: Index ix
--            => [Int] -> ix -> (Scheduler a -> Int -> Int -> Int -> IO b) -> IO ()
-- splitWork_ wIds sz = void . splitWork wIds sz

-- hasGlobalSchedulerVar :: TVar Bool
-- hasGlobalSchedulerVar = unsafePerformIO $ newTVarIO False
-- {-# NOINLINE hasGlobalSchedulerVar #-}


-- globalWorkersVar :: TVar Workers
-- globalWorkersVar = unsafePerformIO $ hireWorkers [] >>= newTVarIO
-- {-# NOINLINE globalWorkersVar #-}


-- hireWorkers :: [Int] -> IO Workers
-- hireWorkers wIds = do
--   workerIds <-
--     if null wIds
--       then do
--         wNum <- getNumCapabilities
--         return [0 .. wNum-1]
--       else return wIds
--   workersJobQueue <- newTQueueIO
--   workersException <- newEmptyTMVarIO
--   workerThreadIds <- startWorkers workersException workersJobQueue workerIds
--   return Workers {..}


-- runWorker :: TQueue Job -> Int -> IO ()
-- runWorker jQueue wid = do
--   job <- atomically $ tryReadTQueue jQueue
--   case job of
--     Just (Job action) -> do
--       action wid
--       runWorker jQueue wid
--     Just Retire -> return ()
--     Nothing -> threadDelay 1000 >> runWorker jQueue wid


-- startWorkers :: TMVar SomeException -> TQueue Job -> [Int] -> IO [(StablePtr ThreadId, Async ())]
-- startWorkers wExcMVar jQueue wIds = do
--   tid <- myThreadId
--   mapM
--     (\wId -> do
--        a <-
--          asyncOn wId $
--          withException
--            (runWorker jQueue wId)
--            (\exc -> do
--               suc <- atomically $ tryPutTMVar wExcMVar exc
--               when suc $ putStrLn $ "Will attempt to kill: " ++ show tid)
--        --s <- newStablePtr $ asyncThreadId a
--        return (undefined, a))
--     wIds
--   -- a <- asyncOn wId1 $ runWorker jQueue wId1
--   -- link a
--   -- foldM (\acc@(prevA:_) wId -> do
--   --           curA <- asyncOn wId $ runWorker jQueue wId
--   --           link2 prevA curA
--   --           return (curA:acc)
--   --       ) [a] wIds
--   -- mapM
--   --   (\ !wId -> do
--        -- mask_ $
--        -- asyncOnWithUnmask wId $ \unmask -> do
--        --   withException
--        --     (unmask $ runWorker jQueue wId)
--        --     (\exc -> atomically (tryPutTMVar wExcMVar exc)))
--        -- uninterruptibleMask_ $
--        -- asyncOnWithUnmask wId $ \unmask ->
--        --   withException
--        --     (unmask $ runWorker jQueue wId)
--        --     (atomically . tryPutTMVar wExcMVar))
--          -- asyncOn wId $
--          -- withException
--          --   (runWorker jQueue wId)
--          --   (atomically . tryPutTMVar wExcMVar))
--        -- uninterruptibleMask_ $
--        -- asyncOnWithUnmask wId $ \unmask ->
--        --   catchAsync (unmask (runWorker jQueue wId)) $ \exc -> do
--        --     _ <- atomically (tryPutTMVar wExcMVar exc)
--        --     throwIO exc)



-- bracketOnError' :: forall m a c. (C.MonadMask m)
--                => m a -> (SomeException -> a -> m SomeException) -> (a -> m c) -> m c
-- bracketOnError' before after thing = C.mask $ \restore -> do
--     x <- before
--     res1 <- C.try $ restore (thing x)
--     case res1 of
--         Left (e1 :: SomeException) -> do
--             -- ignore the exception, see bracket for explanation
--             eRes :: Either SomeException SomeException <-
--                 C.try $ C.uninterruptibleMask_ $ after e1 x
--             C.throwM $ either (const e1) id eRes
--         Right y -> return y
