{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Control.Massiv.Scheduler
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Control.Massiv.Scheduler
  ( -- * Scheduler and strategies
    Comp(..)
  , Scheduler(..)
  -- * Initialize Scheduler
  , withScheduler
  , withScheduler_
  -- * Helper functions
  , fromWorkerAsyncException
  , mapConcurrently
  , mapConcurrently_
  , traverse_
  ) where

import Control.Concurrent
import Control.Exception
import Control.Massiv.Scheduler.Computation
import Control.Massiv.Scheduler.Queue
import Control.Monad
import Control.Monad.IO.Unlift
import Data.Atomics (atomicModifyIORefCAS, atomicModifyIORefCAS_)
import Data.Foldable as F (foldl')
import Data.IORef

data Jobs m a = Jobs
  { jobsNumWorkers :: {-# UNPACK #-} !Int
  , jobsQueue      :: !(JQueue m a)
  , jobsCountRef   :: !(IORef Int)
  }
  -- TODO: Make Jobs a sum type with: ?
                       -- ParScheduler
                       -- SeqScheduler
  --                     sSeqResRef :: !(Maybe (IORef [a]))

-- | Main type for scheduling work. See `withScheduler` or `withScheduler_` for the only ways to get
-- access to such data type.
--
-- @since 0.1.0
data Scheduler m a = Scheduler
  { numWorkers :: {-# UNPACK #-} !Int
  -- ^ Get the number of workers.
  , scheduleWork :: m a -> m ()
  -- ^ Schedule an action to be picked up and computed by a worker from a pool.
  }

-- | This is generally a faster way to traverse while ignoring the result rather than using `mapM_`.
--
-- @since 0.1.0
traverse_ :: (Foldable t, Applicative f) => (a -> f ()) -> t a -> f ()
traverse_ f = F.foldl' (\c a -> c *> f a) (pure ())


-- | Map an action over each element of the `Foldable` @t@ acccording to the supplied computation
-- strategy.
--
-- @since 0.1.0
mapConcurrently :: (MonadUnliftIO m, Foldable t) => Comp -> (a -> m b) -> t a -> m [b]
mapConcurrently comp f xs = withScheduler comp $ \s -> traverse_ (scheduleWork s . f) xs

-- | Just like `mapConcurrently`, but discard the results of computation.
--
-- @since 0.1.0
mapConcurrently_ :: (MonadUnliftIO m, Foldable t) => Comp -> (a -> m b) -> t a -> m ()
mapConcurrently_ comp f xs = withScheduler_ comp $ \s -> traverse_ (scheduleWork s . f) xs

scheduleJobs :: MonadIO m => Jobs m a -> m a -> m ()
scheduleJobs = scheduleJobsWith mkJob

-- | Similarly to `scheduleWork`, but ignores the result of computation, thus having less overhead.
--
-- @since 0.1.0
scheduleJobs_ :: MonadIO m => Jobs m a -> m b -> m ()
scheduleJobs_ = scheduleJobsWith (return . Job_ . void)

scheduleJobsWith :: MonadIO m => (m b -> m (Job m a)) -> Jobs m a -> m b -> m ()
scheduleJobsWith mkJob' Jobs {jobsQueue, jobsCountRef, jobsNumWorkers} action = do
  liftIO $ atomicModifyIORefCAS_ jobsCountRef (+ 1)
  job <-
    mkJob' $ do
      res <- action
      res `seq` dropCounterOnZero jobsCountRef $ retireWorkersN jobsQueue jobsNumWorkers
      return res
  pushJQueue jobsQueue job

-- | Helper function to place `Retire` instructions on the job queue.
retireWorkersN :: MonadIO m => JQueue m a -> Int -> m ()
retireWorkersN jobsQueue n = traverse_ (pushJQueue jobsQueue) $ replicate n Retire

-- | Decrease a counter by one and perform an action when it drops down to zero.
dropCounterOnZero :: MonadIO m => IORef Int -> m () -> m ()
dropCounterOnZero counterRef onZero = do
  jc <-
    liftIO $ atomicModifyIORefCAS
      counterRef
      (\ !i' ->
         let !i = i' - 1
          in (i, i))
  when (jc == 0) onZero


-- | Runs the worker until the job queue is exhausted, at which point it will exhecute the final task
-- of retirement and return
runWorker :: MonadIO m =>
             JQueue m a
          -> m () -- ^ Action to run upon retirement
          -> m ()
runWorker jQueue onRetire = go
  where
    go =
      popJQueue jQueue >>= \case
        Just job -> job >> go
        Nothing -> onRetire


-- | Initialize a scheduler and submit jobs that will be computed sequentially or in parallelel,
-- which is determined by the `Comp`utation strategy.
--
-- Here are some cool properties about the `withScheduler`:
--
-- * This function will block until all of the submitted jobs have finished or at least one of them
--   resulted in an exception, which will be re-thrown here.
--
-- * It is totally fine for nested jobs to submit more jobs for the same scheduler
--
-- * It is ok to initialize new schedulers, although tht will likely result in suboptimal computation
--   time, unless they do not share the same capabilities.
--
-- * __Warning__ It is very dangerous to schedule jobs that do blocking `IO`, since it can lead to a
--   deadlock very quickly, if you are not careful. Consider this example. First execution works fine,
--   since there are two scheduled workers, and one can unblock another one, but the second scenario
--   immediately results in a deadlock.
--
-- >>> withScheduler (ParOn [1,2]) $ \s -> newEmptyMVar >>= (\ mv -> scheduleWork s (readMVar mv) >> scheduleWork s (putMVar mv ()))
-- [(),()]
-- >>> withScheduler (ParOn [1]) $ \s -> newEmptyMVar >>= (\ mv -> scheduleWork s (readMVar mv) >> scheduleWork s (putMVar mv ()))
-- Interrupted.
--
-- __Important__: In order to get work done truly in parallel, program needs to be compiled with
-- @-threaded@ GHC flag and executed with @+RTS -N -RTS@ to use all available cores.
--
-- @since 0.1.0
withScheduler ::
     MonadUnliftIO m
  => Comp -- ^ Computation strategy
  -> (Scheduler m a -> m b)
     -- ^ Action that will be scheduling all the work.
  -> m [a]
withScheduler comp = withSchedulerInternal comp scheduleJobs flushResults


-- | Same as `withScheduler`, but discards results of submitted jobs.
--
-- @since 0.1.0
withScheduler_ ::
     MonadUnliftIO m
  => Comp -- ^ Computation strategy
  -> (Scheduler m a -> m b)
     -- ^ Action that will be scheduling all the work.
  -> m ()
withScheduler_ comp = withSchedulerInternal comp scheduleJobs_ (const (pure ()))


withSchedulerInternal ::
     MonadUnliftIO m
  => Comp -- ^ Computation strategy
  -> (Jobs m a -> m a -> m ()) -- ^ How to schedule work
  -> (JQueue m a -> m c) -- ^ How to collect results
  -> (Scheduler m a -> m b)
     -- ^ Action that will be scheduling all the work.
  -> m c
withSchedulerInternal comp submitWork collect onScheduler = do
  jobsNumWorkers <-
    case comp of
      Seq -> return 1
      Par -> liftIO getNumCapabilities
      ParOn ws -> return $ length ws
      ParN 0 -> liftIO getNumCapabilities
      ParN n -> return $ fromIntegral n
  sWorkersCounterRef <- liftIO $ newIORef jobsNumWorkers
  jobsQueue <- newJQueue
  jobsCountRef <- liftIO $ newIORef 0
  workDoneMVar <- liftIO newEmptyMVar
  let jobs = Jobs {..}
      scheduler = Scheduler {numWorkers = jobsNumWorkers, scheduleWork = submitWork jobs}
      onRetire = dropCounterOnZero sWorkersCounterRef $ liftIO (putMVar workDoneMVar Nothing)
  -- / Wait for the initial jobs to get scheduled before spawining off the workers, otherwise it would
  -- be trickier to identify the beginning and the end of a job pool.
  _ <- onScheduler scheduler
  -- / Ensure at least something gets scheduled, so retirement can be triggered
  jc <- liftIO $ readIORef jobsCountRef
  when (jc == 0) $ scheduleJobs_ jobs (pure ())
  let spawnWorkersWith fork ws =
        forM ws $ \w ->
          withRunInIO $ \run ->
            mask_ $
            fork w $ \unmask ->
              catch
                (unmask $ run $ runWorker jobsQueue onRetire)
                (unmask . run . handleWorkerException jobsQueue workDoneMVar jobsNumWorkers)
      {-# INLINE spawnWorkersWith #-}
      spawnWorkers =
        case comp of
          Seq -> return []
            -- \ no need to fork threads for a sequential computation
          Par -> spawnWorkersWith forkOnWithUnmask [1 .. jobsNumWorkers]
          ParOn ws -> spawnWorkersWith forkOnWithUnmask ws
          ParN _ -> spawnWorkersWith (\_ -> forkIOWithUnmask) [1 .. jobsNumWorkers]
      {-# INLINE spawnWorkers #-}
      doWork = do
        when (comp == Seq) $ runWorker jobsQueue onRetire
        mExc <- liftIO $ readMVar workDoneMVar
        -- \ wait for all worker to finish. If any one of them had a problem this MVar will
        -- contain an exception
        case mExc of
          Nothing
             -- / Now we are sure all workers have done their job we can safely read all of the
             -- IORefs with results
           -> collect jobsQueue
          Just exc -- / Here we need to unwrap the legit worker exception and rethrow it, so the
                     -- main thread will think like it's his own
            | Just (WorkerException wexc) <- fromException exc -> liftIO $ throwIO wexc
          Just exc -> liftIO $ throwIO exc -- Somethig funky is happening, propagate it.
      {-# INLINE doWork #-}
  safeBracketOnError
    spawnWorkers
    (liftIO . traverse_ (`throwTo` WorkerTerminateException))
    (const doWork)


-- | Specialized exception handler for the work scheduler.
handleWorkerException ::
  MonadIO m => JQueue m a -> MVar (Maybe SomeException) -> Int -> SomeException -> m ()
handleWorkerException jQueue workDoneMVar nWorkers exc =
  case fromException exc of
    Just WorkerTerminateException -> return ()
      -- \ some co-worker died, we can just move on with our death.
    _ ->
      case fromException exc of
        Just asyncExc -> do
          liftIO $ putMVar workDoneMVar $ Just $ toException $ WorkerAsyncException asyncExc
          -- \ Let the main thread know about this terrible async exception.
          -- / Do the co-worker cleanup
          retireWorkersN jQueue (nWorkers - 1)
          liftIO $ throwIO asyncExc
          -- \ do not recover from an async exception
        Nothing -> do
          liftIO $ putMVar workDoneMVar $ Just $ toException $ WorkerException exc
          -- \ Main thread must know how we died
          -- / Do the co-worker cleanup
          retireWorkersN jQueue (nWorkers - 1)
          -- / As to one self, gracefully leave off into the outer world


-- | This exception should normally be not seen in the wild. The only one that could possibly pop up
-- is the `WorkerAsyncException`.
data WorkerException
  = WorkerException !SomeException
  -- ^ One of workers experienced an exception, main thread will receive the same `SomeException`.
  | WorkerAsyncException !SomeAsyncException
  -- ^ If a worker recieves an async exception, something is utterly wrong. This exception in
  -- particular would mean that one of the workers have died of an asyncronous exception. We don't
  -- want to raise that async exception synchronously in the main thread, therefore it will receive
  -- this wrapper exception instead.
  | WorkerTerminateException
  -- ^ When a brother worker dies of some exception, all the other ones will be terminated
  -- asynchronously with this one.
  deriving Show

instance Exception WorkerException where
  displayException workerExc =
    case workerExc of
      WorkerException exc ->
        "A worker handled a job that ended with exception: " ++ displayException exc
      WorkerAsyncException exc ->
        "A worker was killed with an async exception: " ++ displayException exc
      WorkerTerminateException -> "A worker was terminated by the scheduler"


-- | If any one of the workers dies with an async exception, it is possible to recover that
-- exception in the main thread with this function. This does not apply to `Seq`uential computation,
-- since no workers are created in such case and async exception will be received by the main thread
-- directly.
--
-- >>> let didAWorkerDie = handleJust fromWorkerAsyncException (return . (== ThreadKilled)) . fmap or
-- >>> :t didAWorkerDie
-- didAWorkerDie :: Foldable t => IO (t Bool) -> IO Bool
-- >>> didAWorkerDie $ withScheduler Par $ \ s -> scheduleWork s $ pure False
-- False
-- >>> didAWorkerDie $ withScheduler Par $ \ s -> scheduleWork s $ myThreadId >>= killThread >> pure False
-- True
-- >>> withScheduler Par $ \ s -> scheduleWork s $ myThreadId >>= killThread >> pure False
-- *** Exception: WorkerAsyncException thread killed
--
-- @since 0.1.0
fromWorkerAsyncException :: Exception e => SomeException -> Maybe e
fromWorkerAsyncException exc =
  case fromException exc of
    Just (WorkerAsyncException asyncExc) -> asyncExceptionFromException (toException asyncExc)
    _                                    -> Nothing


-- Copy from unliftio:
safeBracketOnError :: MonadUnliftIO m => m a -> (a -> m b) -> (a -> m c) -> m c
safeBracketOnError before after thing = withRunInIO $ \run -> mask $ \restore -> do
  x <- run before
  res1 <- try $ restore $ run $ thing x
  case res1 of
    Left (e1 :: SomeException) -> do
      -- ignore the exception, see bracket for explanation
      _ :: Either SomeException b <-
        try $ uninterruptibleMask_ $ run $ after x
      throwIO e1
    Right y -> return y
