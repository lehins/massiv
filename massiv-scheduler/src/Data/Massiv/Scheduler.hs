{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Massiv.Scheduler
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Scheduler
  ( -- * Scheduler and strategies
    Scheduler
  , Comp(..)
  , pattern Par
  , numWorkers
  -- * Initialize Scheduler
  , withScheduler
  , withScheduler_
  -- * Schedule work
  , scheduleWork
  , scheduleWork_
  , fromWorkerAsyncException
  -- * Helper functions
  , traverse_
  , mapConcurrently
  ) where

import           Control.Exception
import           Control.Concurrent
import           Control.Monad
import           Data.Atomics                      (atomicModifyIORefCAS,
                                                    atomicModifyIORefCAS_)
import           Data.Foldable                     as F (foldl')
import           Data.IORef
import           Data.Massiv.Scheduler.Computation
import           Data.Massiv.Scheduler.Queue
import           Control.Monad.IO.Unlift


-- | Main type for scheduling work. See `withScheduler` for the only way to get access to such data
-- type.
--
-- @since 0.1.0
data Scheduler m a = Scheduler
  { sNumWorkers        :: {-# UNPACK #-} !Int
  , sWorkersCounterRef :: !(IORef Int)
  , sJQueue            :: !(JQueue m a)
  , sJobsCountRef      :: !(IORef Int)
  }
  -- TODO: Make Scheduler a sum type with: ?
                       -- ParScheduler
                       -- SeqScheduler
  --                     sSeqResRef :: !(Maybe (IORef [a]))

-- | Get number of workers from the scheduler
--
-- @since 0.1.0
numWorkers :: Scheduler m a -> Int
numWorkers = sNumWorkers


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

-- | Schedule an action to be picked up and computed by a worker from a pool. See `withScheduler` to
-- initialize a scheduler. Use `scheduleWork_` if you do not intend to keep the result of the
-- computation, as it will result in faster code.
--
-- @since 0.1.0
scheduleWork :: MonadIO m => Scheduler m a -> m a -> m ()
scheduleWork = scheduleWorkInternal mkJob

-- | Similarly to `scheduleWork`, but ignores the result of computation, thus having less overhead.
--
-- @since 0.1.0
scheduleWork_ :: MonadIO m => Scheduler m a -> m b -> m ()
scheduleWork_ = scheduleWorkInternal (return . Job_ . void)

scheduleWorkInternal :: MonadIO m => (m b -> m (Job m a)) -> Scheduler m a -> m b -> m ()
scheduleWorkInternal mkJob' Scheduler {sJQueue, sJobsCountRef, sNumWorkers} action = do
  liftIO $ atomicModifyIORefCAS_ sJobsCountRef (+ 1)
  job <-
    mkJob' $ do
      res <- action
      res `seq` dropCounterOnZero sJobsCountRef $ retireWorkersN sJQueue sNumWorkers
      return res
  pushJQueue sJQueue job

-- | Helper function to place `Retire` instructions on the job queue.
retireWorkersN :: MonadIO m => JQueue m a -> Int -> m ()
retireWorkersN sJQueue n = traverse_ (pushJQueue sJQueue) $ replicate n Retire

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
withScheduler comp submitWork = do
  sNumWorkers <-
    case comp of
      Seq -> return 1
      Par -> liftIO getNumCapabilities
      ParOn ws -> return $ length ws
      ParN 0 -> liftIO getNumCapabilities
      ParN n -> return $ fromIntegral n
  sWorkersCounterRef <- liftIO $ newIORef sNumWorkers
  sJQueue <- newJQueue
  sJobsCountRef <- liftIO $ newIORef 0
  workDoneMVar <- liftIO newEmptyMVar
  let scheduler = Scheduler {..}
      onRetire = dropCounterOnZero sWorkersCounterRef $ liftIO (putMVar workDoneMVar Nothing)
  -- / Wait for the initial jobs to get scheduled before spawining off the workers, otherwise it would
  -- be trickier to identify the beginning and the end of a job pool.
  _ <- submitWork scheduler
  -- / Ensure at least something gets scheduled, so retirement can be triggered
  jc <- liftIO $ readIORef sJobsCountRef
  when (jc == 0) $ scheduleWork_ scheduler (pure ())
  let spawnWorkersWith fork ws =
        forM ws $ \w ->
          withRunInIO $ \run ->
            mask_ $
            fork w $ \unmask ->
              catch
                (unmask $ run $ runWorker sJQueue onRetire)
                (unmask . run . handleWorkerException sJQueue workDoneMVar sNumWorkers)
      {-# INLINE spawnWorkersWith #-}
  tids <-
    case comp of
      Seq -- / no need to fork threads for a sequential computation
       -> runWorker sJQueue onRetire >> return []
      Par -> spawnWorkersWith forkOnWithUnmask [1 .. sNumWorkers]
      ParOn ws -> spawnWorkersWith forkOnWithUnmask ws
      ParN _ -> spawnWorkersWith (\_ -> forkIOWithUnmask) [1 .. sNumWorkers]
  -- / wait for all worker to finish. If any one of them had a problem this MVar will contain an
  -- exception
  mExc <- liftIO $ readMVar workDoneMVar
  case mExc of
    Nothing
     -- / Now we are sure all workers have done their job we can safely read all of the IORefs eith
     -- results
     -> flushResults sJQueue
    Just exc -- / Here we need to unwrap the legit worker exception and rethrow it, so the main thread
             -- will think like it's his own
      | Just (WorkerException wexc) <- fromException exc ->
        liftIO $ do
          traverse_ (`throwTo` WorkerTerminateException) tids
          throwIO wexc
    Just exc -> liftIO $ throwIO exc -- Somethig funky is happening, propagate it.

-- | Same as `withScheduler`, but discards results of submitted jobs. Make sure to use
-- `scheduleWork_` to get optimal performance.
--
-- @since 0.1.0
withScheduler_ ::
     MonadUnliftIO m
  => Comp -- ^ Computation strategy
  -> (Scheduler m a -> m b)
              -- ^ Action that will be scheduling all the work.
  -> m ()
withScheduler_ comp = void . withScheduler comp


-- | Specialized exception handler for the work scheduler.
handleWorkerException ::
  MonadIO m => JQueue m a -> MVar (Maybe SomeException) -> Int -> SomeException -> m ()
handleWorkerException jQueue workDoneMVar nWorkers exc =
  case fromException exc of
    Just wexc | WorkerTerminateException <- wexc -> return ()
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
-- >>> didAWorkerDie $ withScheduler Par $ \ s -> scheduleWork s $ pure False
-- False
-- >>> didAWorkerDie $ withScheduler Par $ \ s -> scheduleWork s $ myThreadId >>= killThread >> pure False
-- True
--
-- @since 0.1.0
fromWorkerAsyncException :: Exception e => SomeException -> Maybe e
fromWorkerAsyncException exc =
  case fromException exc of
    Just (WorkerAsyncException asyncExc) -> asyncExceptionFromException (toException asyncExc)
    _                                    -> Nothing
