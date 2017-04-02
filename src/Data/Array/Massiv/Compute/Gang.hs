-- |
-- Module      : Data.Array.Massiv.Compute.Gang
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Compute.Gang (
  theGang
  , Gang, forkGang, gangSize, gangIO, gangST
  )
where
import           Control.Concurrent.MVar
import           Control.Exception       (assert)
import           Control.Monad
import           GHC.Conc                (forkOn)
import           GHC.Conc                (numCapabilities)
import           GHC.IO
import           GHC.ST
import           System.IO


-- TheGang --------------------------------------------------------------------
-- | This globally shared gang is auto-initialised at startup and shared by all
--   Repa computations.
--
--   In a data parallel setting, it does not help to have multiple gangs running
--   at the same time. This is because a single data parallel computation should
--   already be able to keep all threads busy. If we had multiple gangs running
--   at the same time, then the system as a whole would run slower as the gangs
--   would contend for cache and thrash the scheduler.
--
--   If, due to laziness or otherwise, you try to start multiple parallel
--   Repa computations at the same time, then you will get the following
--   warning on stderr at runtime:
--
-- @Data.Array.Repa: Performing nested parallel computation sequentially.
--    You've probably called the 'compute' or 'copy' function while another
--    instance was already running. This can happen if the second version
--    was suspended due to lazy evaluation. Use 'deepSeqArray' to ensure that
--    each array is fully evaluated before you 'compute' the next one.
-- @
--
theGang :: Gang
theGang =
  unsafePerformIO $ do
    let caps = numCapabilities
    forkGang caps
{-# NOINLINE theGang #-}


-- Requests -------------------------------------------------------------------
-- | The 'Req' type encapsulates work requests for individual members of a gang.
data Req
        -- | Instruct the worker to run the given action.
        = ReqDo        (Int -> IO ())

        -- | Tell the worker that we're shutting the gang down.
        --   The worker should signal that it's receieved the request by
        --   writing to its result var before returning to the caller (forkGang).
        | ReqShutdown


-- Gang -----------------------------------------------------------------------
-- | A 'Gang' is a group of threads that execute arbitrary work requests.
data Gang
        = Gang
        { -- | Number of threads in the gang.
          _gangThreads     :: !Int

          -- | Workers listen for requests on these vars.
        , _gangRequestVars :: [MVar Req]

          -- | Workers put their results in these vars.
        , _gangResultVars  :: [MVar ()]

          -- | Indicates that the gang is busy.
        , _gangBusy        :: MVar Bool
        }

instance Show Gang where
  showsPrec p (Gang n _ _ _)
        = showString "<<"
        . showsPrec p n
        . showString " threads>>"


-- | O(1). Yield the number of threads in the 'Gang'.
gangSize :: Gang -> Int
gangSize (Gang n _ _ _)
        = n


-- | Fork a 'Gang' with the given number of threads (at least 1).
forkGang :: Int -> IO Gang
forkGang n =
  assert (n > 0) $ do
    mvsRequest <- sequence $ replicate n $ newEmptyMVar
        -- Create the vars we'll use to signal that threads are done.
    mvsDone <- sequence $ replicate n $ newEmptyMVar
        -- Add finalisers so we can shut the workers down cleanly if they
        -- become unreachable.
    zipWithM_
      (\varReq varDone -> mkWeakMVar varReq (finaliseWorker varReq varDone))
      mvsRequest
      mvsDone
        -- Create all the worker threads
    zipWithM_ forkOn [0 ..] $
      zipWith3 gangWorker [0 .. n - 1] mvsRequest mvsDone
        -- The gang is currently idle.
    busy <- newMVar False
    return $ Gang n mvsRequest mvsDone busy



-- | The worker thread of a 'Gang'.
--   The threads blocks on the MVar waiting for a work request.
gangWorker :: Int -> MVar Req -> MVar () -> IO ()
gangWorker threadId varRequest varDone = do
  req <- takeMVar varRequest
  case req of
    ReqDo action
                -- Run the action we were given.
     -> do
      action threadId
                -- Signal that the action is complete.
      putMVar varDone ()
                -- Wait for more requests.
      gangWorker threadId varRequest varDone
    ReqShutdown -> putMVar varDone ()


-- | Finaliser for worker threads.
--   We want to shutdown the corresponding thread when it's MVar becomes
--   unreachable.
--   Without this Repa programs can complain about "Blocked indefinitely
--   on an MVar" because worker threads are still blocked on the request
--   MVars when the program ends. Whether the finalizer is called or not
--   is very racey. It happens about 1 in 10 runs when for the
--   repa-edgedetect benchmark, and less often with the others.
--
--   We're relying on the comment in System.Mem.Weak that says
--    "If there are no other threads to run, the runtime system will
--     check for runnablefinalizers before declaring the system to be
--     deadlocked."
--
--   If we were creating and destroying the gang cleanly we wouldn't need
--     this, but theGang is created with a top-level unsafePerformIO.
--     Hacks beget hacks beget hacks...
--
finaliseWorker :: MVar Req -> MVar () -> IO ()
finaliseWorker varReq varDone = do
  putMVar varReq ReqShutdown
  takeMVar varDone
  return ()


-- | Issue work requests for the 'Gang' and wait until they complete.
--
--   If the gang is already busy then print a warning to `stderr` and just
--   run the actions sequentially in the requesting thread.
gangIO  :: Gang
        -> (Int -> IO ())
        -> IO ()
gangIO gang@(Gang _ _ _ busy) action = do
  b <- swapMVar busy True
  if b
    then do
      seqIO gang action
    else do
      parIO gang action
      _ <- swapMVar busy False
      return ()
{-# NOINLINE gangIO #-}


-- | Run an action on the gang sequentially.
seqIO   :: Gang -> (Int -> IO ()) -> IO ()
seqIO (Gang n _ _ _) action = do
  hPutStr stderr $
    unlines
      [ "Data.Array.Repa: Performing nested parallel computation sequentially."
      , "  You've probably called the 'compute' or 'copy' function while another"
      , "  instance was already running. This can happen if the second version"
      , "  was suspended due to lazy evaluation. Use 'deepSeqArray' to ensure"
      , "  that each array is fully evaluated before you 'compute' the next one."
      , ""
      ]
  mapM_ action [0 .. n - 1]

-- | Run an action on the gang in parallel.
parIO   :: Gang -> (Int -> IO ()) -> IO ()
parIO (Gang _ mvsRequest mvsResult _) action = do
  mapM_ (\v -> putMVar v (ReqDo action)) mvsRequest
        -- Wait for all the requests to complete.
  mapM_ takeMVar mvsResult


-- | Same as 'gangIO' but in the 'ST' monad.
gangST :: Gang -> (Int -> ST s ()) -> ST s ()
gangST g p = unsafeIOToST . gangIO g $ unsafeSTToIO . p


