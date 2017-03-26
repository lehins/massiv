{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Data.Array.Massiv.Manifest.Scheduler
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Manifest.Scheduler where

import           Control.Concurrent
import           System.IO.Unsafe   (unsafePerformIO)
-- import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM
-- import Control.Concurrent.STM.TBQueue -- (TBQueue, newTBQueueIO, writeTBQueue)


data Worker = Worker { workerId :: !Int } deriving Show


foo n = unsafePerformIO $ print "started" >> threadDelay n >> print "ended"
{-# NOINLINE foo #-}


numWorkers :: Int
numWorkers = unsafePerformIO getNumCapabilities
{-# NOINLINE numWorkers #-}

workers :: TBQueue Worker
workers =
  unsafePerformIO $ do
    numCap <- getNumCapabilities
    queue <- newTBQueueIO numCap
    -- atomically $ sequence_ $ map (writeTBQueue queue . Worker) [0 .. numCap - 1]
    return queue
{-# NOINLINE workers #-}



-- computeIO :: Int -- ^ Number of workers to engage. (should be in range from 1 to
--                  -- `getNumCapabilities`)
--           -> (Int -> IO a)
--           -> IO [(Int, a)]
-- computeIO n f = do
  
