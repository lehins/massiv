{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Array.Massiv.SchedulerSpec (spec) where

import           Control.Concurrent
import           Control.Exception.Base        (ArithException (DivideByZero),
                                                AsyncException (ThreadKilled))
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.CommonSpec  (ArrIxP (..), assertException,
                                                assertExceptionIO)
import           Data.Array.Massiv.Delayed
import           Data.Array.Massiv.DelayedSpec ()
import           Data.Array.Massiv.Manifest
import           Data.Array.Massiv.Mutable
import           Data.Array.Massiv.Ops         as M
import           Data.Array.Massiv.Scheduler
import           Data.List                     (sortBy)
import           Prelude                       as P
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic


prop_CatchDivideByZero :: ArrIxP D Ix2 Int -> Property
prop_CatchDivideByZero (ArrIxP arr ix) =
  assertException
    (== DivideByZero)
    (computeAs U $
     M.imap
       (\ix' x ->
          if ix == ix'
            then x `div` 0
            else x)
       arr)


prop_CatchNested :: ArrIxP D Ix1 (ArrIxP D Ix1 Int) -> Property
prop_CatchNested (ArrIxP arr ix) =
  assertException
    (== DivideByZero)
    (-- M.sum $
     computeAs U $
     M.map M.sum $
     M.imap
       (\ix' (ArrIxP iarr ixi) ->
          if ix == ix'
            then M.imap
                   (\ixi' e ->
                      if ixi == ixi'
                        then e `div` 0
                        else e)
                   iarr
            else iarr)
       arr)

exc_SchedulerRetiredSubmit :: IO ()
exc_SchedulerRetiredSubmit = do
  scheduler <- makeScheduler []
  submitRequest scheduler (JobRequest (return True))
  _ <- collectResults scheduler (\jRes acc -> acc && jobResult jRes) True
  submitRequest scheduler (JobRequest (return False))

exc_SchedulerRetiredCollect :: IO Bool
exc_SchedulerRetiredCollect = do
  scheduler <- makeScheduler []
  submitRequest scheduler (JobRequest (return True))
  resTrue <- collectResults scheduler (\jRes acc -> acc && jobResult jRes) True
  collectResults scheduler (\jRes acc -> acc && jobResult jRes) resTrue


-- | Check weather `jobRequestId` matches `jobResultId` for all jobs
prop_SchedulerRequestIdMatch :: [Int] -> Positive Int -> Property
prop_SchedulerRequestIdMatch wIds (Positive numJobs) =
  monadicIO $
  (run $ do
     scheduler <- makeScheduler wIds
     P.mapM_ (\jId -> submitRequest scheduler (JobRequest (return jId))) [0..numJobs]
     collectResults
       scheduler
       (\jRes acc -> acc && jobResult jRes == jobResultId jRes)
       True) >>=
  assert

-- | Check weather all jobs have been completed
prop_SchedulerAllJobsProcessed :: [Int] -> OrderedList Int -> Property
prop_SchedulerAllJobsProcessed wIds (Ordered ids) =
  monadicIO $ do
    res <-
      run $ do
        scheduler <- makeScheduler wIds
        P.mapM_
          (\jId -> submitRequest scheduler (JobRequest (return jId)))
          ids
        collectResults scheduler (\jRes acc -> jobResult jRes : acc) []
    assert (sortBy compare ids == sortBy compare res)


-- | Make sure there is no deadlock if all workers get killed
prop_AllWorkersDied :: [Int] -> Int -> [Int] -> Property
prop_AllWorkersDied wIds hId ids =
  assertExceptionIO
    (maybe False (== ThreadKilled) . fromWorkerException)
    (do scheduler <- makeScheduler wIds
        P.mapM_
          (\_ ->
             submitRequest scheduler (JobRequest $ myThreadId >>= killThread))
          (hId:ids)
        waitTillDone scheduler)

spec :: Spec
spec = do
  describe "Exceptions" $ do
    it "CatchDivideByZero" $ property prop_CatchDivideByZero
    it "CatchNested" $ property prop_CatchNested
    it "AllWorkersDied" $ property prop_AllWorkersDied
    it "SchedulerRetiredSubmit" $
      exc_SchedulerRetiredSubmit `shouldThrow` (== SchedulerRetired)
    it "SchedulerRetiredCollect" $
      exc_SchedulerRetiredCollect `shouldThrow` (== SchedulerRetired)
  describe "Properties" $ do
    it "SchedulerRequestIdMatch" $ property prop_SchedulerRequestIdMatch
    it "SchedulerAllJobsProcessed" $ property prop_SchedulerAllJobsProcessed
