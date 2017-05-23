{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Array.Massiv.SchedulerSpec (spec) where

import           Control.Exception.Base        (ArithException (DivideByZero))
import           Data.Array.Massiv             as M
import           Data.Array.Massiv.CommonSpec  (ArrIx (..), assertException)
import           Data.Array.Massiv.DelayedSpec ()
import           Data.Array.Massiv.Scheduler
import           Data.List                     (sortBy)
import           Prelude                       as P
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic




prop_CatchDivideByZero :: ArrIx D DIM2 Int -> Property
prop_CatchDivideByZero (ArrIx arr ix) =
  assertException
    (== DivideByZero)
    (computeUnboxedP $
     M.imap
       (\ix' x ->
          if ix == ix'
            then x `div` 0
            else x)
       arr)


prop_CatchNested :: ArrIx D DIM1 (ArrIx D DIM1 Int) -> Property
prop_CatchNested (ArrIx arr ix) =
  assertException
    (== DivideByZero)
    (M.sumP $
     M.map M.sumP $
     M.imap
       (\ix' (ArrIx iarr ixi) ->
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
prop_SchedulerRequestIdMatch :: Positive Int -> Property
prop_SchedulerRequestIdMatch (Positive numJobs) =
  monadicIO $
  (run $ do
     scheduler <- makeScheduler []
     P.mapM_ (\jId -> submitRequest scheduler (JobRequest (return jId))) [0..numJobs]
     collectResults
       scheduler
       (\jRes acc -> acc && jobResult jRes == jobResultId jRes)
       True) >>=
  assert

-- | Check weather all jobs have been completed
prop_SchedulerAllJobsProcessed :: OrderedList Int -> Property
prop_SchedulerAllJobsProcessed (Ordered ids) =
  monadicIO $ do
    res <-
      run $ do
        scheduler <- makeScheduler []
        P.mapM_
          (\jId -> submitRequest scheduler (JobRequest (return jId)))
          ids
        collectResults scheduler (\jRes acc -> jobResult jRes : acc) []
    assert (sortBy compare ids == sortBy compare res)


spec :: Spec
spec = do
  describe "Exceptions" $ do
    it "CatchDivideByZero" $ property prop_CatchDivideByZero
    it "CatchNested" $ property prop_CatchNested
    it "SchedulerRetiredSubmit" $
      exc_SchedulerRetiredSubmit `shouldThrow` (== SchedulerRetired)
    it "SchedulerRetiredCollect" $
      exc_SchedulerRetiredCollect `shouldThrow` (== SchedulerRetired)
  describe "Properties" $ do
    it "SchedulerRequestIdMatch" $ property prop_SchedulerRequestIdMatch
    it "SchedulerAllJobsProcessed" $ property prop_SchedulerAllJobsProcessed
