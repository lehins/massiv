{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Array.Massiv.SchedulerSpec (spec) where

import           Control.Exception.Base        (ArithException (DivideByZero),
                                                catchJust)
import           Data.Array.Massiv             as M
import           Data.Array.Massiv.CommonSpec  (ArrIx (..))
import           Data.Array.Massiv.DelayedSpec ()
import           Data.Array.Massiv.Scheduler
import           Data.List                     (sortBy)
import           Prelude                       as P
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

prop_CatchDivideByZero :: ArrIx D DIM2 Int -> Property
prop_CatchDivideByZero (ArrIx arr ix) =
  monadicIO $
  run $
  catchJust
    (\exc ->
       case exc of
         DivideByZero -> Just True
         _            -> Nothing)
    ((computeUnboxedP $
      M.imap
        (\ix' x ->
           if ix == ix'
             then x `div` 0
             else x)
        arr) `seq`
     return True)
    return


prop_CatchNested :: ArrIx D DIM1 (Array D DIM1 Int) -> Property
prop_CatchNested (ArrIx arr ix) =
  monadicIO $
  run $
  catchJust
    (\exc ->
       case exc of
         DivideByZero -> Just True
         _            -> Nothing)
    ((M.sumP $
      (M.map M.sumP) $
      (M.imap
         (\ix' iarr ->
            if ix == ix'
              then M.map (`div` 0) iarr
              else iarr))
        arr) `seq`
     return True)
    return


exc_SchedulerRetiredSubmit :: IO Bool
exc_SchedulerRetiredSubmit = do
  scheduler <- makeScheduler
  submitRequest scheduler (JobRequest 1 (return True))
  _ <- collectResults scheduler (\jRes acc -> acc && jobResult jRes) True
  submitRequest scheduler (JobRequest 2 (return False))
  return False

exc_SchedulerRetiredCollect :: IO Bool
exc_SchedulerRetiredCollect = do
  scheduler <- makeScheduler
  submitRequest scheduler (JobRequest 1 (return True))
  resTrue <- collectResults scheduler (\jRes acc -> acc && jobResult jRes) True
  collectResults scheduler (\jRes acc -> acc && jobResult jRes) resTrue


-- | Check weather `jobRequestId` matches `jobResultId` for all jobs
prop_SchedulerRequestIdMatch :: [Int] -> Property
prop_SchedulerRequestIdMatch ids =
  monadicIO $
  (run $ do
     scheduler <- makeScheduler
     P.mapM_ (\jId -> submitRequest scheduler (JobRequest jId (return jId))) ids
     collectResults
       scheduler
       (\jRes acc -> acc && jobResult jRes == jobResultId jRes)
       True) >>=
  assert

-- | Check weather all jobs have been completed
prop_SchedulerAllJobsProcessed :: [Int] -> Property
prop_SchedulerAllJobsProcessed ids =
  monadicIO $ do
    res <-
      run $ do
        scheduler <- makeScheduler
        P.mapM_
          (\jId -> submitRequest scheduler (JobRequest jId (return jId)))
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
