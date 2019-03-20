{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Massiv.Core.SchedulerSpec (spec) where

import Control.Concurrent
import Control.Exception.Base (ArithException(DivideByZero),
                               AsyncException(ThreadKilled), displayException)
import Control.Massiv.Scheduler
import Data.Massiv.CoreArbitrary as A
import Prelude as P


-- | Ensure proper exception handling.
prop_CatchDivideByZero :: ArrIx D Ix2 Int -> [Int] -> Property
prop_CatchDivideByZero (ArrIx arr ix) caps =
  assertException
    (== DivideByZero)
    (A.sum $
     A.imap
       (\ix' x ->
          if ix == ix'
            then x `div` 0
            else x)
       (setComp (ParOn caps) arr))

-- | Ensure proper exception handling in nested parallel computation
prop_CatchNested :: ArrIx D Ix1 (ArrIxP D Ix1 Int) -> [Int] -> Property
prop_CatchNested (ArrIx arr ix) caps =
  assertException
    (== DivideByZero)
    (computeAs U $
     A.map A.sum $
     A.imap
       (\ix' (ArrIxP iarr ixi) ->
          if ix == ix'
            then A.imap
                   (\ixi' e ->
                      if ixi == ixi'
                        then e `div` 0
                        else e)
                   iarr
            else iarr)
       (setComp (ParOn caps) arr))

-- | Make sure there is no deadlock if all workers get killed
prop_AllWorkersDied :: [Int] -> (Int, [Int]) -> Property
prop_AllWorkersDied wIds (hId, ids) =
  assertExceptionIO
    (\exc ->
       case fromWorkerAsyncException exc of
         Just ThreadKilled -> True
         _ -> error $ "Received unexpected exception: " ++ displayException exc)
    (withScheduler_ Par $ \scheduler1 ->
       scheduleWork
         scheduler1
         (withScheduler_ (ParOn wIds) $ \scheduler ->
            P.mapM_ (\_ -> scheduleWork scheduler (myThreadId >>= killThread)) (hId : ids)))


-- | Check weather all jobs have been completed and returned order is correct
prop_SchedulerAllJobsProcessed :: Comp -> OrderedList Int -> Property
prop_SchedulerAllJobsProcessed comp (Ordered jobs) =
  monadicIO
    ((=== jobs) <$>
          run (withScheduler comp $ \scheduler -> P.mapM_ (scheduleWork scheduler . return) jobs))


spec :: Spec
spec =
  describe "Exceptions" $ do
    it "CatchDivideByZero" $ property prop_CatchDivideByZero
    it "CatchNested" $ property prop_CatchNested
    it "AllWorkersDied" $ property prop_AllWorkersDied
    it "SchedulerAllJobsProcessed" $ property prop_SchedulerAllJobsProcessed
