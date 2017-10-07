{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Massiv.Core.SchedulerSpec (spec) where

import           Control.Concurrent
import           Control.Exception.Base        (ArithException (DivideByZero),
                                                AsyncException (ThreadKilled))
import           Data.Massiv.Array.Delayed
import           Data.Massiv.Array.Manifest
import           Data.Massiv.Array.Mutable
import           Data.Massiv.Array.Ops         as M
import           Data.Massiv.Core.Scheduler
import           Data.Massiv.CoreArbitrary
import           Prelude                       as P
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic


-- | Ensure proper exception handling.
prop_CatchDivideByZero :: ArrIx D Ix2 Int -> [Int] -> Property
prop_CatchDivideByZero (ArrIx arr ix) caps =
  assertException
    (== DivideByZero)
    (M.sum $
     M.imap
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
       (setComp (ParOn caps) arr))

-- | Make sure there is no deadlock if all workers get killed
prop_AllWorkersDied :: [Int] -> (Int, [Int]) -> Property
prop_AllWorkersDied wIds (hId, ids) =
  assertExceptionIO
    (== ThreadKilled)
    (withScheduler_ [] $ \scheduler1 ->
       scheduleWork
         scheduler1
         (withScheduler_ wIds $ \scheduler ->
            P.mapM_
              (\_ -> scheduleWork scheduler (myThreadId >>= killThread))
              (hId : ids)))


-- | Check weather all jobs have been completed and returned order is correct
prop_SchedulerAllJobsProcessed :: [Int] -> OrderedList Int -> Property
prop_SchedulerAllJobsProcessed wIds (Ordered jobs) =
  monadicIO $ do
    res <- (run $ withScheduler' wIds $ \scheduler ->
               P.mapM_ (scheduleWork scheduler . return) jobs)
    return (res === jobs)


spec :: Spec
spec = do
  describe "Exceptions" $ do
    it "CatchDivideByZero" $ property prop_CatchDivideByZero
    it "CatchNested" $ property prop_CatchNested
    it "AllWorkersDied" $ property prop_AllWorkersDied
    it "SchedulerAllJobsProcessed" $ property prop_SchedulerAllJobsProcessed
