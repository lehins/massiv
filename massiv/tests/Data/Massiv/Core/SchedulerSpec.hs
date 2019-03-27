{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Massiv.Core.SchedulerSpec (spec) where

import Control.Exception.Base (ArithException(DivideByZero))
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
    it "SchedulerAllJobsProcessed" $ property prop_SchedulerAllJobsProcessed
