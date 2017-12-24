--{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

module Main where

import           Test.Hspec
import Data.Massiv.Core.SchedulerSpec as Scheduler
import Data.Massiv.Core.IndexSpec as Index


-- | Main entry point. Returns ExitFailure if a test fails.
main :: IO ()
main = hspec $ do
  Scheduler.spec
  Index.spec

-- -- | Test suite.
-- spec :: Spec
-- spec = 
