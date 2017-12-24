--{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

module Main where

import           Test.Hspec
import Data.Massiv.Core.SchedulerSpec

-- | Main entry point. Returns ExitFailure if a test fails.
main :: IO ()
main = hspec spec

-- -- | Test suite.
-- spec :: Spec
-- spec = 
