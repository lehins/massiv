--{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

module Main where

import           Data.Massiv.Array.DelayedSpec  as Delayed
import           Data.Massiv.Array.StencilSpec  as Stencil
import           Data.Massiv.Core.IndexSpec     as Index
import           Data.Massiv.Core.SchedulerSpec as Scheduler
import           System.IO                      (BufferMode (LineBuffering),
                                                 hSetBuffering, stdout)
import           Test.Hspec


-- | Main entry point. Returns ExitFailure if a test fails.
main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hspec $ do
    Scheduler.spec
    Index.spec
    Delayed.spec
    Stencil.spec

-- -- | Test suite.
-- spec :: Spec
-- spec =
