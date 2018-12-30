module Main where

import           Data.Massiv.Array.Delayed.WindowedSpec as Windowed
import           Data.Massiv.Array.DelayedSpec          as Delayed
import           Data.Massiv.Array.Manifest.VectorSpec  as Vector
import           Data.Massiv.Array.ManifestSpec         as Manifest
import           Data.Massiv.Array.MutableSpec         as Mutable
import           Data.Massiv.Array.Ops.ConstructSpec    as Construct
import           Data.Massiv.Array.Ops.FoldSpec         as Fold
import           Data.Massiv.Array.Ops.MapSpec          as Map
import           Data.Massiv.Array.Ops.SliceSpec        as Slice
import           Data.Massiv.Array.Ops.TransformSpec    as Transform
import           Data.Massiv.Array.StencilSpec          as Stencil
import           Data.Massiv.Core.IndexSpec             as Index
import           Data.Massiv.Core.SchedulerSpec         as Scheduler
import           System.IO                              (BufferMode (LineBuffering),
                                                         hSetBuffering, stdout)
import           Test.Hspec


-- | Main entry point. Returns ExitFailure if a test fails.
main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hspec $ do
    describe "Core" $ do
      Scheduler.spec
      Index.spec
    describe "Ops" $ do
      Construct.spec
      Fold.spec
      Slice.spec
      Transform.spec
      Map.spec
    describe "Delayed" $ Delayed.spec
    describe "Windowed" $ Windowed.spec
    describe "Manifest" $ Manifest.spec
    describe "Mutable" $ Mutable.spec
    describe "Stencil" $ Stencil.spec
    describe "Vector" $ Vector.spec
