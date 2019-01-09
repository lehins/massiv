module Data.Massiv.SchedulerSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Massiv.Scheduler


prop_SameList :: Comp -> [Int] -> Property
prop_SameList comp xs =
  monadicIO $ run $ do
    xs' <- withScheduler comp $ \scheduler -> mapM_ (scheduleWork scheduler . return) xs
    return (xs === xs')

spec :: Spec
spec = do
  describe "Seq" $ do
    it "SameList" $ property $ prop_SameList Seq
  describe "ParOn" $ do
    it "SameList" $ property $ \ cs -> prop_SameList (ParOn cs)














