module Data.Massiv.SchedulerSpec (spec) where

import           Control.Concurrent      (killThread, myThreadId, threadDelay)
import           Control.Concurrent.MVar
import           Control.DeepSeq
import           Control.Exception       hiding (assert)
import           Control.Exception.Base  (ArithException (DivideByZero))
import           Data.List               (sort)
import           Data.Massiv.Scheduler
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic


instance Arbitrary Comp where
  arbitrary =
    frequency
      [ (20, pure Seq)
      , (10, pure Par)
      , (35, ParOn <$> arbitrary)
      , (35, ParN . getSmall <$> arbitrary)
      ]

prop_SameList :: Comp -> [Int] -> Property
prop_SameList comp xs =
  monadicIO $ run $ do
    xs' <- withScheduler comp $ \scheduler -> mapM_ (scheduleWork scheduler . return) xs
    return (xs === xs')

prop_Recursive :: Comp -> [Int] -> Property
prop_Recursive comp xs =
  monadicIO $
  run $ do
    xs' <- withScheduler comp (schedule xs)
    return (sort xs === sort xs')
  where
    schedule [] _ = return ()
    schedule (y:ys) scheduler = scheduleWork scheduler (schedule ys scheduler >> return y)


prop_Serially :: Comp -> [Int] -> Property
prop_Serially comp xs =
  monadicIO $
  run $ do
    xs' <- schedule xs
    return (xs === concat xs')
  where
    schedule [] = return []
    schedule (y:ys) = do
      y' <- withScheduler comp (\s -> scheduleWork s (return y))
      ys' <- schedule ys
      return (y':ys')

prop_Nested :: Comp -> [Int] -> Property
prop_Nested comp xs =
  monadicIO $
  run $ do
    xs' <- schedule xs
    return (sort xs === sort (concat xs'))
  where
    schedule [] = return []
    schedule (y:ys) =
      withScheduler comp (\s -> scheduleWork s (schedule ys >>= \ys' -> return (y : concat ys')))

prop_ArbitraryCompNested :: [(Comp, Int)] -> Property
prop_ArbitraryCompNested xs =
  monadicIO $
  run $ do
    xs' <- schedule xs
    return (sort (map snd xs) === sort (concat xs'))
  where
    schedule [] = return []
    schedule ((c, y):ys) =
      withScheduler c (\s -> scheduleWork s (schedule ys >>= \ys' -> return (y : concat ys')))

-- | Ensure proper exception handling.
prop_CatchDivideByZero :: Comp -> Int -> [Positive Int] -> Property
prop_CatchDivideByZero comp k xs =
  assertExceptionIO
    (== DivideByZero)
    (mapConcurrently
       comp
       (\i -> return (k `div` i))
       (map getPositive xs ++ [0] ++ map getPositive xs))

-- | Ensure proper exception handling.
prop_CatchDivideByZeroNested :: Comp -> Int -> Positive Int -> Property
prop_CatchDivideByZeroNested comp a (Positive k) = assertExceptionIO (== DivideByZero) (schedule k)
  where
    schedule i
      | i < 0 = return []
      | otherwise =
        withScheduler comp (\s -> scheduleWork s (schedule (i - 1) >> return (a `div` i)))


-- | Make sure one co-worker can kill another one, of course when there are at least two of.
prop_KillBlockedCoworker :: Comp -> Property
prop_KillBlockedCoworker comp =
  assertExceptionIO
    (== DivideByZero)
    (withScheduler_ comp $ \scheduler ->
       if numWorkers scheduler < 2
         then scheduleWork scheduler $ return ((1 :: Int) `div` (0 :: Int))
         else do
           mv <- newEmptyMVar
           scheduleWork scheduler $ readMVar mv
           scheduleWork scheduler $ return ((1 :: Int) `div` (0 :: Int)))

-- | Make sure one co-worker can kill another one, of course when there are at least two of.
prop_KillSleepingCoworker :: Comp -> Property
prop_KillSleepingCoworker comp =
  assertExceptionIO
    (== DivideByZero)
    (withScheduler_ comp $ \scheduler -> do
       scheduleWork scheduler $ return ((1 :: Int) `div` (0 :: Int))
       scheduleWork scheduler $ do
         threadDelay 500000
         error "This should never happen! Thread should have been killed by now.")


prop_ExpectAsyncException :: Comp -> Property
prop_ExpectAsyncException comp =
  let didAWorkerDie = handleJust fromWorkerAsyncException' (return . (== ThreadKilled)) . fmap or
      fromWorkerAsyncException' =
        case comp of
          Seq -> asyncExceptionFromException
          _ -> fromWorkerAsyncException
   in (monadicIO .
       run .
       didAWorkerDie .
       withScheduler comp $ \s -> scheduleWork s (myThreadId >>= killThread >> pure False)) .&&.
      (monadicIO .
       run . fmap not . didAWorkerDie . withScheduler Par $ \s -> scheduleWork s $ pure False)

spec :: Spec
spec = do
  describe "Seq" $ do
    it "SameList" $ property $ prop_SameList Seq
    it "Recursive" $ property $ prop_Recursive Seq
    it "Nested" $ property $ prop_Nested Seq
    it "Serially" $ property $ prop_Serially Seq
  describe "ParOn" $ do
    it "SameList" $ property $ \ cs -> prop_SameList (ParOn cs)
    it "Recursive" $ property $ \ cs -> prop_Recursive (ParOn cs)
    it "Nested" $ property $ \ cs -> prop_Nested (ParOn cs)
    it "Serially" $ property $ \ cs -> prop_Serially (ParOn cs)
  describe "Arbitrary Comp" $ do
    it "ArbitraryNested" $ property prop_ArbitraryCompNested
  describe "Exceptions" $ do
    it "CatchDivideByZero" $ property prop_CatchDivideByZero
    it "CatchDivideByZeroNested" $ property prop_CatchDivideByZeroNested
    it "KillBlockedCoworker" $ property $ prop_KillBlockedCoworker
    it "KillSleepingCoworker" $ property $ prop_KillSleepingCoworker
    it "ExpectAsyncException" $ property $ prop_ExpectAsyncException

assertExceptionIO :: (NFData a, Exception exc) =>
                     (exc -> Bool) -- ^ Return True if that is the exception that was expected
                  -> IO a -- ^ IO Action that should throw an exception
                  -> Property
assertExceptionIO isExc action =
  monadicIO $ do
    hasFailed <-
      run
        (catch
           (do res <- action
               res `deepseq` return False) $ \exc ->
           displayException exc `deepseq` return (isExc exc))
    assert hasFailed








