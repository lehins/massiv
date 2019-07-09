module Test.Massiv.Utils where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.DeepSeq (NFData, deepseq)
import UnliftIO.Exception (Exception(..), SomeException, catch, catchAny)

assertException :: (NFData a, Exception exc) =>
                   (exc -> Bool) -- ^ Return True if that is the exception that was expected
                -> a -- ^ Value that should throw an exception, when fully evaluated
                -> Property
assertException isExc = assertExceptionIO isExc . pure


assertSomeException :: NFData a => a -> Property
assertSomeException = assertSomeExceptionIO . pure


assertExceptionIO :: (NFData a, Exception exc) =>
                     (exc -> Bool) -- ^ Return True if that is the exception that was expected
                  -> IO a -- ^ IO Action that should throw an exception
                  -> Property
assertExceptionIO isExc action =
  monadicIO $
  assert =<<
  run
    (catch
       (do res <- action
           res `deepseq` return False)
       (\exc -> displayException exc `deepseq` return (isExc exc)))

assertSomeExceptionIO :: NFData a => IO a -> Property
assertSomeExceptionIO action =
  monadicIO $
  assert =<<
  run
    (catchAny
       (do res <- action
           res `deepseq` return False)
       (\exc -> displayException exc `deepseq` return True))


toStringException :: Either SomeException a -> Either String a
toStringException = either (Left . displayException) Right


data ExpectedException = ExpectedException deriving (Show, Eq)

instance Exception ExpectedException
