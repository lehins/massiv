{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Data.Array.Massiv.CommonSpec
  ( Arr(..)
  , ArrIx(..)
  , ArrP(..)
  , ArrIxP(..)
  , assertException
  , assertSomeException
  , assertExceptionIO
  , assertSomeExceptionIO
  , spec
  ) where

import           Control.DeepSeq                    (NFData, deepseq)
import           Control.Exception                  (Exception, SomeException,
                                                     catch)
import           Data.Array.Massiv
import           Data.Array.Massiv.Common.IndexSpec (Ix (..), Sz (..), SzZ (..))
import           Data.Typeable
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

data Arr r ix e = Arr (Array r ix e) deriving Show

data ArrP r ix e = ArrP (Array r ix e) deriving Show

data ArrIx r ix e = ArrIx (Array r ix e) ix deriving Show

data ArrIxP r ix e = ArrIxP (Array r ix e) ix deriving Show


-- | Arbitrary array
instance (CoArbitrary ix, Arbitrary ix, Typeable r, Typeable e, Massiv r ix e, Arbitrary e) =>
         Arbitrary (Array r ix e) where
  arbitrary = do
    SzZ sz <- arbitrary
    func <- arbitrary
    return $ makeArray Seq sz func


-- | Arbitrary non-empty array
instance (CoArbitrary ix, Arbitrary ix, Typeable r, Typeable e, Massiv r ix e, Arbitrary e) =>
         Arbitrary (Arr r ix e) where
  arbitrary = do
    Sz sz <- arbitrary
    func <- arbitrary
    return $ Arr $ makeArray Seq sz func

-- | Arbitrary non-empty array with a valid index
instance (CoArbitrary ix, Arbitrary ix, Typeable r, Typeable e, Massiv r ix e, Arbitrary e) =>
         Arbitrary (ArrIx r ix e) where
  arbitrary = do
    Ix (Sz sz) ix <- arbitrary
    func <- arbitrary
    return $ ArrIx (makeArray Seq sz func) ix

instance (CoArbitrary ix, Arbitrary ix, Typeable r, Typeable e, Massiv r ix e, Arbitrary e) =>
         Arbitrary (ArrP r ix e) where
  arbitrary = do
    Arr arr <- arbitrary
    return $ ArrP (setComp Par arr)


-- | Arbitrary non-empty array with a valid index
instance (CoArbitrary ix, Arbitrary ix, Typeable r, Typeable e, Massiv r ix e, Arbitrary e) =>
         Arbitrary (ArrIxP r ix e) where
  arbitrary = do
    ArrIx arrIx ix <- arbitrary
    return $ ArrIxP (setComp Par arrIx) ix


assertException :: (NFData a, Exception exc) =>
                   (exc -> Bool) -- ^ Return True if that is the exception that was expected
                -> a -- ^ Value that should throw an exception, when fully evaluated
                -> Property
assertException isExc action = assertExceptionIO isExc (return action)


assertSomeException :: NFData a => a -> Property
assertSomeException = assertSomeExceptionIO . return


assertExceptionIO :: (NFData a, Exception exc) =>
                     (exc -> Bool) -- ^ Return True if that is the exception that was expected
                  -> IO a -- ^ IO Action that should throw an exception
                  -> Property
assertExceptionIO isExc action =
  monadicIO $ do
  hasFailed <- run
    (catch
       (do res <- action
           res `deepseq` return False) $ \exc -> show exc `deepseq` return (isExc exc))
  assert hasFailed

assertSomeExceptionIO :: NFData a => IO a -> Property
assertSomeExceptionIO = assertExceptionIO (\exc -> const True (exc :: SomeException))


spec :: Spec
spec = return ()
