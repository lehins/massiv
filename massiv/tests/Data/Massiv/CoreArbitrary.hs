{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}
module Data.Massiv.CoreArbitrary
  ( Arr(..)
  , ArrIx(..)
  , ArrP(..)
  , ArrIxP(..)
  , Sz(..)
  , SzIx(..)
  , SzZ(..)
  , DimIx(..)
  , assertException
  , assertSomeException
  , assertExceptionIO
  , assertSomeExceptionIO
  , module Data.Massiv.Array
  ) where

import           Control.DeepSeq            (NFData, deepseq)
import           Control.Exception          (Exception, SomeException, catch)
--import           Data.Massiv.Array.Ops.Construct
import           Data.Massiv.Array
import           Data.Massiv.Core.IndexSpec hiding (spec)
import           Data.Typeable
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

data Arr r ix e = Arr (Array r ix e)

data ArrS r ix e = ArrS (Array r ix e)

data ArrP r ix e = ArrP (Array r ix e)

data ArrIx r ix e = ArrIx (Array r ix e) ix

data ArrIxS r ix e = ArrIxS (Array r ix e) ix

data ArrIxP r ix e = ArrIxP (Array r ix e) ix

deriving instance (Show (Array r ix e)) => Show (Arr r ix e)
deriving instance (Show (Array r ix e)) => Show (ArrS r ix e)
deriving instance (Show (Array r ix e)) => Show (ArrP r ix e)
deriving instance (Show (Array r ix e), Show ix) => Show (ArrIx r ix e)
deriving instance (Show (Array r ix e), Show ix) => Show (ArrIxS r ix e)
deriving instance (Show (Array r ix e), Show ix) => Show (ArrIxP r ix e)

instance Arbitrary Comp where
  arbitrary = oneof [pure Seq, fmap ParOn arbitrary]


-- | Arbitrary array
instance (CoArbitrary ix, Arbitrary ix, Typeable e, Construct r ix e, Arbitrary e) =>
         Arbitrary (Array r ix e) where
  arbitrary = do
    SzZ sz <- arbitrary
    func <- arbitrary
    comp <- oneof [pure Seq, pure Par]
    return $ makeArray comp sz func


-- | Arbitrary non-empty array. Computation strategy can be either `Seq` or `Par`.
instance (CoArbitrary ix, Arbitrary ix, Typeable e, Construct r ix e, Arbitrary e) =>
         Arbitrary (Arr r ix e) where
  arbitrary = do
    Sz sz <- arbitrary
    func <- arbitrary
    comp <- oneof [pure Seq, pure Par]
    return $ Arr $ makeArray comp sz func

-- | Arbitrary non-empty array
instance (CoArbitrary ix, Arbitrary ix, Typeable e, Construct r ix e, Arbitrary e) =>
         Arbitrary (ArrS r ix e) where
  arbitrary = do
    Sz sz <- arbitrary
    func <- arbitrary
    return $ ArrS $ makeArray Seq sz func

instance (CoArbitrary ix, Arbitrary ix, Typeable e, Construct r ix e, Arbitrary e) =>
         Arbitrary (ArrP r ix e) where
  arbitrary = do
    Arr arr <- arbitrary
    return $ ArrP (setComp Par arr)

-- | Arbitrary non-empty array with a valid index
instance (CoArbitrary ix, Arbitrary ix, Typeable e, Construct r ix e, Arbitrary e) =>
         Arbitrary (ArrIx r ix e) where
  arbitrary = do
    SzIx (Sz sz) ix <- arbitrary
    func <- arbitrary
    comp <- arbitrary
    return $ ArrIx (makeArray comp sz func) ix

-- | Arbitrary non-empty array with a valid index
instance (CoArbitrary ix, Arbitrary ix, Typeable e, Construct r ix e, Arbitrary e) =>
         Arbitrary (ArrIxS r ix e) where
  arbitrary = do
    SzIx (Sz sz) ix <- arbitrary
    func <- arbitrary
    return $ ArrIxS (makeArray Seq sz func) ix


-- | Arbitrary non-empty array with a valid index
instance (CoArbitrary ix, Arbitrary ix, Typeable e, Construct r ix e, Arbitrary e) =>
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
    hasFailed <-
      run
        (catch
           (do res <- action
               res `deepseq` return False) $ \exc ->
           show exc `deepseq` return (isExc exc))
    assert hasFailed

assertSomeExceptionIO :: NFData a => IO a -> Property
assertSomeExceptionIO = assertExceptionIO (\exc -> const True (exc :: SomeException))
