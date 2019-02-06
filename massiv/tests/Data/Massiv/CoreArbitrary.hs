{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}
module Data.Massiv.CoreArbitrary
  ( Arr(..)
  , ArrTiny(..)
  , ArrTiny1(..)
  , ArrIx(..)
  , ArrP(..)
  , ArrS(..)
  , ArrIxP(..)
  , SzIx(..)
  , SzNE(..)
  , DimIx(..)
  , assertException
  , assertSomeException
  , assertExceptionIO
  , assertSomeExceptionIO
  , module Data.Massiv.Array
  , Semigroup((<>))
  ) where

import           Control.DeepSeq            (NFData, deepseq)
import           Control.Exception          (Exception, SomeException, catch)
import           Data.Massiv.Array
import           Data.Massiv.Core.IndexSpec hiding (spec)
import           Data.Typeable
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup
#endif

-- | Arbitrary non-empty array. Computation strategy can be either `Seq` or `Par`.
newtype Arr r ix e = Arr {unArr :: Array r ix e}

-- | Arbitrary small and possibly empty array. Computation strategy can be either `Seq` or `Par`.
newtype ArrTiny r ix e = ArrTiny {unArrTiny :: Array r ix e}

-- | Tiny but non-empty
newtype ArrTiny1 r ix e = ArrTiny1 {unArrTiny1 :: Array r ix e}

newtype ArrS r ix e = ArrS {unArrS :: Array r ix e}

newtype ArrP r ix e = ArrP {unArrP :: Array r ix e}

-- | Arbitrary non-empty array with a valid index. Can be either `Seq` or `Par`
data ArrIx r ix e = ArrIx (Array r ix e) ix

-- | Arbitrary non-empty array with a valid index and `Seq` computation strategy
data ArrIxS r ix e = ArrIxS (Array r ix e) ix

-- | Arbitrary non-empty array with a valid index and `Par` computation strategy
data ArrIxP r ix e = ArrIxP (Array r ix e) ix

deriving instance (Show (Array r ix e)) => Show (Arr r ix e)
deriving instance (Show (Array r ix e)) => Show (ArrTiny r ix e)
deriving instance (Show (Array r ix e)) => Show (ArrTiny1 r ix e)
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
    sz <- arbitrary
    func <- arbitrary
    comp <- oneof [pure Seq, pure Par]
    return $ makeArray comp sz func


instance (CoArbitrary ix, Arbitrary ix, Typeable e, Construct r ix e, Arbitrary e) =>
         Arbitrary (ArrTiny r ix e) where
  arbitrary = do
    sz <- unSz <$> arbitrary
    func <- arbitrary
    comp <- oneof [pure Seq, pure Par]
    return $ ArrTiny $ makeArray comp (Sz (liftIndex (`mod` 10) sz)) func

-- | Arbitrary small and possibly empty array. Computation strategy can be either `Seq` or `Par`.
instance (CoArbitrary ix, Arbitrary ix, Typeable e, Construct r ix e, Arbitrary e) =>
         Arbitrary (ArrTiny1 r ix e) where
  arbitrary = do
    sz <- unSz <$> arbitrary
    func <- arbitrary
    comp <- oneof [pure Seq, pure Par]
    return $ ArrTiny1 $ makeArray comp (Sz (liftIndex (succ . (`mod` 10)) sz)) func

instance (CoArbitrary ix, Arbitrary ix, Typeable e, Construct r ix e, Arbitrary e) =>
         Arbitrary (Arr r ix e) where
  arbitrary = do
    SzNE sz <- arbitrary
    func <- arbitrary
    comp <- oneof [pure Seq, pure Par]
    return $ Arr $ makeArray comp sz func


-- | Arbitrary non-empty array
instance (CoArbitrary ix, Arbitrary ix, Typeable e, Construct r ix e, Arbitrary e) =>
         Arbitrary (ArrS r ix e) where
  arbitrary = do
    SzNE sz <- arbitrary
    func <- arbitrary
    return $ ArrS $ makeArray Seq sz func

instance (CoArbitrary ix, Arbitrary ix, Typeable e, Construct r ix e, Arbitrary e) =>
         Arbitrary (ArrP r ix e) where
  arbitrary = do
    Arr arr <- arbitrary
    return $ ArrP (setComp Par arr)

instance (CoArbitrary ix, Arbitrary ix, Typeable e, Construct r ix e, Arbitrary e) =>
         Arbitrary (ArrIx r ix e) where
  arbitrary = do
    SzIx sz ix <- arbitrary
    func <- arbitrary
    comp <- arbitrary
    return $ ArrIx (makeArray comp sz func) ix

instance (CoArbitrary ix, Arbitrary ix, Typeable e, Construct r ix e, Arbitrary e) =>
         Arbitrary (ArrIxS r ix e) where
  arbitrary = do
    SzIx sz ix <- arbitrary
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
