{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Massiv.Utils
  ( showsType
  , showsArrayType
  , assertException
  , assertExceptionIO
  , assertSomeException
  , assertSomeExceptionIO
  , toStringException
  , ExpectedException(..)
  , applyFun2Compat
  , module X
  ) where

import Control.Monad as X
import Control.Monad.ST as X
import Data.Maybe as X (fromMaybe, isJust, isNothing)
import Data.Typeable as X
import Test.QuickCheck as X hiding ((.&.))
import Test.QuickCheck.Monadic as X
import Test.Hspec as X
import Test.QuickCheck.Function as X
import Control.DeepSeq (NFData, deepseq)
import UnliftIO.Exception (Exception(..), SomeException, catch, catchAny)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup as X ((<>))
#endif


-- | Use Typeable to show the type.
showsType :: forall t . Typeable t => ShowS
showsType = showsTypeRep (typeRep (Proxy :: Proxy t))

-- | Use Typeable to show the array type
showsArrayType :: forall r ix e . (Typeable r, Typeable ix, Typeable e) => ShowS
showsArrayType =
  ("Array " ++) . showsType @r . (" (" ++) . showsType @ix . (") " ++) . showsType @e


assertException ::
     (Testable b, NFData a, Exception exc)
  => (exc -> b) -- ^ Return True if that is the exception that was expected
  -> a -- ^ Value that should throw an exception, when fully evaluated
  -> Property
assertException isExc = assertExceptionIO isExc . pure


assertSomeException :: NFData a => a -> Property
assertSomeException = assertSomeExceptionIO . pure


assertExceptionIO ::
     (Testable b, NFData a, Exception exc)
  => (exc -> b) -- ^ Return True if that is the exception that was expected
  -> IO a -- ^ IO Action that should throw an exception
  -> Property
assertExceptionIO isExc action =
  monadicIO $
  run $
  catch
    (do res <- action
        res `deepseq` return (counterexample "Did not receive an exception" False))
    (\exc -> displayException exc `deepseq` return (property (isExc exc)))

assertSomeExceptionIO :: NFData a => IO a -> Property
assertSomeExceptionIO action =
  monadicIO $
  run $
  catchAny
    (do res <- action
        res `deepseq` return (counterexample "Did not receive an exception" False))
    (\exc -> displayException exc `deepseq` return (property True))


toStringException :: Either SomeException a -> Either String a
toStringException = either (Left . displayException) Right


data ExpectedException = ExpectedException deriving (Show, Eq)

instance Exception ExpectedException


applyFun2Compat :: Fun (a, b) c -> (a -> b -> c)
#if MIN_VERSION_QuickCheck(2,10,0)
applyFun2Compat = applyFun2
#else
applyFun2Compat (Fun _ f) a b = f (a, b)
instance Function Word where
  function = functionMap fromIntegral fromInteger
#endif
