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
  , expectProp
  -- * Epsilon comparison
  , epsilonExpect
  , epsilonFoldableExpect
  , epsilonMaybeEq
  , epsilonEq
  , epsilonEqDouble
  , epsilonEqFloat
  , module X
  ) where

import qualified Data.Foldable as F
import Control.Monad as X
import Control.Monad.ST as X
import Data.Maybe as X (fromMaybe, isJust, isNothing)
import Data.Typeable as X
import Test.QuickCheck as X hiding ((.&.))
import Test.QuickCheck.Monadic as X
import Test.Hspec as X
import Test.Hspec.QuickCheck as X
import Test.QuickCheck.Function as X
import Control.DeepSeq as X (NFData, deepseq)
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

-- | Convert an hspec Expectation to a quickcheck Property.
--
-- @since 1.5.0
expectProp :: Expectation -> Property
expectProp = monadicIO . run


epsilonExpect ::
     (HasCallStack, Show a, RealFloat a)
  => a -- ^ Epsilon, a maximum tolerated error. Sign is ignored.
  -> a -- ^ Expected result.
  -> a -- ^ Tested value.
  -> Expectation
epsilonExpect epsilon x y =
  X.forM_ (epsilonMaybeEq epsilon x y) $ \errMsg ->
    expectationFailure $ "Expected: " ++ show x ++ " but got: " ++ show y ++ "\n   " ++ errMsg


epsilonFoldableExpect ::
     (HasCallStack, Foldable f, Show (f e), Show e, RealFloat e) => e -> f e -> f e -> Expectation
epsilonFoldableExpect epsilon x y = do
  F.length x `shouldBe` F.length y
  unless (F.null x) $
    X.forM_ (zipWithM (epsilonMaybeEq epsilon) (F.toList x) (F.toList y)) $ \errMsgs ->
      expectationFailure $
      "Expected: " ++ show x ++ " but got: " ++ show y ++ "\n" ++ unlines (fmap ("    " ++) errMsgs)


epsilonMaybeEq ::
     (Show a, RealFloat a)
  => a -- ^ Epsilon, a maximum tolerated error. Sign is ignored.
  -> a -- ^ Expected result.
  -> a -- ^ Tested value.
  -> Maybe String
epsilonMaybeEq epsilon x y
  | isNaN x && not (isNaN y) = Just $ "Expected NaN, but got: " ++ show y
  | x == y = Nothing
  | diff > n = Just $ concat [show x, " /= ", show y, " (Tolerance: ", show diff, " > ", show n, ")"]
  | otherwise = Nothing
  where
    (absx, absy) = (abs x, abs y)
    n = epsilon * (1 + max absx absy)
    diff = abs (y - x)


epsilonEq ::
     (Show a, RealFloat a)
  => a -- ^ Epsilon, a maximum tolerated error. Sign is ignored.
  -> a -- ^ Expected result.
  -> a -- ^ Tested value.
  -> Property
epsilonEq epsilon x y = property $ epsilonExpect epsilon x y

epsilonEqDouble ::
     Double -- ^ Expected result.
  -> Double -- ^ Tested value.
  -> Property
epsilonEqDouble = epsilonEq epsilon
  where
    epsilon = 1e-12

epsilonEqFloat ::
     Float -- ^ Expected result.
  -> Float -- ^ Tested value.
  -> Property
epsilonEqFloat = epsilonEq epsilon
  where
    epsilon = 1e-6
