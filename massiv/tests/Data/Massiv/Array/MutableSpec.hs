{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Massiv.Array.MutableSpec (spec) where

import           Control.Monad.ST
import           Data.Functor.Identity
import           Data.Massiv.CoreArbitrary as A
import           Data.Proxy
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Function

prop_MapMapM :: (Show (Array r ix Int), Eq (Array r ix Int), Mutable r ix Int) =>
                r -> Proxy ix -> Fun Int Int -> ArrTiny D ix Int -> Property
prop_MapMapM r _ f (ArrTiny arr) =
  computeAs r (A.map (apply f) arr) === runIdentity (A.mapMR r (return . apply f) arr)

prop_iMapiMapM :: (Show (Array r ix Int), Eq (Array r ix Int), Mutable r ix Int) =>
                r -> Proxy ix -> Fun (ix, Int) Int -> ArrTiny D ix Int -> Property
prop_iMapiMapM r _ f (ArrTiny arr) =
  computeAs r (A.imap (curry (apply f)) arr) ===
  runIdentity (A.imapMR r (\ix e -> return $ apply f (ix, e)) arr)


prop_generateMakeST :: (Show (Array r ix Int), Eq (Array r ix Int), Mutable r ix Int) =>
                             r -> Proxy ix -> Arr r ix Int -> Property
prop_generateMakeST _ _ (Arr arr) =
  arr === runST (generateArray (getComp arr) (size arr) (return . evaluateAt arr))

prop_generateMakeIO :: (Show (Array r ix Int), Eq (Array r ix Int), Mutable r ix Int) =>
                             r -> Proxy ix -> Arr r ix Int -> Property
prop_generateMakeIO _ _ (Arr arr) = do
  arr <- generateArray (getComp arr) (size arr) (return . evaluateAt arr)
  arr === arr'

mutableSpec ::
     ( Show r
     , Show (Array r Ix3 Int)
     , Show (Array r Ix1 Int)
     , Show (Array r Ix2 Int)
     , Eq (Array r Ix3 Int)
     , Eq (Array r Ix1 Int)
     , Eq (Array r Ix2 Int)
     , Mutable r Ix3 Int
     , Mutable r Ix1 Int
     , Mutable r Ix2 Int
     )
  => r
  -> SpecWith ()
mutableSpec r = do
  describe (show r) $ do
    describe "map == mapM" $ do
      it "Ix1" $ property $ prop_MapMapM r (Proxy :: Proxy Ix1)
      it "Ix2" $ property $ prop_MapMapM r (Proxy :: Proxy Ix2)
      it "Ix3" $ property $ prop_MapMapM r (Proxy :: Proxy Ix3)
    describe "imap == imapM" $ do
      it "Ix1" $ property $ prop_iMapiMapM r (Proxy :: Proxy Ix1)
      it "Ix2T" $ property $ prop_iMapiMapM r (Proxy :: Proxy Ix2)
      it "Ix3T" $ property $ prop_iMapiMapM r (Proxy :: Proxy Ix3)
    describe "makeArray == generateArray" $ do
      it "Ix1" $ property $ prop_generateMakeIdentity r (Proxy :: Proxy Ix1)
      it "Ix2" $ property $ prop_generateMakeIdentity r (Proxy :: Proxy Ix2)
      it "Ix3" $ property $ prop_generateMakeIdentity r (Proxy :: Proxy Ix3)


generateSpec :: Spec
generateSpec = do
  mutableSpec P
  mutableSpec S
  mutableSpec U
  mutableSpec B
  mutableSpec N


spec :: Spec
spec = describe "GenerateM" generateSpec
