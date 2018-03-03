{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Massiv.Array.MutableSpec (spec) where

import           Data.Massiv.CoreArbitrary as A
import           Data.Proxy
import           Data.Functor.Identity
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Function


prop_MapMapM :: (Show (Array r ix Int), Eq (Array r ix Int), Mutable r ix Int) =>
                r -> Proxy ix -> Fun Int Int -> ArrTiny D ix Int -> Property
prop_MapMapM r _ f (ArrTiny arr) =
  computeAs r (A.map (apply f) arr) === runIdentity (A.mapM r (return . apply f) arr)

prop_iMapiMapM :: (Show (Array r ix Int), Eq (Array r ix Int), Mutable r ix Int) =>
                r -> Proxy ix -> Fun (ix, Int) Int -> ArrTiny D ix Int -> Property
prop_iMapiMapM r _ f (ArrTiny arr) =
  computeAs r (A.imap (curry (apply f)) arr) ===
  runIdentity (A.imapM r (\ix e -> return $ apply f (ix, e)) arr)


generateSpec :: Spec
generateSpec = do
  describe "map == mapM" $ do
    describe "P" $ do
      it "Ix1" $ property $ prop_MapMapM P (Proxy :: Proxy Ix1)
      it "Ix2" $ property $ prop_MapMapM P (Proxy :: Proxy Ix2)
      it "Ix3" $ property $ prop_MapMapM P (Proxy :: Proxy Ix3)
    describe "U" $ do
      it "Ix1" $ property $ prop_MapMapM U (Proxy :: Proxy Ix1)
      it "Ix2" $ property $ prop_MapMapM U (Proxy :: Proxy Ix2)
      it "Ix3" $ property $ prop_MapMapM U (Proxy :: Proxy Ix3)
    describe "S" $ do
      it "Ix1" $ property $ prop_MapMapM S (Proxy :: Proxy Ix1)
      it "Ix2" $ property $ prop_MapMapM S (Proxy :: Proxy Ix2)
      it "Ix3" $ property $ prop_MapMapM S (Proxy :: Proxy Ix3)
    describe "B" $ do
      it "Ix1" $ property $ prop_MapMapM B (Proxy :: Proxy Ix1)
      it "Ix2" $ property $ prop_MapMapM B (Proxy :: Proxy Ix2)
      it "Ix3" $ property $ prop_MapMapM B (Proxy :: Proxy Ix3)
  describe "imap == imapM" $ do
    describe "P" $ do
      it "Ix1" $ property $ prop_iMapiMapM P (Proxy :: Proxy Ix1)
      it "Ix2T" $ property $ prop_iMapiMapM P (Proxy :: Proxy Ix2T)
      it "Ix3T" $ property $ prop_iMapiMapM P (Proxy :: Proxy Ix3T)
    describe "U" $ do
      it "Ix1" $ property $ prop_iMapiMapM U (Proxy :: Proxy Ix1)
      it "Ix2T" $ property $ prop_iMapiMapM U (Proxy :: Proxy Ix2T)
      it "Ix3T" $ property $ prop_iMapiMapM U (Proxy :: Proxy Ix3T)
    describe "S" $ do
      it "Ix1" $ property $ prop_iMapiMapM S (Proxy :: Proxy Ix1)
      it "Ix2T" $ property $ prop_iMapiMapM S (Proxy :: Proxy Ix2T)
      it "Ix3T" $ property $ prop_iMapiMapM S (Proxy :: Proxy Ix3T)
    describe "B" $ do
      it "Ix1" $ property $ prop_iMapiMapM B (Proxy :: Proxy Ix1)
      it "Ix2T" $ property $ prop_iMapiMapM B (Proxy :: Proxy Ix2T)
      it "Ix3T" $ property $ prop_iMapiMapM B (Proxy :: Proxy Ix3T)


spec :: Spec
spec = describe "GenerateM" generateSpec
