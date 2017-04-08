{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Array.Massiv.ManifestSpec (spec) where


import           Data.Array.Massiv
import           Data.Array.Massiv.CommonSpec (ArrIx (..))
import           Data.Array.Massiv.Compute
import           Test.Hspec
import           Test.QuickCheck


instance (CoArbitrary ix, Arbitrary ix, Index ix, Unbox e, Arbitrary e) =>
         Arbitrary (Array M ix e) where
  arbitrary = do
    sz <- arbitrary
    func <- arbitrary
    return $ computeS U $ makeArray sz func

-- | Generation of a non-empty array together with a valid index
instance (CoArbitrary ix, Arbitrary ix, Index ix, Unbox e, Arbitrary e) =>
         Arbitrary (ArrIx M ix e) where
  arbitrary = do
    sz <- arbitrary
    ix <- arbitrary
    let sz' = liftIndex ((+ 1) . (`mod` 20)) sz
        ix' = liftIndex2 mod ix sz'
    func <- arbitrary
    return $ (ArrIx (computeS U $ makeArray sz' func) ix')



spec :: Spec
spec = return ()
