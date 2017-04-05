{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Array.Massiv.DelayedSpec (spec) where

import           Data.Array.Massiv
import           Data.Array.Massiv.CommonSpec (ArrIx (..))
import           Test.Hspec
import           Test.QuickCheck


instance (CoArbitrary ix, Arbitrary ix, Index ix, Arbitrary e) => Arbitrary (Array D ix e) where
  arbitrary = do
    sz <- arbitrary
    func <- arbitrary
    return $ makeArray sz func

-- | Generation of a non-empty array together with a valid index
instance (CoArbitrary ix, Arbitrary ix, Index ix, Arbitrary e) => Arbitrary (ArrIx D ix e) where
  arbitrary = do
    sz <- arbitrary
    ix <- arbitrary
    let sz' = liftIndex ((+1) . abs) sz
        ix' = liftIndex2 mod ix sz'
    func <- arbitrary
    return $ (ArrIx (makeArray sz' func) ix')



spec :: Spec
spec = return ()
  -- describe "DIM1" $ do
  --   specShapeN (Nothing :: Maybe (D, DIM1, Int))
  -- describe "DIM2" $ do
  --   specShapeN (Nothing :: Maybe (D, DIM2, Int))
  --   specSliceN (Nothing :: Maybe (D, DIM2, Int))
  --   specSliceDim2
  -- describe "DIM3" $ do
  --   specShapeN (Nothing :: Maybe (D, DIM3, Int))
  --   specSliceN (Nothing :: Maybe (D, DIM3, Int))
