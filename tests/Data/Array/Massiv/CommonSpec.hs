{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Data.Array.Massiv.CommonSpec (ArrIx(..), spec) where

import           Data.Array.Massiv
import           Test.Hspec
--import           Test.QuickCheck


data ArrIx r ix e = ArrIx (Array r ix e) ix deriving Show


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
