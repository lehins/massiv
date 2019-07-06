{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Test.Massiv.Core.IndexSpec (spec) where

import Test.Massiv.Core.Index
import Control.Monad
import Data.Functor.Identity
import Test.Massiv.Core.Index
import Data.Proxy
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function
import Data.Typeable

prop_IsSafeIx :: Index ix => proxy ix -> SzIx ix -> Bool
prop_IsSafeIx _ (SzIx sz ix) = isSafeIndex sz ix

specClasses :: forall ix . (Typeable ix, Num ix, Index ix, Arbitrary ix) => Spec
specClasses = do
  ixSpec @ix
  ixNumSpec @ix
  szNumSpec @ix

spec :: Spec
spec = do
  -- describe "Tuple based indices" $ do
  --   describe "Ix1T" $ do
  --     specDimN (Nothing :: Maybe Ix1T)
  --     it "prop_BorderIx1" $ property prop_BorderIx1
  --   describe "Ix2T" $ do
  --     specDimN (Nothing :: Maybe Ix2T)
  --     specDim2AndUp (Nothing :: Maybe Ix2T)
  --   describe "Ix3T" $ do
  --     specDimN (Nothing :: Maybe Ix3T)
  --     specDim2AndUp (Nothing :: Maybe Ix3T)
  --   describe "Ix4T" $ do
  --     specDimN (Nothing :: Maybe Ix4T)
  --     specDim2AndUp (Nothing :: Maybe Ix4T)
  --   describe "Ix5T" $ do
  --     specDimN (Nothing :: Maybe Ix5T)
  --     specDim2AndUp (Nothing :: Maybe Ix5T)
  -- describe "Specialized indices" $ do
  --   describe "Ix2" $ do
  --     -- These can be used to quickly debug monotonicity
  --     it "Monotonic'" $
  --       property $ prop_IterMonotonic' (Nothing :: Maybe Ix2) 20000
  --     it "MonotonicBackwards'" $
  --       property $ prop_IterMonotonicBackwards' (Nothing :: Maybe Ix2) 20000
  --     specDimN (Nothing :: Maybe Ix2)
  --     specDim2AndUp (Nothing :: Maybe Ix2)
  --   describe "Ix3" $ do
  --     specDimN (Nothing :: Maybe Ix3)
  --     specDim2AndUp (Nothing :: Maybe Ix3)
  --   describe "Ix4" $ do
  --     specDimN (Nothing :: Maybe Ix4)
  --     specDim2AndUp (Nothing :: Maybe Ix4)
  --   describe "Ix5" $ do
  --     specDimN (Nothing :: Maybe Ix5)
  --     specDim2AndUp (Nothing :: Maybe Ix5)
  specClasses @Ix1
  specClasses @Ix2
  specClasses @Ix3
  specClasses @Ix4
  specClasses @Ix5
