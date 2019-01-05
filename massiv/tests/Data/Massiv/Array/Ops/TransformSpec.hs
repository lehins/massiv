{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Massiv.Array.Ops.TransformSpec (spec) where

import           Data.Massiv.CoreArbitrary as A
import           Data.Typeable             (Typeable)
import           Test.Hspec
import           Test.QuickCheck


prop_ExtractAppend
  :: (Eq e, Source r ix e, Extract r ix e, Source (EltRepr r ix) ix e, Arbitrary (ArrIx r ix e))
  => proxy (r, ix, e) -> DimIx ix -> ArrIx r ix e -> Bool
prop_ExtractAppend _ (DimIx dim) (ArrIx arr ix) =
  maybe False ((delay arr ==) . uncurry (append' dim)) $
  A.splitAt dim (getDim' ix dim) arr


prop_transposeOuterInner :: Arr D Ix2 Int -> Property
prop_transposeOuterInner (Arr arr) = transposeOuter arr === transpose arr


specN ::
     ( Eq e
     , Extract r ix e
     , Source r ix e
     , Source (EltRepr r ix) ix e
     , Typeable e
     , Show (Array r ix e)
     , Arbitrary (ArrIx r ix e)
     )
  => proxy (r, ix, e)
  -> Spec
specN r = do
  it "ExtractAppend" $ property $ prop_ExtractAppend r


spec :: Spec
spec = do
  it "transposeOuterInner" $ property prop_transposeOuterInner
  describe "Delayed" $ do
    describe "Ix1" $ specN (Nothing :: Maybe (D, Ix1, Int))
    describe "Ix2" $ specN (Nothing :: Maybe (D, Ix2, Int))
    describe "Ix3" $ specN (Nothing :: Maybe (D, Ix3, Int))
    describe "Ix4" $ specN (Nothing :: Maybe (D, Ix4, Int))
  describe "Unboxed" $ do
    describe "Ix1" $ specN (Nothing :: Maybe (U, Ix1, Int))
    describe "Ix2" $ specN (Nothing :: Maybe (U, Ix2, Int))
    describe "Ix3" $ specN (Nothing :: Maybe (U, Ix3, Int))
    describe "Ix4" $ specN (Nothing :: Maybe (U, Ix4, Int))
