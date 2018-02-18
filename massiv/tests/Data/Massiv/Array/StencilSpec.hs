{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Massiv.Array.StencilSpec (spec) where

import           Control.DeepSeq           (deepseq)
import           Data.Massiv.Array.Stencil
import           Data.Massiv.CoreArbitrary as A
import           Data.Maybe                (fromJust)
import           Data.Proxy
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Data.Default              ()
-- sum3x3Stencil :: (Default a, Fractional a) => Border a -> Stencil Ix2 a a
-- sum3x3Stencil b = mkConvolutionStencil b (3 :. 3) (1 :. 1) $ \ get ->
--   get (-1 :. -1) 1 . get (-1 :. 0) 1 . get (-1 :. 1) 1 .
--   get ( 0 :. -1) 1 . get ( 0 :. 0) 1 . get ( 0 :. 1) 1 .
--   get ( 1 :. -1) 1 . get ( 1 :. 0) 1 . get ( 1 :. 1) 1
-- {-# INLINE sum3x3Stencil #-}


singletonStencil :: (Num ix, Index ix) => (Int -> Int) -> Border Int -> Stencil ix Int Int
singletonStencil f b = makeStencil b 1 0 $ \ get -> fmap f (get zeroIndex)
{-# INLINE singletonStencil #-}


prop_MapSingletonStencil :: (Load DW ix Int, Manifest U ix Int, Num ix) =>
                            Proxy ix -> Fun Int Int -> Border Int -> ArrP U ix Int -> Bool
prop_MapSingletonStencil _ f b (ArrP arr) =
  computeAs U (mapStencil (singletonStencil (apply f) b) arr) == computeAs U (A.map (apply f) arr)

-- Tests out of bounds stencil indexing
prop_DangerousStencil ::
     Index ix => Proxy ix -> NonZero Int -> DimIx ix -> Border Int -> SzIx ix -> Property
prop_DangerousStencil _ (NonZero s) (DimIx r) b (SzIx (Sz sz) ix) =
  ix' `deepseq` assertSomeException $ makeStencil b sz ix $ \get -> get ix'
  where
    ix' =
      liftIndex (* signum s) $
      fromJust $ do
        i <- getIndex sz r
        setIndex zeroIndex r i


stencilSpec :: Spec
stencilSpec = do
  describe "MapSingletonStencil" $ do
    it "Ix1" $ property $ prop_MapSingletonStencil (Proxy :: Proxy Ix1)
    it "Ix2" $ property $ prop_MapSingletonStencil (Proxy :: Proxy Ix2)
    it "Ix3" $ property $ prop_MapSingletonStencil (Proxy :: Proxy Ix3)
    it "Ix4" $ property $ prop_MapSingletonStencil (Proxy :: Proxy Ix4)
  describe "DangerousStencil" $ do
    it "Ix1" $ property $ prop_DangerousStencil (Proxy :: Proxy Ix1)
    it "Ix2" $ property $ prop_DangerousStencil (Proxy :: Proxy Ix2)
    it "Ix3" $ property $ prop_DangerousStencil (Proxy :: Proxy Ix3)
    it "Ix4" $ property $ prop_DangerousStencil (Proxy :: Proxy Ix4)
--   describe "Storable" $ do
--     it "Ix1" $ property $ prop_toFromVector (Nothing :: Maybe Ix1) S
--     it "Ix2" $ property $ prop_toFromVector (Nothing :: Maybe Ix2) S
--     it "Ix3" $ property $ prop_toFromVector (Nothing :: Maybe Ix3) S
--   describe "Primitive" $ do
--     it "Ix1" $ property $ prop_toFromVector (Nothing :: Maybe Ix1) P
--     it "Ix2" $ property $ prop_toFromVector (Nothing :: Maybe Ix2) P
--     it "Ix3" $ property $ prop_toFromVector (Nothing :: Maybe Ix3) P
--   describe "Boxed" $ do
--     it "Ix1" $ property $ prop_toFromVector (Nothing :: Maybe Ix1) B
--     it "Ix2" $ property $ prop_toFromVector (Nothing :: Maybe Ix2) B
--     it "Ix3" $ property $ prop_toFromVector (Nothing :: Maybe Ix3) B



spec :: Spec
spec = describe "Stencil" stencilSpec
