{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
module Data.Array.Massiv.StencilSpec (spec) where

import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Common.IndexSpec (SzIx (..), Sz(..))
import           Data.Array.Massiv.CommonSpec       (Arr (..))
import           Data.Array.Massiv.Delayed.Windowed (WD)
import           Data.Array.Massiv.Manifest
import           Data.Array.Massiv.Mutable
import           Data.Array.Massiv.Ops              as A
import           Data.Array.Massiv.Stencil
import           Data.Typeable
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Function



-- sum3x3Stencil :: (Default a, Fractional a) => Border a -> Stencil Ix2 a a
-- sum3x3Stencil b = mkConvolutionStencil b (3 :. 3) (1 :. 1) $ \ get ->
--   get (-1 :. -1) 1 . get (-1 :. 0) 1 . get (-1 :. 1) 1 .
--   get ( 0 :. -1) 1 . get ( 0 :. 0) 1 . get ( 0 :. 1) 1 .
--   get ( 1 :. -1) 1 . get ( 1 :. 0) 1 . get ( 1 :. 1) 1
-- {-# INLINE sum3x3Stencil #-}


singletonStencil :: Index ix => (Int -> Int) -> Border Int -> Stencil ix Int Int
singletonStencil f b = makeStencil b oneIndex zeroIndex $ \ get -> f (get zeroIndex)
{-# INLINE singletonStencil #-}


-- dangerStencil :: Stencil Int Int Int
-- dangerStencil =
--   makeStencil Edge 3 1 $ \get -> get (get zeroIndex)

-- Tests out of bounds stencil indexing
-- dangerousStencil :: Index ix => (Int -> Int) -> Border Int -> SzIx ix -> Stencil ix Int Int
-- dangerousStencil f b (SzIx (Sz sz) ix) =
--   makeStencil b sz ix $ \get -> f (get (liftIndex2 (+) sz zeroIndex))



prop_MapSingletonStencil :: (Load WD ix Int, Manifest U ix Int) =>
                            Fun Int Int -> Border Int -> Arr U ix Int -> Bool
prop_MapSingletonStencil f b (Arr arr) =
  computeAs U (mapStencil (singletonStencil (apply f) b) arr) == computeAs U (A.map (apply f) arr)


stencilSpec :: Spec
stencilSpec = do
  describe "MapSingletonStencil" $ do
    it "Ix1" $ property $ prop_MapSingletonStencil @Ix1
    it "Ix2" $ property $ prop_MapSingletonStencil @Ix2
    it "Ix3" $ property $ prop_MapSingletonStencil @Ix3
    it "Ix4" $ property $ prop_MapSingletonStencil @Ix4
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
