{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Massiv.Core.Common (
  ArrNE (..),
  ArrTiny (..),
  ArrTinyNE (..),
  ArrIx (..),
  ArrDW (..),
  module X,
) where

import Data.Massiv.Array
import Test.Massiv.Core.Index as X
import Test.Massiv.Utils

-- | Arbitrary non-empty array. Computation strategy can be either `Seq` or `Par`.
--
-- @since 0.1.0
newtype ArrNE r ix e = ArrNE
  { unArr :: Array r ix e
  }

-- | Arbitrary small and possibly empty array. Computation strategy can be either `Seq` or `Par`.
--
-- @since 0.1.0
newtype ArrTiny r ix e = ArrTiny
  { unArrTiny :: Array r ix e
  }

-- | Tiny but non-empty
--
-- @since 0.1.0
newtype ArrTinyNE r ix e = ArrTinyNE
  { unArrTinyNE :: Array r ix e
  }

-- | Arbitrary non-empty array with a valid index. Can be either `Seq` or `Par`
--
-- @since 0.1.0
data ArrIx r ix e = ArrIx (Array r ix e) ix

deriving instance Show (Array r ix e) => Show (ArrNE r ix e)

deriving instance Show (Array r ix e) => Show (ArrTiny r ix e)

deriving instance Show (Array r ix e) => Show (ArrTinyNE r ix e)

deriving instance (Show (Array r ix e), Show ix) => Show (ArrIx r ix e)

instance Arbitrary Comp where
  arbitrary =
    frequency
      [ (20, pure Seq)
      , (10, pure Par)
      , (10, pure Par')
      , (15, ParOn <$> arbitrary)
      , (15, ParN . getSmall <$> arbitrary)
      ]

arbitraryArray :: (Load r ix e, Arbitrary e) => Gen (Sz ix) -> Gen (Array r ix e)
arbitraryArray szGen =
  oneof
    [ do
        comp <- arbitrary
        sz <- szGen
        f <- arbitrary
        pure $ makeArray comp sz (applyFun f . toLinearIndex sz)
    , makeArrayLinear <$> arbitrary <*> szGen <*> arbitrary
    ]

-- | Arbitrary array
instance (Arbitrary ix, Index ix, Arbitrary e) => Arbitrary (Array D ix e) where
  arbitrary = arbitraryArray arbitrary

instance (Arbitrary ix, Index ix, Arbitrary e) => Arbitrary (Array DL ix e) where
  arbitrary = arbitraryArray arbitrary

instance (Arbitrary ix, Index ix, Arbitrary e) => Arbitrary (Array DI ix e) where
  arbitrary = arbitraryArray arbitrary

instance (Arbitrary ix, Load DW ix e, Arbitrary e) => Arbitrary (Array DW ix e) where
  arbitrary = arbitraryArray arbitrary

instance (ix ~ Ix1, Arbitrary e) => Arbitrary (Array DS ix e) where
  arbitrary = arbitraryArray arbitrary

instance (Arbitrary ix, Index ix, Arbitrary e) => Arbitrary (Array B ix e) where
  arbitrary = arbitraryArray arbitrary

instance (Arbitrary ix, Index ix, Arbitrary e) => Arbitrary (Array BL ix e) where
  arbitrary = arbitraryArray arbitrary

instance (Arbitrary ix, Index ix, Arbitrary e, NFData e) => Arbitrary (Array BN ix e) where
  arbitrary = arbitraryArray arbitrary

instance (Arbitrary ix, Index ix, Arbitrary e, Prim e) => Arbitrary (Array P ix e) where
  arbitrary = arbitraryArray arbitrary

instance (Arbitrary ix, Index ix, Arbitrary e, Storable e) => Arbitrary (Array S ix e) where
  arbitrary = arbitraryArray arbitrary

instance (Arbitrary ix, Index ix, Arbitrary e, Unbox e) => Arbitrary (Array U ix e) where
  arbitrary = arbitraryArray arbitrary

instance (Arbitrary ix, Load L ix e, Arbitrary e) => Arbitrary (Array L ix e) where
  arbitrary = arbitraryArray arbitrary

instance (Arbitrary ix, Load r ix e, Arbitrary e) => Arbitrary (ArrTiny r ix e) where
  arbitrary = ArrTiny <$> arbitraryArray (liftSz (`mod` 10) <$> arbitrary)

-- | Arbitrary small and possibly empty array.
instance
  (Arbitrary ix, Load r ix e, Arbitrary e)
  => Arbitrary (ArrTinyNE r ix e)
  where
  arbitrary = ArrTinyNE <$> arbitraryArray (liftSz (succ . (`mod` 10)) <$> arbitrary)

instance
  (Arbitrary ix, Load r ix e, Arbitrary e)
  => Arbitrary (ArrNE r ix e)
  where
  arbitrary = ArrNE <$> arbitraryArray (unSzNE <$> arbitrary)

instance
  (Arbitrary ix, Load r ix e, Arbitrary e)
  => Arbitrary (ArrIx r ix e)
  where
  arbitrary = do
    SzIx sz ix <- arbitrary
    func <- arbitrary
    comp <- arbitrary
    return $ ArrIx (makeArrayLinear comp sz func) ix

data ArrDW ix e = ArrDW (Array D ix e) (Array DW ix e)

instance
  (Show ix, Index ix, Ragged L ix e, Load DW ix e, Show e)
  => Show (ArrDW ix e)
  where
  show (ArrDW d dw) =
    "Delayed:\n"
      ++ show d
      ++ "\nCorresponding Windowed:\n"
      ++
      -- show dw ++
      windowInfo
    where
      windowInfo =
        maybe
          "\n No Window"
          ( \Window{windowStart, windowSize} ->
              "\n With Window starting index ("
                ++ show windowStart
                ++ ") and size ("
                ++ show windowSize
                ++ ")"
          )
          $ getWindow dw

instance
  (Arbitrary ix, CoArbitrary ix, Load DW ix e, Arbitrary e, Typeable e)
  => Arbitrary (ArrDW ix e)
  where
  arbitrary = do
    ArrTiny (arr :: Array D ix e) <- arbitrary
    let sz = size arr
    ArrDW arr
      <$> if totalElem sz == 0
        then return (makeArray (getComp arr) sz (evaluate' arr))
        else do
          wix <- flip (liftIndex2 mod) (unSz sz) <$> arbitrary
          wsz <- liftIndex (+ 1) . flip (liftIndex2 mod) (liftIndex2 (-) (unSz sz) wix) <$> arbitrary
          return $ makeWindowedArray arr wix (Sz wsz) (evaluate' arr)
