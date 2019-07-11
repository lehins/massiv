{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Test.Massiv.Core.Common
  ( ArrNE(..)
  , ArrTiny(..)
  , ArrTinyNE(..)
  , ArrIx(..)
  , module X
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

deriving instance (Show (Array r ix e)) => Show (ArrNE r ix e)
deriving instance (Show (Array r ix e)) => Show (ArrTiny r ix e)
deriving instance (Show (Array r ix e)) => Show (ArrTinyNE r ix e)
deriving instance (Show (Array r ix e), Show ix) => Show (ArrIx r ix e)



instance Arbitrary Comp where
  arbitrary =
    frequency
      [ (20, pure Seq)
      , (10, pure Par)
      , (15, ParOn <$> arbitrary)
      , (15, ParN . getSmall <$> arbitrary)
      ]


arbitraryArray :: (Construct r ix e, Arbitrary e) => Gen (Sz ix) -> Gen (Array r ix e)
arbitraryArray szGen = makeArrayLinear <$> arbitrary <*> szGen <*> arbitrary

-- | Arbitrary array
instance (Arbitrary ix, Construct r ix e, Arbitrary e) =>
         Arbitrary (Array r ix e) where
  arbitrary = makeArrayLinear <$> arbitrary <*> arbitrary <*> arbitrary


instance (Arbitrary ix, Construct r ix e, Arbitrary e) => Arbitrary (ArrTiny r ix e) where
  arbitrary = ArrTiny <$> arbitraryArray (liftSz (`mod` 10) <$> arbitrary)

-- | Arbitrary small and possibly empty array. Computation strategy can be either `Seq` or `Par`.
instance (Arbitrary ix, Construct r ix e, Arbitrary e) =>
         Arbitrary (ArrTinyNE r ix e) where
  arbitrary = ArrTinyNE <$> arbitraryArray (liftSz (succ . (`mod` 10)) <$> arbitrary)

instance (Arbitrary ix, Construct r ix e, Arbitrary e) =>
         Arbitrary (ArrNE r ix e) where
  arbitrary = ArrNE <$> arbitraryArray (unSzNE <$> arbitrary)


instance (Arbitrary ix, Construct r ix e, Arbitrary e) =>
         Arbitrary (ArrIx r ix e) where
  arbitrary = do
    SzIx sz ix <- arbitrary
    func <- arbitrary
    comp <- arbitrary
    return $ ArrIx (makeArrayLinear comp sz func) ix
