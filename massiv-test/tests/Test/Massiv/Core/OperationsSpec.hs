{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Massiv.Core.OperationsSpec (spec) where

import Data.Massiv.Array
import Data.Massiv.Core.Operations
import Test.Massiv.Core


-- | Arbritary Numeric array doesn't seem possible as of now
-- type NumericArraySpec ix e
--    = ( Show e
--      , Eq e
-- --     , Ragged L Ix1 e
-- --     , Num e
--      , Index ix
--      , Arbitrary ix
--      , Eq (Array D ix e)
--      , Show (Array D ix e)
-- --     , Num (Array r ix e)
--      , Numeric D e
--      , Extract D ix e
-- --     , Resize D ix
--      , Arbitrary (Array D ix e)
--      )

-- prop_Noop_operations :: (NumericArraySpec ix e) => Array D ix e -> Property
-- prop_Noop_operations arr =
--   arr === minusScalar (plusScalar arr 3) 3

-- spec :: forall ix e. (NumericArraySpec ix e) => Spec
-- spec = do
--   describe "Numeric" $
--     it "Noop_add_minus" $ property $ prop_Noop_operations @ix @e


-- | Initial test that works:
prop_Noop_operations ::
     Int
  -> Property
prop_Noop_operations i =
  arr === minusScalar (plusScalar arr 3) 3
  where
    arr :: Array D Ix2 Int
    arr = makeArray Par (Sz2 1 5) (const i)

spec :: Spec
spec = do
  describe "Numeric" $
    it "Noop_add_minus" $ property prop_Noop_operations
