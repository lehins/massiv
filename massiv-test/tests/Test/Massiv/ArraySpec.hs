{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- Here are tests for all instances for all main classes
module Test.Massiv.ArraySpec (
  spec,
) where

import Data.Massiv.Array
import Test.Massiv.Core

prop_Construct_makeArray_Manifest
  :: forall r ix
   . (Ragged L ix Int, Source r Int, Load r ix Int)
  => Comp
  -> Sz ix
  -> Fun Int Int
  -> Property
prop_Construct_makeArray_Manifest comp sz f =
  makeArrayLinearR D comp sz (apply f)
    === delay (setComp Seq (makeArray comp sz (apply f . toLinearIndex sz) :: Array r ix Int))

prop_Construct_makeArray_Delayed
  :: forall r ix
   . (Ragged L ix Int, Load r ix Int)
  => Comp
  -> Sz ix
  -> Fun Int Int
  -> Property
prop_Construct_makeArray_Delayed comp sz f =
  makeArrayLinearR P comp sz (apply f)
    === compute (setComp Seq (makeArrayLinear comp sz (apply f)) :: Array r ix Int)

prop_Functor
  :: forall r ix
   . (Ragged L ix Int, Load r ix Int, Functor (Array r ix))
  => Comp
  -> Sz ix
  -> Fun Int Int
  -> Fun Int Int
  -> Property
prop_Functor comp sz f g =
  makeArrayLinearR P comp sz (apply g . apply f)
    === compute (fmap (apply g) (makeArrayLinear comp sz (apply f) :: Array r ix Int))

prop_Extract
  :: forall r ix
   . ( Ragged L ix Int
     , Load r ix Int
     , Source r Int
     )
  => Comp
  -> Sz ix
  -> Fun Int Int
  -> ix
  -> Sz ix
  -> Property
prop_Extract comp sz f start newSize =
  (computeAs P <$> toStringException (extractM start newSize arrD))
    === (compute <$> toStringException (extractM start newSize arr))
  where
    arrD = makeArrayLinearR D comp sz (apply f)
    arr = makeArrayLinear comp sz (apply f) :: Array r ix Int

prop_IxUnbox
  :: forall ix
   . ( Ragged L ix ix
     , Source U ix
     , Unbox ix
     )
  => Comp
  -> Sz ix
  -> Fun Int ix
  -> Property
prop_IxUnbox comp sz f =
  makeArrayLinearR D comp sz (apply f)
    === delay (makeArrayLinear comp sz (apply f) :: Array U ix ix)

prop_computeWithStride
  :: forall r ix
   . (Ragged L ix Int, StrideLoad r ix Int)
  => Comp
  -> Sz ix
  -> Fun Int Int
  -> Stride ix
  -> Property
prop_computeWithStride comp sz f stride =
  (arr === computeWithStride stride arrL)
    .&&. (arr === compute (fromStrideLoad stride arrL))
  where
    arrL = makeArrayLinear comp sz (apply f) :: Array r ix Int
    arr = computeWithStrideAs P stride (makeArrayLinearR D comp sz (apply f))

specCommon
  :: forall ix
   . (Arbitrary ix, StrideLoad DW ix Int, Ragged L ix Int, Ragged L ix ix, Unbox ix)
  => Spec
specCommon =
  describe "Construct" $ do
    prop "Construct_makeArray B" $ prop_Construct_makeArray_Manifest @B @ix
    prop "Construct_makeArray BN" $ prop_Construct_makeArray_Manifest @BN @ix
    prop "Construct_makeArray BL" $ prop_Construct_makeArray_Manifest @BL @ix
    prop "Construct_makeArray S" $ prop_Construct_makeArray_Manifest @S @ix
    prop "Construct_makeArray P" $ prop_Construct_makeArray_Manifest @P @ix
    prop "Construct_makeArray U" $ prop_Construct_makeArray_Manifest @U @ix
    prop "Construct_makeArray_Delayed DI" $ prop_Construct_makeArray_Delayed @DI @ix
    prop "Construct_makeArray_Delayed DL" $ prop_Construct_makeArray_Delayed @DL @ix
    prop "Construct_makeArray_Delayed DW" $ prop_Construct_makeArray_Delayed @DW @ix
    prop "Functor D" $ prop_Functor @D @ix
    prop "Functor DI" $ prop_Functor @DI @ix
    prop "Functor DL" $ prop_Functor @DL @ix
    prop "Functor DW" $ prop_Functor @DW @ix
    prop "Extract B" $ prop_Extract @B @ix
    prop "Extract BN" $ prop_Extract @BN @ix
    prop "Extract BL" $ prop_Extract @BL @ix
    prop "Extract S" $ prop_Extract @S @ix
    prop "Extract U" $ prop_Extract @U @ix
    prop "computeWithStride DI" $ prop_computeWithStride @DI @ix
    prop "computeWithStride DW" $ prop_computeWithStride @DW @ix
    prop "computeWithStride B" $ prop_computeWithStride @B @ix
    prop "computeWithStride BN" $ prop_computeWithStride @BN @ix
    prop "computeWithStride BL" $ prop_computeWithStride @BL @ix
    prop "computeWithStride S" $ prop_computeWithStride @S @ix
    prop "computeWithStride U" $ prop_computeWithStride @U @ix
    prop "IxUnbox" $ prop_IxUnbox @ix

spec :: Spec
spec = do
  specCommon @Ix1
  specCommon @Ix2
  specCommon @Ix3

-- FIXME: Uses too much RAM when compiling
-- specCommon @Ix4
-- specCommon @Ix5
