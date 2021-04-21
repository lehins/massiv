{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- Here are tests for all instances for all main classes
module Test.Massiv.ArraySpec
  ( spec
  ) where

import Data.Massiv.Array
import Test.Massiv.Core


prop_Construct_makeArray_Manifest ::
     forall r ix. (Ragged L ix Int, Source r Int, Load r ix Int)
  => Comp
  -> Sz ix
  -> Fun Int Int
  -> Property
prop_Construct_makeArray_Manifest comp sz f =
  makeArrayLinearR D comp sz (apply f) ===
  delay (setComp Seq (makeArray comp sz (apply f . toLinearIndex sz) :: Array r ix Int))

prop_Construct_makeArray_Delayed ::
     forall r ix. (Ragged L ix Int, Load r ix Int)
  => Comp
  -> Sz ix
  -> Fun Int Int
  -> Property
prop_Construct_makeArray_Delayed comp sz f =
  makeArrayLinearR P comp sz (apply f) ===
  compute (setComp Seq (makeArrayLinear comp sz (apply f)) :: Array r ix Int)

prop_Functor ::
     forall r ix.
     (Ragged L ix Int, Load r ix Int, Functor (Array r ix))
  => Comp
  -> Sz ix
  -> Fun Int Int
  -> Fun Int Int
  -> Property
prop_Functor comp sz f g =
  makeArrayLinearR P comp sz (apply g . apply f) ===
  compute (fmap (apply g) (makeArrayLinear comp sz (apply f) :: Array r ix Int))

prop_Extract ::
     forall r ix.
     ( Ragged L ix Int
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
  (computeAs P <$> toStringException (extractM start newSize arrD))  ===
  (compute <$> toStringException (extractM start newSize arr))
  where
    arrD = makeArrayLinearR D comp sz (apply f)
    arr = makeArrayLinear comp sz (apply f) :: Array r ix Int

prop_IxUnbox ::
     forall ix.
     ( Ragged L ix ix
     , Source U ix
     , Unbox ix
     )
  => Comp
  -> Sz ix
  -> Fun Int ix
  -> Property
prop_IxUnbox comp sz f =
  makeArrayLinearR D comp sz (apply f) ===
  delay (makeArrayLinear comp sz (apply f) :: Array U ix ix)

prop_computeWithStride ::
     forall r ix. (Ragged L ix Int, StrideLoad r ix Int)
  => Comp
  -> Sz ix
  -> Fun Int Int
  -> Stride ix
  -> Property
prop_computeWithStride comp sz f stride =
  arr === computeWithStride stride arrL .&&.
  arr === compute (fromStrideLoad stride arrL)
  where
    arrL = makeArrayLinear comp sz (apply f) :: Array r ix Int
    arr = computeWithStrideAs P stride (makeArrayLinearR D comp sz (apply f))


specCommon ::
     forall ix.
     (Arbitrary ix, StrideLoad DW ix Int, Ragged L ix Int, Ragged L ix ix, Unbox ix)
  => Spec
specCommon =
  describe "Construct" $ do
    it "Construct_makeArray B" $ property $ prop_Construct_makeArray_Manifest @B @ix
    it "Construct_makeArray N" $ property $ prop_Construct_makeArray_Manifest @N @ix
    it "Construct_makeArray S" $ property $ prop_Construct_makeArray_Manifest @S @ix
    it "Construct_makeArray P" $ property $ prop_Construct_makeArray_Manifest @P @ix
    it "Construct_makeArray U" $ property $ prop_Construct_makeArray_Manifest @U @ix
    it "Construct_makeArray_Delayed DI" $ property $ prop_Construct_makeArray_Delayed @DI @ix
    it "Construct_makeArray_Delayed DL" $ property $ prop_Construct_makeArray_Delayed @DL @ix
    it "Construct_makeArray_Delayed DW" $ property $ prop_Construct_makeArray_Delayed @DW @ix
    it "Functor D" $ property $ prop_Functor @D @ix
    it "Functor DI" $ property $ prop_Functor @DI @ix
    it "Functor DL" $ property $ prop_Functor @DL @ix
    it "Functor DW" $ property $ prop_Functor @DW @ix
    it "Extract B" $ property $ prop_Extract @B @ix
    it "Extract N" $ property $ prop_Extract @N @ix
    it "Extract S" $ property $ prop_Extract @S @ix
    it "Extract U" $ property $ prop_Extract @U @ix
    it "computeWithStride DI" $ property $ prop_computeWithStride @DI @ix
    it "computeWithStride DW" $ property $ prop_computeWithStride @DW @ix
    it "computeWithStride B" $ property $ prop_computeWithStride @B @ix
    it "computeWithStride N" $ property $ prop_computeWithStride @N @ix
    it "computeWithStride S" $ property $ prop_computeWithStride @S @ix
    it "computeWithStride U" $ property $ prop_computeWithStride @U @ix
    it "IxUnbox" $ property $ prop_IxUnbox @ix


spec :: Spec
spec = do
  specCommon @Ix1
  specCommon @Ix2
  specCommon @Ix3
  -- FIXME: Uses too much RAM when compiling
  -- specCommon @Ix4
  -- specCommon @Ix5
