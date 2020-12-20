{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Data.Massiv.Array.Manifest.PrimitiveSpec (spec) where

import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe
import Test.Massiv.Core
import Data.Word
import Data.Primitive.ByteArray

prop_ToFromByteArray ::
     forall ix. (Ragged L ix Word16)
  => Array P ix Word16
  -> Property
prop_ToFromByteArray arr =
  expectProp $ do
    let sz = size arr
        ba = unwrapByteArray arr
    resizeM sz (fromByteArray Seq (toByteArray arr)) `shouldReturn` arr
    fromByteArrayM Seq sz (toByteArray arr) `shouldReturn` arr
    fromByteArrayOffsetM Seq sz (unwrapByteArrayOffset arr) ba `shouldReturn` arr

    marr <- unsafeThaw arr
    (reallocated, mba) <- toMutableByteArray marr
    -- check that no reallocation happens when there was no slicing
    reallocated `shouldBe` (2 * totalElem sz == sizeofByteArray ba)
    marr' <- fromMutableByteArrayM sz mba
    unsafeFreeze Seq marr' `shouldReturn` arr

    let mba' = unwrapMutableByteArrayOffset marr
    marr'' <- fromMutableByteArrayOffsetM sz mba' (unwrapMutableByteArray marr)
    unsafeFreeze Seq marr'' `shouldReturn` arr


prop_ToFromPrimitiveVector ::
     forall ix. (Ragged L ix Word)
  => Array P ix Word
  -> Property
prop_ToFromPrimitiveVector arr =
  expectProp $ do
    fromPrimitiveVector (toPrimitiveVector arr) `shouldBe` flatten arr

    marr <- unsafeThaw arr
    arr' <- unsafeFreeze Seq $ fromPrimitiveMVector $ toPrimitiveMVector marr
    arr' `shouldBe` flatten arr

spec :: Spec
spec =
  describe "Primitive" $ do
    describe "ToFromByteArray" $ do
      prop "ToFromByteArray" $ prop_ToFromByteArray @Ix1
      prop "ToFromByteArray" $ prop_ToFromByteArray @Ix2
      prop "ToFromByteArray" $ prop_ToFromByteArray @Ix3
    describe "ToFromPrimitiveVector" $ do
      prop "ToFromPrimitiveVector" $ prop_ToFromPrimitiveVector @Ix1
      prop "ToFromPrimitiveVector" $ prop_ToFromPrimitiveVector @Ix2
      prop "ToFromPrimitiveVector" $ prop_ToFromPrimitiveVector @Ix3
