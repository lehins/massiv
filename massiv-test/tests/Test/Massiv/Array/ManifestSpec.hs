{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Test.Massiv.Array.ManifestSpec (spec) where

import Data.ByteString as S
import Data.ByteString.Builder as S
import Data.ByteString.Lazy as SL
import Data.Massiv.Array as A
import Data.Word (Word8)
import Test.Massiv.Core

-- ByteString
prop_toFromByteString
  :: (Show (Vector r Word8), Eq (Vector r Word8), Load r Ix1 Word8) => Vector r Word8 -> Property
prop_toFromByteString arr = arr === fromByteString (getComp arr) (toByteString arr)

prop_castToFromByteString :: Vector S Word8 -> Property
prop_castToFromByteString arr = arr === castFromByteString (getComp arr) (castToByteString arr)

prop_fromToByteString :: Comp -> [Word8] -> Property
prop_fromToByteString comp ls = bs === toByteString (fromByteString comp bs :: Vector P Word8)
  where
    bs = S.pack ls

prop_toBuilder :: Array P Ix1 Word8 -> Property
prop_toBuilder arr = bs === SL.toStrict (S.toLazyByteString (toBuilder S.word8 arr))
  where
    bs = toByteString arr

conversionSpec :: Spec
conversionSpec =
  describe "ByteString" $ do
    it "castTo/TromByteString" $ property prop_castToFromByteString
    it "to/from ByteString P" $ property (prop_toFromByteString @P)
    it "to/from ByteString S" $ property (prop_toFromByteString @S)
    it "from/to ByteString" $ property prop_fromToByteString
    it "toBuilder" $ property prop_toBuilder

spec :: Spec
spec = describe "Conversion" conversionSpec
