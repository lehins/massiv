{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Massiv.Array.MutableSpec where

import Data.Int
import Data.Massiv.Array as A
import GHC.Exts
import Test.Massiv.Array.Delayed
import Test.Massiv.Array.Load
import Test.Massiv.Array.Mutable
import Test.Massiv.Core
import Test.Massiv.Core.Mutable

type MutableArraySpec r ix e =
  ( Show e
  , Eq e
  , Arbitrary e
  , Arbitrary ix
  , CoArbitrary e
  , Function e
  , Eq (Array r ix e)
  , Show (Array r ix e)
  , Eq (Vector r e)
  , Show (Vector r e)
  , Load r ix e
  , Arbitrary (Array r ix e)
  , Manifest r e
  , Stream r ix e
  )

type MutableSpec r e =
  ( Typeable e
  , IsList (Array r Ix1 e)
  , Item (Array r Ix1 e) ~ e
  , MutableArraySpec r Ix1 e
  , MutableArraySpec r Ix2 e
  , MutableArraySpec r Ix3 e
  , MutableArraySpec r Ix4 e
  , MutableArraySpec r Ix5 e
  )

localMutableSpec :: forall r ix e. MutableArraySpec r ix e => Spec
localMutableSpec = do
  describe "toStream/toList" $
    it "toStream" $
      property (prop_toStream @r @ix @e)
  describe "Manifest operations" $ do
    it "write" $ property (prop_Write @r @ix @e)
    it "modify" $ property (prop_Modify @r @ix @e)
    it "swap" $ property (prop_Swap @r @ix @e)

specMutableR :: forall r e. MutableSpec r e => Spec
specMutableR = do
  unsafeMutableSpec @r @Ix1 @e
  unsafeMutableSpec @r @Ix2 @e
  unsafeMutableSpec @r @Ix3 @e
  unsafeMutableSpec @r @Ix4 @e
  unsafeMutableSpec @r @Ix5 @e
  mutableSpec @r @Ix1 @e
  mutableSpec @r @Ix2 @e
  mutableSpec @r @Ix3 @e
  mutableSpec @r @Ix4 @e
  -- mutableSpec @r @Ix5 @e -- slows down the test suite
  loadSpec @r @Ix1 @e
  loadSpec @r @Ix2 @e
  loadSpec @r @Ix3 @e
  loadSpec @r @Ix4 @e
  localMutableSpec @r @Ix1 @e
  localMutableSpec @r @Ix2 @e
  localMutableSpec @r @Ix3 @e
  localMutableSpec @r @Ix4 @e
  localMutableSpec @r @Ix5 @e
  describe "NonFlat" $ do
    specMutableNonFlatR @r @Ix2 @e
    specMutableNonFlatR @r @Ix3 @e
    specMutableNonFlatR @r @Ix4 @e
    specMutableNonFlatR @r @Ix5 @e
  describe "toStream/toList" $
    it "toStreamIsList" $
      property (prop_toStreamIsList @r @e)

specMutableNonFlatR
  :: forall r ix e
   . ( Arbitrary ix
     , Typeable e
     , Arbitrary e
     , Index (Lower ix)
     , Load r ix e
     , Manifest r e
     , Eq (Array r (Lower ix) e)
     , Show (Array r (Lower ix) e)
     , Show (Array r ix e)
     )
  => Spec
specMutableNonFlatR = do
  describe (showsArrayType @r @ix @e "") $
    prop "outerSliceMArrayM" $
      prop_outerSliceMArrayM @r @ix @e

specUnboxedMutableR :: forall r e. MutableSpec r e => Spec
specUnboxedMutableR = do
  specMutableR @r @e
  unsafeMutableUnboxedSpec @r @Ix1 @e
  unsafeMutableUnboxedSpec @r @Ix2 @e
  unsafeMutableUnboxedSpec @r @Ix3 @e
  unsafeMutableUnboxedSpec @r @Ix4 @e
  unsafeMutableUnboxedSpec @r @Ix5 @e

prop_Write
  :: forall r ix e
   . (Index ix, Manifest r e, Eq e, Show e)
  => Array r ix e
  -> ix
  -> e
  -> Property
prop_Write arr ix e =
  monadicIO $
    run $ do
      marr <- thaw arr
      A.read marr ix >>= \case
        Nothing ->
          let withExcept = assertDeepExceptionIO (== IndexOutOfBoundsException (size arr) ix)
           in pure
                ( withExcept (writeM marr ix e)
                    .&&. (write marr ix e `shouldReturn` False)
                    .&&. (write_ marr ix e `shouldReturn` ())
                )
        Just olde ->
          pure $
            property $ do
              indexM arr ix `shouldReturn` olde
              A.write marr ix e `shouldReturn` True
              A.read marr ix `shouldReturn` Just e

              marr' <- thaw arr
              writeM marr' ix e `shouldReturn` ()
              arr' <- freeze (getComp arr) marr'
              indexM arr' ix `shouldReturn` e

              arr'' <- withMArray_ arr (\_ ma -> write_ ma ix e)
              index' arr'' ix `shouldBe` e

prop_Modify
  :: forall r ix e
   . (Index ix, Manifest r e, Eq e, Show e)
  => Array r ix e
  -> Fun e e
  -> ix
  -> Property
prop_Modify arr f ix =
  monadicIO $
    run $ do
      marr <- thaw arr
      modify marr (pure . apply f) ix >>= \case
        Nothing ->
          let withExcept = assertDeepExceptionIO (== IndexOutOfBoundsException (size arr) ix)
           in pure
                ( withExcept (void $ indexM arr ix)
                    .&&. withExcept (void $ readM marr ix)
                    .&&. withExcept (void $ modifyM marr (pure . apply f) ix)
                    .&&. withExcept (modifyM_ marr (pure . apply f) ix)
                    .&&. (modify_ marr (pure . apply f) ix `shouldReturn` ())
                )
        Just e ->
          pure $
            property $ do
              let fM = pure . apply f
                  fe = apply f e
              indexM arr ix `shouldReturn` e
              A.read marr ix `shouldReturn` Just fe

              marr' <- thawS arr
              readM marr' ix `shouldReturn` e
              modifyM marr' fM ix `shouldReturn` e
              arr' <- freezeS marr'
              indexM arr' ix `shouldReturn` fe

              arr'' <- withMArrayS_ arr (\ma -> modify_ ma fM ix)
              index' arr'' ix `shouldBe` fe

prop_Swap
  :: forall r ix e
   . (Index ix, Manifest r e, Eq e, Show e)
  => Array r ix e
  -> ix
  -> ix
  -> Property
prop_Swap arr ix1 ix2 =
  monadicIO $
    run $ do
      marr <- thaw arr
      swap marr ix1 ix2 >>= \case
        Nothing ->
          let withExcept =
                assertDeepExceptionIO
                  ( \case
                      IndexOutOfBoundsException _ _ -> True
                      _ -> False
                  )
           in pure
                ( withExcept (void $ indexM arr ix1 >> indexM arr ix2)
                    .&&. withExcept (void $ readM marr ix1 >> readM marr ix2)
                    .&&. withExcept (void $ swapM marr ix1 ix2)
                    .&&. withExcept (void $ swapM marr ix2 ix1)
                    .&&. withExcept (swapM_ marr ix1 ix2)
                    .&&. withExcept (swapM_ marr ix2 ix1)
                    .&&. (swap_ marr ix1 ix2 `shouldReturn` ())
                    .&&. (swap_ marr ix2 ix1 `shouldReturn` ())
                )
        Just (e1, e2) ->
          pure $
            property $ do
              indexM arr ix1 `shouldReturn` e1
              indexM arr ix2 `shouldReturn` e2
              readM marr ix1 `shouldReturn` e2
              readM marr ix2 `shouldReturn` e1

              marr' <- thawS arr
              swapM marr' ix1 ix2 `shouldReturn` (e1, e2)
              arr' <- freezeS marr'
              indexM arr' ix1 `shouldReturn` e2
              indexM arr' ix2 `shouldReturn` e1

              let arr'' = withMArrayST_ arr (\ma -> swap_ ma ix1 ix2)
              index' arr'' ix1 `shouldBe` e2
              index' arr'' ix2 `shouldBe` e1

prop_outerSliceMArrayM
  :: forall r ix e
   . ( Index ix
     , Index (Lower ix)
     , Manifest r e
     , Eq (Array r (Lower ix) e)
     , Show (Array r (Lower ix) e)
     )
  => ArrNE r ix e
  -> Property
prop_outerSliceMArrayM (ArrNE arr) =
  forAll genIxInAndOut $ \(iIn, iOut) ->
    propIO $ do
      marr <- thawS arr
      (outerSliceMArrayM marr iIn >>= freezeS) `shouldReturn` arr !> iIn
      outerSliceMArrayM marr iOut `shouldThrow` (== IndexOutOfBoundsException (Sz nOuter) iOut)
  where
    (Sz nOuter, _) = unconsSz $ size arr
    genIxInAndOut = do
      let n = max 0 (nOuter - 1)
      iIn <- chooseInt (0, n)
      iOut <- oneof [chooseInt (minBound, -1), chooseInt (n, maxBound)]
      pure (iIn, iOut)

spec :: Spec
spec = do
  specMutableR @B @Int16
  specMutableR @BN @Int16
  specMutableR @BL @Int16
  atomicIntSpec @Ix1
  atomicIntSpec @Ix2
  atomicIntSpec @Ix3
  atomicIntSpec @Ix4
  atomicIntSpec @Ix5
