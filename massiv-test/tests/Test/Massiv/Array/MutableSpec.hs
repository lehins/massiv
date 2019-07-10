{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Test.Massiv.Array.MutableSpec (spec) where

import Data.Massiv.Array
import Test.Massiv.Core
import Test.Massiv.Core.Mutable


specMutableR ::
     forall e r.
     ( Show r
     , Eq (Array r Ix1 e), Show (Array r Ix1 e), Arbitrary (Array r Ix1 e), Mutable r Ix1 e
     , Eq (Array r Ix2 e), Show (Array r Ix2 e), Arbitrary (Array r Ix2 e), Mutable r Ix2 e
     , Eq (Array r Ix3 e), Show (Array r Ix3 e), Arbitrary (Array r Ix3 e), Mutable r Ix3 e
     , Eq (Array r Ix4 e), Show (Array r Ix4 e), Arbitrary (Array r Ix4 e), Mutable r Ix4 e
     , Eq (Array r Ix5 e), Show (Array r Ix5 e), Arbitrary (Array r Ix5 e), Mutable r Ix5 e
     )
  => r -> Spec
specMutableR r =
  describe (show r) $ do
    mutableSpec @r @Ix1 @e
    mutableSpec @r @Ix2 @e
    mutableSpec @r @Ix3 @e
    mutableSpec @r @Ix4 @e
    mutableSpec @r @Ix5 @e

spec :: Spec
spec = do
  specMutableR @Int B
  specMutableR @Int N
  specMutableR @Int S
  specMutableR @Int P
  specMutableR @Int U
