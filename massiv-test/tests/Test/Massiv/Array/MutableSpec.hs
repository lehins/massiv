{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Test.Massiv.Array.MutableSpec (spec) where

import Data.Massiv.Array
import Test.Massiv.Core
import Test.Massiv.Core.Mutable

type MutableSpec r ix e
   = ( Eq (Array r ix e)
     , Show (Array r ix e)
     , Eq (Array (EltRepr r Ix1) Ix1 e)
     , Show (Array (EltRepr r Ix1) Ix1 e)
     , Extract r Ix1 e
     , Resize r ix
     , Arbitrary (Array r ix e)
     , Mutable r ix e
     , Construct r ix e)

specMutableR ::
     forall e r.
     ( Show r
     , Show e
     , Arbitrary e
     , MutableSpec r Ix1 e
     , MutableSpec r Ix2 e
     , MutableSpec r Ix3 e
     , MutableSpec r Ix4 e
     , MutableSpec r Ix5 e
     )
  => r
  -> Spec
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
