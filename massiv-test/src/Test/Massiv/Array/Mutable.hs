{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Massiv.Array.Mutable
  ( -- * Spec for safe Mutable instance
    prop_iMapiMapM
  , mutableSpec
  ) where

import Data.Functor.Identity
import Data.Massiv.Array as A
import Test.Massiv.Core.Common ()
import Test.Massiv.Utils


-- prop_MapMapM :: forall r ix(Show (Array r ix Word), Eq (Array r ix Word), Mutable r ix Word) =>
--                 Fun Word Word -> ArrTiny D ix Word -> Property
-- prop_MapMapM r _ f (ArrTiny arr) =
--   computeAs r (A.map (apply f) arr) === runIdentity (A.mapMR r (return . apply f) arr)

prop_iMapiMapM ::
     forall r ix e. (Show (Array r ix e), Eq (Array r ix e), Mutable r ix e)
  => Fun (ix, e) e
  -> Array D ix e
  -> Property
prop_iMapiMapM f arr =
  (compute (A.imap (curry (apply f)) arr) :: Array r ix e) ===
  runIdentity (A.imapM (\ix e -> pure $ apply f (ix, e)) arr)


mutableSpec ::
     forall r ix e.
     ( Show (Array D ix e)
     , Show (Array r ix e)
     , Eq (Array r ix e)
     , Typeable e
     , Show e
     , Mutable r ix e
     , CoArbitrary ix
     , Arbitrary e
     , CoArbitrary e
     , Arbitrary ix
     , Typeable ix
     , Function ix
     , Function e
     )
  => Spec
mutableSpec = do
  describe ("Mutable " ++ showsArrayType @r @ix @e " (Safe)") $ do
    it "map == mapM" $ property $ prop_iMapiMapM @r @ix @e
