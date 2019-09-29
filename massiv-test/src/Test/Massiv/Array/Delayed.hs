{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Test.Massiv.Array.Delayed
  ( -- * Spec for safe Mutable instance
    delayedStreamSpec
    -- * Useful properties for testing toList conversion
  , prop_toStream
  , prop_toStreamIsList
  , prop_toStreamFoldable
  , prop_filterS
  , prop_mapMaybeS
  , prop_takeDrop
  , prop_unfoldr
  ) where


import Data.Maybe as M
import Data.Foldable as F
import Data.Massiv.Array as A
import qualified Data.Massiv.Array.Manifest.Vector.Stream as S
import Test.Massiv.Core.Common ()
import Test.Massiv.Utils as T
import qualified GHC.Exts as Exts
import Data.List as L

compareAsListAndLoaded ::
     (Eq e, Show e, Foldable (Array r' Ix1), Load r' Ix1 e) => Array r' Ix1 e -> [e] -> Property
compareAsListAndLoaded str ls =
  F.toList str === ls .&&. computeAs B str === A.fromList Seq ls

-- | Compare `toStream` and `A.toList`
prop_toStream ::
     forall r ix e. (Source r ix e, Stream r ix e, Show e, Eq e)
  => Array r ix e
  -> Property
prop_toStream arr =
  A.toList arr === S.toList (toStream arr)

-- | Compare `toStream` and `Exts.toList`
prop_toStreamIsList ::
     forall r e.
     (Exts.Item (Array r Ix1 e) ~ e, Exts.IsList (Array r Ix1 e), Stream r Ix1 e, Show e, Eq e)
  => Array r Ix1 e
  -> Property
prop_toStreamIsList arr =
  Exts.toList arr === S.toList (toStream arr)

-- | Compare `toStream` and `F.toList`
prop_toStreamFoldable ::
     forall r ix e.
     (Foldable (Array r ix), Stream r ix e, Show e, Eq e)
  => Array r ix e
  -> Property
prop_toStreamFoldable arr =
  F.toList arr === S.toList (toStream arr)


prop_filterS ::
     forall r ix e. (Eq e, Show e, Stream r ix e, Foldable (Array r ix))
  => Array r ix e
  -> Fun e Bool
  -> Property
prop_filterS arr f =
  compareAsListAndLoaded (A.filterS (apply f) arr) (L.filter (apply f) (F.toList arr))

prop_mapMaybeS ::
     forall r ix e a. (Eq a, Show a, Stream r ix e, Foldable (Array r ix))
  => Array r ix e
  -> Fun e (Maybe a)
  -> Property
prop_mapMaybeS arr f =
  compareAsListAndLoaded (A.mapMaybeS (apply f) arr) (M.mapMaybe (apply f) (F.toList arr))


prop_unfoldr ::
     forall e s. (Eq e, Show e)
  => Fun s (Maybe (e, s))
  -> s
  -> NonNegative Int
  -> Property
prop_unfoldr f s0 (NonNegative n) =
  compareAsListAndLoaded
    (A.takeS (Sz n) (A.unfoldr (apply f) s0))
    (L.take n (L.unfoldr (apply f) s0))

prop_unfoldrN ::
     forall e s. (Eq e, Show e)
  => Fun s (Maybe (e, s))
  -> s
  -> Int
  -> Property
prop_unfoldrN f s0 n =
  compareAsListAndLoaded (A.unfoldrN (Sz n) (apply f) s0) (L.take n (L.unfoldr (apply f) s0))

prop_takeDrop ::
     forall r ix e. (Eq e, Show e, Stream r ix e, Foldable (Array r ix))
  => Array r ix e
  -> Int
  -> Int
  -> Property
prop_takeDrop arr t d =
  Exts.toList (A.takeS (Sz t) (A.dropS (Sz d) arr)) === take t (drop d (F.toList arr)) .&&.
  Exts.toList (A.dropS (Sz d) (A.takeS (Sz t) arr)) ===
  drop d (take t (F.toList arr))


delayedStreamSpec :: Spec
delayedStreamSpec =
  describe "DS Spec" $ do
    it "filterS" $ property (prop_filterS @DS @Ix1 @Int)
    it "mapMaybeS" $ property (prop_mapMaybeS @DS @Ix1 @Int @Word)
    it "unfoldr" $ property (prop_unfoldr @Int @Word)
    it "unfoldrN" $ property (prop_unfoldrN @Int @Word)
    it "takeDrop" $ property (prop_takeDrop @DS @Ix1 @Int)
