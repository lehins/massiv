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
  , prop_sfilter
  , prop_smapMaybe
  , prop_takeDrop
  , prop_sunfoldr
  -- * Random reimplementations
  , stackSlices'
  ) where


import Data.Maybe as M
import Data.Foldable as F
import Data.Massiv.Array as A
import qualified Data.Massiv.Vector.Stream as S
import Test.Massiv.Core.Common ()
import Test.Massiv.Utils as T
import qualified GHC.Exts as Exts
import Data.List as L

-- | Alternative implementation of `stackSlicesM` with `concat'`. Useful for testing and benchmarks
stackSlices' ::
     (Functor f, Foldable f, Resize r, Source r e, Index ix, Load r (Lower ix) e)
  => Dim
  -> f (Array r (Lower ix) e)
  -> Array DL ix e
stackSlices' dim arrsF =
  let fixupSize arr = resize' (Sz (insertDim' (unSz (size arr)) dim 1)) arr
   in concat' dim $ fmap fixupSize arrsF


compareAsListAndLoaded ::
     (Eq e, Show e, Foldable (Array r' Ix1), Load r' Ix1 e) => Array r' Ix1 e -> [e] -> Property
compareAsListAndLoaded str ls =
  F.toList str === ls .&&. computeAs B str === A.fromList Seq ls

-- | Compare `toStream` and `A.toList`
prop_toStream ::
     forall r ix e. (Source r e, Stream r ix e, Show e, Eq e)
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


prop_sfilter ::
     forall r ix e. (Eq e, Show e, Stream r ix e, Foldable (Array r ix))
  => Array r ix e
  -> Fun e Bool
  -> Property
prop_sfilter arr f =
  compareAsListAndLoaded (A.sfilter (apply f) arr) (L.filter (apply f) (F.toList arr))

prop_smapMaybe ::
     forall r ix e a. (Eq a, Show a, Stream r ix e, Foldable (Array r ix))
  => Array r ix e
  -> Fun e (Maybe a)
  -> Property
prop_smapMaybe arr f =
  compareAsListAndLoaded (A.smapMaybe (apply f) arr) (M.mapMaybe (apply f) (F.toList arr))


prop_sunfoldr ::
     forall e s. (Eq e, Show e)
  => Fun s (Maybe (e, s))
  -> s
  -> NonNegative Int
  -> Property
prop_sunfoldr f s0 (NonNegative n) =
  compareAsListAndLoaded
    (A.stake (Sz n) (A.sunfoldr (apply f) s0))
    (L.take n (L.unfoldr (apply f) s0))

prop_sunfoldrN ::
     forall e s. (Eq e, Show e)
  => Fun s (Maybe (e, s))
  -> s
  -> Int
  -> Property
prop_sunfoldrN f s0 n =
  compareAsListAndLoaded (A.sunfoldrN (Sz n) (apply f) s0) (L.take n (L.unfoldr (apply f) s0))


prop_stakesDrop ::
     forall r e.
     ( Eq e
     , Show e
     , Stream r Ix1 e
     , Foldable (Array r Ix1)
     )
  => Vector r e
  -> Int
  -> Int
  -> Property
prop_stakesDrop arr t d =
  conjoin
    [ stoList (A.stake (Sz t) (A.sdrop (Sz d) arr)) === L.take t (L.drop d (F.toList arr))
    , stoList (A.sdrop (Sz d) (A.stake (Sz t) arr)) === L.drop d (L.take t (F.toList arr))
    ]

prop_takeDrop ::
     forall r e.
     ( Eq e
     , Show e
     , Source r e
     , Foldable (Array r Ix1)
     )
  => Vector r e
  -> Int
  -> Int
  -> Property
prop_takeDrop arr t d =
  conjoin
    [ A.toList (A.take (Sz t) (A.drop (Sz d) arr)) === L.take t (L.drop d (F.toList arr))
    , A.toList (A.drop (Sz d) (A.take (Sz t) arr)) === L.drop d (L.take t (F.toList arr))
    ]

delayedStreamSpec :: Spec
delayedStreamSpec = do
  describe "D Spec" $
    it "takeDrop" $ property (prop_takeDrop @D @Int)
  describe "DS Spec" $ do
    it "sfilter" $ property (prop_sfilter @DS @Ix1 @Int)
    it "smapMaybe" $ property (prop_smapMaybe @DS @Ix1 @Int @Word)
    it "sunfoldr" $ property (prop_sunfoldr @Int @Word)
    it "sunfoldrN" $ property (prop_sunfoldrN @Int @Word)
    it "stakesDrop" $ property (prop_stakesDrop @DS @Int)
