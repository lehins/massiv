{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Data.Massiv.Array.Ops.MapSpec (spec) where

import           Control.Monad.ST
import           Data.Foldable             as F
import           Data.Massiv.Array.Unsafe
import           Data.Massiv.CoreArbitrary as A
import           Prelude                   as P


-- | For backwards compatibility with QuickCheck <= 2.9.2
applyFun2' :: Fun (a, b) c -> (a -> b -> c)
applyFun2' (Fun _ f) a b = f (a, b)

prop_zipUnzip ::
     (Index ix, Show (Array D ix Int))
  => Array D ix Int
  -> Array D ix Int
  -> Property
prop_zipUnzip arr1 arr2 =
  (extract' zeroIndex sz arr1, extract' zeroIndex sz arr2) === A.unzip (A.zip arr1 arr2)
  where sz = Sz (liftIndex2 min (unSz (size arr1)) (unSz (size arr2)))

prop_zipFlip ::
     (Index ix, Show (Array D ix (Int, Int)))
  => Array D ix Int
  -> Array D ix Int
  -> Property
prop_zipFlip arr1 arr2 =
  A.zip arr1 arr2 ===
  A.map (\(e2, e1) -> (e1, e2)) (A.zip arr2 arr1)

prop_zipUnzip3 ::
     (Index ix, Show (Array D ix Int))
  => Array D ix Int
  -> Array D ix Int
  -> Array D ix Int
  -> Property
prop_zipUnzip3 arr1 arr2 arr3 =
  (extract' zeroIndex sz arr1, extract' zeroIndex sz arr2, extract' zeroIndex sz arr3) ===
  A.unzip3 (A.zip3 arr1 arr2 arr3)
  where
    sz =
      Sz (liftIndex2 min (liftIndex2 min (unSz (size arr1)) (unSz (size arr2))) (unSz (size arr3)))

prop_zipFlip3 ::
     (Index ix, Show (Array D ix (Int, Int, Int)))
  => Array D ix Int
  -> Array D ix Int
  -> Array D ix Int
  -> Property
prop_zipFlip3 arr1 arr2 arr3 =
  A.zip3 arr1 arr2 arr3 === A.map (\(e3, e2, e1) -> (e1, e2, e3)) (A.zip3 arr3 arr2 arr1)



prop_itraverseA ::
     (Index ix, Show (Array U ix Int)) => Array D ix Int -> Fun (ix, Int) Int -> Property
prop_itraverseA arr fun =
  alt_imapM (\ix -> Just . applyFun2' fun ix) arr ===
  itraverseAR U (\ix -> Just . applyFun2' fun ix) arr


mapSpec ::
     forall ix.
     ( Arbitrary ix
     , CoArbitrary ix
     , Index ix
     , Function ix
     , Show (Array U ix Int)
     , Show (Array D ix Int)
     , Show (Array D ix (Int, Int))
     , Show (Array D ix (Int, Int, Int))
     )
  => Spec
mapSpec = do
  describe "Zipping" $ do
    it "zipUnzip" $ property $ prop_zipUnzip @ix
    it "zipFlip" $ property $ prop_zipFlip @ix
    it "zipUnzip3" $ property $ prop_zipUnzip3 @ix
    it "zipFlip3" $ property $ prop_zipFlip3 @ix
  describe "Traversing" $ do
    it "itraverseA" $ property $ prop_itraverseA @ix

spec :: Spec
spec = do
  describe "Ix1" $ mapSpec @Ix1
  describe "Ix2" $ mapSpec @Ix2
  describe "Ix3" $ mapSpec @Ix3
  describe "Ix4" $ mapSpec @Ix4



alt_imapM
  :: (Applicative f, Mutable r2 t1 b, Source r1 t1 t2) =>
     (t1 -> t2 -> f b) -> Array r1 t1 t2 -> f (Array r2 t1 b)
alt_imapM f arr = fmap loadList $ P.traverse (uncurry f) $ foldrS (:) [] (zipWithIndex arr)
  where
    loadList xs =
      runST $ do
        marr <- unsafeNew (size arr)
        _ <- F.foldlM (\i e -> unsafeLinearWrite marr i e >> return (i + 1)) 0 xs
        unsafeFreeze (getComp arr) marr
    {-# INLINE loadList #-}

zipWithIndex :: forall r ix e . Source r ix e => Array r ix e -> Array D ix (ix, e)
zipWithIndex arr = A.zip (range Seq zeroIndex (unSz (size arr))) arr
{-# INLINE zipWithIndex #-}
