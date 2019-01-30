{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Data.Massiv.Array.Ops.MapSpec (spec) where

import           Control.Monad.ST
import           Data.Foldable             as F
import           Data.Massiv.Array.Unsafe
import           Data.Massiv.CoreArbitrary as A
import           Data.Proxy
import           Prelude                   as P
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Function

prop_zipUnzip ::
     (Arbitrary ix, CoArbitrary ix, Index ix, Show (Array D ix Int))
  => proxy ix
  -> Array D ix Int
  -> Array D ix Int
  -> Property
prop_zipUnzip _ arr1 arr2 =
  (extract' zeroIndex sz arr1, extract' zeroIndex sz arr2) === A.unzip (A.zip arr1 arr2)
  where sz = Sz (liftIndex2 min (unSz (size arr1)) (unSz (size arr2)))

prop_zipFlip ::
     (Arbitrary ix, CoArbitrary ix, Index ix, Show (Array D ix Int), Show (Array D ix (Int, Int)))
  => proxy ix
  -> Array D ix Int
  -> Array D ix Int
  -> Property
prop_zipFlip _ arr1 arr2 =
  A.zip arr1 arr2 ===
  A.map (\(e2, e1) -> (e1, e2)) (A.zip arr2 arr1)

prop_zipUnzip3 ::
     (Arbitrary ix, CoArbitrary ix, Index ix, Show (Array D ix Int))
  => proxy ix
  -> Array D ix Int
  -> Array D ix Int
  -> Array D ix Int
  -> Property
prop_zipUnzip3 _ arr1 arr2 arr3 =
  (extract' zeroIndex sz arr1, extract' zeroIndex sz arr2, extract' zeroIndex sz arr3) ===
  A.unzip3 (A.zip3 arr1 arr2 arr3)
  where
    sz =
      Sz (liftIndex2 min (liftIndex2 min (unSz (size arr1)) (unSz (size arr2))) (unSz (size arr3)))

prop_zipFlip3 ::
     ( Arbitrary ix
     , CoArbitrary ix
     , Index ix
     , Show (Array D ix Int)
     , Show (Array D ix (Int, Int, Int))
     )
  => proxy ix
  -> Array D ix Int
  -> Array D ix Int
  -> Array D ix Int
  -> Property
prop_zipFlip3 _ arr1 arr2 arr3 =
  A.zip3 arr1 arr2 arr3 === A.map (\(e3, e2, e1) -> (e1, e2, e3)) (A.zip3 arr3 arr2 arr1)



prop_itraverseA ::
     ( Arbitrary ix
     , CoArbitrary ix
     , Index ix
     , Function ix
     , Show (Array U ix Int)
     )
  => proxy ix
  -> Array D ix Int
  -> Fun (ix, Int) Int
  -> Property
prop_itraverseA _ arr fun =
  alt_imapM (\ix -> Just . applyFun2 fun ix) arr ===
  itraverseAR U (\ix -> Just . applyFun2 fun ix) arr


mapSpec ::
     ( Arbitrary ix
     , CoArbitrary ix
     , Index ix
     , Function ix
     , Show (Array U ix Int)
     , Show (Array D ix Int)
     , Show (Array D ix (Int, Int))
     , Show (Array D ix (Int, Int, Int))
     )
  => proxy ix -> Spec
mapSpec proxy = do
  describe "Zipping" $ do
    it "zipUnzip" $ property $ prop_zipUnzip proxy
    it "zipFlip" $ property $ prop_zipFlip proxy
    it "zipUnzip3" $ property $ prop_zipUnzip3 proxy
    it "zipFlip3" $ property $ prop_zipFlip3 proxy
  describe "Traversing" $ do
    it "itraverseA" $ property $ prop_itraverseA proxy

spec :: Spec
spec = do
  describe "Ix1" $ mapSpec (Proxy :: Proxy Ix1)
  describe "Ix2" $ mapSpec (Proxy :: Proxy Ix2)
  describe "Ix3" $ mapSpec (Proxy :: Proxy Ix3)
  describe "Ix4" $ mapSpec (Proxy :: Proxy Ix4)



alt_imapM
  :: (Applicative f, Mutable r2 t1 b, Source r1 t1 t2) =>
     (t1 -> t2 -> f b) -> Array r1 t1 t2 -> f (Array r2 t1 b)
alt_imapM f arr = fmap loadList $ P.traverse (uncurry f) $ foldrS (:) [] (zipWithIndex arr)
  where
    g ix e acc = (: acc) <$> f ix e
    loadList xs =
      runST $ do
        marr <- unsafeNew (size arr)
        _ <- F.foldlM (\i e -> unsafeLinearWrite marr i e >> return (i + 1)) 0 xs
        unsafeFreeze (getComp arr) marr
    {-# INLINE loadList #-}

zipWithIndex :: forall r ix e . Source r ix e => Array r ix e -> Array D ix (ix, e)
zipWithIndex arr = A.zip (range Seq zeroIndex (unSz (size arr))) arr
{-# INLINE zipWithIndex #-}
