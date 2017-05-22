{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Array.Massiv.Ops.ConstructSpec (spec) where


import           Data.Array.Massiv             as M
import           Data.Array.Massiv.DelayedSpec ()
import           Prelude                       as P
import           Prelude                       hiding (map)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Function


prop_rangeEqRangeStep1 :: Int -> Int -> Bool
prop_rangeEqRangeStep1 from to = range from to == rangeStep from 1 to

prop_rangeEqEnumFromN :: Int -> Int -> Bool
prop_rangeEqEnumFromN from to = range from to == enumFromN from (to - from)

prop_rangeStepEqEnumFromStepN :: Int -> NonZero Int -> Int -> Bool
prop_rangeStepEqEnumFromStepN from (NonZero step) sz =
  rangeStep from step (from + step * sz) == enumFromStepN from step sz


prop_rangeStepExc :: Int -> Int -> Property
prop_rangeStepExc from to = expectFailure $ M.sumS (rangeStep from 0 to) == 0


prop_toFromList1D :: DIM1 -> Fun DIM1 Int -> Bool
prop_toFromList1D sz f =
  let arr = makeArray sz (apply f)
  in arr == fromListAsS1D U (toListS1D arr)


prop_toFromListS2D :: DIM2 -> Fun DIM2 Int -> Property
prop_toFromListS2D sz' f =
  sz == zeroIndex || totalElem sz /= 0 ==>
  arr == fromListAsS2D U (toListS2D arr)
  where arr = makeArray sz' (apply f)
        sz = size arr


prop_excFromToListS2D :: [[Int]] -> Property
prop_excFromToListS2D ls2 =
  let lsL = P.map length ls2
  in (if P.null lsL || all (head lsL ==) lsL
        then property
        else expectFailure)
       (toListS2D (fromListAsS2D U ls2) == ls2)


prop_toFromListS3D :: DIM3 -> Fun DIM3 Int -> Property
prop_toFromListS3D sz' f =
  sz == zeroIndex || totalElem sz /= 0 ==>
  arr == fromListAsS3D U (toListS3D arr)
  where arr = makeArray sz' (apply f)
        sz = size arr

prop_excFromToListS3D :: [[[Int]]] -> Property
prop_excFromToListS3D ls3 =
  (if P.null lsL ||
      (all (head lsL ==) lsL &&
       (P.null (head lsLL) || P.and (P.map (all (head (head lsLL) ==)) lsLL)))
     then property
     else expectFailure)
    (toListS3D (fromListAsS3D U ls3) == ls3)
  where
    lsL = P.map length ls3
    lsLL = P.map (P.map length) ls3


prop_toFromListP2D :: DIM2 -> Fun DIM2 Int -> Property
prop_toFromListP2D sz' f =
  sz == zeroIndex || totalElem sz /= 0 ==>
  computeUnboxedP arr == fromListAsP2D U (toListS2D arr)
  where arr = makeArray2D sz' (apply f)
        sz = size arr

prop_excFromToListP2D :: [[Int]] -> Property
prop_excFromToListP2D ls2 =
  let lsL = P.map length ls2
  in (if P.null lsL || all (head lsL ==) lsL
        then property
        else expectFailure)
       (toListS2D (fromListAsP2D U ls2) == ls2)


prop_toFromListP3D :: DIM3 -> Fun DIM3 Int -> Property
prop_toFromListP3D sz' f =
  sz == zeroIndex || totalElem sz /= 0 ==>
  computeUnboxedP arr == fromListAsP3D U (toListS3D arr)
  where arr = makeArray3D sz' (apply f)
        sz = size arr

prop_excFromToListP3D :: [[[Int]]] -> Property
prop_excFromToListP3D ls3 =
  (if P.null lsL ||
      (all (head lsL ==) lsL &&
       (P.null (head lsLL) || P.and (P.map (all (head (head lsLL) ==)) lsLL)))
     then property
     else expectFailure)
    (toListS3D (fromListAsP3D U ls3) == ls3)
  where
    lsL = P.map length ls3
    lsLL = P.map (P.map length) ls3


spec1D :: Spec
spec1D = do
  it "rangeEqRangeStep1" $ property prop_rangeEqRangeStep1
  it "rangeEqEnumFromN" $ property prop_rangeEqEnumFromN
  it "rangeStepEqEnumFromStepN" $ property prop_rangeStepEqEnumFromStepN
  it "rangeStepExc" $ property prop_rangeStepExc
  it "toFromList1D" $ property prop_toFromList1D

spec2D :: Spec
spec2D = do
  it "toFromListS2D" $ property prop_toFromListS2D
  it "excFromToListS2D" $ property prop_excFromToListS2D
  it "toFromListP2D" $ property prop_toFromListP2D
  it "excFromToListP2D" $ property prop_excFromToListP2D

spec3D :: Spec
spec3D = do
  it "toFromListS3D" $ property prop_toFromListS3D
  it "excFromToListS3D" $ property prop_excFromToListS3D
  it "toFromListP3D" $ property prop_toFromListP3D
  it "excFromToListP3D" $ property prop_excFromToListP3D


spec :: Spec
spec = do
  describe "DIM1" spec1D
  describe "DIM2" spec2D
  describe "DIM3" spec3D
