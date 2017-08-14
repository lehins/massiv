{-# LANGUAGE GADTs  #-}
module Data.Array.Massiv.Ops.ConstructSpec (spec) where


import           Data.Array.Massiv                  as M
import           Data.Array.Massiv.Common.IndexSpec (Sz (..))
import           Data.Array.Massiv.CommonSpec       (Arr (..),
                                                     assertSomeException)
import           Data.Array.Massiv.DelayedSpec      ()
import           Prelude                            as P
import           Prelude                            hiding (map)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Function


prop_rangeEqRangeStep1 :: Int -> Int -> Property
prop_rangeEqRangeStep1 from to = range from to === rangeStep from 1 to

prop_rangeEqEnumFromN :: Int -> Int -> Property
prop_rangeEqEnumFromN from to = range from to === enumFromN from (to - from)

prop_rangeStepEqEnumFromStepN :: Int -> NonZero Int -> Int -> Property
prop_rangeStepEqEnumFromStepN from (NonZero step) sz =
  rangeStep from step (from + step * sz) === enumFromStepN from step sz


prop_rangeStepExc :: Int -> Int -> Property
prop_rangeStepExc from to =
  assertSomeException (computeAs U (rangeStep from 0 to))


prop_toFromListIx1 :: Ix1 -> Fun Ix1 Int -> Bool
prop_toFromListIx1 sz f =
  let arr = makeArray Seq sz (apply f)
  in arr == fromListIx1As U Seq (toListIx1 arr)


prop_toFromListSIx2 :: Arr U Ix2 Int -> Property
prop_toFromListSIx2 (Arr arr) = arr === fromListIx2As U Seq (toListIx2 arr)


prop_excFromToListSIx2 :: [[Int]] -> Property
prop_excFromToListSIx2 ls2 =
  if P.null lsL || all (head lsL ==) lsL
     then label "Expected Success" $ resultLs === ls2
     else label "Expected Failure" $ assertSomeException resultLs
  where
    lsL = P.map length ls2
    resultLs = toListIx2 (fromListIx2As U Seq ls2)


prop_toFromListSIx3 :: Arr U Ix3 Int -> Property
prop_toFromListSIx3 (Arr arr) = arr === fromListIx3As U Seq (toListIx3 arr)


prop_excFromToListSIx3 :: [[[Int]]] -> Property
prop_excFromToListSIx3 ls3 =
  if P.null lsL ||
     (all (head lsL ==) lsL &&
      (P.null (head lsLL) || P.and (P.map (all (head (head lsLL) ==)) lsLL)))
    then classify True "Expected Success" $ resultLs === ls3
    else classify True "Expected Failure" $
         assertSomeException resultLs
  where
    resultLs = toListIx3 (fromListIx3As U Seq ls3)
    lsL = P.map length ls3
    lsLL = P.map (P.map length) ls3


prop_toFromListPIx2 :: Sz Ix2 -> Fun Ix2T Int -> Property
prop_toFromListPIx2 (Sz sz) f =
  arr === fromListIx2As U Par (toListIx2 arr)
  where
    arr = makeArray Par sz (toIxF2 (apply f))

prop_excFromToListPIx2 :: [[Int]] -> Property
prop_excFromToListPIx2 ls2 =
  if P.null lsL || all (head lsL ==) lsL
    then ls2 === toListIx2 (fromListIx2As U Par ls2)
    else assertSomeException res
  where
    res = toListIx2 (fromListIx2As U Par ls2)
    lsL = P.map length ls2

prop_toFromListPIx3 :: Sz Ix3 -> Fun Ix3T Int -> Property
prop_toFromListPIx3 (Sz sz) f =
  arr === fromListIx3As U Par (toListIx3 arr)
  where
    arr = makeArray Par sz (toIxF3 (apply f))

prop_excFromToListPIx3 :: [[[Int]]] -> Property
prop_excFromToListPIx3 ls3 =
  if P.null lsL ||
     (all (head lsL ==) lsL &&
      (P.null (head lsLL) || P.and (P.map (all (head (head lsLL) ==)) lsLL)))
    then classify True "Expected Success" $ resultLs === ls3
    else classify True "Expected Failure" $
         assertSomeException resultLs
  where
    resultLs = toListIx3 (fromListIx3As U Par ls3)
    lsL = P.map length ls3
    lsLL = P.map (P.map length) ls3


specIx1 :: Spec
specIx1 = do
  it "rangeEqRangeStep1" $ property prop_rangeEqRangeStep1
  it "rangeEqEnumFromN" $ property prop_rangeEqEnumFromN
  it "rangeStepEqEnumFromStepN" $ property prop_rangeStepEqEnumFromStepN
  it "rangeStepExc" $ property prop_rangeStepExc
  it "toFromListIx1" $ property prop_toFromListIx1

specIx2 :: Spec
specIx2 = do
  it "toFromListSIx2" $ property prop_toFromListSIx2
  it "excFromToListSIx2" $ property prop_excFromToListSIx2
  it "toFromListPIx2" $ property prop_toFromListPIx2
  it "excFromToListPIx2" $ property prop_excFromToListPIx2

specIx3 :: Spec
specIx3 = do
  it "toFromListSIx3" $ property prop_toFromListSIx3
  it "excFromToListSIx3" $ property prop_excFromToListSIx3
  it "toFromListPIx3" $ property prop_toFromListPIx3
  it "excFromToListPIx3" $ property prop_excFromToListPIx3


spec :: Spec
spec = do
  describe "DIM1" specIx1
  describe "DIM2" specIx2
  describe "DIM3" specIx3
