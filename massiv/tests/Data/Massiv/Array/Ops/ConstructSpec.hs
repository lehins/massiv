{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs            #-}
module Data.Massiv.Array.Ops.ConstructSpec (spec) where

import           Data.Massiv.Array.Manifest
import           Data.Massiv.Array.Mutable
import           Data.Massiv.Array.Ops
import           Data.Massiv.Ragged
import           Data.Massiv.CoreArbitrary
import           Data.Proxy
import qualified GHC.Exts                   as GHC (IsList (..))
import           Prelude                    as P
import           Prelude                    hiding (map)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Function


prop_rangeEqRangeStep1 :: Int -> Int -> Property
prop_rangeEqRangeStep1 from to = range Seq from to === rangeStep Par from 1 to

prop_rangeEqEnumFromN :: Int -> Int -> Property
prop_rangeEqEnumFromN from to = range Seq from to === enumFromN Par from (to - from)

prop_rangeStepEqEnumFromStepN :: Int -> NonZero Int -> Int -> Property
prop_rangeStepEqEnumFromStepN from (NonZero step) sz =
  rangeStep Seq from step (from + step * sz) === enumFromStepN Par from step sz


prop_rangeStepExc :: Int -> Int -> Property
prop_rangeStepExc from to =
  assertSomeException (computeAs U (rangeStep Seq from 0 to))


prop_toFromListIx1 :: Ix1 -> Fun Ix1 Int -> Bool
prop_toFromListIx1 sz f =
  let arr = makeArray Seq sz (apply f)
  in arr == fromListIx1As U Seq (toListIx1 arr)


prop_toFromListIsList ::
     (Show (Array U ix Int), GHC.IsList (Array U ix Int), Index ix)
  => Proxy ix
  -> Arr U ix Int
  -> Property
prop_toFromListIsList _ (Arr arr) = arr === GHC.fromList (GHC.toList arr)

prop_toFromList ::
  forall ix . (Show (Array U ix Int), Ragged LN ix Int, Construct L ix Int, Index ix)
  => Proxy ix
  -> Arr U ix Int
  -> Property
prop_toFromList _ (Arr arr) = arr === fromList (getComp arr) (toList arr :: [ListItem ix Int])


prop_excFromToListSIx2 :: [[Int]] -> Property
prop_excFromToListSIx2 ls2 =
  if P.null lsL || all (head lsL ==) lsL
     then label "Expected Success" $ resultLs === ls2
     else label "Expected Failure" $ assertSomeException resultLs
  where
    lsL = P.map P.length ls2
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
    lsL = P.map P.length ls3
    lsLL = P.map (P.map P.length) ls3


prop_toFromListPIx2 :: Sz Ix2 -> Fun Ix2T Int -> Property
prop_toFromListPIx2 (Sz sz) f =
  arr === fromListIx2As U Par (toListIx2 arr)
  where
    arr = makeArray Par sz (apply f . fromIx2)

prop_excFromToListPIx2 :: [[Int]] -> Property
prop_excFromToListPIx2 ls2 =
  if P.null lsL || all (head lsL ==) lsL
    then ls2 === toListIx2 (fromListIx2As U Par ls2)
    else assertSomeException res
  where
    res = toListIx2 (fromListIx2As U Par ls2)
    lsL = P.map P.length ls2

prop_toFromListPIx3 :: Sz Ix3 -> Fun Ix3T Int -> Property
prop_toFromListPIx3 (Sz sz) f =
  arr === fromListIx3As U Par (toListIx3 arr)
  where
    arr = makeArray Par sz (apply f . fromIx3)

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
    lsL = P.map P.length ls3
    lsLL = P.map (P.map P.length) ls3


specIx1 :: Spec
specIx1 = do
  it "toFromList" $ property (prop_toFromList (Proxy :: Proxy Ix1))
  it "toFromListIsList" $ property (prop_toFromListIsList (Proxy :: Proxy Ix1))
  it "rangeEqRangeStep1" $ property prop_rangeEqRangeStep1
  it "rangeEqEnumFromN" $ property prop_rangeEqEnumFromN
  it "rangeStepEqEnumFromStepN" $ property prop_rangeStepEqEnumFromStepN
  it "rangeStepExc" $ property prop_rangeStepExc
  it "toFromListIx1" $ property prop_toFromListIx1

specIx2 :: Spec
specIx2 = do
  it "toFromList" $ property (prop_toFromList (Proxy :: Proxy Ix2))
  it "toFromListIsList" $ property (prop_toFromListIsList (Proxy :: Proxy Ix2))
  it "excFromToListSIx2" $ property prop_excFromToListSIx2
  it "toFromListPIx2" $ property prop_toFromListPIx2
  it "excFromToListPIx2" $ property prop_excFromToListPIx2

specIx3 :: Spec
specIx3 = do
  it "toFromList" $ property (prop_toFromList (Proxy :: Proxy Ix2))
  it "toFromListIsList" $ property (prop_toFromListIsList (Proxy :: Proxy Ix3))
  it "toFromListSIx3" $ property prop_toFromListSIx3
  it "excFromToListSIx3" $ property prop_excFromToListSIx3
  it "toFromListPIx3" $ property prop_toFromListPIx3
  it "excFromToListPIx3" $ property prop_excFromToListPIx3


spec :: Spec
spec = do
  describe "DIM1" specIx1
  describe "DIM2" specIx2
  describe "DIM3" specIx3
