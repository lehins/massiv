{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs            #-}
module Data.Massiv.Array.Ops.ConstructSpec (spec) where

import           Data.Massiv.CoreArbitrary as A
import           Data.Proxy
import qualified GHC.Exts                   as GHC (IsList (..))
import           Prelude                    as P
import           Prelude                    hiding (map)
import           Test.Hspec
import           Test.QuickCheck


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

prop_toFromListIsList ::
     (Show (Array U ix Int), GHC.IsList (Array U ix Int), Index ix)
  => Proxy ix
  -> Arr U ix Int
  -> Property
prop_toFromListIsList _ (Arr arr) = arr === GHC.fromList (GHC.toList arr)


prop_toFromList ::
  forall ix . (Show (Array U ix Int), Nested LN ix Int, Nested L ix Int, Ragged L ix Int)
  => Proxy ix
  -> Arr U ix Int
  -> Property
prop_toFromList _ (Arr arr) = comp === comp' .&&. arr === arr'
  where comp = getComp arr
        arr' = fromLists' comp (toLists arr)
        comp' = getComp arr'


prop_excFromToListIx2 :: Comp -> [[Int]] -> Property
prop_excFromToListIx2 comp ls2 =
  if P.null lsL || P.all (head lsL ==) lsL
     then label "Expected Success" $ resultLs === ls2
     else label "Expected Failure" $ assertSomeException resultLs
  where
    lsL = P.map P.length ls2
    resultLs = toLists (fromLists' comp ls2 :: Array U Ix2 Int)


prop_excFromToListIx3 :: Comp -> [[[Int]]] -> Property
prop_excFromToListIx3 comp ls3 =
  if P.null (P.concat (P.concat ls3)) ||
     (P.all (head lsL ==) lsL &&
      (P.null (head lsLL) || P.and (P.map (P.all (head (head lsLL) ==)) lsLL)))
    then classify True "Expected Success" $
         counterexample (show arr) $ totalElem (size arr) === 0 .||. resultLs === ls3
    else classify True "Expected Failure" $ assertSomeException resultLs
  where
    arr = fromLists' comp ls3 :: Array U Ix3 Int
    resultLs = toLists arr
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

specIx2 :: Spec
specIx2 = do
  it "toFromList" $ property (prop_toFromList (Proxy :: Proxy Ix2))
  it "toFromListIsList" $ property (prop_toFromListIsList (Proxy :: Proxy Ix2))
  it "excFromToListIx2" $ property prop_excFromToListIx2

specIx3 :: Spec
specIx3 = do
  it "toFromList" $ property (prop_toFromList (Proxy :: Proxy Ix3))
  it "toFromListIsList" $ property (prop_toFromListIsList (Proxy :: Proxy Ix3))
  it "excFromToListIx3" $ property prop_excFromToListIx3


spec :: Spec
spec = do
  describe "Ix1" specIx1
  describe "Ix2" specIx2
  describe "Ix3" specIx3
