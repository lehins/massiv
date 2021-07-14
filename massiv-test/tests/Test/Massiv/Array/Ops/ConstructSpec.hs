{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Massiv.Array.Ops.ConstructSpec (spec) where

import Data.List as L
import Data.Massiv.Array as A
import Test.Massiv.Core
import qualified GHC.Exts as GHC (IsList(..))
import Prelude as P

prop_rangeEqRangeStep1 :: Int -> Int -> Property
prop_rangeEqRangeStep1 from to = range Seq from to === rangeStep' Par from 1 to

prop_rangeEqEnumFromN :: Int -> Int -> Property
prop_rangeEqEnumFromN from to = range Seq from to === enumFromN Par from (Sz (to - from))

prop_rangeStepEqEnumFromStepN :: Int -> NonZero Int -> Int -> Property
prop_rangeStepEqEnumFromStepN from (NonZero step) sz =
  rangeStep' Seq from step (from + step * sz) === enumFromStepN Par from step (Sz sz)


prop_rangeStepExc :: Int -> Int -> Property
prop_rangeStepExc from to =
  assertException
    selectImpreciseException
    (computeAs U (rangeStep' Seq from 0 to))

prop_toFromListIsList ::
     (Show (Array U ix Int), GHC.IsList (Array U ix Int), Index ix)
  => Proxy ix
  -> ArrNE U ix Int
  -> Property
prop_toFromListIsList _ (ArrNE arr) = arr === GHC.fromList (GHC.toList arr)


prop_toFromList ::
  forall ix . (Show (Array U ix Int), Ragged L ix Int)
  => Proxy ix
  -> ArrNE U ix Int
  -> Property
prop_toFromList _ (ArrNE arr) = comp === comp' .&&. arr === arr'
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
prop_excFromToListIx3 comp ls3
  | P.null (P.concat (P.concat ls3)) =
    classify True "Expected Success" $ counterexample (show arr) $ totalElem (size arr) === 0
  | P.all (head lsL ==) lsL && P.all (P.all (head (head lsLL) ==)) lsLL =
    classify True "Expected Success" $ counterexample (show arr) $ resultLs === ls3
  | otherwise = classify True "Expected Failure" $ assertSomeException arr
  where
    arr = fromLists' comp ls3 :: Array U Ix3 Int
    resultLs = toLists arr
    lsL = P.map P.length ls3
    lsLL = P.map (P.map P.length) ls3


specConstructIx1 :: Spec
specConstructIx1 = do
  it "toFromList" $ property (prop_toFromList (Proxy :: Proxy Ix1))
  it "toFromListIsList" $ property (prop_toFromListIsList (Proxy :: Proxy Ix1))
  it "rangeEqRangeStep1" $ property prop_rangeEqRangeStep1
  it "rangeEqEnumFromN" $ property prop_rangeEqEnumFromN
  it "rangeStepEqEnumFromStepN" $ property prop_rangeStepEqEnumFromStepN
  it "rangeStepExc" $ property prop_rangeStepExc

specConstructIx2 :: Spec
specConstructIx2 = do
  it "toFromList" $ property (prop_toFromList (Proxy :: Proxy Ix2))
  it "toFromListIsList" $ property (prop_toFromListIsList (Proxy :: Proxy Ix2))
  it "excFromToListIx2" $ property prop_excFromToListIx2

specConstructIx3 :: Spec
specConstructIx3 = do
  it "toFromList" $ property (prop_toFromList (Proxy :: Proxy Ix3))
  it "toFromListIsList" $ property (prop_toFromListIsList (Proxy :: Proxy Ix3))
  it "excFromToListIx3" $ property prop_excFromToListIx3

mkIntermediate :: Int -> Array U Ix1 Int
mkIntermediate t = A.fromList Seq [t + 50, t + 75]

initArr :: Array N Ix1 (Array U Ix1 Int)
initArr = makeArray Seq (Sz1 3) mkIntermediate

initArr2 :: Array N Ix2 (Array U Ix1 Int)
initArr2 = makeArray Seq (Sz 2) (\ (x :. y) -> mkIntermediate (x+y))

prop_unfoldrList :: Sz1 -> Fun Word (Int, Word) -> Word -> Property
prop_unfoldrList sz1 f i =
  conjoin $
  L.zipWith
    (===)
    (A.toList (computeAs P $ unfoldrS_ sz1 (apply f) i))
    (L.unfoldr (Just . apply f) i)

specExpand :: Spec
specExpand = do
  it "expandOuter" $ compute (expandOuter 2 A.index' initArr :: Array D Ix2 Int) `shouldBe`
    resize' (Sz2 2 3) (fromList Seq [50, 51, 52, 75, 76, 77] :: Array U Ix1 Int)
  it "expandInner" $ compute (expandInner 2 A.index' initArr :: Array D Ix2 Int) `shouldBe`
    resize' (Sz2 3 2) (fromList Seq [50, 75, 51, 76, 52, 77] :: Array U Ix1 Int)
  it "expandwithin" $ compute (expandWithin Dim1 2 A.index' initArr2 :: Array D Ix3 Int) `shouldBe`
    resize' (Sz 2) (fromList Seq [50, 75, 51, 76, 51, 76, 52, 77] :: Array U Ix1 Int)
  it "expandwithin'" $ compute (expandWithin' 1 2 A.index' initArr2 :: Array D Ix3 Int) `shouldBe`
    resize' (Sz 2) (fromList Seq [50, 75, 51, 76, 51, 76, 52, 77] :: Array U Ix1 Int)

spec :: Spec
spec = do
  describe "Ix1" specConstructIx1
  describe "Ix2" specConstructIx2
  describe "Ix3" specConstructIx3
  describe "Expand" specExpand
  describe "Unfolding" $ it "unfoldrS_" $ property prop_unfoldrList
