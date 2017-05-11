{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Array.Massiv.SchedulerSpec (spec) where

import           Control.Exception.Base
import           Data.Array.Massiv             as M
import           Data.Array.Massiv.CommonSpec  (ArrIx (..))
import           Data.Array.Massiv.DelayedSpec ()
import           Data.Array.Massiv.Scheduler
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic


prop_CatchDivideByZero :: ArrIx D DIM2 Int -> Property
prop_CatchDivideByZero (ArrIx arr ix) =
  monadicIO $
  run $
  catchJust
    (\exc ->
       case exc of
         DivideByZero -> Just True
         _            -> Nothing)
    ((computeUnboxedP $
      M.imap
        (\ix' x ->
           if ix == ix'
             then x `div` 0
             else x)
        arr) `seq`
     return True)
    return


prop_CatchNested :: ArrIx D DIM1 (Array D DIM1 Int) -> Property
prop_CatchNested (ArrIx arr ix) =
  monadicIO $
  run $
  catchJust
    (\exc ->
       case exc of
         DivideByZero -> Just True
         _            -> Nothing)
    ((M.sumP $
      (M.map M.sumP) $
      (M.imap
         (\ix' iarr ->
            if ix == ix'
              then M.map (`div` 0) iarr
              else iarr))
        arr) `seq`
     return True)
    return


spec :: Spec
spec = do
  describe "Exceptions" $ do
    it "CatchDivideByZero" $ property prop_CatchDivideByZero
    it "CatchNested" $ property prop_CatchNested

