{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
module Data.Massiv.Array.Delayed.WindowedSpec (spec) where

import           Data.Massiv.Array.Delayed
import           Data.Massiv.Array.Unsafe
import           Data.Massiv.CoreArbitrary as A
import           Data.Proxy
import           Data.Typeable
import           Test.Hspec
import           Test.QuickCheck


data ArrDW ix e = ArrDW (Array D ix e) (Array DW ix e)

instance (Show ix, Index ix, Show (Array D ix e), Show (Array DW ix e)) => Show (ArrDW ix e) where
  show (ArrDW d dw) =
    "Delayed: \n" ++
    show d ++
    "\nCorresponding Windowed: \n" ++
    --show dw ++
    windowInfo ++
    "\nStride: (" ++ show (getStride dw) ++ ")" ++ "\nComputed: (" ++ show (getComp dw) ++ ")"
    where
      windowInfo =
        maybe
          "\n No Window"
          (\Window {windowStart, windowSize} ->
             "\n With Window starting index (" ++
             show windowStart ++ ") and size (" ++ show windowSize ++ ")") $
        getWindow dw

instance (Arbitrary ix, CoArbitrary ix, Index ix, Arbitrary e, Typeable e) =>
         Arbitrary (ArrDW ix e) where
  arbitrary = do
    ArrTiny (arr' :: Array D ix e) <- arbitrary
    let arr = setComp Seq arr'
    let sz = size arr
    stride <- liftIndex abs <$> arbitrary
    ArrDW arr <$>
      if totalElem sz == 0
        then return $ (setStride stride $ unsafeMakeArray (getComp arr) sz (unsafeIndex arr))
        else do
          wix <- flip (liftIndex2 mod) sz <$> arbitrary
          wsz <- liftIndex (+1) . flip (liftIndex2 mod) (liftIndex2 (-) sz wix) <$> arbitrary
          return $ setStride stride $ makeWindowedArray arr wix wsz (unsafeIndex arr)


prop_EqDelayed ::
     (Ragged L ix Int, Load DW ix Int) => Proxy ix -> ArrDW ix Int -> Property
prop_EqDelayed _ (ArrDW arrD arrDW) =
  computeAs P (backpermute sz ixMap arrD) === computeAs P arrDW
  where
    stride = getStride arrDW
    sz = size arrDW
    ixMap = liftIndex2 (*) stride


spec :: Spec
spec = do
  describe "Equivalency with Delayed" $ do
    it "Ix1" $ property $ prop_EqDelayed (Proxy :: Proxy Ix1)
    it "Ix2" $ property $ prop_EqDelayed (Proxy :: Proxy Ix2)
    it "Ix3" $ property $ prop_EqDelayed (Proxy :: Proxy Ix3)
    -- it "Ix4" $ property $ prop_EqDelayed (Proxy :: Proxy Ix4)
    -- it "Ix5" $ property $ prop_EqDelayed (Proxy :: Proxy Ix5)
    -- it "Ix2T" $ property $ prop_EqDelayed (Proxy :: Proxy Ix2T)
    -- it "Ix3T" $ property $ prop_EqDelayed (Proxy :: Proxy Ix3T)
    -- it "Ix4T" $ property $ prop_EqDelayed (Proxy :: Proxy Ix4T)
    -- it "Ix5T" $ property $ prop_EqDelayed (Proxy :: Proxy Ix5T)
