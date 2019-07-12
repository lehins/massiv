{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Massiv.Array.Delayed.PushSpec (spec) where

-- import Data.Massiv.Array.Delayed
-- import Data.Massiv.Array.Unsafe
-- import Data.Massiv.Array as A
import Test.Massiv.Core


-- prop_upsampleDifferentDefault ::
--      Proxy ix -> Comp -> SzIx ix -> Int -> Maybe Int -> Property
-- prop_upsampleDifferentDefault _ comp (SzIx sz ix) v mDef =
--   computeAs P (unsafeMakeLoadArray comp sz mDef $ \ put -> put ix v)


spec :: Spec
spec = pure ()
  -- describe "upsampleDifferentDefault" $ do
  --   it "Ix1" $ property $ prop_upsampleDifferentDefault (Proxy :: Proxy Ix1)
  --   it "Ix2" $ property $ prop_upsampleDifferentDefault (Proxy :: Proxy Ix2)
  --   it "Ix3" $ property $ prop_upsampleDifferentDefault (Proxy :: Proxy Ix3)
  --   it "Ix4" $ property $ prop_upsampleDifferentDefault (Proxy :: Proxy Ix4)
  --   it "Ix5" $ property $ prop_upsampleDifferentDefault (Proxy :: Proxy Ix5)


-- identityDL :: Int -> Array DL Ix2 Int
-- identityDL n = makeLoadArrayS (Sz2 n n) 0 $ \ writeCell -> do
--   let f i = writeCell (i :. i) 1
--   A.mapM_ f (0 ... n - 1)
