module LibSpec (spec) where

import Common

spec :: Spec
spec = do
  describe "someFunc" $ do
    xit "DoesSomething" $ property $ \ a@() -> () === a
