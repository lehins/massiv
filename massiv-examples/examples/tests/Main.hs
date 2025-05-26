module Main where

import Spec
import System.IO
import Test.Hspec

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hspec spec
