module Main where

import Spec
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import Test.Hspec

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hspec spec
