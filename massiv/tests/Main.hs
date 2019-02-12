module Main where

import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)
import Test.Hspec
import Spec
import Data.Massiv.Array

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hspec spec
