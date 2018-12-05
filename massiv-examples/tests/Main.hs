module Main where

import System.IO

import Test.Hspec

import Spec

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hspec spec
