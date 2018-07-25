{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Massiv.Core.Computation where

import Import

import Test.QuickCheck

import Data.Massiv.Core

instance GenUnchecked Comp

instance GenValid Comp
