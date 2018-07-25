{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Massiv.Core.Index.Ix where

import Import

import Data.Massiv.Core

intToList :: Int -> [Int]
intToList x =
    if x >= 0
        then [0 .. x - 1]
        else reverse [x + 1 .. 0]

instance GenUnchecked Ix0

instance GenValid Ix0

instance GenUnchecked Ix2 where
    genUnchecked = (:.) <$> genUnchecked <*> genUnchecked
    shrinkUnchecked (a :. b) = [i :. j | i <- intToList a, j <- intToList b]

instance GenValid Ix2 where
    genValid = (:.) <$> genValid <*> genValid

instance GenUnchecked (Ix (n - 1)) => GenUnchecked (IxN n) where
    genUnchecked = (:>) <$> genUnchecked <*> genUnchecked
    shrinkUnchecked (a :> b) =
        [i :> j | i <- intToList a, j <- shrinkUnchecked b]

instance GenValid (Ix (n - 1)) => GenValid (IxN n) where
    genValid = (:>) <$> genValid <*> genValid
