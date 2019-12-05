{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Criterion.Main
import Data.Massiv.Array as A
import Data.Massiv.Array.Manifest.Vector as A
import Data.Massiv.Bench as A
import qualified Data.Vector.Primitive as VP
import Data.Primitive.ByteArray
import Data.Primitive.PrimArray
import Prelude as P
import Control.Monad.Trans.Maybe
import Control.DeepSeq

instance Prim a => NFData (PrimArray a) where
  rnf (PrimArray _) = ()

main :: IO ()
main = do
  let !sz = Sz (600 :. 1000)
      !arr = computeAs P $ resize' (Sz $ totalElem sz) $ arrRLightIx2 DL Seq sz
      incNotNaN x
        | isNaN x = Nothing
        | otherwise = Just (x + 1)
      incNotNaNT x
        | isNaN x = MaybeT (return Nothing)
        | otherwise = MaybeT (return (Just (x + 1)))
      toPrimArray :: Array P ix e -> PrimArray e
      toPrimArray a =
        case toByteArray a of
          ByteArray ba -> PrimArray ba
  defaultMain
    [ bgroup
        "traverse"
        [ env (return arr) $ \arr' ->
            bench "Array (P)" $ nf (A.traverseA @P incNotNaN) arr'
        , env (return arr) $ \arr' ->
            bench "Array (DS)" $ nf (fmap (A.computeAs P) . A.traverseS incNotNaN) arr'
        , env (return (toVector arr)) $ \v ->
            bench "Vector (VP)" $ nf (VP.mapM incNotNaN) v
        , env (return (toPrimArray arr)) $ \v ->
            bench "PrimArray" $ nf (traversePrimArray incNotNaN) v
        ]
    , bgroup "traversePrim"
        [ env (pure arr) $ \a ->
            bench "Array P" $ nfIO (runMaybeT $ traversePrim @P incNotNaNT a)
        , env (return (toPrimArray arr)) $ \v ->
            bench "PrimArray" $ nfIO (runMaybeT $ traversePrimArrayP incNotNaNT v)
        ]
    ]
