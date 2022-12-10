{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.State.Strict
import Criterion.Main
import Data.Massiv.Array as A
import qualified Data.Vector.Primitive as VP
import System.Random

main :: IO ()
main = do
  let !sz = Sz1 1000000
      !gen = mkStdGen 2022
  arr :: Array P Int Word <- computeIO $ uniformArray gen Seq sz

  let !v = toPrimitiveVector arr

  defaultMain
    [ bgroup
        "countEven"
        [ bench "postscanlStateST" $ nf (\a -> runST $ postscanlStatePrim @P countEven 0 a) arr
        , bench "postscanlStateIO" $ nfIO (postscanlStatePrim @P countEven 0 arr)
        , bench "spostscanl" $ nf (computeAs P . spostscanl countEven 0) arr
        , bench "VP.postscanl'" $ nf (VP.postscanl' countEven 0) v
        , bench "postscanlStateM" $ nf (postscanlState @P countEven 0) arr
        ]
    ]

countEven :: Integral a => Int -> a -> Int
countEven acc e
  | even e = acc + 1
  | otherwise = acc
{-# INLINE countEven #-}

postscanlState
  :: forall r r' ix e t
   . (Index ix, Manifest r e, Source r' t)
  => (e -> t -> e)
  -> e
  -> Array r' ix t
  -> Array r ix e
postscanlState f acc =
  flip evalState acc
    . A.traverseA
      ( \x ->
          state $ \s -> let s' = f s x in (s', s')
      )
{-# INLINE postscanlState #-}

postscanlStatePrim
  :: forall r r' ix e t m
   . (Index ix, Manifest r e, Source r' t, PrimMonad m)
  => (e -> t -> e)
  -> e
  -> Array r' ix t
  -> m (Array r ix e)
postscanlStatePrim f acc arr =
  flip evalStateT acc $
    A.traversePrim
      ( \x ->
          state $ \s -> let s' = f s x in (s', s')
      )
      arr
{-# INLINEABLE postscanlStatePrim #-}
