{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE TypeApplications #-}
module Main where

import Criterion.Main
import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe as A
import Data.Massiv.Bench as A
import Prelude as P


main :: IO ()
main = do
  let !sz = Sz2 16000 12000
  pGroup <- bgroup "P" <$> mkBench sz P
  sGroup <- bgroup "S" <$> mkBench sz S
  defaultMain [pGroup, sGroup]


mkBench ::
     forall r. (Load r Ix2 Double, Manifest r Double)
  => Sz2
  -> r
  -> IO [Benchmark]
mkBench sz r = do
  let !arr = arrRLightIx2 r Seq sz
  marr <- unsafeThaw arr
  pure
    [ bgroup
        "Thaw"
        [ bench "thaw Par" $ whnfIO (thaw (setComp (ParN 2) arr))
        , bench "thawS" $ whnfIO (thawS arr)
        -- , bench "makeMArrayLinearS" $
        --   whnfIO (makeMArrayLinearS @r (size arr) (pure . unsafeLinearIndexM arr))
        ]
    , bgroup
        "Freeze"
        [ bench "freeze Par" $ whnfIO (freeze (ParN 2) marr)
        , bench "freezeS" $ whnfIO (freezeS marr)
        -- , bench "generateArrayLinearS" $
        --   whnfIO (generateArrayLinearS @r Seq (msize marr) (unsafeLinearRead marr))
        ]
    ]
