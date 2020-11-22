{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Massiv.Bench.Matrix
  ( MxM(..)
  , randomMxM
  , benchMxM
  , MxV(..)
  , randomMxV
  , benchMxV
  , VxM(..)
  , randomVxM
  , benchVxM
  ) where

import Data.Massiv.Array
import Data.Typeable
import Criterion.Main
import System.Random
import Control.DeepSeq

stdGen :: StdGen
stdGen = mkStdGen 2020

-- | Use Typeable to show the type.
showsType :: forall t . Typeable t => ShowS
showsType = showsTypeRep (typeRep (Proxy :: Proxy t))

aMxMsize :: Sz2
aMxMsize = Sz2 500 800

bMxMsize :: Sz2
bMxMsize =
  case aMxMsize of
    Sz2 _ n -> Sz2 n 500


randomMxM :: (Mutable r Ix2 e, Random e) => MxM r e
randomMxM =
  case randomArrayS stdGen aMxMsize random of
    (g, a) -> MxM {aMxM = a, bMxM = snd $ randomArrayS g bMxMsize random}

data MxM r e =
  MxM
    { aMxM :: !(Matrix r e)
    , bMxM :: !(Matrix r e)
    }

instance NFData (Matrix r e) => NFData (MxM r e) where
  rnf (MxM a b) = a `deepseq` b `deepseq` ()


showSizeMxM :: Load r Ix2 e => MxM r e -> String
showSizeMxM MxM {..} = show m1 <> "x" <> show n1 <> " X " <> show m2 <> "x" <> show n2
  where
    Sz2 m1 n1 = size aMxM
    Sz2 m2 n2 = size bMxM


benchMxM ::
     forall r e. (Typeable r, Typeable e, Numeric r e, Mutable r Ix2 e)
  => MxM r e
  -> Benchmark
benchMxM !mxm@MxM {..} =
  bgroup (showsType @(MxM r e) (" - (" <> showSizeMxM mxm <> ")"))
  [ bench "Seq" $ whnfIO (computeIO @r =<< aMxM .><. bMxM)
  , bench "Par" $ whnfIO (computeIO @r =<< aMxM .><. setComp Par bMxM)
  ]
{-# INLINEABLE benchMxM #-}





data MxV r e =
  MxV
    { aMxV :: !(Matrix r e)
    , bMxV :: !(Vector r e)
    }
instance (NFData (Matrix r e), NFData (Vector r e)) => NFData (MxV r e) where
  rnf (MxV a b) = a `deepseq` b `deepseq` ()

aMxVsize :: Sz2
aMxVsize = bMxMsize

bMxVsize :: Sz1
bMxVsize =
  case aMxVsize of
    Sz2 _ n -> Sz1 n

randomMxV :: (Mutable r Ix2 e, Mutable r Ix1 e, Random e) => MxV r e
randomMxV =
  case randomArrayS stdGen aMxVsize random of
    (g, a) -> MxV {aMxV = a, bMxV = snd $ randomArrayS g bMxVsize random}

showSizeMxV :: (Load r Ix1 e, Load r Ix2 e) => MxV r e -> String
showSizeMxV MxV {..} = show m1 <> "x" <> show n1 <> " X " <> show n <> "x1"
  where
    Sz2 m1 n1 = size aMxV
    Sz1 n = size bMxV


benchMxV ::
     forall r e. (Typeable r, Typeable e, Numeric r e, Mutable r Ix1 e, Mutable r Ix2 e)
  => MxV r e
  -> Benchmark
benchMxV !mxv@MxV {..} =
  bgroup (showsType @(MxV r e) (" - (" <> showSizeMxV mxv <> ")"))
  [ bench "Seq" $ whnfIO (computeIO @r =<< aMxV .>< bMxV)
  , bench "Par" $ whnfIO (computeIO @r =<< aMxV .>< setComp Par bMxV)
  ]




data VxM r e =
  VxM
    { aVxM :: !(Vector r e)
    , bVxM :: !(Matrix r e)
    }
instance (NFData (Matrix r e), NFData (Vector r e)) => NFData (VxM r e) where
  rnf (VxM a b) = a `deepseq` b `deepseq` ()

aVxMsize :: Sz1
aVxMsize =
  case bVxMsize of
    Sz2 m _ -> Sz1 m

bVxMsize :: Sz2
bVxMsize = aMxMsize

showSizeVxM :: (Load r Ix1 e, Load r Ix2 e) => VxM r e -> String
showSizeVxM VxM {..} = "1x" <> show n <> " X " <> show m2 <> "x" <> show n2
  where
    Sz1 n = size aVxM
    Sz2 m2 n2 = size bVxM


randomVxM :: (Mutable r Ix2 e, Mutable r Ix1 e, Random e) => VxM r e
randomVxM =
  case randomArrayS stdGen aVxMsize random of
    (g, a) -> VxM {aVxM = a, bVxM = snd $ randomArrayS g bVxMsize random}

benchVxM ::
     forall r e. (Typeable r, Typeable e, Numeric r e, Mutable r Ix1 e, Mutable r Ix2 e)
  => VxM r e
  -> Benchmark
benchVxM !mxv@VxM {..} =
  bgroup (showsType @(VxM r e) (" - (" <> showSizeVxM mxv <> ")"))
  [ bench "Seq" $ whnfIO (computeIO @r =<< aVxM ><. bVxM)
  , bench "Par" $ whnfIO (computeIO @r =<< aVxM ><. setComp Par bVxM)
  ]
{-# INLINEABLE benchVxM #-}
