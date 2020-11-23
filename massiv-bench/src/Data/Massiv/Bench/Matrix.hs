{-# LANGUAGE AllowAmbiguousTypes #-}
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
  , showsType
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
    Sz2 m n -> Sz2 n m


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
benchMxM mxm@MxM {..} =
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
aMxVsize = Sz2 5000 8000

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
benchMxV mxv@MxV {..} =
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
bVxMsize = Sz2 5000 8000

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
     forall r e.
     ( Typeable r
     , Typeable e
     , Numeric r e
     , Mutable r Ix1 e
     , Mutable r Ix2 e
     , Load (R r) Ix1 e
     , InnerSlice r Ix2 e
     )
  => VxM r e
  -> Benchmark
benchVxM mxv@VxM {..} =
  bgroup (showsType @(VxM r e) (" - (" <> showSizeVxM mxv <> ")"))
  [ bench "Seq" $ whnfIO (computeIO @r =<< aVxM ><. bVxM)
  , bench "Par" $ whnfIO (computeIO @r =<< aVxM ><. setComp Par bVxM)
  -- , bgroup "vectorMatrixMultiply"
  --   [ bench "Seq" $ whnfIO (vectorMatrixMultiply aVxM bVxM)
  --   , bench "Par" $ whnfIO (vectorMatrixMultiply aVxM (setComp Par bVxM))
  --   ]
  ]
{-# INLINEABLE benchVxM #-}


-- -- | This is an experimental version of `><.` which is slower but uses
-- -- contiguous chunk of memory needed for SIMD
-- vectorMatrixMultiply ::
--      ( MonadThrow m
--      , Numeric r e
--      , Mutable r Ix1 e
--      , Mutable r Ix2 e
--      , Load (R r) Ix1 e
--      , InnerSlice r Ix2 e
--      )
--   => Vector r e -- ^ Row vector
--   -> Matrix r e -- ^ Matrix
--   -> m (Vector r e)
-- vectorMatrixMultiply v mm
--   | mRows /= n = throwM $ SizeMismatchException (Sz2 1 n) (size mm)
--   | otherwise =
--     pure $!
--     unsafePerformIO $ do
--       states <- initWorkerStates comp $ \_ -> unsafeNew sz
--       generateArrayLinearWS states (Sz1 mCols) $ \i tmpVector -> do
--         mv' <- unsafeLoadIntoS tmpVector (unsafeInnerSlice mm kSlice i)
--         v' <- unsafeFreeze Seq mv'
--         pure $! unsafeDotProduct v' v
--   where
--     comp = getComp mm <> getComp v
--     Sz2 mRows mCols = size mm
--     kSlice = (SafeSz mRows, SafeSz mCols)
--     sz@(Sz1 n) = size v
-- {-# INLINE vectorMatrixMultiply #-}

--  No performance difference from ><.
-- vectorMatrixMultiplyUnrolled :: (MonadThrow m, Numeric r e, Source r Ix1 e, Source r Ix2 e) =>
--          Vector r e -- ^ Row vector (Used many times, so make sure it is computed)
--       -> Matrix r e -- ^ Matrix
--       -> m (Vector D e)
-- vectorMatrixMultiplyUnrolled v mm
--   | mRows /= n = throwM $ SizeMismatchException (Sz2 1 n) (size mm)
--   | otherwise =
--     pure $ makeArray (getComp mm <> getComp v) (Sz1 mCols) $ \i ->
--       let go j !acc
--             | j < n4 =
--               let !j0' = j * mCols + i
--                   !j1' = j0' + mCols
--                   !j2' = j1' + mCols
--                   !j3' = j2' + mCols
--                   !acc' = unsafeLinearIndex v j       * unsafeLinearIndex mm j0' +
--                           unsafeLinearIndex v (j + 1) * unsafeLinearIndex mm j1' +
--                           unsafeLinearIndex v (j + 2) * unsafeLinearIndex mm j2' +
--                           unsafeLinearIndex v (j + 3) * unsafeLinearIndex mm j3' +
--                           acc
--               in go (j + 4) acc'
--             | j < n = go (j + 1) (unsafeLinearIndex v j * unsafeLinearIndex mm (j * mCols + i) + acc)
--             | otherwise = acc
--       in go 0 0
--   where
--     Sz2 mRows mCols = size mm
--     Sz1 n = size v
--     n4 = n - (n `rem` 4)
-- {-# INLINE vectorMatrixMultiplyUnrolled #-}



-- squareMsize :: Sz1
-- squareMsize = Sz1 1000

-- benchSquareM :: forall r e. Benchmark
-- benchSquareM mxv@VxM {..} =
--   bgroup (showsType @(Matrix r e) (" - (" <> showSizeVxM mxv <> ")"))
--   [ bgro "Seq" $ whnfIO (computeIO @r $ identityMatrix squareMatrix)
--   , bench "Par" $ whnfIO (computeIO @r =<< aVxM ><. setComp Par bVxM)
--   ]
-- {-# INLINEABLE benchSquareM #-}
