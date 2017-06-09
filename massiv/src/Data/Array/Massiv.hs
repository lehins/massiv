{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Array.Massiv
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv
  ( module Data.Array.Massiv.Common
  , module Data.Array.Massiv.Common.Shape
  , module Data.Array.Massiv.Delayed
  , module Data.Array.Massiv.Manifest
  , module Data.Array.Massiv.Mutable
  , module Data.Array.Massiv.Ops.Map
  , module Data.Array.Massiv.Ops.Fold
  , module Data.Array.Massiv.Ops.Construct
  , module Data.Array.Massiv.Ops.Transform
  -- * Accessors
  -- ** Size information
  --, size
  , null
  , smakeArray
  , smakeArrayR
  , smap
  , szipWith
  , stranspose
  , computeF
  , uncomputeF
  ) where

import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Common.Shape
import           Data.Array.Massiv.Delayed
import           Data.Array.Massiv.Manifest
import           Data.Array.Massiv.Mutable
import           Data.Array.Massiv.Ops.Construct
import           Data.Array.Massiv.Ops.Fold
import           Data.Array.Massiv.Ops.Map
import           Data.Array.Massiv.Ops.Transform
import           Prelude                         as P hiding (length, map,
                                                       mapM_, null, unzip,
                                                       unzip3, zip, zip3,
                                                       zipWith, zipWith3)
-- import Text.Printf
-- import qualified Data.Foldable as F
-- import qualified Data.List as L


-- showPrintf :: (PrintfArg a, Foldable t) => String -> t a -> String
-- showPrintf format arr = concat $ fmap (printf format) $ F.toList arr


-- instance PrettyShow DIM1 where
--   prettyShow format arr = concat $ fmap (printf format) $ F.toList arr

-- instance Show FormatAdjustment where
--   show ZeroPad = "ZeroPad"
--   show LeftAdjust = "LeftAdjust"

-- instance Show FormatSign where
--   show SignPlus = "SignPlus"
--   show SignSpace = "SignSpace"

-- instance Show FieldFormat where
--   show (FieldFormat {..}) =
--     concat $ L.map (++",")
--       [ show fmtWidth
--       , show fmtPrecision
--       , show fmtAdjust
--       , show fmtSign
--       , show fmtAlternate
--       , show fmtModifiers
--       , show fmtChar
--       ]

-- instance (Source r Int e, PrintfArg e) => PrintfArg (Array r Int e) where

--   formatArg arr = \ ff -> --error $ show ff
--                     ((formatArg (unsafeIndex arr 0)) ff) .
--                     ((formatArg (unsafeIndex arr 1)) ff) .
--                     ((formatArg (unsafeIndex arr 2)) ff)
--   -- parseFormat _ (c : cs) = FormatParse "" c cs
--   -- parseFormat _ "" = errorShortFormat

length :: Massiv r ix e => Array r ix e -> Int
length = totalElem . size
{-# INLINE length #-}

null :: Massiv r ix e => Array r ix e -> Bool
null !arr = 0 == length arr
{-# INLINE null #-}


makeArrayR :: Massiv r ix e => r -> Comp -> ix -> (ix -> e) -> Array r ix e
makeArrayR _ = makeArray
{-# INLINE makeArrayR #-}

computeF :: Target r ix e => Array D ix e -> Array r ix e
computeF = compute
{-# INLINE [1] computeF #-}

uncomputeF :: Source r ix e => Array r ix e -> Array D ix e
uncomputeF = delay
{-# INLINE [1] uncomputeF #-}

smakeArray :: Target r ix e => Comp -> ix -> (ix -> e) -> Array r ix e
smakeArray c sz f = computeF (makeArrayR D c sz f)
{-# INLINE [2] smakeArray #-}

smakeArrayR :: Target r ix e => r -> Comp -> ix -> (ix -> e) -> Array r ix e
smakeArrayR _ c sz f = computeF (makeArrayR D c sz f)
{-# INLINE [2] smakeArrayR #-}


smap :: (Target r ix e', Target r ix e) => (e' -> e) -> Array r ix e' -> Array r ix e
smap f arr = computeF (map f arr)
{-# INLINE [2] smap #-}


szipWith :: (Target r1 ix e1, Target r2 ix e2, Target r ix e) =>
            (e1 -> e2 -> e) -> Array r1 ix e1 -> Array r2 ix e2 -> Array r ix e
szipWith f arr1 arr2 = computeF (zipWith f (uncomputeF arr1) (uncomputeF arr2))
{-# INLINE [2] szipWith #-}


stranspose :: Target r DIM2 e
           => Array r DIM2 e -> Array r DIM2 e
stranspose = computeF . transpose . uncomputeF
{-# INLINE [2] stranspose #-}

{-# RULES
"stream/unstream" forall arr . uncomputeF (computeF arr) = arr
 #-}
