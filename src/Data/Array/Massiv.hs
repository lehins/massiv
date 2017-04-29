{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}
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
  , module Data.Array.Massiv.Ops.Map
  , module Data.Array.Massiv.Ops.Fold
  , module Data.Array.Massiv.Ops.Construct
  , module Data.Array.Massiv.Ops.Transform
  -- * Accessors
  -- ** Size information
  , size
  , null
  -- * Geometric Operators
  ) where

import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Common.Shape
import           Data.Array.Massiv.Delayed
import           Data.Array.Massiv.Manifest
-- import           Data.Array.Massiv.Mutable
import           Data.Array.Massiv.Ops.Construct
import           Data.Array.Massiv.Ops.Fold
import           Data.Array.Massiv.Ops.Map
import           Data.Array.Massiv.Ops.Transform
import           Prelude                            hiding (length, map, null,
                                                     zipWith, zipWith3)
-- import Text.Printf
-- import qualified Data.List as L


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

