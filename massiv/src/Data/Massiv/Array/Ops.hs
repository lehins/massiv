-- |
-- Module      : Data.Massiv.Array.Ops
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Ops
  (  -- * Construction
    makeVectorR
  , makeArray
  , makeArrayR
  -- * Enumeration
  , range
  , rangeStep
  , enumFromN
  , enumFromStepN
  -- * Conversion
  -- ** From List
  , fromListIx1
  , fromListIx1As
  , fromListIx2
  , fromListIx2As
  , fromListIx3
  , fromListIx3As
  -- ** To List
  --
  -- Conversion to List is done sequentially regardless of the internal
  -- computation type, because constructing nested lists in parallel turns out
  -- to be slower then doing so sequentially.
  , toListIx1
  , toListIx2
  , toListIx2'
  , toListIx3
  , toListIx4
  , module Data.Massiv.Array.Ops.Map
  , module Data.Massiv.Array.Ops.Slice
  , module Data.Massiv.Array.Ops.Fold
  , module Data.Massiv.Array.Ops.Transform
  , module Data.Massiv.Array.Ops.Numeric
  ) where

import           Data.Massiv.Array.Ops.Construct
import           Data.Massiv.Array.Ops.Fold
import           Data.Massiv.Array.Ops.Map
import           Data.Massiv.Array.Ops.Numeric
import           Data.Massiv.Array.Ops.Slice
import           Data.Massiv.Array.Ops.Transform
