{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : Data.Massiv.Array
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array
  ( -- * Construct
    module Data.Massiv.Array.Ops.Construct
  -- * Compute
  , getComp
  , setComp
  , compute
  , computeAs
  , convert
  , convertAs
  -- * Size
  , size
  , elemsCount
  , isEmpty
  -- * Indexing
  , (!)
  , (!?)
  , (?)
  , index
  , maybeIndex
  , defaultIndex
  , borderIndex
  , evaluateAt
  -- * Slicing
  , module Data.Massiv.Array.Ops.Slice
  -- * Mapping
  , module Data.Massiv.Array.Ops.Map
  -- * Folding
  , module Data.Massiv.Array.Ops.Fold
  -- * Transforming
  , module Data.Massiv.Array.Ops.Transform
  -- * Conversion
  , module Data.Massiv.Array.Manifest.List
  -- * Stencil
  , module Data.Massiv.Array.Stencil
  -- * Core
  , module Data.Massiv.Core
  -- * Representations
  , module Data.Massiv.Array.Delayed
  , module Data.Massiv.Array.Manifest
  ) where

import           Data.Massiv.Array.Delayed
import           Data.Massiv.Array.Manifest
import           Data.Massiv.Array.Manifest.List
import           Data.Massiv.Array.Manifest.Mutable as A
import           Data.Massiv.Array.Ops.Construct
import           Data.Massiv.Array.Ops.Fold
import           Data.Massiv.Array.Ops.Map
import           Data.Massiv.Array.Ops.Slice
import           Data.Massiv.Array.Ops.Transform
import           Data.Massiv.Array.Stencil
import           Data.Massiv.Core
import           Data.Massiv.Core.Common
import           Prelude                            as P hiding (foldl, foldr)

-- | /O(1)/ - Get the number of elements in the array
elemsCount :: Size r ix e => Array r ix e -> Int
elemsCount = totalElem . size
{-# INLINE elemsCount #-}

-- | /O(1)/ - Check if array has no elements.
isEmpty :: Size r ix e => Array r ix e -> Bool
isEmpty !arr = 0 == elemsCount arr
{-# INLINE isEmpty #-}


