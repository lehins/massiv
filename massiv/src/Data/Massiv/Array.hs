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
  ( isEmpty
  , elemsCount
  , module A
  -- , module Data.Massiv.Core
  -- , module Data.Massiv.Array.Delayed
  -- , module Data.Massiv.Array.Manifest
  -- , module Data.Massiv.Array.Mutable
  -- , module Data.Massiv.Array.Ops
  ) where

import           Data.Massiv.Array.Delayed as A
import           Data.Massiv.Array.Delayed.Internal as A
import           Data.Massiv.Array.Manifest as A
import           Data.Massiv.Array.Mutable as A
import           Data.Massiv.Array.Ops as A
import           Data.Massiv.Core as A
import           Prelude                    as P hiding (length, null)


-- | /O(1)/ - Get number of elements in the array
elemsCount :: Construct r ix e => Array r ix e -> Int
elemsCount = totalElem . size
{-# INLINE elemsCount #-}

-- | /O(1)/ - Check if array has no elements.
isEmpty :: Construct r ix e => Array r ix e -> Bool
isEmpty !arr = 0 == elemsCount arr
{-# INLINE isEmpty #-}
