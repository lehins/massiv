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
    makeArray
  , makeArrayR
  , singleton
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
  -- * Slicing
  -- ** From the outside
  , (!>)
  , (!?>)
  , (?>)
  -- ** From the inside
  , (<!)
  , (<!?)
  , (<?)
  -- ** From within
  , (<!>)
  , (<!?>)
  , (<?>)
  -- * Mapping
  , module Data.Massiv.Array.Ops.Map
  -- * Folding
  , module Data.Massiv.Array.Ops.Fold
  -- * Conversion
  -- ** List
  , fromList
  , toList
  -- ** Vector
  , fromVector
  , toVector

  ) where

--import           Data.Massiv.Array.Delayed as A
import           Data.Massiv.Array.Delayed.Internal as A
import           Data.Massiv.Array.Manifest         as A
--import Data.Massiv.Array.Mutable as A
import           Data.Massiv.Array.Ops              as A
import           Data.Massiv.Array.Ops.Fold
import           Data.Massiv.Array.Ops.Map
import           Data.Massiv.Core                   as A


-- | /O(1)/ - Get the number of elements in the array
elemsCount :: Construct r ix e => Array r ix e -> Int
elemsCount = totalElem . size
{-# INLINE elemsCount #-}

-- | /O(1)/ - Check if array is empty and has no elements.
isEmpty :: Construct r ix e => Array r ix e -> Bool
isEmpty !arr = 0 == elemsCount arr
{-# INLINE isEmpty #-}


