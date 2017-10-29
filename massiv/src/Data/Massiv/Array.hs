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
  ( null
  , length
  , module Data.Massiv.Core
  , module Data.Massiv.Array.Delayed
  , module Data.Massiv.Array.Manifest
  , module Data.Massiv.Array.Mutable
  , module Data.Massiv.Array.Ops
  ) where

import           Data.Massiv.Array.Delayed
import           Data.Massiv.Array.Delayed.Internal
import           Data.Massiv.Array.Manifest
import           Data.Massiv.Array.Mutable
import           Data.Massiv.Array.Ops
import           Data.Massiv.Core
import           Prelude                    as P hiding (length, null)

length :: Construct r ix e => Array r ix e -> Int
length = totalElem . size
{-# INLINE length #-}

null :: Construct r ix e => Array r ix e -> Bool
null !arr = 0 == length arr
{-# INLINE null #-}
