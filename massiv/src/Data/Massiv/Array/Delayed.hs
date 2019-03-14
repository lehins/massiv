-- |
-- Module      : Data.Massiv.Array.Delayed
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Delayed
  ( -- * Delayed
  -- ** Delayed Pull Array
    D(..)
  , delay
  -- ** Delayed Push Array
  , DL(..)
  , toLoadArray
  , makeLoadArray
  , fromStrideLoad
  -- ** Delayed Interleaved Array
  , DI(..)
  , toInterleaved
  , fromInterleaved
  -- ** Delayed Windowed Array
  , DW(..)
  , Window(..)
  , insertWindow
  , getWindow
  , dropWindow
  , makeWindowedArray
  ) where

import           Data.Massiv.Array.Delayed.Interleaved
import           Data.Massiv.Array.Delayed.Pull
import           Data.Massiv.Array.Delayed.Push
import           Data.Massiv.Array.Delayed.Windowed
