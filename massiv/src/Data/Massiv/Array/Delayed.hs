{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : Data.Massiv.Array.Delayed
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Delayed
  ( D(..)
  , delay
  , DI
  , toInterleaved
  , DW
  ) where

import           Data.Massiv.Array.Delayed.Interleaved
import           Data.Massiv.Array.Delayed.Internal
import           Data.Massiv.Array.Delayed.Windowed
