-- |
-- Module      : Data.Massiv.Types
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Types
  ( Massiv
  , Layout(..)
  , P(..)
  , U(..)
  , S(..)
  , B(..)
  , N(..)
  , D(..)
  , DI
  , DW
  , Stencil
  , Prim
  , Unbox
  , Storable
  , NFData
  , module Data.Massiv.Core
  ) where

import           Control.DeepSeq            (NFData)
import           Data.Massiv.Array.Delayed
import           Data.Massiv.Array.Manifest
import           Data.Massiv.Array.Stencil.Internal
import           Data.Massiv.Core
import           Data.Massiv.Internal

