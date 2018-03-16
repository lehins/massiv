{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Massiv.Array.Manifest.Foreign.Internal
-- Copyright   : (c) Alexey Kuleshevich 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Manifest.Foreign.Internal
  ( F(..)
  , Array(..)
  ) where

import           Data.Massiv.Array.Manifest.Internal
import           Data.Massiv.Core.Common
import           Foreign.ForeignPtr


-- | Representation for `Storable` elements
data F = F deriving Show

data instance Array F ix e = FArray { fComp :: !Comp
                                    , fSize :: !ix
                                    , fData :: !(ForeignPtr e)
                                    }

type instance EltRepr F ix = M
