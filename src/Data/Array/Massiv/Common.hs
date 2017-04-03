{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Data.Array.Massiv.Common
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Common
  ( Array
  , Massiv(..)
  , Source(..)
  , Load(..)
  , module Data.Array.Massiv.Common.Index
  , module Data.Array.Massiv.Common.Iterator
  , ifoldl
  ) where

import Control.Monad.ST (ST)
import Data.Array.Massiv.Common.Index hiding (Z)
import Data.Array.Massiv.Common.Iterator

data family Array r ix e :: *


-- | Immutable, shape polymorphic array construction and indexing.
class Index ix => Massiv r ix where

  size :: Array r ix e -> ix



instance Massiv r ix => Show (Array r ix e) where
  show arr = "<Array: " ++ show (size arr) ++ ">"


class Massiv r ix => Source r ix e where

  unsafeIndex :: Massiv r ix => Array r ix e -> ix -> e
  unsafeIndex !arr = unsafeLinearIndex arr . toLinearIndex (size arr)
  {-# INLINE unsafeIndex #-}

  unsafeLinearIndex :: Array r ix e -> Int -> e
  unsafeLinearIndex !arr = unsafeIndex arr . fromLinearIndex (size arr)
  {-# INLINE unsafeLinearIndex #-}


class Massiv r ix => Load r ix where
  -- | Load an array into memory sequentially
  loadS
    :: Array r ix e -- ^ Array that is being loaded
    -> (Int -> e -> ST s ()) -- ^ Write element
    -> ST s ()

  -- | Load an array into memory in parallel
  loadP
    :: Array r ix e -- ^ Array that is being loaded
    -> (Int -> e -> IO ()) -- ^ Write element
    -> IO ()


ifoldl
  :: (Iterator i ix, Source r ix a)
  => i -> (ix -> b -> a -> b) -> b -> Array r ix a -> b
ifoldl i f !acc !arr = iter i zeroIndex (size arr) acc $ \ !ix !a -> f ix a (unsafeIndex arr ix)
{-# INLINE ifoldl #-}

