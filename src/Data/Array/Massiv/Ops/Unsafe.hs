{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : Data.Array.Massiv.Ops.Unsafe
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Ops.Unsafe
  ( unsafeBackpermute
  ) where

import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Delayed

unsafeBackpermute :: Source r ix1 e => ix -> (ix -> ix1) -> Array r ix1 e -> Array D ix e
unsafeBackpermute !sz ixF !arr = DArray sz $ \ !ix -> unsafeIndex arr (ixF ix)
{-# INLINE unsafeBackpermute #-}
