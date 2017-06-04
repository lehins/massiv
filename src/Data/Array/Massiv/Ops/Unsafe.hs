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


unsafeBackpermute :: (Source r' ix' e, Massiv r ix e) =>
                     ix -> (ix -> ix') -> Array r' ix' e -> Array r ix e
unsafeBackpermute !sz ixF !arr =
  unsafeMakeArray (getComp arr) sz $ \ !ix -> unsafeIndex arr (ixF ix)
{-# INLINE unsafeBackpermute #-}

