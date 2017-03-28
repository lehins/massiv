{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Data.Array.Massiv.Windowed
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Windowed where

import           Data.Array.Massiv.Index
import           Data.Array.Massiv.Common
-- import           Data.Array.Massiv.Manifest
--import           GHC.Base                 (quotRemInt)


-- | Windowed representation.
data W


-- data instance Array W ix e = WArray { wSize :: !ix
--                                     , wSafeIndexBorder :: Int -> e
--                                     , wWindowStartIndex :: !ix
--                                     , wWindowSize :: !ix
--                                     , wWindowUnsafeIndex :: Int -> e }


-- instance Index ix => Massiv W ix where
--   size = wSize
--   {-# INLINE size #-}



-- instance Functor (Array W ix) where
--   fmap f !arr =
--     arr
--     { wSafeIndexBorder = f . wSafeIndexBorder arr
--     , wWindowUnsafeIndex = f . wWindowUnsafeIndex arr
--     }
--   {-# INLINE fmap #-}


-- makeArrayWindowed
--   :: Source M ix
--   => Array M ix e -> ix -> ix -> (Int -> e) -> Array W ix e
-- makeArrayWindowed !arr !wIx !wSz wUnsafeIndex =
--   WArray
--   { wSize = size arr
--   , wSafeIndexBorder = unsafeLinearIndex arr
--   , wWindowStartIndex = wIx
--   , wWindowSize = wSz
--   , wWindowUnsafeIndex = wUnsafeIndex
--   }
-- {-# INLINE makeArrayWindowed #-}


data instance Array W ix e = WArray { wSize :: !ix
                                    , wSafeIndexBorder :: ix -> e
                                    , wWindowStartIndex :: !ix
                                    , wWindowSize :: !ix
                                    , wWindowUnsafeIndex :: ix -> e }


instance Index ix => Massiv W ix where
  size = wSize
  {-# INLINE size #-}



instance Functor (Array W ix) where
  fmap f !arr =
    arr
    { wSafeIndexBorder = f . wSafeIndexBorder arr
    , wWindowUnsafeIndex = f . wWindowUnsafeIndex arr
    }
  {-# INLINE fmap #-}


makeArrayWindowed
  :: Source r ix
  => Array r ix e -> ix -> ix -> (ix -> e) -> Array W ix e
makeArrayWindowed !arr !wIx !wSz wUnsafeIndex =
  WArray
  { wSize = size arr
  , wSafeIndexBorder = unsafeIndex arr
  , wWindowStartIndex = wIx
  , wWindowSize = wSz
  , wWindowUnsafeIndex = wUnsafeIndex
  }
{-# INLINE makeArrayWindowed #-}

