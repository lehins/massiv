{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : Data.Array.Massiv.Ops.Transform
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Ops.Transform
  ( transpose
  , transposeInner
  , transposeOuter
  , backpermute
  , unsafeBackpermute
  , reshape
  , reshape'
  , extract
  , extractFromTo
  , append
  , append'
  ) where

import           Control.Monad                  (guard)
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Common.Shape
import           Data.Array.Massiv.Delayed
import           Data.Maybe                     (fromMaybe)


transpose :: Source r DIM2 e => Array r DIM2 e -> Array D DIM2 e
transpose = transposeInner
{-# INLINE transpose #-}

transposeInner
  :: (Index (Lower ix), Source r ix e)
  => Array r ix e -> Array D ix e
transposeInner !arr = DArray (transInner (size arr)) newVal
  where
    transInner !ix =
      fromMaybe (error "transposeInner: Impossible happened") $ do
        n <- getIndex ix (rank ix)
        m <- getIndex ix (rank ix - 1)
        ix' <- setIndex ix (rank ix) m
        setIndex ix' (rank ix - 1) n
    {-# INLINE transInner #-}
    newVal !ix = unsafeIndex arr (transInner ix)
    {-# INLINE newVal #-}
{-# INLINE transposeInner #-}

transposeOuter
  :: (Index (Lower ix), Source r ix e)
  => Array r ix e -> Array D ix e
transposeOuter !arr = DArray (transOuter (size arr)) newVal
  where
    transOuter !ix =
      fromMaybe (error "transposeOuter: Impossible happened") $ do
        n <- getIndex ix 1
        m <- getIndex ix 2
        ix' <- setIndex ix 1 m
        setIndex ix' 2 n
    {-# INLINE transOuter #-}
    newVal !ix = unsafeIndex arr (transOuter ix)
    {-# INLINE newVal #-}
{-# INLINE transposeOuter #-}


backpermute :: Source r ix1 e => ix -> (ix -> ix1) -> Array r ix1 e -> Array D ix e
backpermute !sz ixF !arr = DArray sz $ \ !ix -> evaluateAt arr (ixF ix)
{-# INLINE backpermute #-}


unsafeBackpermute :: Source r ix1 e => ix -> (ix -> ix1) -> Array r ix1 e -> Array D ix e
unsafeBackpermute !sz ixF !arr = DArray sz $ \ !ix -> unsafeIndex arr (ixF ix)
{-# INLINE unsafeBackpermute #-}




append
  :: (Source r1 ix e, Source r ix e) =>
     Int -> Array r1 ix e -> Array r ix e -> Maybe (Array D ix e)
append !n !arr1 !arr2 = do
  let !sz1 = size arr1
      !sz2 = size arr2
  k1 <- getIndex sz1 n
  k2 <- getIndex sz2 n
  sz1' <- setIndex sz2 n k1
  guard $ sz1 == sz1'
  newSz <- setIndex sz1 n (k1 + k2)
  return $
    DArray newSz $ \ !ix ->
      fromMaybe (error "append: Impossible happened") $ do
        k' <- getIndex ix n
        if k' < k1
          then Just (unsafeIndex arr1 ix)
          else do
            i <- getIndex ix n
            ix' <- setIndex ix n (i - k1)
            return $ unsafeIndex arr2 ix'
{-# INLINE append #-}

append'
  :: (Source r1 ix e, Source r2 ix e) =>
     Int -> Array r1 ix e -> Array r2 ix e -> Array D ix e
append' !n !arr1 !arr2 =
  case append n arr1 arr2 of
    Just arr -> arr
    Nothing ->
      error $
      if 0 < n && n <= rank (size arr1)
        then "Dimension mismatch: " ++ show arr1 ++ " and " ++ show arr2
        else "Invalid dimension index: " ++ show n
{-# INLINE append' #-}
