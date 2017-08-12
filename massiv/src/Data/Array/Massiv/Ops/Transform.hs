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
  , reshape
  , reshape'
  , extract
  , extractFromTo
  , append
  , append'
  , splitAt
  , traverse
  , traverse2
  ) where

import           Control.Monad                   (guard)
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Common.Shape
import           Data.Array.Massiv.Delayed
import           Data.Array.Massiv.Ops.Construct
import           Data.Maybe                      (fromMaybe)
import           Prelude                         hiding (splitAt, traverse)


transpose :: Source r Ix2 e => Array r Ix2 e -> Array D Ix2 e
transpose = transposeInner
{-# INLINE transpose #-}

transposeInner :: (Index (Lower ix), Source r' ix e)
               => Array r' ix e -> Array D ix e
transposeInner !arr = unsafeMakeArray (getComp arr) (transInner (size arr)) newVal
  where
    transInner !ix =
      fromMaybe (error "transposeInner: Impossible happened") $ do
        n <- getIndex ix (rank ix)
        m <- getIndex ix (rank ix - 1)
        ix' <- setIndex ix (rank ix) m
        setIndex ix' (rank ix - 1) n
    {-# INLINE transInner #-}
    newVal = unsafeIndex arr . transInner
    {-# INLINE newVal #-}
{-# INLINE transposeInner #-}

transposeOuter :: (Index (Lower ix), Source r' ix e)
               => Array r' ix e -> Array D ix e
transposeOuter !arr = unsafeMakeArray (getComp arr) (transOuter (size arr)) newVal
  where
    transOuter !ix =
      fromMaybe (error "transposeOuter: Impossible happened") $ do
        n <- getIndex ix 1
        m <- getIndex ix 2
        ix' <- setIndex ix 1 m
        setIndex ix' 2 n
    {-# INLINE transOuter #-}
    newVal = unsafeIndex arr . transOuter
    {-# INLINE newVal #-}
{-# INLINE transposeOuter #-}


backpermute :: (Source r' ix' e, Index ix) =>
               ix -> (ix -> ix') -> Array r' ix' e -> Array D ix e
backpermute sz ixF !arr = makeArray (getComp arr) sz (evaluateAt arr . ixF)
{-# INLINE backpermute #-}



append :: (Source r1 ix e, Source r2 ix e) =>
          Int -> Array r1 ix e -> Array r2 ix e -> Maybe (Array D ix e)
append n !arr1 !arr2 = do
  let sz1 = size arr1
      sz2 = size arr2
  k1 <- getIndex sz1 n
  k2 <- getIndex sz2 n
  sz1' <- setIndex sz2 n k1
  guard $ sz1 == sz1'
  newSz <- setIndex sz1 n (k1 + k2)
  return $
    unsafeMakeArray (getComp arr1) newSz $ \ !ix ->
      fromMaybe (error "append: Impossible happened") $ do
        k' <- getIndex ix n
        if k' < k1
          then Just (unsafeIndex arr1 ix)
          else do
            i <- getIndex ix n
            ix' <- setIndex ix n (i - k1)
            return $ unsafeIndex arr2 ix'
{-# INLINE append #-}

append' :: (Source r1 ix e, Source r2 ix e) =>
           Int -> Array r1 ix e -> Array r2 ix e -> Array D ix e
append' n arr1 arr2 =
  case append n arr1 arr2 of
    Just arr -> arr
    Nothing ->
      error $
      if 0 < n && n <= rank (size arr1)
        then "append': Dimension mismatch: " ++ show (size arr1) ++ " and " ++ show (size arr2)
        else "append': Invalid dimension index: " ++ show n
{-# INLINE append' #-}

-- | /O(1)/ - Split an array at an index in a particular dimension.
splitAt :: Shape r ix e => Int -> ix -> Array r ix e -> Maybe (Array (R r) ix e, Array (R r) ix e)
splitAt dim ix arr = do
  let sz = size arr
  i <- getIndex ix dim
  eIx <- setIndex sz dim i
  sIx <- setIndex zeroIndex dim i
  arr1 <- extractFromTo zeroIndex eIx arr
  arr2 <- extractFromTo sIx sz arr
  return (arr1, arr2)
{-# INLINE splitAt #-}


traverse
  :: (Source r1 ix1 e1, Index ix)
  => ix
  -> ((ix1 -> e1) -> ix -> e)
  -> Array r1 ix1 e1
  -> Array D ix e
traverse sz f arr1 = makeArray (getComp arr1) sz (f (evaluateAt arr1))
{-# INLINE traverse #-}


traverse2
  :: (Source r1 ix1 e1, Source r2 ix2 e2, Index ix)
  => ix
  -> ((ix1 -> e1) -> (ix2 -> e2) -> ix -> e)
  -> Array r1 ix1 e1
  -> Array r2 ix2 e2
  -> Array D ix e
traverse2 sz f arr1 arr2 = makeArray (getComp arr1) sz (f (evaluateAt arr1) (evaluateAt arr2))
{-# INLINE traverse2 #-}
