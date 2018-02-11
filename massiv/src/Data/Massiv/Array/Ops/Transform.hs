{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Data.Massiv.Array.Ops.Transform
-- Copyright   : (c) Alexey Kuleshevich 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Ops.Transform
  ( -- ** Transpose
    transpose
  , transposeInner
  , transposeOuter
  -- ** Backpermute
  , backpermute
  -- ** Resize
  , resize
  , resize'
  -- ** Extract
  , extract
  , extractFromTo
  -- ** Append/Split
  , append
  , append'
  , splitAt
  -- * Traverse
  , traverse
  , traverse2
  ) where

import           Control.Monad                      (guard)
import           Data.Massiv.Array.Delayed.Internal
import           Data.Massiv.Array.Ops.Construct
import           Data.Massiv.Core.Common
import           Data.Maybe                         (fromMaybe)
import           Prelude                            hiding (splitAt, traverse)



extract :: Size r ix e => ix -> ix -> Array r ix e -> Maybe (Array (EltRepr r ix) ix e)
extract !sIx !newSz !arr
  | isSafeIndex sz1 sIx && isSafeIndex eIx1 sIx && isSafeIndex sz1 eIx =
    Just $ unsafeExtract sIx newSz arr
  | otherwise = Nothing
  where
    sz1 = liftIndex (+1) (size arr)
    eIx1 = liftIndex (+1) eIx
    eIx = liftIndex2 (+) sIx newSz
{-# INLINE extract #-}

extractFromTo :: Size r ix e => ix -> ix -> Array r ix e -> Maybe (Array (EltRepr r ix) ix e)
extractFromTo sIx eIx = extract sIx newSz
  where
    newSz = liftIndex2 (-) eIx sIx
{-# INLINE extractFromTo #-}

-- | /O(1)/ - Changes the shape of the array. Will return `Nothing` if total
-- number of elements does not match the source array.
resize :: (Index ix', Size r ix e) => ix' -> Array r ix e -> Maybe (Array r ix' e)
resize !sz !arr
  | totalElem sz == totalElem (size arr) = Just $ unsafeResize sz arr
  | otherwise = Nothing
{-# INLINE resize #-}

-- | Same as `resize`, but will throw an error if supplied dimensions are incorrect.
resize' :: (Index ix', Size r ix e) => ix' -> Array r ix e -> Array r ix' e
resize' !sz !arr =
  maybe
    (error $
     "Total number of elements do not match: " ++
     show sz ++ " vs " ++ show (size arr))
    id $
  resize sz arr
{-# INLINE resize' #-}


-- | Transpose a 2-dimensional array
--
-- ===__Examples__
--
-- >>> let arr = makeArrayR U Seq (2 :. 3) (\ (i :. j) -> j + i * 3)
-- >>> arr
-- (ArrayU Seq (2 :. 3)
-- [ [ 0,1,2 ]
-- , [ 3,4,5 ]
-- ])
-- >>> transpose arr
-- (Array D Seq (3 :. 2)
-- [ [ 0,3 ]
-- , [ 1,4 ]
-- , [ 2,5 ]
-- ])
--
transpose :: Source r Ix2 e => Array r Ix2 e -> Array D Ix2 e
transpose = transposeInner
{-# INLINE transpose #-}


-- | Transpose inner two dimensions of at least rank-2 array.
--
-- ===___Examples__
--
-- >>> let arr = makeArrayR U Seq (2 :> 3 :. 4) fromIx3
-- >>> arr
-- (Array U Seq (2 :> 3 :. 4)
-- [ [ [ (0,0,0),(0,0,1),(0,0,2),(0,0,3) ]
--   , [ (0,1,0),(0,1,1),(0,1,2),(0,1,3) ]
--   , [ (0,2,0),(0,2,1),(0,2,2),(0,2,3) ]
--   ]
-- , [ [ (1,0,0),(1,0,1),(1,0,2),(1,0,3) ]
--   , [ (1,1,0),(1,1,1),(1,1,2),(1,1,3) ]
--   , [ (1,2,0),(1,2,1),(1,2,2),(1,2,3) ]
--   ]
-- ])
-- >>> transposeInner arr
-- (Array D Seq (3 :> 2 :. 4)
-- [ [ [ (0,0,0),(0,0,1),(0,0,2),(0,0,3) ]
--   , [ (1,0,0),(1,0,1),(1,0,2),(1,0,3) ]
--   ]
-- , [ [ (0,1,0),(0,1,1),(0,1,2),(0,1,3) ]
--   , [ (1,1,0),(1,1,1),(1,1,2),(1,1,3) ]
--   ]
-- , [ [ (0,2,0),(0,2,1),(0,2,2),(0,2,3) ]
--   , [ (1,2,0),(1,2,1),(1,2,2),(1,2,3) ]
--   ]
-- ])
--
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

-- | Transpose outer two dimensions of at least rank-2 array.
--
-- ===___Examples__
--
-- >>> let arr = makeArrayR U Seq (2 :> 3 :. 4) fromIx3
-- >>> arr
-- (Array U Seq (2 :> 3 :. 4)
-- [ [ [ (0,0,0),(0,0,1),(0,0,2),(0,0,3) ]
--   , [ (0,1,0),(0,1,1),(0,1,2),(0,1,3) ]
--   , [ (0,2,0),(0,2,1),(0,2,2),(0,2,3) ]
--   ]
-- , [ [ (1,0,0),(1,0,1),(1,0,2),(1,0,3) ]
--   , [ (1,1,0),(1,1,1),(1,1,2),(1,1,3) ]
--   , [ (1,2,0),(1,2,1),(1,2,2),(1,2,3) ]
--   ]
-- ])
-- >>> transposeOuter arr
-- (Array D Seq (2 :> 4 :. 3)
-- [ [ [ (0,0,0),(0,1,0),(0,2,0) ]
--   , [ (0,0,1),(0,1,1),(0,2,1) ]
--   , [ (0,0,2),(0,1,2),(0,2,2) ]
--   , [ (0,0,3),(0,1,3),(0,2,3) ]
--   ]
-- , [ [ (1,0,0),(1,1,0),(1,2,0) ]
--   , [ (1,0,1),(1,1,1),(1,2,1) ]
--   , [ (1,0,2),(1,1,2),(1,2,2) ]
--   , [ (1,0,3),(1,1,3),(1,2,3) ]
--   ]
-- ])
--
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


-- | Rearrange elements of an array into a new one.
--
-- ===__Examples__
--
-- >>> let arr = makeArrayR U Seq (2 :> 3 :. 4) fromIx3
-- >>> arr
-- (Array U Seq (2 :> 3 :. 4)
-- [ [ [ (0,0,0),(0,0,1),(0,0,2),(0,0,3) ]
--   , [ (0,1,0),(0,1,1),(0,1,2),(0,1,3) ]
--   , [ (0,2,0),(0,2,1),(0,2,2),(0,2,3) ]
--   ]
-- , [ [ (1,0,0),(1,0,1),(1,0,2),(1,0,3) ]
--   , [ (1,1,0),(1,1,1),(1,1,2),(1,1,3) ]
--   , [ (1,2,0),(1,2,1),(1,2,2),(1,2,3) ]
--   ]
-- ])
-- >>> backpermute (4 :. 3) (\(i :. j) -> 0 :> j :. i) arr
-- (Array D Seq (4 :. 3)
-- [ [ (0,0,0),(0,1,0),(0,2,0) ]
-- , [ (0,0,1),(0,1,1),(0,2,1) ]
-- , [ (0,0,2),(0,1,2),(0,2,2) ]
-- , [ (0,0,3),(0,1,3),(0,2,3) ]
-- ])
--
backpermute :: (Source r' ix' e, Index ix) =>
               ix -- ^ Size of the result array
            -> (ix -> ix') -- ^ A function that maps indices of old array into the source one.
            -> Array r' ix' e -- ^ Source array.
            -> Array D ix e
backpermute sz ixF !arr = makeArray (getComp arr) sz (evaluateAt arr . ixF)
{-# INLINE backpermute #-}


-- | Append two arrays together along a particular dimension.
append :: (Source r1 ix e, Source r2 ix e) =>
          Dim -> Array r1 ix e -> Array r2 ix e -> Maybe (Array D ix e)
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
           Dim -> Array r1 ix e -> Array r2 ix e -> Array D ix e
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
splitAt :: (Size r ix e, r' ~ EltRepr r ix) =>
  Dim -> ix -> Array r ix e -> Maybe (Array r' ix e, Array r' ix e)
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
