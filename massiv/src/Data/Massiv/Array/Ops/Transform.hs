{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- |
-- Module      : Data.Massiv.Array.Ops.Transform
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
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
  -- ** Reverse
  , reverse
  , reverse'
  , reverseM
  -- ** Backpermute
  , backpermuteM
  , backpermute'
  -- ** Resize
  , resizeM
  , resize'
  , flatten
  -- ** Extract
  , extractM
  , extract'
  , extractFromToM
  , extractFromTo'
  , deleteRowsM
  , deleteColumnsM
  , deleteRegionM
  -- ** Append/Split
  , cons
  , unconsM
  , snoc
  , unsnocM
  , appendM
  , append'
  , concatM
  , concat'
  , splitAtM
  , splitAt'
  , splitExtractM
  -- ** Upsample/Downsample
  , upsample
  , downsample
  -- ** Zoom
  , zoomWithGrid
  -- ** Transform
  , transformM
  , transform'
  , transform2M
  , transform2'
  ) where

import Control.Scheduler (traverse_)
import Control.Monad as M (foldM_, unless, forM_)
import Data.Bifunctor (bimap)
import Data.Foldable as F (foldl', foldrM, toList)
import qualified Data.List as L (uncons)
import Data.Massiv.Array.Delayed.Pull
import Data.Massiv.Array.Delayed.Push
import Data.Massiv.Array.Mutable
import Data.Massiv.Array.Ops.Construct
import Data.Massiv.Array.Ops.Map
import Data.Massiv.Core.Common
import Data.Massiv.Core.Index.Internal (Sz(SafeSz))
import Prelude as P hiding (concat, splitAt, traverse, mapM_, reverse)


-- | Extract a sub-array from within a larger source array. Array that is being extracted must be
-- fully encapsulated in a source array, otherwise `SizeSubregionException` will be thrown.
extractM :: (MonadThrow m, Extract r ix e)
         => ix -- ^ Starting index
         -> Sz ix -- ^ Size of the resulting array
         -> Array r ix e -- ^ Source array
         -> m (Array (R r) ix e)
extractM !sIx !newSz !arr
  | isSafeIndex sz1 sIx && isSafeIndex eIx1 sIx && isSafeIndex sz1 eIx =
    pure $ unsafeExtract sIx newSz arr
  | otherwise = throwM $ SizeSubregionException (size arr) sIx newSz
  where
    sz1 = Sz (liftIndex (+1) (unSz (size arr)))
    eIx1 = Sz (liftIndex (+1) eIx)
    eIx = liftIndex2 (+) sIx $ unSz newSz
{-# INLINE extractM #-}

-- | Same as `extractM`, but will throw a runtime exception from pure code if supplied dimensions
-- are incorrect.
--
-- @since 0.1.0
extract' :: Extract r ix e
        => ix -- ^ Starting index
        -> Sz ix -- ^ Size of the resulting array
        -> Array r ix e -- ^ Source array
        -> Array (R r) ix e
extract' sIx newSz = either throw id . extractM sIx newSz
{-# INLINE extract' #-}


-- | Similar to `extractM`, except it takes starting and ending index. Result array will not include
-- the ending index.
--
-- @since 0.3.0
extractFromToM :: (MonadThrow m, Extract r ix e) =>
                  ix -- ^ Starting index
               -> ix -- ^ Index up to which elements should be extracted.
               -> Array r ix e -- ^ Source array.
               -> m (Array (R r) ix e)
extractFromToM sIx eIx = extractM sIx (Sz (liftIndex2 (-) eIx sIx))
{-# INLINE extractFromToM #-}

-- | Same as `extractFromTo`, but throws an error on invalid indices.
--
-- @since 0.2.4
extractFromTo' :: Extract r ix e =>
                 ix -- ^ Starting index
              -> ix -- ^ Index up to which elmenets should be extracted.
              -> Array r ix e -- ^ Source array.
              -> Array (R r) ix e
extractFromTo' sIx eIx = extract' sIx $ Sz (liftIndex2 (-) eIx sIx)
{-# INLINE extractFromTo' #-}


-- | /O(1)/ - Changes the shape of an array. Returns `Nothing` if total
-- number of elements does not match the source array.
--
-- @since 0.3.0
resizeM ::
     (MonadThrow m, Index ix', Load r ix e, Resize r ix)
  => Sz ix'
  -> Array r ix e
  -> m (Array r ix' e)
resizeM sz arr = guardNumberOfElements (size arr) sz >> pure (unsafeResize sz arr)
{-# INLINE resizeM #-}

-- | Same as `resizeM`, but will throw an error if supplied dimensions are incorrect.
--
-- @since 0.1.0
resize' :: (Index ix', Load r ix e, Resize r ix) => Sz ix' -> Array r ix e -> Array r ix' e
resize' sz = either throw id . resizeM sz
{-# INLINE resize' #-}

-- | /O(1)/ - Reduce a multi-dimensional array into a flat vector
--
-- @since 0.3.1
flatten :: (Load r ix e, Resize r ix) => Array r ix e -> Array r Ix1 e
flatten arr = unsafeResize (SafeSz (totalElem (size arr))) arr
{-# INLINE flatten #-}


-- | Transpose a 2-dimensional array
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> arr = makeArrayLinearR D Seq (Sz (2 :. 3)) id
-- >>> arr
-- Array D Seq (Sz (2 :. 3))
--   [ [ 0, 1, 2 ]
--   , [ 3, 4, 5 ]
--   ]
-- >>> transpose arr
-- Array D Seq (Sz (3 :. 2))
--   [ [ 0, 3 ]
--   , [ 1, 4 ]
--   , [ 2, 5 ]
--   ]
--
-- @since 0.1.0
transpose :: Source r Ix2 e => Array r Ix2 e -> Array D Ix2 e
transpose = transposeInner
{-# INLINE [1] transpose #-}

{-# RULES
"transpose . transpose" [~1] forall arr . transpose (transpose arr) = delay arr
"transposeInner . transposeInner" [~1] forall arr . transposeInner (transposeInner arr) = delay arr
"transposeOuter . transposeOuter" [~1] forall arr . transposeOuter (transposeOuter arr) = delay arr
 #-}


-- | Transpose inner two dimensions of at least rank-2 array.
--
-- ===__Examples__
--
-- >>> import Data.Massiv.Array
-- >>> arr = makeArrayLinearR U Seq (Sz (2 :> 3 :. 4)) id
-- >>> arr
-- Array U Seq (Sz (2 :> 3 :. 4))
--   [ [ [ 0, 1, 2, 3 ]
--     , [ 4, 5, 6, 7 ]
--     , [ 8, 9, 10, 11 ]
--     ]
--   , [ [ 12, 13, 14, 15 ]
--     , [ 16, 17, 18, 19 ]
--     , [ 20, 21, 22, 23 ]
--     ]
--   ]
-- >>> transposeInner arr
-- Array D Seq (Sz (3 :> 2 :. 4))
--   [ [ [ 0, 1, 2, 3 ]
--     , [ 12, 13, 14, 15 ]
--     ]
--   , [ [ 4, 5, 6, 7 ]
--     , [ 16, 17, 18, 19 ]
--     ]
--   , [ [ 8, 9, 10, 11 ]
--     , [ 20, 21, 22, 23 ]
--     ]
--   ]
--
-- @since 0.1.0
transposeInner :: (Index (Lower ix), Source r' ix e)
               => Array r' ix e -> Array D ix e
transposeInner !arr = makeArray (getComp arr) newsz newVal
  where
    transInner !ix =
      either throwImpossible id $ do
        n <- getDimM ix dix
        m <- getDimM ix (dix - 1)
        ix' <- setDimM ix dix m
        setDimM ix' (dix - 1) n
    {-# INLINE transInner #-}
    newVal = unsafeIndex arr . transInner
    {-# INLINE newVal #-}
    !newsz = Sz (transInner (unSz (size arr)))
    !dix = dimensions newsz
{-# INLINE [1] transposeInner #-}

-- | Transpose outer two dimensions of at least rank-2 array.
--
-- ====__Examples__
--
-- >>> import Data.Massiv.Array
-- >>> :set -XTypeApplications
-- >>> arr = makeArrayLinear @U Seq (Sz (2 :> 3 :. 4)) id
-- >>> arr
-- Array U Seq (Sz (2 :> 3 :. 4))
--   [ [ [ 0, 1, 2, 3 ]
--     , [ 4, 5, 6, 7 ]
--     , [ 8, 9, 10, 11 ]
--     ]
--   , [ [ 12, 13, 14, 15 ]
--     , [ 16, 17, 18, 19 ]
--     , [ 20, 21, 22, 23 ]
--     ]
--   ]
-- >>> transposeOuter arr
-- Array D Seq (Sz (2 :> 4 :. 3))
--   [ [ [ 0, 4, 8 ]
--     , [ 1, 5, 9 ]
--     , [ 2, 6, 10 ]
--     , [ 3, 7, 11 ]
--     ]
--   , [ [ 12, 16, 20 ]
--     , [ 13, 17, 21 ]
--     , [ 14, 18, 22 ]
--     , [ 15, 19, 23 ]
--     ]
--   ]
--
--
-- @since 0.1.0
transposeOuter :: (Index (Lower ix), Source r' ix e)
               => Array r' ix e -> Array D ix e
transposeOuter !arr = makeArray (getComp arr) newsz newVal
  where
    transOuter !ix =
      either throwImpossible id $ do
        n <- getDimM ix 1
        m <- getDimM ix 2
        ix' <- setDimM ix 1 m
        setDimM ix' 2 n
    {-# INLINE transOuter #-}
    newVal = unsafeIndex arr . transOuter
    {-# INLINE newVal #-}
    !newsz = Sz (transOuter (unSz (size arr)))
{-# INLINE [1] transposeOuter #-}

-- | Reverse an array along some dimension. Dimension supplied is checked at compile time.
--
-- @since 0.4.1
reverse :: (IsIndexDimension ix n, Source r ix e) => Dimension n -> Array r ix e -> Array D ix e
reverse dim = reverse' (fromDimension dim)
{-# INLINE reverse #-}

-- | Reverse an array along some dimension. Throws `IndexDimensionException` for an incorrect
-- dimension.
--
-- @since 0.4.1
reverseM :: (MonadThrow m, Source r ix e) => Dim -> Array r ix e -> m (Array D ix e)
reverseM dim arr = do
  let sz = size arr
  k <- getDimM (unSz sz) dim
  pure $ makeArray (getComp arr) sz $ \ ix ->
    unsafeIndex arr (snd $ modifyDim' ix dim (\i -> k - i - 1))
{-# INLINE reverseM #-}

-- | Reverse an array along some dimension. Same as `reverseM`, but throws exception from
-- pure code.
--
-- @since 0.4.1
reverse' :: Source r ix e => Dim -> Array r ix e -> Array D ix e
reverse' dim = either throw id . reverseM dim
{-# INLINE reverse' #-}

-- | Rearrange elements of an array into a new one by using a function that maps indices of the
-- newly created one into the old one. This function can throw `IndexOutOfBoundsException`.
--
-- ===__Examples__
--
-- >>> import Data.Massiv.Array
-- >>> :set -XTypeApplications
-- >>> arr = makeArrayLinear @D Seq (Sz (2 :> 3 :. 4)) id
-- >>> arr
-- Array D Seq (Sz (2 :> 3 :. 4))
--   [ [ [ 0, 1, 2, 3 ]
--     , [ 4, 5, 6, 7 ]
--     , [ 8, 9, 10, 11 ]
--     ]
--   , [ [ 12, 13, 14, 15 ]
--     , [ 16, 17, 18, 19 ]
--     , [ 20, 21, 22, 23 ]
--     ]
--   ]
-- >>> backpermuteM @U (Sz (4 :. 2)) (\(i :. j) -> j :> j :. i) arr
-- Array U Seq (Sz (4 :. 2))
--   [ [ 0, 16 ]
--   , [ 1, 17 ]
--   , [ 2, 18 ]
--   , [ 3, 19 ]
--   ]
--
-- @since 0.3.0
backpermuteM ::
     forall r ix e r' ix' m.
     (Mutable r ix e, Source r' ix' e, MonadUnliftIO m, PrimMonad m, MonadThrow m)
  => Sz ix -- ^ Size of the result array
  -> (ix -> ix') -- ^ A function that maps indices of the new array into the source one.
  -> Array r' ix' e -- ^ Source array.
  -> m (Array r ix e)
backpermuteM sz ixF !arr = generateArray (getComp arr) sz (evaluateM arr . ixF)
{-# INLINE backpermuteM #-}

-- | Similar to `backpermuteM`, with a few notable differences:
--
-- * Creates a delayed array, instead of manifest, therefore it can be fused
-- * Respects computation strategy, so it can be parallelized
-- * Throws a runtime `IndexOutOfBoundsException` from pure code.
--
-- @since 0.3.0
backpermute' :: (Source r' ix' e, Index ix) =>
                Sz ix -- ^ Size of the result array
             -> (ix -> ix') -- ^ A function that maps indices of the new array into the source one.
             -> Array r' ix' e -- ^ Source array.
             -> Array D ix e
backpermute' sz ixF !arr = makeArray (getComp arr) sz (evaluate' arr . ixF)
{-# INLINE backpermute' #-}


-- | /O(1)/ - Add an element to the vector from the left side
--
-- @since 0.3.0
cons :: e -> Array DL Ix1 e -> Array DL Ix1 e
cons e arr =
  arr
    { dlSize = SafeSz (1 + unSz (dlSize arr))
    , dlLoad =
        \scheduler startAt uWrite ->
          uWrite startAt e >> dlLoad arr scheduler (startAt + 1) uWrite
    }
{-# INLINE cons #-}

-- | /O(1)/ - Take one element off the vector from the left side.
--
-- @since 0.3.0
unconsM :: (MonadThrow m, Source r Ix1 e) => Array r Ix1 e -> m (e, Array D Ix1 e)
unconsM arr
  | 0 == totalElem sz = throwM $ SizeEmptyException sz
  | otherwise =
    pure
      ( unsafeLinearIndex arr 0
      , makeArray (getComp arr) (SafeSz (unSz sz - 1)) (\ !i -> unsafeLinearIndex arr (i + 1)))
  where
    !sz = size arr
{-# INLINE unconsM #-}

-- | /O(1)/ - Add an element to the vector from the right side
--
-- @since 0.3.0
snoc :: Array DL Ix1 e -> e -> Array DL Ix1 e
snoc arr e =
  arr
    { dlSize = SafeSz (1 + k)
    , dlLoad =
        \scheduler startAt uWrite -> dlLoad arr scheduler startAt uWrite >> uWrite (k + startAt) e
    }
  where
    !k = unSz (size arr)
{-# INLINE snoc #-}


-- | /O(1)/ - Take one element off the vector from the right side.
--
-- @since 0.3.0
unsnocM :: (MonadThrow m, Source r Ix1 e) => Array r Ix1 e -> m (Array D Ix1 e, e)
unsnocM arr
  | k < 0 = throwM $ SizeEmptyException sz
  | otherwise =
    pure (makeArray (getComp arr) (SafeSz k) (unsafeLinearIndex arr), unsafeLinearIndex arr k)
  where
    !sz = size arr
    !k = unSz sz - 1
{-# INLINE unsnocM #-}



-- | Append two arrays together along a particular dimension. Sizes of both arrays must match, with
-- an allowed exception of the dimension they are being appended along, otherwise `Nothing` is
-- returned.
--
-- ===__Examples__
--
-- Append two 2D arrays along both dimensions. Note that they do agree on inner dimensions.
--
-- >>> import Data.Massiv.Array
-- >>> arrA = makeArrayR U Seq (Sz2 2 3) (\(i :. j) -> ('A', i, j))
-- >>> arrB = makeArrayR U Seq (Sz2 2 3) (\(i :. j) -> ('B', i, j))
-- >>> appendM 1 arrA arrB
-- Array DL Seq (Sz (2 :. 6))
--   [ [ ('A',0,0), ('A',0,1), ('A',0,2), ('B',0,0), ('B',0,1), ('B',0,2) ]
--   , [ ('A',1,0), ('A',1,1), ('A',1,2), ('B',1,0), ('B',1,1), ('B',1,2) ]
--   ]
-- >>> appendM 2 arrA arrB
-- Array DL Seq (Sz (4 :. 3))
--   [ [ ('A',0,0), ('A',0,1), ('A',0,2) ]
--   , [ ('A',1,0), ('A',1,1), ('A',1,2) ]
--   , [ ('B',0,0), ('B',0,1), ('B',0,2) ]
--   , [ ('B',1,0), ('B',1,1), ('B',1,2) ]
--   ]
--
-- Now appending arrays with different sizes:
--
-- >>> arrC = makeArrayR U Seq (Sz (2 :. 4)) (\(i :. j) -> ('C', i, j))
-- >>> appendM 1 arrA arrC
-- Array DL Seq (Sz (2 :. 7))
--   [ [ ('A',0,0), ('A',0,1), ('A',0,2), ('C',0,0), ('C',0,1), ('C',0,2), ('C',0,3) ]
--   , [ ('A',1,0), ('A',1,1), ('A',1,2), ('C',1,0), ('C',1,1), ('C',1,2), ('C',1,3) ]
--   ]
-- >>> appendM 2 arrA arrC
-- *** Exception: SizeMismatchException: (Sz (2 :. 3)) vs (Sz (2 :. 4))
--
-- @since 0.3.0
appendM :: (MonadThrow m, Source r1 ix e, Source r2 ix e) =>
          Dim -> Array r1 ix e -> Array r2 ix e -> m (Array DL ix e)
appendM n !arr1 !arr2 = do
  let !sz1 = size arr1
      !sz2 = size arr2
  (k1, szl1) <- pullOutSzM sz1 n
  (k2, szl2) <- pullOutSzM sz2 n
  unless (szl1 == szl2) $ throwM $ SizeMismatchException sz1 sz2
  let k1' = unSz k1
  newSz <- insertSzM szl1 n (SafeSz (k1' + unSz k2))
  return $
    DLArray
      { dlComp = getComp arr1 <> getComp arr2
      , dlSize = newSz
      , dlDefault = Nothing
      , dlLoad =
          \scheduler startAt dlWrite -> do
            scheduleWork scheduler $
              iterM_ zeroIndex (unSz sz1) (pureIndex 1) (<) $ \ix ->
                dlWrite (startAt + toLinearIndex newSz ix) (unsafeIndex arr1 ix)
            scheduleWork scheduler $
              iterM_ zeroIndex (unSz sz2) (pureIndex 1) (<) $ \ix ->
                let i = getDim' ix n
                    ix' = setDim' ix n (i + k1')
                 in dlWrite (startAt + toLinearIndex newSz ix') (unsafeIndex arr2 ix)
      }
{-# INLINE appendM #-}


-- | Same as `appendM`, but will throw an exception in pure code on mismatched sizes.
--
-- @since 0.3.0
append' :: (Source r1 ix e, Source r2 ix e) =>
           Dim -> Array r1 ix e -> Array r2 ix e -> Array DL ix e
append' dim arr1 arr2 = either throw id $ appendM dim arr1 arr2
{-# INLINE append' #-}

-- | Concat many arrays together along some dimension.
--
-- @since 0.3.0
concat' :: (Foldable f, Source r ix e) => Dim -> f (Array r ix e) -> Array DL ix e
concat' n arrs = either throw id $ concatM n arrs
{-# INLINE concat' #-}

-- | Concatenate many arrays together along some dimension. It is important that all sizes are
-- equal, with an exception of the dimensions along which concatenation happens, otherwise it doues
-- result in a `SizeMismatchException` exception.
--
-- @since 0.3.0
concatM ::
     (MonadThrow m, Foldable f, Source r ix e) => Dim -> f (Array r ix e) -> m (Array DL ix e)
concatM n !arrsF =
  case L.uncons (F.toList arrsF) of
    Nothing -> pure empty
    Just (a, arrs) -> do
      let sz = unSz (size a)
          szs = P.map (unSz . size) arrs
      (k, szl) <- pullOutDimM sz n
      -- / remove the dimension out of all sizes along which concatenation will happen
      (ks, szls) <-
        F.foldrM (\ !csz (ks, szls) -> bimap (: ks) (: szls) <$> pullOutDimM csz n) ([], []) szs
      -- / make sure to fail as soon as at least one of the arrays has a mismatching inner size
      traverse_
        (\(sz', _) -> throwM (SizeMismatchException (SafeSz sz) (SafeSz sz')))
        (dropWhile ((== szl) . snd) $ P.zip szs szls)
      let kTotal = SafeSz $ F.foldl' (+) k ks
      newSz <- insertSzM (SafeSz szl) n kTotal
      return $
        DLArray
          { dlComp = mconcat $ P.map getComp arrs
          , dlSize = newSz
          , dlDefault = Nothing
          , dlLoad =
              \scheduler startAt dlWrite ->
                let arrayLoader !kAcc (kCur, arr) = do
                      scheduleWork scheduler $
                        iterM_ zeroIndex (unSz (size arr)) (pureIndex 1) (<) $ \ix ->
                          let i = getDim' ix n
                              ix' = setDim' ix n (i + kAcc)
                           in dlWrite (startAt + toLinearIndex newSz ix') (unsafeIndex arr ix)
                      pure (kAcc + kCur)
                 in M.foldM_ arrayLoader 0 $ (k, a) : P.zip ks arrs
          }
{-# INLINE concatM #-}


-- | /O(1)/ - Split an array at an index along a specified dimension.
--
-- @since 0.3.0
splitAtM ::
     (MonadThrow m, Extract r ix e)
  => Dim -- ^ Dimension along which to split
  -> Int -- ^ Index along the dimension to split at
  -> Array r ix e -- ^ Source array
  -> m (Array (R r) ix e, Array (R r) ix e)
splitAtM dim i arr = do
  let Sz sz = size arr
  eIx <- setDimM sz dim i
  sIx <- setDimM zeroIndex dim i
  arr1 <- extractFromToM zeroIndex eIx arr
  arr2 <- extractFromToM sIx sz arr
  return (arr1, arr2)
{-# INLINE splitAtM #-}

-- | Same as `splitAt`, but will throw an error instead of returning `Nothing` on wrong dimension
-- and index out of bounds.
--
-- @since 0.1.0
splitAt' :: Extract r ix e =>
            Dim -> Int -> Array r ix e -> (Array (R r) ix e, Array (R r) ix e)
splitAt' dim i arr = either throw id $ splitAtM dim i arr
{-# INLINE splitAt' #-}


-- | Split an array in three parts across some dimension
--
-- @since 0.3.5
splitExtractM ::
     (MonadThrow m, Extract r ix e, Source (R r) ix e)
  => Dim -- ^ Dimension along which to do the extraction
  -> Ix1 -- ^ Start index along the dimension that needs to be extracted
  -> Sz Ix1 -- ^ Size of the extracted array along the dimension that it will be extracted
  -> Array r ix e
  -> m (Array (R r) ix e, Array (R r) ix e, Array (R r) ix e)
splitExtractM dim startIx1 (Sz extractSzIx1) arr = do
  let Sz szIx = size arr
  midStartIx <- setDimM zeroIndex dim startIx1
  midExtractSzIx <- setDimM szIx dim extractSzIx1
  midArr <- extractM midStartIx (Sz midExtractSzIx) arr
  leftArrSzIx <- setDimM szIx dim startIx1
  leftArr <- extractM zeroIndex (Sz leftArrSzIx) arr
  rightArrStartIx <- setDimM zeroIndex dim (startIx1 + extractSzIx1)
  rightArr <- extractFromToM rightArrStartIx szIx arr
  pure (leftArr, midArr, rightArr)
{-# INLINE splitExtractM #-}

-- | Delete a region from an array along the specified dimension.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> arr = fromIx3 <$> (0 :> 0 :. 0 ..: 3 :> 2 :. 6)
-- >>> deleteRegionM 1 2 3 arr
-- Array DL Seq (Sz (3 :> 2 :. 3))
--   [ [ [ (0,0,0), (0,0,1), (0,0,5) ]
--     , [ (0,1,0), (0,1,1), (0,1,5) ]
--     ]
--   , [ [ (1,0,0), (1,0,1), (1,0,5) ]
--     , [ (1,1,0), (1,1,1), (1,1,5) ]
--     ]
--   , [ [ (2,0,0), (2,0,1), (2,0,5) ]
--     , [ (2,1,0), (2,1,1), (2,1,5) ]
--     ]
--   ]
-- >>> v = Ix1 0 ... 10
-- >>> deleteRegionM 1 3 5 v
-- Array DL Seq (Sz1 6)
--   [ 0, 1, 2, 8, 9, 10 ]
--
-- @since 0.3.5
deleteRegionM ::
     (MonadThrow m, Extract r ix e, Source (R r) ix e)
  => Dim -- ^ Along which axis should the removal happen
  -> Ix1 -- ^ At which index to start dropping slices
  -> Sz Ix1 -- ^ Number of slices to drop
  -> Array r ix e -- ^ Array that will have it's subarray removed
  -> m (Array DL ix e)
deleteRegionM dim ix sz arr = do
  (leftArr, _, rightArr) <- splitExtractM dim ix sz arr
  appendM dim leftArr rightArr
{-# INLINE deleteRegionM #-}

-- | Similar to `deleteRegionM`, but drop a specified number of rows from an array that
-- has at least 2 dimensions.
--
-- ====__Example__
--
-- >>> import Data.Massiv.Array
-- >>> arr = fromIx2 <$> (0 :. 0 ..: 3 :. 6)
-- >>> arr
-- Array D Seq (Sz (3 :. 6))
--   [ [ (0,0), (0,1), (0,2), (0,3), (0,4), (0,5) ]
--   , [ (1,0), (1,1), (1,2), (1,3), (1,4), (1,5) ]
--   , [ (2,0), (2,1), (2,2), (2,3), (2,4), (2,5) ]
--   ]
-- >>> deleteRowsM 1 1 arr
-- Array DL Seq (Sz (2 :. 6))
--   [ [ (0,0), (0,1), (0,2), (0,3), (0,4), (0,5) ]
--   , [ (2,0), (2,1), (2,2), (2,3), (2,4), (2,5) ]
--   ]
--
-- @since 0.3.5
deleteRowsM ::
     (MonadThrow m, Extract r ix e, Source (R r) ix e, Index (Lower ix))
  => Ix1
  -> Sz Ix1
  -> Array r ix e
  -> m (Array DL ix e)
deleteRowsM = deleteRegionM 2
{-# INLINE deleteRowsM #-}

-- | Similar to `deleteRegionM`, but drop a specified number of columns an array.
--
-- ====__Example__
--
-- >>> import Data.Massiv.Array
-- >>> arr = fromIx2 <$> (0 :. 0 ..: 3 :. 6)
-- >>> arr
-- Array D Seq (Sz (3 :. 6))
--   [ [ (0,0), (0,1), (0,2), (0,3), (0,4), (0,5) ]
--   , [ (1,0), (1,1), (1,2), (1,3), (1,4), (1,5) ]
--   , [ (2,0), (2,1), (2,2), (2,3), (2,4), (2,5) ]
--   ]
-- >>> deleteColumnsM 2 3 arr
-- Array DL Seq (Sz (3 :. 3))
--   [ [ (0,0), (0,1), (0,5) ]
--   , [ (1,0), (1,1), (1,5) ]
--   , [ (2,0), (2,1), (2,5) ]
--   ]
--
-- @since 0.3.5
deleteColumnsM ::
     (MonadThrow m, Extract r ix e, Source (R r) ix e)
  => Ix1
  -> Sz Ix1
  -> Array r ix e
  -> m (Array DL ix e)
deleteColumnsM = deleteRegionM 1
{-# INLINE deleteColumnsM #-}


-- | Discard elements from the source array according to the stride.
--
-- @since 0.3.0
downsample :: Source r ix e => Stride ix -> Array r ix e -> Array DL ix e
downsample stride arr =
  DLArray
    { dlComp = getComp arr
    , dlSize = resultSize
    , dlDefault = defaultElement arr
    , dlLoad =
        \scheduler startAt dlWrite ->
          splitLinearlyWithStartAtM_
            scheduler
            startAt
            (totalElem resultSize)
            (pure . unsafeLinearWriteWithStride)
            dlWrite
    }
  where
    resultSize = strideSize stride (size arr)
    strideIx = unStride stride
    unsafeLinearWriteWithStride =
      unsafeIndex arr . liftIndex2 (*) strideIx . fromLinearIndex resultSize
    {-# INLINE unsafeLinearWriteWithStride #-}
{-# INLINE downsample #-}


-- | Insert the same element into a `Load`able array according to the stride.
--
-- @since 0.3.0
upsample
  :: Load r ix e => e -> Stride ix -> Array r ix e -> Array DL ix e
upsample !fillWith safeStride arr =
  DLArray
    { dlComp = getComp arr
    , dlSize = newsz
    , dlDefault = Just fillWith
    , dlLoad =
        \scheduler startAt dlWrite -> do
          M.forM_ (defaultElement arr) $ \prevFillWith ->
            loopM_
              startAt
              (< totalElem sz)
              (+ 1)
              (\i -> dlWrite (adjustLinearStride (i + startAt)) prevFillWith)
          loadArrayM scheduler arr (\i -> dlWrite (adjustLinearStride (i + startAt)))
    }
  where
    adjustLinearStride = toLinearIndex newsz . timesStride . fromLinearIndex sz
    {-# INLINE adjustLinearStride #-}
    timesStride !ix = liftIndex2 (*) stride ix
    {-# INLINE timesStride #-}
    !stride = unStride safeStride
    !sz = size arr
    !newsz = SafeSz (timesStride $ unSz sz)
{-# INLINE upsample #-}


-- | General array transformation, that forces computation and produces a manifest array.
--
-- @since 0.3.0
transformM ::
     forall r ix e r' ix' e' a m.
     (Mutable r ix e, Source r' ix' e', MonadUnliftIO m, PrimMonad m, MonadThrow m)
  => (Sz ix' -> m (Sz ix, a))
  -> (a -> (ix' -> m e') -> ix -> m e)
  -> Array r' ix' e'
  -> m (Array r ix e)
transformM getSzM getM arr = do
  (sz, a) <- getSzM (size arr)
  generateArray (getComp arr) sz (getM a (evaluateM arr))
{-# INLINE transformM #-}


-- | General array transformation
--
-- @since 0.3.0
transform' ::
     (Source r' ix' e', Index ix)
  => (Sz ix' -> (Sz ix, a))
  -> (a -> (ix' -> e') -> ix -> e)
  -> Array r' ix' e'
  -> Array D ix e
transform' getSz get arr = makeArray (getComp arr) sz (get a (evaluate' arr))
  where
    (sz, a) = getSz (size arr)
{-# INLINE transform' #-}

-- | Same as `transformM`, but operates on two arrays
--
-- @since 0.3.0
transform2M ::
     (Mutable r ix e, Source r1 ix1 e1, Source r2 ix2 e2, MonadUnliftIO m, PrimMonad m, MonadThrow m)
  => (Sz ix1 -> Sz ix2 -> m (Sz ix, a))
  -> (a -> (ix1 -> m e1) -> (ix2 -> m e2) -> ix -> m e)
  -> Array r1 ix1 e1
  -> Array r2 ix2 e2
  -> m (Array r ix e)
transform2M getSzM getM arr1 arr2 = do
  (sz, a) <- getSzM (size arr1) (size arr2)
  generateArray (getComp arr1 <> getComp arr2) sz (getM a (evaluateM arr1) (evaluateM arr2))
{-# INLINE transform2M #-}


-- | Same as `transform'`, but operates on two arrays
--
-- @since 0.3.0
transform2' ::
     (Source r1 ix1 e1, Source r2 ix2 e2, Index ix)
  => (Sz ix1 -> Sz ix2 -> (Sz ix, a))
  -> (a -> (ix1 -> e1) -> (ix2 -> e2) -> ix -> e)
  -> Array r1 ix1 e1
  -> Array r2 ix2 e2
  -> Array D ix e
transform2' getSz get arr1 arr2 =
  makeArray (getComp arr1 <> getComp arr2) sz (get a (evaluate' arr1) (evaluate' arr2))
  where
    (sz, a) = getSz (size arr1) (size arr2)
{-# INLINE transform2' #-}



-- | Replicate each element of the array by a factor in stride along each dimension and surround each
-- such group with a box of supplied grid value. It will essentially zoom up an array and create a
-- grid around each element from the original array. Very useful for zooming up images to inspect
-- individual pixels.
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Array as A
-- >>> zoomWithGrid 0 (Stride (2 :. 3)) $ resize' (Sz2 3 2) (Ix1 1 ... 6)
-- Array DL Seq (Sz (10 :. 9))
--   [ [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
--   , [ 0, 1, 1, 1, 0, 2, 2, 2, 0 ]
--   , [ 0, 1, 1, 1, 0, 2, 2, 2, 0 ]
--   , [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
--   , [ 0, 3, 3, 3, 0, 4, 4, 4, 0 ]
--   , [ 0, 3, 3, 3, 0, 4, 4, 4, 0 ]
--   , [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
--   , [ 0, 5, 5, 5, 0, 6, 6, 6, 0 ]
--   , [ 0, 5, 5, 5, 0, 6, 6, 6, 0 ]
--   , [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
--   ]
--
-- @since 0.3.1
zoomWithGrid ::
     Source r ix e
  => e -- ^ Value to use for the grid
  -> Stride ix -- ^ Scaling factor
  -> Array r ix e -- ^ Source array
  -> Array DL ix e
zoomWithGrid gridVal (Stride zoomFactor) arr =
  unsafeMakeLoadArray Seq newSz (Just gridVal) $ \scheduler _ writeElement ->
    iforSchedulerM_ scheduler arr $ \ !ix !e -> do
      let !kix = liftIndex2 (*) ix kx
      mapM_ (\ !ix' -> writeElement (toLinearIndex newSz ix') e) $
        range Seq (liftIndex (+1) kix) (liftIndex2 (+) kix kx)
  where
    !kx = liftIndex (+1) zoomFactor
    !lastNewIx = liftIndex2 (*) kx $ unSz (size arr)
    !newSz = Sz (liftIndex (+1) lastNewIx)
{-# INLINE zoomWithGrid #-}
