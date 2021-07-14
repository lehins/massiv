{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- |
-- Module      : Data.Massiv.Array.Ops.Transform
-- Copyright   : (c) Alexey Kuleshevich 2018-2021
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
  , appendOuterM
  , appendM
  , append'
  , concatOuterM
  , concatM
  , concat'
  , stackSlicesM
  , stackOuterSlicesM
  , stackInnerSlicesM
  , splitAtM
  , splitAt'
  , splitExtractM
  , replaceSlice
  , replaceOuterSlice
  -- ** Upsample/Downsample
  , upsample
  , downsample
  -- ** Zoom
  , zoom
  , zoomWithGrid
  -- ** Transform
  , transformM
  , transform'
  , transform2M
  , transform2'
  ) where

import Control.Scheduler (traverse_)
import Control.Monad as M (foldM_, forM_)
import Data.Bifunctor (bimap)
import Data.Foldable as F (foldl', foldrM, toList, length)
import qualified Data.List as L (uncons)
import Data.Massiv.Array.Delayed.Pull
import Data.Massiv.Array.Delayed.Push
import Data.Massiv.Array.Mutable
import Data.Massiv.Array.Ops.Construct
import Data.Massiv.Array.Ops.Map
import Data.Massiv.Core.Common
import Prelude as P hiding (concat, splitAt, traverse, mapM_, reverse, take, drop)


-- | Extract a sub-array from within a larger source array. Array that is being extracted must be
-- fully encapsulated in a source array, otherwise `SizeSubregionException` will be thrown.
extractM ::
     forall r ix e m. (Raises m, Index ix, Source r e)
  => ix -- ^ Starting index
  -> Sz ix -- ^ Size of the resulting array
  -> Array r ix e -- ^ Source array
  -> m (Array D ix e)
extractM !sIx !newSz !arr
  | isSafeIndex sz1 sIx && isSafeIndex eIx1 sIx && isSafeIndex sz1 eIx =
    pure $ unsafeExtract sIx newSz arr
  | otherwise = raiseM $ SizeSubregionException (size arr) sIx newSz
  where
    sz1 = Sz (liftIndex (+1) (unSz (size arr)))
    eIx1 = Sz (liftIndex (+1) eIx)
    eIx = liftIndex2 (+) sIx $ unSz newSz
{-# INLINE extractM #-}

-- | Same as `extractM`, but will throw a runtime exception from pure code if supplied dimensions
-- are incorrect.
--
-- @since 0.1.0
extract' ::
     forall r ix e. (HasCallStack, Index ix, Source r e)
  => ix -- ^ Starting index
  -> Sz ix -- ^ Size of the resulting array
  -> Array r ix e -- ^ Source array
  -> Array D ix e
extract' sIx newSz = raiseLeftImprecise . extractM sIx newSz
{-# INLINE extract' #-}


-- | Similar to `extractM`, except it takes starting and ending index. Result array will not include
-- the ending index.
--
-- @since 0.3.0
extractFromToM ::
     forall r ix e m. (Raises m, Index ix, Source r e)
  => ix -- ^ Starting index
  -> ix -- ^ Index up to which elements should be extracted.
  -> Array r ix e -- ^ Source array.
  -> m (Array D ix e)
extractFromToM sIx eIx = extractM sIx (Sz (liftIndex2 (-) eIx sIx))
{-# INLINE extractFromToM #-}

-- | Same as `extractFromTo`, but throws an error on invalid indices.
--
-- @since 0.2.4
extractFromTo' ::
     forall r ix e. (HasCallStack, Index ix, Source r e)
  => ix -- ^ Starting index
  -> ix -- ^ Index up to which elmenets should be extracted.
  -> Array r ix e -- ^ Source array.
  -> Array D ix e
extractFromTo' sIx eIx = extract' sIx $ Sz (liftIndex2 (-) eIx sIx)
{-# INLINE extractFromTo' #-}


-- | /O(1)/ - Changes the shape of an array. Returns `Nothing` if total
-- number of elements does not match the source array.
--
-- @since 0.3.0
resizeM ::
     forall r ix ix' e m. (Raises m, Index ix', Index ix, Resize r)
  => Sz ix'
  -> Array r ix e
  -> m (Array r ix' e)
resizeM sz arr = guardNumberOfElements (size arr) sz >> pure (unsafeResize sz arr)
{-# INLINE resizeM #-}

-- | Same as `resizeM`, but will throw an error if supplied dimensions are incorrect.
--
-- @since 0.1.0
resize' ::
     forall r ix ix' e. (HasCallStack, Index ix', Index ix, Resize r)
  => Sz ix'
  -> Array r ix e
  -> Array r ix' e
resize' sz = raiseLeftImprecise . resizeM sz
{-# INLINE resize' #-}

-- | /O(1)/ - Reduce a multi-dimensional array into a flat vector
--
-- @since 0.3.1
flatten :: forall r ix e. (Index ix, Resize r) => Array r ix e -> Vector r e
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
transpose :: forall r e. Source r e => Matrix r e -> Matrix D e
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
transposeInner ::
     forall r ix e. (Index (Lower ix), Index ix, Source r e)
  => Array r ix e
  -> Array D ix e
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
transposeOuter ::
     forall r ix e. (Index (Lower ix), Index ix, Source r e)
  => Array r ix e
  -> Array D ix e
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
-- ==== __Example__
--
-- >>> import Data.Massiv.Array as A
-- >>> arr = makeArrayLinear Seq (Sz2 4 5) (+10) :: Array D Ix2 Int
-- >>> arr
-- Array D Seq (Sz (4 :. 5))
--   [ [ 10, 11, 12, 13, 14 ]
--   , [ 15, 16, 17, 18, 19 ]
--   , [ 20, 21, 22, 23, 24 ]
--   , [ 25, 26, 27, 28, 29 ]
--   ]
-- >>> A.reverse Dim1 arr
-- Array D Seq (Sz (4 :. 5))
--   [ [ 14, 13, 12, 11, 10 ]
--   , [ 19, 18, 17, 16, 15 ]
--   , [ 24, 23, 22, 21, 20 ]
--   , [ 29, 28, 27, 26, 25 ]
--   ]
-- >>> A.reverse Dim2 arr
-- Array D Seq (Sz (4 :. 5))
--   [ [ 25, 26, 27, 28, 29 ]
--   , [ 20, 21, 22, 23, 24 ]
--   , [ 15, 16, 17, 18, 19 ]
--   , [ 10, 11, 12, 13, 14 ]
--   ]
--
-- @since 0.4.1
reverse ::
     forall n r ix e. (IsIndexDimension ix n, Index ix, Source r e)
  => Dimension n
  -> Array r ix e
  -> Array D ix e
reverse dim = reverse' (fromDimension dim)
{-# INLINE reverse #-}

-- | Similarly to `reverse`, flip an array along a particular dimension, but throws
-- `IndexDimensionException` for an incorrect dimension.
--
-- @since 0.4.1
reverseM ::
     forall r ix e m. (Raises m, Index ix, Source r e)
  => Dim
  -> Array r ix e
  -> m (Array D ix e)
reverseM dim arr = do
  let sz = size arr
  k <- getDimM (unSz sz) dim
  pure $ makeArray (getComp arr) sz $ \ ix ->
    unsafeIndex arr (snd $ modifyDim' ix dim (\i -> k - i - 1))
{-# INLINE reverseM #-}

-- | Reverse an array along some dimension. Same as `reverseM`, but throws the
-- `IndexDimensionException` from pure code.
--
-- @since 0.4.1
reverse' ::
     forall r ix e. (HasCallStack, Index ix, Source r e)
  => Dim
  -> Array r ix e
  -> Array D ix e
reverse' dim = raiseLeftImprecise . reverseM dim
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
     (Mutable r e, Index ix, Source r' e, Index ix', UnliftPrimal RW m)
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
backpermute' ::
     forall r ix ix' e. (HasCallStack, Source r e, Index ix, Index ix')
  => Sz ix' -- ^ Size of the result array
  -> (ix' -> ix) -- ^ A function that maps indices of the new array into the source one.
  -> Array r ix e -- ^ Source array.
  -> Array D ix' e
backpermute' sz ixF !arr = makeArray (getComp arr) sz (evaluate' arr . ixF)
{-# INLINE backpermute' #-}


-- | Append two arrays together along a particular dimension. Sizes of both arrays must match, with
-- an allowed exception of the dimension they are being appended along, otherwise `Nothing` is
-- returned.
--
-- ====__Examples__
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
appendM ::
     forall r1 r2 ix e m. (Raises m, Index ix, Source r1 e, Source r2 e)
  => Dim
  -> Array r1 ix e
  -> Array r2 ix e
  -> m (Array DL ix e)
appendM n !arr1 !arr2 = do
  let !sz1 = size arr1
      !sz2 = size arr2
  (k1, szl1) <- pullOutSzM sz1 n
  (k2, szl2) <- pullOutSzM sz2 n
  unless (szl1 == szl2) $ raiseM $ SizeMismatchException sz1 sz2
  let !k1' = unSz k1
  newSz <- insertSzM szl1 n (SafeSz (k1' + unSz k2))
  let load :: Loader e
      load scheduler !startAt dlWrite _dlSet = do
        scheduleWork scheduler $
          iterM_ zeroIndex (unSz sz1) (pureIndex 1) (<) $ \ix ->
            dlWrite (startAt + toLinearIndex newSz ix) (unsafeIndex arr1 ix)
        scheduleWork scheduler $
          iterM_ zeroIndex (unSz sz2) (pureIndex 1) (<) $ \ix ->
            let i = getDim' ix n
                ix' = setDim' ix n (i + k1')
             in dlWrite (startAt + toLinearIndex newSz ix') (unsafeIndex arr2 ix)
      {-# INLINE load #-}
  return $
    DLArray
      {dlComp = getComp arr1 <> getComp arr2, dlSize = newSz, dlLoad = load}
{-# INLINE appendM #-}


-- | Same as `appendM`, but will throw an exception in pure code on mismatched sizes.
--
-- @since 0.3.0
append' ::
     forall r1 r2 ix e. (HasCallStack, Index ix, Source r1 e, Source r2 e)
  => Dim
  -> Array r1 ix e
  -> Array r2 ix e
  -> Array DL ix e
append' dim arr1 arr2 = raiseLeftImprecise $ appendM dim arr1 arr2
{-# INLINE append' #-}

-- | Concat many arrays together along some dimension.
--
-- @since 0.3.0
concat' ::
     forall f r ix e. (HasCallStack, Foldable f, Index ix, Source r e)
  => Dim
  -> f (Array r ix e)
  -> Array DL ix e
concat' n = raiseLeftImprecise . concatM n
{-# INLINE concat' #-}

-- | Concatenate many arrays together along some dimension. It is important that all sizes are
-- equal, with an exception of the dimensions along which concatenation happens.
--
-- /__Exceptions__/: `IndexDimensionException`, `SizeMismatchException`
--
-- @since 0.3.0
concatM ::
     forall r ix e f m. (Raises m, Foldable f, Index ix, Source r e)
  => Dim
  -> f (Array r ix e)
  -> m (Array DL ix e)
concatM n !arrsF =
  case L.uncons (F.toList arrsF) of
    Nothing -> pure empty
    Just (a, arrs) -> do
      let sz = unSz (size a)
          szs = unSz . size <$> arrs
      (k, szl) <- pullOutDimM sz n
      -- / remove the dimension out of all sizes along which concatenation will happen
      (ks, szls) <-
        F.foldrM (\ !csz (ks, szls) -> bimap (: ks) (: szls) <$> pullOutDimM csz n) ([], []) szs
      -- / make sure to fail as soon as at least one of the arrays has a mismatching inner size
      traverse_
        (\(sz', _) -> raiseM (SizeMismatchException (SafeSz sz) (SafeSz sz')))
        (dropWhile ((== szl) . snd) $ P.zip szs szls)
      let kTotal = SafeSz $ F.foldl' (+) k ks
      newSz <- insertSzM (SafeSz szl) n kTotal
      let load :: Loader e
          load scheduler startAt dlWrite _dlSet =
            let arrayLoader !kAcc (kCur, arr) = do
                  scheduleWork scheduler $
                    iforM_ arr $ \ix e ->
                      let i = getDim' ix n
                          ix' = setDim' ix n (i + kAcc)
                       in dlWrite (startAt + toLinearIndex newSz ix') e
                  pure (kAcc + kCur)
             in M.foldM_ arrayLoader 0 $ (k, a) : P.zip ks arrs
          {-# INLINE load #-}
      return $
        DLArray {dlComp = foldMap getComp arrsF, dlSize = newSz, dlLoad = load}
{-# INLINE concatM #-}


-- | Stack slices on top of each other along the specified dimension.
--
-- /__Exceptions__/: `IndexDimensionException`, `SizeMismatchException`
--
-- ====__Examples__
--
-- Here are the three different ways to stack up two 2D Matrix pages into a 3D array.
--
-- >>> import Data.Massiv.Array as A
-- >>> x = compute (iterateN 3 succ 0) :: Matrix P Int
-- >>> y = compute (iterateN 3 succ 9) :: Matrix P Int
-- >>> x
-- Array P Seq (Sz (3 :. 3))
--   [ [ 1, 2, 3 ]
--   , [ 4, 5, 6 ]
--   , [ 7, 8, 9 ]
--   ]
-- >>> y
-- Array P Seq (Sz (3 :. 3))
--   [ [ 10, 11, 12 ]
--   , [ 13, 14, 15 ]
--   , [ 16, 17, 18 ]
--   ]
-- >>> stackSlicesM 1 [x, y] :: IO (Array DL Ix3 Int)
-- Array DL Seq (Sz (3 :> 3 :. 2))
--   [ [ [ 1, 10 ]
--     , [ 2, 11 ]
--     , [ 3, 12 ]
--     ]
--   , [ [ 4, 13 ]
--     , [ 5, 14 ]
--     , [ 6, 15 ]
--     ]
--   , [ [ 7, 16 ]
--     , [ 8, 17 ]
--     , [ 9, 18 ]
--     ]
--   ]
-- >>> stackSlicesM 2 [x, y] :: IO (Array DL Ix3 Int)
-- Array DL Seq (Sz (3 :> 2 :. 3))
--   [ [ [ 1, 2, 3 ]
--     , [ 10, 11, 12 ]
--     ]
--   , [ [ 4, 5, 6 ]
--     , [ 13, 14, 15 ]
--     ]
--   , [ [ 7, 8, 9 ]
--     , [ 16, 17, 18 ]
--     ]
--   ]
-- >>> stackSlicesM 3 [x, y] :: IO (Array DL Ix3 Int)
-- Array DL Seq (Sz (2 :> 3 :. 3))
--   [ [ [ 1, 2, 3 ]
--     , [ 4, 5, 6 ]
--     , [ 7, 8, 9 ]
--     ]
--   , [ [ 10, 11, 12 ]
--     , [ 13, 14, 15 ]
--     , [ 16, 17, 18 ]
--     ]
--   ]
--
-- @since 0.5.4
stackSlicesM ::
     forall r ix e f m. (Foldable f, Raises m, Index (Lower ix), Source r e, Index ix)
  => Dim
  -> f (Array r (Lower ix) e)
  -> m (Array DL ix e)
stackSlicesM dim !arrsF = do
  case L.uncons (F.toList arrsF) of
    Nothing -> pure empty
    Just (a, arrs) -> do
      let sz = size a
          len = SafeSz (F.length arrsF)
      -- / make sure all arrays have the same size
      M.forM_ arrsF $ \arr ->
         unless (sz == size arr) $ raiseM (SizeMismatchException sz (size arr))
      newSz <- insertSzM sz dim len
      let load :: Loader e
          load scheduler startAt dlWrite _dlSet =
            let loadIndex k ix = dlWrite (toLinearIndex newSz (insertDim' ix dim k) + startAt)
                arrayLoader !k arr = (k + 1) <$ scheduleWork scheduler (imapM_ (loadIndex k) arr)
                {-# INLINE arrayLoader #-}
             in M.foldM_ arrayLoader 0 arrsF
          {-# INLINE load #-}
      return $
        DLArray {dlComp = foldMap getComp arrs, dlSize = newSz, dlLoad = load}
{-# INLINE stackSlicesM #-}

-- | Specialized `stackSlicesM` to handling stacking from the outside. It is the inverse of
-- `Data.Massiv.Array.outerSlices`.
--
-- /__Exceptions__/: `SizeMismatchException`
--
-- ====__Examples__
--
-- In this example we stack vectors as row of a matrix from top to bottom:
--
-- >>> import Data.Massiv.Array as A
-- >>> x = compute (iterateN 3 succ 0) :: Matrix P Int
-- >>> x
-- Array P Seq (Sz (3 :. 3))
--   [ [ 1, 2, 3 ]
--   , [ 4, 5, 6 ]
--   , [ 7, 8, 9 ]
--   ]
-- >>> rows = outerSlices x
-- >>> A.mapM_ print rows
-- Array P Seq (Sz1 3)
--   [ 1, 2, 3 ]
-- Array P Seq (Sz1 3)
--   [ 4, 5, 6 ]
-- Array P Seq (Sz1 3)
--   [ 7, 8, 9 ]
-- >>> stackOuterSlicesM rows :: IO (Matrix DL Int)
-- Array DL Seq (Sz (3 :. 3))
--   [ [ 1, 2, 3 ]
--   , [ 4, 5, 6 ]
--   , [ 7, 8, 9 ]
--   ]
--
-- @since 0.5.4
stackOuterSlicesM ::
     forall r ix e f m. (Foldable f, Raises m, Index (Lower ix), Source r e, Index ix)
  => f (Array r (Lower ix) e)
  -> m (Array DL ix e)
stackOuterSlicesM = stackSlicesM (dimensions (Proxy :: Proxy ix))
{-# INLINE stackOuterSlicesM #-}

-- | Specialized `stackSlicesM` to handling stacking from the inside. It is the inverse of
-- `Data.Massiv.Array.innerSlices`.
--
-- /__Exceptions__/: `SizeMismatchException`
--
-- ====__Examples__
--
-- In this example we stack vectors as columns of a matrix from left to right:
--
-- >>> import Data.Massiv.Array as A
-- >>> x = compute (iterateN 3 succ 0) :: Matrix P Int
-- >>> x
-- Array P Seq (Sz (3 :. 3))
--   [ [ 1, 2, 3 ]
--   , [ 4, 5, 6 ]
--   , [ 7, 8, 9 ]
--   ]
-- >>> columns = innerSlices x
-- >>> A.mapM_ print columns
-- Array D Seq (Sz1 3)
--   [ 1, 4, 7 ]
-- Array D Seq (Sz1 3)
--   [ 2, 5, 8 ]
-- Array D Seq (Sz1 3)
--   [ 3, 6, 9 ]
-- >>> stackInnerSlicesM columns :: IO (Matrix DL Int)
-- Array DL Seq (Sz (3 :. 3))
--   [ [ 1, 2, 3 ]
--   , [ 4, 5, 6 ]
--   , [ 7, 8, 9 ]
--   ]
--
-- @since 0.5.4
stackInnerSlicesM ::
     forall r ix e f m. (Foldable f, Raises m, Index (Lower ix), Source r e, Index ix)
  => f (Array r (Lower ix) e)
  -> m (Array DL ix e)
stackInnerSlicesM = stackSlicesM 1
{-# INLINE stackInnerSlicesM #-}


-- | /O(1)/ - Split an array into two at an index along a specified dimension.
--
-- /Related/: `splitAt'`, `splitExtractM`, `Data.Massiv.Vector.sliceAt'`, `Data.Massiv.Vector.sliceAtM`
--
-- /__Exceptions__/: `IndexDimensionException`, `SizeSubregionException`
--
-- @since 0.3.0
splitAtM ::
     forall r ix e m. (Raises m, Index ix, Source r e)
  => Dim -- ^ Dimension along which to split
  -> Int -- ^ Index along the dimension to split at
  -> Array r ix e -- ^ Source array
  -> m (Array D ix e, Array D ix e)
splitAtM dim i arr = do
  let Sz sz = size arr
  eIx <- setDimM sz dim i
  sIx <- setDimM zeroIndex dim i
  arr1 <- extractFromToM zeroIndex eIx arr
  arr2 <- extractFromToM sIx sz arr
  return (arr1, arr2)
{-# INLINE splitAtM #-}

-- | /O(1)/ - Split an array into two at an index along a specified dimension. Throws an
-- error for a wrong dimension or incorrect indices.
--
-- /Related/: `splitAtM`, `splitExtractM`, `Data.Massiv.Vector.sliceAt'`, `Data.Massiv.Vector.sliceAtM`
--
-- ==== __Examples__
--
--
-- @since 0.1.0
splitAt' ::
     forall r ix e. (HasCallStack, Index ix, Source r e)
  => Dim
  -> Int
  -> Array r ix e
  -> (Array D ix e, Array D ix e)
splitAt' dim i = raiseLeftImprecise . splitAtM dim i
{-# INLINE splitAt' #-}


-- | Split an array in three parts across some dimension
--
-- @since 0.3.5
splitExtractM ::
     forall r ix e m. (Raises m, Index ix, Source r e)
  => Dim -- ^ Dimension along which to do the extraction
  -> Ix1 -- ^ Start index along the dimension that needs to be extracted
  -> Sz Ix1 -- ^ Size of the extracted array along the dimension that it will be extracted
  -> Array r ix e
  -> m (Array D ix e, Array D ix e, Array D ix e)
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



-- | Replace a slice of an array with another one
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Array
-- >>> arr = makeArrayR U Seq (Sz3 3 4 5) fromIx3
-- >>> arr' = makeArrayR U Seq (Sz3 3 4 5) (fromIx3 . liftIndex (* 100))
-- >>> replaceSlice 2 1 (arr' <!> (2, 3)) arr
-- Array DL Seq (Sz (3 :> 4 :. 5))
--   [ [ [ (0,0,0), (0,0,1), (0,0,2), (0,0,3), (0,0,4) ]
--     , [ (0,300,0), (0,300,100), (0,300,200), (0,300,300), (0,300,400) ]
--     , [ (0,2,0), (0,2,1), (0,2,2), (0,2,3), (0,2,4) ]
--     , [ (0,3,0), (0,3,1), (0,3,2), (0,3,3), (0,3,4) ]
--     ]
--   , [ [ (1,0,0), (1,0,1), (1,0,2), (1,0,3), (1,0,4) ]
--     , [ (100,300,0), (100,300,100), (100,300,200), (100,300,300), (100,300,400) ]
--     , [ (1,2,0), (1,2,1), (1,2,2), (1,2,3), (1,2,4) ]
--     , [ (1,3,0), (1,3,1), (1,3,2), (1,3,3), (1,3,4) ]
--     ]
--   , [ [ (2,0,0), (2,0,1), (2,0,2), (2,0,3), (2,0,4) ]
--     , [ (200,300,0), (200,300,100), (200,300,200), (200,300,300), (200,300,400) ]
--     , [ (2,2,0), (2,2,1), (2,2,2), (2,2,3), (2,2,4) ]
--     , [ (2,3,0), (2,3,1), (2,3,2), (2,3,3), (2,3,4) ]
--     ]
--   ]
--
-- @since 0.6.1
replaceSlice ::
     forall r r' ix e m. (Raises m, Source r e, Source r' e, Index ix, Index (Lower ix))
  => Dim
  -> Ix1
  -> Array r' (Lower ix) e
  -> Array r ix e
  -> m (Array DL ix e)
replaceSlice dim i sl arr = do
  (l, m, r) <- splitExtractM dim i (SafeSz 1) arr
  m' <- resizeM (size m) sl
  concatM dim [l, delay m', r]
{-# INLINE replaceSlice #-}


-- | Replace an outer slice of an array with another one
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Array
-- >>> arr = makeArrayR U Seq (Sz3 3 4 5) fromIx3
-- >>> arr' = makeArrayR U Seq (Sz3 3 4 5) (fromIx3 . liftIndex (* 100))
-- >>> replaceOuterSlice 1 (arr' !> 2) arr
-- Array DL Seq (Sz (3 :> 4 :. 5))
--   [ [ [ (0,0,0), (0,0,1), (0,0,2), (0,0,3), (0,0,4) ]
--     , [ (0,1,0), (0,1,1), (0,1,2), (0,1,3), (0,1,4) ]
--     , [ (0,2,0), (0,2,1), (0,2,2), (0,2,3), (0,2,4) ]
--     , [ (0,3,0), (0,3,1), (0,3,2), (0,3,3), (0,3,4) ]
--     ]
--   , [ [ (200,0,0), (200,0,100), (200,0,200), (200,0,300), (200,0,400) ]
--     , [ (200,100,0), (200,100,100), (200,100,200), (200,100,300), (200,100,400) ]
--     , [ (200,200,0), (200,200,100), (200,200,200), (200,200,300), (200,200,400) ]
--     , [ (200,300,0), (200,300,100), (200,300,200), (200,300,300), (200,300,400) ]
--     ]
--   , [ [ (2,0,0), (2,0,1), (2,0,2), (2,0,3), (2,0,4) ]
--     , [ (2,1,0), (2,1,1), (2,1,2), (2,1,3), (2,1,4) ]
--     , [ (2,2,0), (2,2,1), (2,2,2), (2,2,3), (2,2,4) ]
--     , [ (2,3,0), (2,3,1), (2,3,2), (2,3,3), (2,3,4) ]
--     ]
--   ]
--
-- @since 0.6.1
replaceOuterSlice ::
     forall r ix e m. (Raises m, Index ix, Source r e, Load r (Lower ix) e)
  => Ix1
  -> Array r (Lower ix) e
  -> Array r ix e
  -> m (Array DL ix e)
replaceOuterSlice i sl arr = replaceSlice (dimensions (size arr)) i sl arr
{-# INLINE replaceOuterSlice #-}


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
     forall r ix e m. (Raises m, Index ix, Source r e)
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
     forall r ix e m. (Raises m, Index ix, Index (Lower ix), Source r e)
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
     forall r ix e m. (Raises m, Index ix, Source r e)
  => Ix1
  -> Sz Ix1
  -> Array r ix e
  -> m (Array DL ix e)
deleteColumnsM = deleteRegionM 1
{-# INLINE deleteColumnsM #-}


-- | Discard elements from the source array according to the stride.
--
-- @since 0.3.0
downsample ::
     forall r ix e. (Source r e, Load r ix e)
  => Stride ix
  -> Array r ix e
  -> Array DL ix e
downsample stride arr =
  DLArray {dlComp = getComp arr, dlSize = resultSize, dlLoad = load}
  where
    resultSize = strideSize stride (size arr)
    strideIx = unStride stride
    unsafeLinearWriteWithStride =
      unsafeIndex arr . liftIndex2 (*) strideIx . fromLinearIndex resultSize
    {-# INLINE unsafeLinearWriteWithStride #-}
    load :: Loader e
    load scheduler startAt dlWrite _ =
      splitLinearlyWithStartAtM_
        scheduler
        startAt
        (totalElem resultSize)
        (pure . unsafeLinearWriteWithStride)
        dlWrite
    {-# INLINE load #-}
{-# INLINE downsample #-}


-- | Insert the same element into a `Load`able array according to the supplied stride.
--
-- ====__Examples__
--
-- >>> import Data.Massiv.Array as A
-- >>> arr = iterateN (Sz2 3 2) succ (0 :: Int)
-- >>> arr
-- Array DL Seq (Sz (3 :. 2))
--   [ [ 1, 2 ]
--   , [ 3, 4 ]
--   , [ 5, 6 ]
--   ]
-- >>> upsample 0 (Stride (2 :. 3)) arr
-- Array DL Seq (Sz (6 :. 6))
--   [ [ 1, 0, 0, 2, 0, 0 ]
--   , [ 0, 0, 0, 0, 0, 0 ]
--   , [ 3, 0, 0, 4, 0, 0 ]
--   , [ 0, 0, 0, 0, 0, 0 ]
--   , [ 5, 0, 0, 6, 0, 0 ]
--   , [ 0, 0, 0, 0, 0, 0 ]
--   ]
-- >>> upsample 9 (Stride (1 :. 2)) arr
-- Array DL Seq (Sz (3 :. 4))
--   [ [ 1, 9, 2, 9 ]
--   , [ 3, 9, 4, 9 ]
--   , [ 5, 9, 6, 9 ]
--   ]
--
-- @since 0.3.0
upsample ::
     forall r ix e. (Resize r, Load r ix e)
  => e -- ^ Element to use for filling the newly added cells
  -> Stride ix -- ^ Fill cells according to this stride
  -> Array r ix e -- ^ Array that will have cells added to
  -> Array DL ix e
upsample !fillWith safeStride arr =
  DLArray
    { dlComp = getComp arr
    , dlSize = newsz
    , dlLoad = load
    }
  where
    load :: Loader e
    load scheduler startAt uWrite uSet = do
      uSet startAt (toLinearSz newsz) fillWith
      loadArrayST scheduler arr (\i -> uWrite (adjustLinearStride (i + startAt)))
    {-# INLINE load #-}
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
     (Mutable r e, Index ix, Source r' e', Index ix', UnliftPrimal RW m)
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
     forall ix e r' ix' e' a.
     (HasCallStack, Source r' e', Index ix', Index ix)
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
     ( Mutable r e
     , Index ix
     , Source r1 e1
     , Source r2 e2
     , Index ix1
     , Index ix2
     , UnliftPrimal RW m
     )
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
     (HasCallStack, Source r1 e1, Source r2 e2, Index ix, Index ix1, Index ix2)
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
-- >>> arr = resize' (Sz2 3 2) (Ix1 1 ... 6)
-- >>> arr
-- Array D Seq (Sz (3 :. 2))
--   [ [ 1, 2 ]
--   , [ 3, 4 ]
--   , [ 5, 6 ]
--   ]
-- >>> zoomWithGrid 0 (Stride (2 :. 3)) arr
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
     forall r ix e. (Index ix, Source r e)
  => e -- ^ Value to use for the grid
  -> Stride ix -- ^ Scaling factor
  -> Array r ix e -- ^ Source array
  -> Array DL ix e
zoomWithGrid gridVal (Stride zoomFactor) arr = unsafeMakeLoadArray Seq newSz (Just gridVal) load
  where
    !kx = liftIndex (+ 1) zoomFactor
    !lastNewIx = liftIndex2 (*) kx $ unSz (size arr)
    !newSz = Sz (liftIndex (+ 1) lastNewIx)
    load :: Scheduler s () -> Ix1 -> (Ix1 -> e -> ST s ()) -> ST s ()
    load scheduler _ writeElement =
      iforSchedulerM_ scheduler arr $ \ !ix !e ->
        let !kix = liftIndex2 (*) ix kx
         in mapM_ (\ !ix' -> writeElement (toLinearIndex newSz ix') e) $
            range Seq (liftIndex (+ 1) kix) (liftIndex2 (+) kix kx)
    {-# INLINE load #-}
{-# INLINE zoomWithGrid #-}

-- | Increaze the size of the array accoridng to the stride multiplier while replicating
-- the same element to fill the neighbors. It is exactly the same as `zoomWithGrid`, but
-- without the grid.
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Array as A
-- >>> arr = resize' (Sz3 1 3 2) (Ix1 1 ... 6)
-- >>> arr
-- Array D Seq (Sz (1 :> 3 :. 2))
--   [ [ [ 1, 2 ]
--     , [ 3, 4 ]
--     , [ 5, 6 ]
--     ]
--   ]
-- >>> zoom (Stride (2 :> 2 :. 3)) arr
-- Array DL Seq (Sz (2 :> 6 :. 6))
--   [ [ [ 1, 1, 1, 2, 2, 2 ]
--     , [ 1, 1, 1, 2, 2, 2 ]
--     , [ 3, 3, 3, 4, 4, 4 ]
--     , [ 3, 3, 3, 4, 4, 4 ]
--     , [ 5, 5, 5, 6, 6, 6 ]
--     , [ 5, 5, 5, 6, 6, 6 ]
--     ]
--   , [ [ 1, 1, 1, 2, 2, 2 ]
--     , [ 1, 1, 1, 2, 2, 2 ]
--     , [ 3, 3, 3, 4, 4, 4 ]
--     , [ 3, 3, 3, 4, 4, 4 ]
--     , [ 5, 5, 5, 6, 6, 6 ]
--     , [ 5, 5, 5, 6, 6, 6 ]
--     ]
--   ]
--
-- @since 0.4.4
zoom ::
     forall r ix e. (Index ix, Source r e)
  => Stride ix -- ^ Scaling factor
  -> Array r ix e -- ^ Source array
  -> Array DL ix e
zoom (Stride zoomFactor) arr = DLArray (getComp arr) newSz load
  where
    !lastNewIx = liftIndex2 (*) zoomFactor $ unSz (size arr)
    !newSz = Sz lastNewIx
    load :: Loader e
    load scheduler _ writeElement _ =
      iforSchedulerM_ scheduler arr $ \ !ix !e ->
        let !kix = liftIndex2 (*) ix zoomFactor
         in mapM_ (\ !ix' -> writeElement (toLinearIndex newSz ix') e) $
            range Seq kix (liftIndex2 (+) kix zoomFactor)
    {-# INLINE load #-}
{-# INLINE zoom #-}
