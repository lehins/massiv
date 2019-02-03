{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
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
  -- ** Backpermute
  , backpermuteM
  , backpermute'
  , backpermute
  -- ** Resize
  , resizeM
  , resize'
  , resize
  -- ** Extract
  , extractM
  , extract
  , extract'
  , extractFromToM
  , extractFromTo
  , extractFromTo'
  -- ** Append/Split
  , cons
  , unconsM
  , snoc
  , unsnocM
  , appendM
  , append
  , append'
  , concatM
  , concat'
  , splitAtM
  , splitAt
  , splitAt'
  -- ** Upsample/Downsample
  , upsample
  , downsample
  -- * Traverse
  , traverse
  , traverse2
  ) where

import           Control.Monad                   as M (foldM_, unless)
import           Data.Bifunctor                  (bimap)
import           Data.Foldable                   as F (foldl', foldrM, toList)
import qualified Data.List                       as L (uncons)
import           Data.Massiv.Array.Delayed.Pull
import           Data.Massiv.Array.Delayed.Push
import           Data.Massiv.Array.Ops.Construct
import           Data.Massiv.Core.Common
import           Data.Massiv.Core.Index.Internal (Sz (SafeSz))
import           Data.Massiv.Scheduler           (traverse_)
import           Prelude                         as P hiding (concat, splitAt,
                                                       traverse)


-- | Extract a sub-array from within a larger source array. Array that is being extracted must be
-- fully encapsulated in a source array, otherwise `Nothing` is returned,
extractM :: (MonadThrow m, Extract r ix e)
         => ix -- ^ Starting index
         -> Sz ix -- ^ Size of the resulting array
         -> Array r ix e -- ^ Source array
         -> m (Array (EltRepr r ix) ix e)
extractM !sIx !newSz !arr
  | isSafeIndex sz1 sIx && isSafeIndex eIx1 sIx && isSafeIndex sz1 eIx =
    pure $ unsafeExtract sIx newSz arr
  | otherwise = throwM $ SizeSubregionException (size arr) sIx newSz
  where
    sz1 = Sz (liftIndex (+1) (unSz (size arr)))
    eIx1 = Sz (liftIndex (+1) eIx)
    eIx = liftIndex2 (+) sIx $ unSz newSz
{-# INLINE extractM #-}

-- | Same as `extract`, but will throw an error if supplied dimensions are incorrect.
extract' :: Extract r ix e
        => ix -- ^ Starting index
        -> Sz ix -- ^ Size of the resulting array
        -> Array r ix e -- ^ Source array
        -> Array (EltRepr r ix) ix e
extract' sIx newSz = either throw id . extractM sIx newSz
{-# INLINE extract' #-}

-- | Extract a sub-array from within a larger source array. Array that is being extracted must be
-- fully encapsulated in a source array, otherwise `Nothing` is returned,
extract :: Extract r ix e
        => ix -- ^ Starting index
        -> Sz ix -- ^ Size of the resulting array
        -> Array r ix e -- ^ Source array
        -> Maybe (Array (EltRepr r ix) ix e)
extract !sIx !newSz !arr
  | isSafeIndex sz1 sIx && isSafeIndex eIx1 sIx && isSafeIndex sz1 eIx =
    Just $ unsafeExtract sIx newSz arr
  | otherwise = Nothing
  where
    sz1 = Sz (liftIndex (+1) (unSz (size arr)))
    eIx1 = Sz (liftIndex (+1) eIx)
    eIx = liftIndex2 (+) sIx $ unSz newSz
{-# INLINE extract #-}
{-# DEPRECATED extract "In favor of a more general `extractM`" #-}


-- | Similar to `extract`, except it takes starting and ending index. Result array will not include
-- the ending index.
extractFromToM :: (MonadThrow m, Extract r ix e) =>
                  ix -- ^ Starting index
               -> ix -- ^ Index up to which elements should be extracted.
               -> Array r ix e -- ^ Source array.
               -> m (Array (EltRepr r ix) ix e)
extractFromToM sIx eIx = extractM sIx (Sz (liftIndex2 (-) eIx sIx))
{-# INLINE extractFromToM #-}

-- | Similar to `extract`, except it takes starting and ending index. Result array will not include
-- the ending index.
extractFromTo :: Extract r ix e =>
                 ix -- ^ Starting index
              -> ix -- ^ Index up to which elmenets should be extracted.
              -> Array r ix e -- ^ Source array.
              -> Maybe (Array (EltRepr r ix) ix e)
extractFromTo sIx eIx = extract sIx $ Sz (liftIndex2 (-) eIx sIx)
{-# INLINE extractFromTo #-}
{-# DEPRECATED extractFromTo "In favor of a more general `extractFromToM`" #-}

-- | Same as `extractFromTo`, but throws an error on invalid indices.
--
-- @since 0.2.4
extractFromTo' :: Extract r ix e =>
                 ix -- ^ Starting index
              -> ix -- ^ Index up to which elmenets should be extracted.
              -> Array r ix e -- ^ Source array.
              -> Array (EltRepr r ix) ix e
extractFromTo' sIx eIx = extract' sIx $ Sz (liftIndex2 (-) eIx sIx)
{-# INLINE extractFromTo' #-}


-- | /O(1)/ - Changes the shape of an array. Returns `Nothing` if total
-- number of elements does not match the source array.
resize ::
     (Index ix', Load r ix e, Resize Array r ix) => Sz ix' -> Array r ix e -> Maybe (Array r ix' e)
resize !sz !arr
  | totalElem sz == totalElem (size arr) = Just $ unsafeResize sz arr
  | otherwise = Nothing
{-# INLINE resize #-}
{-# DEPRECATED resize "In favor of a more general `resizeM`" #-}

-- | /O(1)/ - Changes the shape of an array. Returns `Nothing` if total
-- number of elements does not match the source array.
--
-- @since 0.3.0
resizeM ::
     (MonadThrow m, Index ix', Load r ix e, Resize Array r ix)
  => Sz ix'
  -> Array r ix e
  -> m (Array r ix' e)
resizeM sz arr = guardNumberOfElements (size arr) sz >> pure (unsafeResize sz arr)
{-# INLINE resizeM #-}

-- | Same as `resizeM`, but will throw an error if supplied dimensions are incorrect.
resize' :: (Index ix', Load r ix e, Resize Array r ix) => Sz ix' -> Array r ix e -> Array r ix' e
resize' sz = either throw id . resizeM sz
{-# INLINE resize' #-}


-- | Transpose a 2-dimensional array
--
-- ===__Examples__
--
-- >>> let arr = makeArrayR U Seq (2 :. 3) (toLinearIndex (2 :. 3))
-- >>> arr
-- (ArrayU Seq (2 :. 3)
--   [ [ 0,1,2 ]
--   , [ 3,4,5 ]
--   ])
-- >>> transpose arr
-- (Array D Seq (3 :. 2)
--   [ [ 0,3 ]
--   , [ 1,4 ]
--   , [ 2,5 ]
--   ])
--
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
-- >>> let arr = makeArrayR U Seq (2 :> 3 :. 4) fromIx3
-- >>> arr
-- (Array U Seq (2 :> 3 :. 4)
--   [ [ [ (0,0,0),(0,0,1),(0,0,2),(0,0,3) ]
--     , [ (0,1,0),(0,1,1),(0,1,2),(0,1,3) ]
--     , [ (0,2,0),(0,2,1),(0,2,2),(0,2,3) ]
--     ]
--   , [ [ (1,0,0),(1,0,1),(1,0,2),(1,0,3) ]
--     , [ (1,1,0),(1,1,1),(1,1,2),(1,1,3) ]
--     , [ (1,2,0),(1,2,1),(1,2,2),(1,2,3) ]
--     ]
--   ])
-- >>> transposeInner arr
-- (Array D Seq (3 :> 2 :. 4)
--   [ [ [ (0,0,0),(0,0,1),(0,0,2),(0,0,3) ]
--     , [ (1,0,0),(1,0,1),(1,0,2),(1,0,3) ]
--     ]
--   , [ [ (0,1,0),(0,1,1),(0,1,2),(0,1,3) ]
--     , [ (1,1,0),(1,1,1),(1,1,2),(1,1,3) ]
--     ]
--   , [ [ (0,2,0),(0,2,1),(0,2,2),(0,2,3) ]
--     , [ (1,2,0),(1,2,1),(1,2,2),(1,2,3) ]
--     ]
--   ])
--
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
-- ===__Examples__
--
-- >>> let arr = makeArrayR U Seq (2 :> 3 :. 4) fromIx3
-- >>> arr
-- (Array U Seq (2 :> 3 :. 4)
--   [ [ [ (0,0,0),(0,0,1),(0,0,2),(0,0,3) ]
--     , [ (0,1,0),(0,1,1),(0,1,2),(0,1,3) ]
--     , [ (0,2,0),(0,2,1),(0,2,2),(0,2,3) ]
--     ]
--   , [ [ (1,0,0),(1,0,1),(1,0,2),(1,0,3) ]
--     , [ (1,1,0),(1,1,1),(1,1,2),(1,1,3) ]
--     , [ (1,2,0),(1,2,1),(1,2,2),(1,2,3) ]
--     ]
--   ])
-- >>> transposeOuter arr
-- (Array D Seq (2 :> 4 :. 3)
--   [ [ [ (0,0,0),(0,1,0),(0,2,0) ]
--     , [ (0,0,1),(0,1,1),(0,2,1) ]
--     , [ (0,0,2),(0,1,2),(0,2,2) ]
--     , [ (0,0,3),(0,1,3),(0,2,3) ]
--     ]
--   , [ [ (1,0,0),(1,1,0),(1,2,0) ]
--     , [ (1,0,1),(1,1,1),(1,2,1) ]
--     , [ (1,0,2),(1,1,2),(1,2,2) ]
--     , [ (1,0,3),(1,1,3),(1,2,3) ]
--     ]
--   ])
--
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


-- | Rearrange elements of an array into a new one by using a function that maps indices of the
-- newly created one into the old one. This function can throw `IndexOutOfBoundsException`.
--
-- ===__Examples__
--
-- >>> let arr = makeArrayR U Seq (2 :> 3 :. 4) fromIx3
-- >>> arr
-- (Array U Seq (2 :> 3 :. 4)
--   [ [ [ (0,0,0),(0,0,1),(0,0,2),(0,0,3) ]
--     , [ (0,1,0),(0,1,1),(0,1,2),(0,1,3) ]
--     , [ (0,2,0),(0,2,1),(0,2,2),(0,2,3) ]
--     ]
--   , [ [ (1,0,0),(1,0,1),(1,0,2),(1,0,3) ]
--     , [ (1,1,0),(1,1,1),(1,1,2),(1,1,3) ]
--     , [ (1,2,0),(1,2,1),(1,2,2),(1,2,3) ]
--     ]
--   ])
-- >>> backpermute (4 :. 3) (\(i :. j) -> 0 :> j :. i) arr
-- (Array D Seq (4 :. 3)
--   [ [ (0,0,0),(0,1,0),(0,2,0) ]
--   , [ (0,0,1),(0,1,1),(0,2,1) ]
--   , [ (0,0,2),(0,1,2),(0,2,2) ]
--   , [ (0,0,3),(0,1,3),(0,2,3) ]
--   ])
--
-- @since 0.3.0
backpermuteM :: (MonadThrow m, Source r' ix' e, Mutable r ix e) =>
                Sz ix -- ^ Size of the result array
             -> (ix -> ix') -- ^ A function that maps indices of the new array into the source one.
             -> Array r' ix' e -- ^ Source array.
             -> m (Array r ix e)
backpermuteM sz ixF !arr = makeArrayA (getComp arr) sz (evaluateM arr . ixF)
{-# INLINE backpermuteM #-}

-- | Similar to `backpermuteM`, with few notable differences:
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

-- | See `backpermute'`.
--
-- @since 0.1.0
backpermute :: (Source r' ix' e, Index ix) =>
               Sz ix -- ^ Size of the result array
            -> (ix -> ix') -- ^ A function that maps indices of the new array into the source one.
            -> Array r' ix' e -- ^ Source array.
            -> Array D ix e
backpermute = backpermute'
{-# INLINE backpermute #-}
{-# DEPRECATED backpermute "In favor of a safe `backpermuteM` or an equivalent `backpermute'`" #-}



cons :: e -> Array DL Ix1 e -> Array DL Ix1 e
cons e arr =
  arr
    { dlSize = SafeSz (1 + unSz (dlSize arr))
    , dlLoad =
        \numWorkers scheduleWith startAt uWrite ->
          uWrite startAt e >> dlLoad arr numWorkers scheduleWith (startAt + 1) uWrite
    }
{-# INLINE cons #-}

-- cons :: Load r Ix1 e => e -> Array r Ix1 e -> Array DL Ix1 e
-- cons !e !arr =
--   DLArray (getComp arr) (SafeSz (1 + unSz (size arr))) $ \numWorkers scheduleWith startAt uWrite ->
--     uWrite startAt e >> loadArray numWorkers scheduleWith arr (\ !i -> uWrite (i + startAt))
-- {-# INLINE cons #-}

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

-- snoc :: Load r Ix1 e => Array r Ix1 e -> e -> Array DL Ix1 e
-- snoc !arr !e =
--   DLArray (getComp arr) (SafeSz (1 + k)) $ \numWorkers scheduleWith uWrite ->
--     loadArray numWorkers scheduleWith arr uWrite >> uWrite k e
--   where
--     !k = unSz (size arr)
-- {-# INLINE snoc #-}


snoc :: Array DL Ix1 e -> e -> Array DL Ix1 e
snoc arr e =
  arr
    { dlSize = SafeSz (1 + k)
    , dlLoad =
        \numWorkers scheduleWith startAt uWrite ->
          dlLoad arr numWorkers scheduleWith startAt uWrite >> uWrite (k + startAt) e
    }
  where
    !k = unSz (size arr)
{-# INLINE snoc #-}


unsnocM :: (MonadThrow m, Source r Ix1 e) => Array r Ix1 e -> m (Array D Ix1 e, e)
unsnocM arr
  | 0 == totalElem sz = throwM $ SizeEmptyException sz
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
-- Append two 2D arrays along both dimensions. Note that they have the same shape.
--
-- >>> let arrA = makeArrayR U Seq (2 :. 3) (\(i :. j) -> ('A', i, j))
-- >>> let arrB = makeArrayR U Seq (2 :. 3) (\(i :. j) -> ('B', i, j))
-- >>> append 1 arrA arrB
-- Just (Array D Seq (2 :. 6)
--   [ [ ('A',0,0),('A',0,1),('A',0,2),('B',0,0),('B',0,1),('B',0,2) ]
--   , [ ('A',1,0),('A',1,1),('A',1,2),('B',1,0),('B',1,1),('B',1,2) ]
--   ])
-- >>> append 2 arrA arrB
-- Just (Array D Seq (4 :. 3)
--   [ [ ('A',0,0),('A',0,1),('A',0,2) ]
--   , [ ('A',1,0),('A',1,1),('A',1,2) ]
--   , [ ('B',0,0),('B',0,1),('B',0,2) ]
--   , [ ('B',1,0),('B',1,1),('B',1,2) ]
--   ])
--
-- Now appending arrays with different sizes:
--
-- >>> let arrC = makeArrayR U Seq (2 :. 4) (\(i :. j) -> ('C', i, j))
-- >>> append 1 arrA arrC
-- Just (Array D Seq (2 :. 7)
--   [ [ ('A',0,0),('A',0,1),('A',0,2),('C',0,0),('C',0,1),('C',0,2),('C',0,3) ]
--   , [ ('A',1,0),('A',1,1),('A',1,2),('C',1,0),('C',1,1),('C',1,2),('C',1,3) ]
--   ])
-- >>> append 2 arrA arrC
-- Nothing
--
appendM :: (MonadThrow m, Source r1 ix e, Source r2 ix e) =>
          Dim -> Array r1 ix e -> Array r2 ix e -> m (Array DL ix e)
appendM n !arr1 !arr2 = do --concatM n [delay arr1, delay arr2]
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
      , dlLoad =
          \_numWorkers scheduleWith startAt dlWrite -> do
            scheduleWith $
              iterM_ zeroIndex (unSz sz1) (pureIndex 1) (<) $ \ix ->
                dlWrite (startAt + toLinearIndex newSz ix) (unsafeIndex arr1 ix)
            scheduleWith $
              iterM_ zeroIndex (unSz sz2) (pureIndex 1) (<) $ \ix ->
                let i = getDim' ix n
                    ix' = setDim' ix n (i + k1')
                 in dlWrite (startAt + toLinearIndex newSz ix') (unsafeIndex arr2 ix)
      }
{-# INLINE appendM #-}

-- | Append two arrays together along a specified dimension.
append :: (Source r1 ix e, Source r2 ix e) =>
          Dim -> Array r1 ix e -> Array r2 ix e -> Maybe (Array DL ix e)
append = appendM
{-# INLINE append #-}
{-# DEPRECATED append "In favor of a more general `appendM`" #-}


-- | Same as `appendM`, but will throw an error instead of returning `Nothing` on mismatched sizes.
append' :: (Source r1 ix e, Source r2 ix e) =>
           Dim -> Array r1 ix e -> Array r2 ix e -> Array DL ix e
append' dim arr1 arr2 = either throw id $ appendM dim arr1 arr2
{-# INLINE append' #-}

concat' :: (Foldable f, Source r ix e) => Dim -> f (Array r ix e) -> Array DL ix e
concat' n arrs = either throw id $ concatM n arrs
{-# INLINE concat' #-}

-- | Concatenate many arrays together along some dimension. It is important that all sizes are
-- equal, with an exception of the dimensions along which concatenation happens.
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
        (dropWhile ((== szl) . snd) $ zip szs szls)
      let kTotal = SafeSz $ F.foldl' (+) k ks
      newSz <- insertSzM (SafeSz szl) n kTotal
      return $
        DLArray
          { dlComp = mconcat $ P.map getComp arrs
          , dlSize = newSz
          , dlLoad =
              \_numWorkers scheduleWith startAt dlWrite ->
                let arrayLoader !kAcc (kCur, arr) = do
                      scheduleWith $
                        iterM_ zeroIndex (unSz (size arr)) (pureIndex 1) (<) $ \ix ->
                          let i = getDim' ix n
                              ix' = setDim' ix n (i + kAcc)
                           in dlWrite (startAt + toLinearIndex newSz ix') (unsafeIndex arr ix)
                      pure (kAcc + kCur)
                 in M.foldM_ arrayLoader 0 $ (k, a) : P.zip ks arrs
          }
{-# INLINE concatM #-}


-- | /O(1)/ - Split an array at an index along a specified dimension.
splitAtM ::
     (MonadThrow m, Extract r ix e, r' ~ EltRepr r ix)
  => Dim -- ^ Dimension along which to split
  -> Int -- ^ Index along the dimension to split at
  -> Array r ix e -- ^ Source array
  -> m (Array r' ix e, Array r' ix e)
splitAtM dim i arr = do
  let Sz sz = size arr
  eIx <- setDimM sz dim i
  sIx <- setDimM zeroIndex dim i
  arr1 <- extractFromToM zeroIndex eIx arr
  arr2 <- extractFromToM sIx sz arr
  return (arr1, arr2)
{-# INLINE splitAtM #-}

-- | /O(1)/ - Split an array at an index along a specified dimension.
splitAt ::
     (Extract r ix e, r' ~ EltRepr r ix)
  => Dim -- ^ Dimension along which to split
  -> Int -- ^ Index along the dimension to split at
  -> Array r ix e -- ^ Source array
  -> Maybe (Array r' ix e, Array r' ix e)
splitAt dim i arr = do
  let Sz sz = size arr
  eIx <- setDimM sz dim i
  sIx <- setDimM zeroIndex dim i
  arr1 <- extractFromTo zeroIndex eIx arr
  arr2 <- extractFromTo sIx sz arr
  return (arr1, arr2)
{-# INLINE splitAt #-}
{-# DEPRECATED splitAt "In favor of a more general `splitAtM`" #-}

-- | Same as `splitAt`, but will throw an error instead of returning `Nothing` on wrong dimension
-- and index out of bounds.
splitAt' :: (Extract r ix e, r' ~ EltRepr r ix) =>
           Dim -> Int -> Array r ix e -> (Array r' ix e, Array r' ix e)
splitAt' dim i arr =
  case splitAt dim i arr of
    Just res -> res
    Nothing ->
      error $
      "Data.Massiv.Array.splitAt': " ++
      if 0 < dim && dim <= dimensions (size arr)
        then "Index out of bounds: " ++
             show i ++ " for dimension: " ++ show dim ++ " and array with size: " ++ show (size arr)
        else "Invalid dimension: " ++ show dim ++ " for array with size: " ++ show (size arr)
{-# INLINE splitAt' #-}


-- | Discard elements from the source array according to the stride.
--
-- @since 0.3.0
--
downsample :: Source r ix e => Stride ix -> Array r ix e -> Array DL ix e
downsample !stride arr =
  DLArray
    { dlComp = getComp arr
    , dlSize = resultSize
    , dlLoad =
        \numWorkers scheduleWith startAt dlWrite ->
          splitLinearlyWithStartAtM_
            numWorkers
            scheduleWith
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
upsample !fillWith !safeStride arr =
  DLArray
    { dlComp = getComp arr
    , dlSize = newsz
    , dlLoad =
        \numWorkers scheduleWith startAt dlWrite -> do
          unless (stride == pureIndex 1) $
            loopM_ startAt (< totalElem newsz) (+ 1) (`dlWrite` fillWith)
          -- TODO: experiment a bit more. So far the fastest solution is to prefill the whole array
          -- with default value and override non-stride elements afterwards.  This approach seems a
          -- bit wasteful, nevertheless it is fastest
          --
          -- TODO: Is it possible to use fast fill operation that is available for MutableByteArray?
          loadArray numWorkers scheduleWith arr (\i -> dlWrite (adjustLinearStride (i + startAt)))
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

  -- This was a sample optimization, that turned out to be significantly (~ x9) slower
  -- makeLoadArray (getComp arr) newsz $ \numWorkers scheduleWith dlWrite -> do
  --   iterM_ zeroIndex stride (pureIndex 1) (<) $ \ixs ->
  --     if ixs == zeroIndex
  --       then loadArray numWorkers scheduleWith arr $ \ !i -> dlWrite (adjustLinearStride i)
  --       else let !is = toLinearIndex newsz ixs
  --             in scheduleWith $
  --                loopM_ 0 (< totalElem sz) (+ 1) $ \ !i ->
  --                  dlWrite (is + adjustLinearStride i) fillWith



-- | Create an array by traversing a source array.
traverse
  :: (Source r1 ix1 e1, Index ix)
  => Sz ix -- ^ Size of the result array
  -> ((ix1 -> e1) -> ix -> e) -- ^ Function that will receive a source array safe index function and
                              -- an index for an element it should return a value of.
  -> Array r1 ix1 e1 -- ^ Source array
  -> Array D ix e
traverse sz f arr1 = makeArray (getComp arr1) sz (f (evaluate' arr1))
{-# INLINE traverse #-}


-- | Create an array by traversing two source arrays.
traverse2
  :: (Source r1 ix1 e1, Source r2 ix2 e2, Index ix)
  => Sz ix
  -> ((ix1 -> e1) -> (ix2 -> e2) -> ix -> e)
  -> Array r1 ix1 e1
  -> Array r2 ix2 e2
  -> Array D ix e
traverse2 sz f arr1 arr2 = makeArray (getComp arr1) sz (f (evaluate' arr1) (evaluate' arr2))
{-# INLINE traverse2 #-}

