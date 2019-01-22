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
  , backpermute
  -- ** Resize
  , resize
  , resize'
  -- ** Extract
  , extract
  , extract'
  , extractFromTo
  , extractFromTo'
  -- ** Append/Split
  , append
  , append'
  , concat
  , concat'
  , splitAt
  , splitAt'
  -- ** Upsample/Downsample
  , upsample
  , downsample
  -- * Traverse
  , traverse
  , traverse2
  ) where

import           Control.Monad                   as M (foldM_, guard, unless)
import           Data.Foldable                   as F (foldl', foldrM)
import           Data.Bifunctor                  (bimap)
import           Data.Massiv.Array.Delayed.Pull
import           Data.Massiv.Array.Delayed.Push
import           Data.Massiv.Array.Ops.Construct
import           Data.Massiv.Core.Common
import           Data.Massiv.Core.Index.Internal (Sz (SafeSz))
import           Data.Maybe                      (fromMaybe)
import           Prelude                         as P hiding (splitAt, traverse, concat)


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

-- | Same as `extract`, but will throw an error if supplied dimensions are incorrect.
extract' :: Extract r ix e
        => ix -- ^ Starting index
        -> Sz ix -- ^ Size of the resulting array
        -> Array r ix e -- ^ Source array
        -> Array (EltRepr r ix) ix e
extract' !sIx !newSz !arr =
  case extract sIx newSz arr of
    Just arr' -> arr'
    Nothing ->
      error $
      "Data.Massiv.Array.extract': Cannot extract an array of size " ++
      show newSz ++
      " starting at " ++ show sIx ++ " from within an array of size: " ++ show (size arr)
{-# INLINE extract' #-}

-- | Similar to `extract`, except it takes starting and ending index. Result array will not include
-- the ending index.
extractFromTo :: Extract r ix e =>
                 ix -- ^ Starting index
              -> ix -- ^ Index up to which elmenets should be extracted.
              -> Array r ix e -- ^ Source array.
              -> Maybe (Array (EltRepr r ix) ix e)
extractFromTo sIx eIx = extract sIx $ Sz (liftIndex2 (-) eIx sIx)
{-# INLINE extractFromTo #-}

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

-- | Same as `resize`, but will throw an error if supplied dimensions are incorrect.
resize' :: (Index ix', Load r ix e, Resize Array r ix) => Sz ix' -> Array r ix e -> Array r ix' e
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
      fromMaybe (errorImpossible "transposeInner" ix) $ do
        n <- getDim ix dix
        m <- getDim ix (dix - 1)
        ix' <- setDim ix dix m
        setDim ix' (dix - 1) n
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
      fromMaybe (errorImpossible "transposeOuter" ix) $ do
        n <- getDim ix 1
        m <- getDim ix 2
        ix' <- setDim ix 1 m
        setDim ix' 2 n
    {-# INLINE transOuter #-}
    newVal = unsafeIndex arr . transOuter
    {-# INLINE newVal #-}
    !newsz = Sz (transOuter (unSz (size arr)))
{-# INLINE [1] transposeOuter #-}


-- | Rearrange elements of an array into a new one.
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
backpermute :: (Source r' ix' e, Index ix) =>
               Sz ix -- ^ Size of the result array
            -> (ix -> ix') -- ^ A function that maps indices of the new array into the source one.
            -> Array r' ix' e -- ^ Source array.
            -> Array D ix e
backpermute sz ixF !arr = makeArray (getComp arr) sz (evaluateAt arr . ixF)
{-# INLINE backpermute #-}


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
append :: (Source r1 ix e, Source r2 ix e) =>
          Dim -> Array r1 ix e -> Array r2 ix e -> Maybe (Array DL ix e)
append n !arr1 !arr2 = do
  let sz1 = size arr1
      sz2 = size arr2
  (k1, szl1) <- pullOutSz sz1 n
  (k2, szl2) <- pullOutSz sz2 n
  guard $ szl1 == szl2
  let k1' = unSz k1
  newSz <- insertSz szl1 n (SafeSz (k1' + unSz k2))
  return $
    DLArray
      { dlComp = getComp arr1 <> getComp arr2
      , dlSize = newSz
      , dlLoad =
          \_numWorkers scheduleWith dlWrite -> do
            scheduleWith $
              iterM_ zeroIndex (unSz sz1) (pureIndex 1) (<) $ \ix ->
                dlWrite (toLinearIndex newSz ix) (unsafeIndex arr1 ix)
            scheduleWith $
              iterM_ zeroIndex (unSz sz2) (pureIndex 1) (<) $ \ix ->
                let i = getDim' ix n
                    ix' = setDim' ix n (i + k1')
                 in dlWrite (toLinearIndex newSz ix') (unsafeIndex arr2 ix)
      }
{-# INLINE append #-}

concat' :: Source r ix e => Dim -> [Array r ix e] -> Array DL ix e
concat' n arrs =
  case concat n arrs of
    Nothing -> error $ "Data.Massiv.Array.concat': size mismatch "
    Just resArr -> resArr


concat :: Source r ix e => Dim -> [Array r ix e] -> Maybe (Array DL ix e)
concat n !arrs = do
  guard $ not $ null arrs
  let szs = P.map (unSz . size) arrs
  (ks, (szl:szls)) <-
    F.foldrM (\ !sz (ks, szls) -> bimap (: ks) (: szls) <$> pullOutDim sz n) ([], []) szs
  guard $ all (== szl) szls
  let kTotal = SafeSz $ F.foldl' (+) 0 ks
  newSz <- insertSz (SafeSz szl) n kTotal
  return $
    DLArray
      { dlComp = mconcat $ P.map getComp arrs
      , dlSize = newSz
      , dlLoad =
          \_numWorkers scheduleWith dlWrite ->
            let arrayLoader !kAcc (k, arr) = do
                  scheduleWith $
                    iterM_ zeroIndex (unSz (size arr)) (pureIndex 1) (<) $ \ix ->
                      let i = getDim' ix n
                          ix' = setDim' ix n (i + kAcc)
                       in dlWrite (toLinearIndex newSz ix') (unsafeIndex arr ix)
                  pure (kAcc + k)
             in M.foldM_ arrayLoader 0 $ P.zip ks arrs
      }
{-# INLINE concat #-}


-- | Same as `append`, but will throw an error instead of returning `Nothing` on mismatched sizes.
append' :: (Source r1 ix e, Source r2 ix e) =>
           Dim -> Array r1 ix e -> Array r2 ix e -> Array DL ix e
append' dim arr1 arr2 =
  case append dim arr1 arr2 of
    Just arr -> arr
    Nothing ->
      error $
      if 0 < dim && dim <= dimensions (size arr1)
        then "append': Dimension mismatch: " ++ show (size arr1) ++ " and " ++ show (size arr2)
        else "append': Invalid dimension: " ++ show dim
{-# INLINE append' #-}

-- | /O(1)/ - Split an array at an index along a specified dimension.
splitAt ::
     (Extract r ix e, r' ~ EltRepr r ix)
  => Dim -- ^ Dimension along which to split
  -> Int -- ^ Index along the dimension to split at
  -> Array r ix e -- ^ Source array
  -> Maybe (Array r' ix e, Array r' ix e)
splitAt dim i arr = do
  let Sz sz = size arr
  eIx <- setDim sz dim i
  sIx <- setDim zeroIndex dim i
  arr1 <- extractFromTo zeroIndex eIx arr
  arr2 <- extractFromTo sIx sz arr
  return (arr1, arr2)
{-# INLINE splitAt #-}

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
        \numWorkers scheduleWith dlWrite ->
          splitLinearlyWith_
            numWorkers
            scheduleWith
            (totalElem resultSize)
            unsafeLinearWriteWithStride
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
        \numWorkers scheduleWith dlWrite -> do
          unless (stride == pureIndex 1) $
            loopM_ 0 (< totalElem newsz) (+ 1) (`dlWrite` fillWith)
          -- TODO: experiment a bit more. So far the fastest solution is to prefill the whole array
          -- with default value and override non-stride elements afterwards.  This approach seems a
          -- bit wasteful, nevertheless it is fastest
          --
          -- TODO: Is it possible to use fast fill operation that is available for MutableByteArray?
          loadArray numWorkers scheduleWith arr (\i -> dlWrite (adjustLinearStride i))
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
traverse sz f arr1 = makeArray (getComp arr1) sz (f (evaluateAt arr1))
{-# INLINE traverse #-}


-- | Create an array by traversing two source arrays.
traverse2
  :: (Source r1 ix1 e1, Source r2 ix2 e2, Index ix)
  => Sz ix
  -> ((ix1 -> e1) -> (ix2 -> e2) -> ix -> e)
  -> Array r1 ix1 e1
  -> Array r2 ix2 e2
  -> Array D ix e
traverse2 sz f arr1 arr2 = makeArray (getComp arr1) sz (f (evaluateAt arr1) (evaluateAt arr2))
{-# INLINE traverse2 #-}


-- | Throw an impossible error on a `Nothing`
errorImpossible :: Show c => String -> c -> a
errorImpossible fName cause =
  error $ "Data.Massiv.Array." ++ fName ++ ": Impossible happened " ++ show cause
{-# NOINLINE errorImpossible #-}
