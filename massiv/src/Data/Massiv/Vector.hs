{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
-- |
-- Module      : Data.Massiv.Vector
-- Copyright   : (c) Alexey Kuleshevich 2020-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Vector
  ( Vector
  , MVector
  -- * Accessors
  -- *** Size
  , slength
  , maxLinearSize
  , size
  , isNull
  , isNotNull
  -- *** Indexing
  , (!?)
  , (!)
  , index
  , index'
  , head'
  , shead'
  , last'
  -- *** Monadic Indexing
  , indexM
  , headM
  , sheadM
  , lastM
  , unconsM
  , unsnocM
  -- ** Slicing
  , slice
  , slice'
  , sliceM
  , sslice
  , sliceAt
  , sliceAt'
  , sliceAtM
  -- *** Init
  , init
  , init'
  , initM
  -- *** Tail
  , tail
  , tail'
  , tailM
  -- *** Take
  , take
  , take'
  , takeM
  , takeWhile
  , stake
  -- *** Drop
  , drop
  , dropWhile
  , drop'
  , dropM
  , sdrop
  -- * Construction
  -- ** Initialization
  , empty
  , sempty
  , singleton
  , ssingleton
  , cons
  , snoc
  , A.replicate
  , sreplicate
  , generate
  , sgenerate
  -- , iterateN
  -- , iiterateN
  , siterate
  , siterateN
  -- ** Monadic initialization
  , sreplicateM
  , sgenerateM
  , siterateNM
  -- , create
  -- , createT
  -- ** Unfolding
  , sunfoldr
  , sunfoldrM
  , sunfoldrN
  , sunfoldrNM
  , sunfoldrExactN
  , sunfoldrExactNM
  -- , constructN
  -- , constructrN
  -- ** Enumeration
  , (...)
  , (..:)
  , enumFromN
  , senumFromN
  , enumFromStepN
  , senumFromStepN
  -- ** Concatenation
  -- , consS -- cons
  -- , snocS -- snoc
  , sappend  -- (++)
  , sconcat -- concat
  -- -- ** Restricitng memory usage
  -- , force
  -- -- * Modifying
  -- -- ** Bulk updates
  -- , (//)
  -- , update_
  -- -- ** Accumulations
  -- , accum
  -- , accumulate_
  -- -- ** Permutations
  -- , reverse
  -- , backpermute
  -- -- ** Manifest updates
  -- , modify
  -- -- * Elementwise
  -- -- ** Mapping
  , smap
  , simap
  -- , sconcatMap
  -- ** Monadic mapping
  , straverse
  , sitraverse
  , smapM
  , smapM_
  , simapM
  , simapM_
  , sforM
  , sforM_
  , siforM
  , siforM_
  -- ** Zipping
  , szip
  , szip3
  , szip4
  , szip5
  , szip6
  , szipWith
  , szipWith3
  , szipWith4
  , szipWith5
  , szipWith6
  , sizipWith
  , sizipWith3
  , sizipWith4
  , sizipWith5
  , sizipWith6
  -- ** Monadic zipping
  , szipWithM
  , szipWith3M
  , szipWith4M
  , szipWith5M
  , szipWith6M
  , sizipWithM
  , sizipWith3M
  , sizipWith4M
  , sizipWith5M
  , sizipWith6M

  , szipWithM_
  , szipWith3M_
  , szipWith4M_
  , szipWith5M_
  , szipWith6M_
  , sizipWithM_
  , sizipWith3M_
  , sizipWith4M_
  , sizipWith5M_
  , sizipWith6M_
  -- * Predicates
  -- ** Filtering
  , sfilter
  , sifilter
  , sfilterM
  , sifilterM
  -- , uniq -- sunique?
  , smapMaybe
  , smapMaybeM
  , scatMaybes
  , simapMaybe
  , simapMaybeM
  -- , stakeWhile
  -- , sdropWhile
  -- -- ** Partitioning
  -- , partition
  -- , unstablePartition
  -- , partitionWith
  -- , span
  -- , break
  -- -- ** Searching
  -- , elem
  -- , notElem
  -- , find
  , findIndex
  -- , findIndices
  -- , elemIndex
  -- , elemIndices
  -- * Folding
  , sfoldl
  , sfoldlM
  , sfoldlM_
  , sifoldl
  , sifoldlM
  , sifoldlM_
  , sfoldl1'
  , sfoldl1M
  , sfoldl1M_
  -- ** Specialized folds
  , sor
  , sand
  , sall
  , sany
  , ssum
  , sproduct
  , smaximum'
  , smaximumM
  -- , maximumBy
  , sminimum'
  , sminimumM
  -- , minimumBy
  -- , minIndex
  -- , minIndexBy
  -- , maxIndex
  -- , maxIndexBy
  -- -- ** Prefix sums
  -- , prescanl
  -- , prescanl'
  -- , postscanl
  -- , postscanl'
  -- , scanl
  -- , scanl'
  -- , scanl1
  -- , scanl1'
  -- , prescanr
  -- , prescanr'
  -- , postscanr
  -- , postscanr'
  -- , scanr
  -- , scanr'
  -- , scanr1
  -- , scanr1'
  -- * Conversions
  -- ** Lists
  , stoList
  , fromList
  , sfromList
  , sfromListN
  -- * Computation
  , compute
  , computeS
  , computeIO
  , computePrimM
  , computeAs
  , computeProxy
  , computeSource
  , computeWithStride
  , computeWithStrideAs
  , clone
  , convert
  , convertAs
  , convertProxy
  -- ** Re-exports
  , module Data.Massiv.Core
  , module Data.Massiv.Array.Delayed
  , module Data.Massiv.Array.Manifest
  , module Data.Massiv.Array.Mutable
  ) where

import Control.Monad hiding (filterM, replicateM)
import Data.Coerce
import Data.Massiv.Array.Delayed
import Data.Massiv.Array.Delayed.Pull
import Data.Massiv.Array.Delayed.Push
import Data.Massiv.Array.Delayed.Stream
import Data.Massiv.Array.Manifest
import Data.Massiv.Array.Manifest.Internal
import Data.Massiv.Array.Manifest.List (fromList)
import Data.Massiv.Array.Mutable
import Data.Massiv.Array.Ops.Construct
import qualified Data.Massiv.Array.Ops.Construct as A (replicate)
import Data.Massiv.Core
import Data.Massiv.Core.Common
import qualified Data.Massiv.Vector.Stream as S
import Data.Massiv.Vector.Unsafe
import Data.Maybe
import Prelude hiding (drop, dropWhile, init, length, null, replicate, splitAt,
                tail, take, takeWhile)

-- ========= --
-- Accessors --
-- ========= --


------------------------
-- Length information --
------------------------

-- | /O(1)/ - Get the length of a `Stream` array, but only if it is known exactly in
-- constant time without looking at any of the elements in the array.
--
-- /Related/: `maxLinearSize`, `size`, `elemsCount` and `totalElem`
--
-- ==== __Examples__
--
-- >>> slength $ sfromList []
-- Nothing
-- >>> slength $ sreplicate 5 ()
-- Just (Sz1 5)
-- >>> slength $ makeArrayLinearR D Seq (Sz1 5) id
-- Just (Sz1 5)
-- >>> slength $ sunfoldr (\x -> Just (x, x)) (0 :: Int)
-- Nothing
-- >>> slength $ sunfoldrN 10 (\x -> Just (x, x)) (0 :: Int)
-- Nothing
-- >>> slength $ sunfoldrExactN 10 (\x -> (x, x)) (0 :: Int)
-- Just (Sz1 10)
--
-- /__Similar__/:
--
-- [@Data.Foldable.`Data.Foldable.length`@] For some data structures, like a list for
-- example, it is an /O(n)/ operation, because there is a need to evaluate the full spine
-- and possibly even the elements in order to get the full length. With `Stream` vectors
-- that is rarely the case.
--
-- [@Data.Vector.Generic.`Data.Vector.Generic.length`@] In the vector package this
-- function will always break fusion, unless it is the only operation that is applied to
-- the vector.
--
-- @since 0.5.0
slength ::
     forall r ix e. Stream r ix e
  => Array r ix e
  -> Maybe Sz1
slength v =
  case stepsSize (toStream v) of
    LengthExact sz -> Just sz
    _              -> Nothing
{-# INLINE slength #-}


--------------
-- Indexing --
--------------


-- | /O(1)/ - Get the first element of a `Source` vector. Throws an error on empty.
--
-- /Related/: `shead'`, `headM`, `sheadM`, `unconsM`.
--
-- ==== __Examples__
--
-- >>> head' (Ix1 10 ..: 10000000000000)
-- 10
--
-- /__Similar__/:
--
-- [@Data.List.`Data.List.head`@] Also constant time and partial. Fusion is broken if
-- there other consumers of the list.
--
-- [@Data.Vector.Generic.`Data.Vector.Generic.head`@] Also constant time and partial. Will
-- cause materialization of the full vector if any other function is applied to the vector.
--
-- @since 0.5.0
head' ::
     forall r e. (HasCallStack, Source r e)
  => Vector r e
  -> e
head' = throwEither . headM
{-# INLINE head' #-}


-- | /O(1)/ - Get the first element of a `Source` vector.
--
-- /Related/: `head'`, `shead'`, `sheadM`, `unconsM`.
--
-- /__Throws Exceptions__/: `SizeEmptyException` when array is empty
--
-- ==== __Examples__
--
-- >>> headM (Ix1 10 ..: 10000000000000)
-- 10
-- >>> headM (Ix1 10 ..: 10000000000000) :: Maybe Int
-- Just 10
-- >>> headM (empty :: Array D Ix1 Int) :: Maybe Int
-- Nothing
-- >>> either show (const "") $ headM (Ix1 10 ..: 10)
-- "SizeEmptyException: (Sz1 0) corresponds to an empty array"
--
-- /__Similar__/:
--
-- [@Data.Maybe.`Data.Maybe.listToMaybe`@] It also a safe way to get the head of the list,
-- except it is restricted to `Maybe`
--
-- @since 0.5.0
headM ::
     forall r e m. (Source r e, MonadThrow m)
  => Vector r e
  -> m e
headM v
  | elemsCount v == 0 = throwM $ SizeEmptyException (size v)
  | otherwise = pure $ unsafeLinearIndex v 0
{-# INLINE headM #-}


-- | /O(1)/ - Get the first element of a `Stream` vector. Throws an error on empty.
--
-- /Related/: `head'`, `headM`, `sheadM`, `unconsM`.
--
-- ==== __Examples__
--
-- >>> shead' $ sunfoldr (\x -> Just (x, x)) (0 :: Int)
-- 0
-- >>> shead' (Ix1 3 ... 5)
-- 3
--
-- @since 0.5.0
shead' ::
     forall r e. (HasCallStack, Stream r Ix1 e)
  => Vector r e
  -> e
shead' = throwEither . sheadM
{-# INLINE shead' #-}

-- | /O(1)/ - Get the first element of a `Stream` vector.
--
-- /Related/: `head'`, `shead'`, `headM`, `unconsM`.
--
-- /__Throws Exceptions__/: `SizeEmptyException`
--
-- ==== __Examples__
--
-- >>> maybe 101 id $ sheadM (empty :: Vector D Int)
-- 101
-- >>> maybe 101 id $ sheadM (singleton 202 :: Vector D Int)
-- 202
-- >>> sheadM $ sunfoldr (\x -> Just (x, x)) (0 :: Int)
-- 0
-- >>> x <- sheadM $ sunfoldr (\_ -> Nothing) (0 :: Int)
-- *** Exception: SizeEmptyException: (Sz1 0) corresponds to an empty array
--
-- @since 0.5.0
sheadM ::
     forall r e m. (Stream r Ix1 e, MonadThrow m)
  => Vector r e
  -> m e
sheadM v =
  case S.unId (S.headMaybe (toStream v)) of
    Nothing -> throwM $ SizeEmptyException (zeroSz :: Sz1)
    Just e  -> pure e
{-# INLINE sheadM #-}


-- | /O(1)/ - Take one element off of the `Source` vector from the left side, as well as
-- the remaining part of the vector in delayed `D` representation.
--
-- /Related/: `head'`, `shead'`, `headM`, `sheadM`, `cons`
--
-- /__Throws Exceptions__/: `SizeEmptyException`
--
-- ==== __Examples__
--
-- >>> unconsM (fromList Seq [1,2,3] :: Array P Ix1 Int)
-- (1,Array P Seq (Sz1 2)
--   [ 2, 3 ])
--
-- /__Similar__/:
--
-- [@Data.List.`Data.List.uncons`@] Same concept, except it is restricted to `Maybe` instead of
-- the more general `MonadThrow`
--
-- @since 0.3.0
unconsM ::
     forall r e m. (MonadThrow m, Source r e)
  => Vector r e
  -> m (e, Vector r e)
unconsM arr
  | 0 == totalElem sz = throwM $ SizeEmptyException sz
  | otherwise = pure (unsafeLinearIndex arr 0, unsafeLinearSlice 1 (SafeSz (unSz sz - 1)) arr)
  where
    !sz = size arr
{-# INLINE unconsM #-}

-- | /O(1)/ - Take one element off of the vector from the right side, as well as the
-- remaining part of the vector.
--
-- /Related/: `last'`, `lastM`, `snoc`
--
-- /__Throws Exceptions__/: `SizeEmptyException`
--
-- ==== __Examples__
--
-- >>> unsnocM (fromList Seq [1,2,3] :: Array P Ix1 Int)
-- (Array P Seq (Sz1 2)
--   [ 1, 2 ],3)
--
-- @since 0.3.0
unsnocM ::
     forall r e m. (MonadThrow m, Source r e)
  => Vector r e
  -> m (Vector r e, e)
unsnocM arr
  | 0 == totalElem sz = throwM $ SizeEmptyException sz
  | otherwise = pure (unsafeLinearSlice 0 (SafeSz k) arr, unsafeLinearIndex arr k)
  where
    !sz = size arr
    !k = unSz sz - 1
{-# INLINE unsnocM #-}


-- | /O(1)/ - Get the last element of a `Source` vector. Throws an error on empty.
--
-- /Related/: `lastM`, `unsnocM`
--
-- ==== __Examples__
--
-- >>> last' (Ix1 10 ... 10000000000000)
-- 10000000000000
--
-- /__Similar__/:
--
-- [@Data.List.`Data.List.last`@] Also partial, but it has /O(n)/ complexity. Fusion is
-- broken if there other consumers of the list.
--
-- [@Data.Vector.Generic.`Data.Vector.Generic.last`@] Also constant time and partial. Will
-- cause materialization of the full vector if any other function is applied to the vector.
--
-- @since 0.5.0
last' :: forall r e. (HasCallStack, Source r e) => Vector r e -> e
last' = throwEither . lastM
{-# INLINE last' #-}


-- | /O(1)/ - Get the last element of a `Source` vector.
--
-- /Related/: `last'`, `unsnocM`
--
-- /__Throws Exceptions__/: `SizeEmptyException`
--
-- ==== __Examples__
--
-- >>> lastM (Ix1 10 ... 10000000000000)
-- 10000000000000
-- >>> lastM (Ix1 10 ... 10000000000000) :: Maybe Int
-- Just 10000000000000
-- >>> either show (const "") $ lastM (fromList Seq [] :: Array P Ix1 Int)
-- "SizeEmptyException: (Sz1 0) corresponds to an empty array"
--
-- @since 0.5.0
lastM :: forall r e m. (Source r e, MonadThrow m) => Vector r e -> m e
lastM v
  | k == 0 = throwM $ SizeEmptyException (size v)
  | otherwise = pure $ unsafeLinearIndex v (k - 1)
  where k = unSz (size v)
{-# INLINE lastM #-}


-- | /O(1)/ - Take a slice of a `Source` vector. Never fails, instead adjusts the indices.
--
-- ==== __Examples__
--
-- >>> slice 10 5 (Ix1 0 ... 10000000000000)
-- Array D Seq (Sz1 5)
--   [ 10, 11, 12, 13, 14 ]
-- >>> slice (-10) 5 (Ix1 0 ... 10000000000000)
-- Array D Seq (Sz1 5)
--   [ 0, 1, 2, 3, 4 ]
-- >>> slice 9999999999998 50 (Ix1 0 ... 10000000000000)
-- Array D Seq (Sz1 3)
--   [ 9999999999998, 9999999999999, 10000000000000 ]
--
-- @since 0.5.0
slice :: forall r e. Source r e => Ix1 -> Sz1 -> Vector r e -> Vector r e
slice !i (Sz k) v = unsafeLinearSlice i' newSz v
  where
    !i' = min n (max 0 i)
    !newSz = SafeSz (min (n - i') k)
    Sz n = size v
{-# INLINE slice #-}

-- | /O(1)/ - Take a slice of a `Source` vector. Throws an error on incorrect indices.
--
-- ==== __Examples__
--
-- >>> slice' 10 5 (Ix1 0 ... 100)
-- Array D Seq (Sz1 5)
--   [ 10, 11, 12, 13, 14 ]
-- >>> slice' 9999999999998 3 (Ix1 0 ... 10000000000000)
-- Array D Seq (Sz1 3)
--   [ 9999999999998, 9999999999999, 10000000000000 ]
--
-- @since 0.5.0
slice' :: forall r e. (HasCallStack, Source r e) => Ix1 -> Sz1 -> Vector r e -> Vector r e
slice' i k = throwEither . sliceM i k
{-# INLINE slice' #-}


-- | /O(1)/ - Take a slice of a `Source` vector. Throws an error on incorrect indices.
--
-- /__Throws Exceptions__/: `SizeSubregionException`
--
-- ==== __Examples__
--
-- >>> sliceM 10 5 (Ix1 0 ... 100)
-- Array D Seq (Sz1 5)
--   [ 10, 11, 12, 13, 14 ]
-- >>> sliceM (-10) 5 (Ix1 0 ... 100)
-- *** Exception: SizeSubregionException: (Sz1 101) is to small for -10 (Sz1 5)
-- >>> sliceM 98 50 (Ix1 0 ... 100)
-- *** Exception: SizeSubregionException: (Sz1 101) is to small for 98 (Sz1 50)
-- >>> sliceM 9999999999998 3 (Ix1 0 ... 10000000000000)
-- Array D Seq (Sz1 3)
--   [ 9999999999998, 9999999999999, 10000000000000 ]
--
-- @since 0.5.0
sliceM ::
     forall r e m. (Source r e, MonadThrow m)
  => Ix1
  -- ^ Starting index
  -> Sz1
  -- ^ Number of elements to take from the Source vector
  -> Vector r e
  -- ^ Source vector to take a slice from
  -> m (Vector r e)
sliceM i newSz@(Sz k) v
  | i >= 0 && k <= n - i = pure $ unsafeLinearSlice i newSz v
  | otherwise = throwM $ SizeSubregionException sz i newSz
  where
    sz@(Sz n) = size v
{-# INLINE sliceM #-}


-- | Take a slice of a `Stream` vector. Never fails, instead adjusts the indices.
--
-- ==== __Examples__
--
-- >>> sslice 10 5 (Ix1 0 ... 10000000000000)
-- Array DS Seq (Sz1 5)
--   [ 10, 11, 12, 13, 14 ]
-- >>> sslice 10 5 (sfromList [0 :: Int .. ])
-- Array DS Seq (Sz1 5)
--   [ 10, 11, 12, 13, 14 ]
-- >>> sslice (-10) 5 (Ix1 0 ... 10000000000000)
-- Array DS Seq (Sz1 5)
--   [ 0, 1, 2, 3, 4 ]
--
-- Unlike `slice` it has to iterate through each element until the staring index is reached,
-- therefore something like @sslice 9999999999998 50 (Ix1 0 ... 10000000000000)@ will not
-- be feasable.
--
-- >>> import System.Timeout (timeout)
-- >>> let smallArr = sslice 9999999999998 50 (Ix1 0 ... 10000000000000)
-- >>> timeout 500000 (computeIO smallArr :: IO (Array P Ix1 Int))
-- Nothing
--
-- @since 0.5.0
sslice ::
     forall r e. Stream r Ix1 e
  => Ix1
  -- ^ Starting index
  -> Sz1
  -- ^ Number of elements to take from the stream vector
  -> Vector r e
  -- ^ Stream vector to take a slice from
  -> Vector DS e
sslice !i !k = fromSteps . S.slice i k . S.toStream
{-# INLINE sslice #-}


-- | /O(1)/ - Get a vector without the last element. Never fails.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array as A
-- >>> A.init (0 ..: 10)
-- Array D Seq (Sz1 9)
--   [ 0, 1, 2, 3, 4, 5, 6, 7, 8 ]
-- >>> A.init (empty :: Array D Ix1 Int)
-- Array D Seq (Sz1 0)
--   [  ]
--
-- @since 0.5.0
init :: forall r e. Source r e => Vector r e -> Vector r e
init v = unsafeLinearSlice 0 (Sz (coerce (size v) - 1)) v
{-# INLINE init #-}

-- | /O(1)/ - Get a vector without the last element. Throws an error on empty
--
-- ==== __Examples__
--
-- >>> init' (0 ..: 10)
-- Array D Seq (Sz1 9)
--   [ 0, 1, 2, 3, 4, 5, 6, 7, 8 ]
--
-- @since 0.5.0
init' :: forall r e. (HasCallStack, Source r e) => Vector r e -> Vector r e
init' = throwEither . initM
{-# INLINE init' #-}

-- | /O(1)/ - Get a vector without the last element. Throws an error on empty
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array as A
-- >>> initM (0 ..: 10)
-- Array D Seq (Sz1 9)
--   [ 0, 1, 2, 3, 4, 5, 6, 7, 8 ]
-- >>> maybe 0 A.sum $ initM (0 ..: 10)
-- 36
-- >>> maybe 0 A.sum $ initM (empty :: Array D Ix1 Int)
-- 0
--
-- @since 0.5.0
initM :: forall r e m. (Source r e, MonadThrow m) => Vector r e -> m (Vector r e)
initM v = do
  when (elemsCount v == 0) $ throwM $ SizeEmptyException $ size v
  pure $ unsafeInit v
{-# INLINE initM #-}



-- | /O(1)/ - Get a vector without the first element. Never fails
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array as A
-- >>> A.tail (0 ..: 10)
-- Array D Seq (Sz1 9)
--   [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
-- >>> A.tail (empty :: Array D Ix1 Int)
-- Array D Seq (Sz1 0)
--   [  ]
--
-- @since 0.5.0
tail :: forall r e. Source r e => Vector r e -> Vector r e
tail = drop oneSz
{-# INLINE tail #-}


-- | /O(1)/ - Get a vector without the first element. Throws an error on empty
--
-- ==== __Examples__
--
-- λ> tail' (0 ..: 10)
-- Array D Seq (Sz1 9)
--   [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
-- λ> tail' (empty :: Array D Ix1 Int)
-- Array D *** Exception: SizeEmptyException: (Sz1 0) corresponds to an empty array
--
-- @since 0.5.0
tail' :: forall r e. (HasCallStack, Source r e) => Vector r e -> Vector r e
tail' = throwEither . tailM
{-# INLINE tail' #-}


-- | /O(1)/ - Get the vector without the first element. Throws an error on empty
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array as A
-- >>> tailM (0 ..: 10)
-- Array D Seq (Sz1 9)
--   [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
-- >>> maybe 0 A.sum $ tailM (0 ..: 10)
-- 45
-- >>> maybe 0 A.sum $ tailM (empty :: Array D Ix1 Int)
-- 0
--
-- @since 0.5.0
tailM :: forall r e m. (Source r e, MonadThrow m) => Vector r e -> m (Vector r e)
tailM v = do
  when (elemsCount v == 0) $ throwM $ SizeEmptyException $ size v
  pure $ unsafeTail v
{-# INLINE tailM #-}



-- | /O(1)/ - Get the vector with the first @n@ elements. Never fails
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array as A
-- >>> A.take 5 (0 ..: 10)
-- Array D Seq (Sz1 5)
--   [ 0, 1, 2, 3, 4 ]
-- >>> A.take 0 (0 ..: 10)
-- Array D Seq (Sz1 0)
--   [  ]
-- >>> A.take 100 (0 ..: 10)
-- Array D Seq (Sz1 10)
--   [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
--
-- @since 0.5.0
take :: Source r e => Sz1 -> Vector r e -> Vector r e
take k = fst . sliceAt k
{-# INLINE take #-}


-- | Slice a manifest vector in such a way that it will contain all initial elements that
-- satisfy the supplied predicate.
--
-- @since 0.5.5
takeWhile :: Manifest r e => (e -> Bool) -> Vector r e -> Vector r e
takeWhile f v = take (go 0) v
  where
    !k = elemsCount v
    go !i
      | i < k && f (unsafeLinearIndex v i) = go (i + 1)
      | otherwise = SafeSz i
{-# INLINE takeWhile #-}


-- | /O(1)/ - Get the vector with the first @n@ elements. Throws an error size is less
-- than @n@.
--
-- ==== __Examples__
--
-- >>> take' 0 (0 ..: 0)
-- Array D Seq (Sz1 0)
--   [  ]
-- >>> take' 5 (0 ..: 10)
-- Array D Seq (Sz1 5)
--   [ 0, 1, 2, 3, 4 ]
--
-- @since 0.5.0
take' :: forall r e. (HasCallStack, Source r e) => Sz1 -> Vector r e -> Vector r e
take' k = throwEither . takeM k
{-# INLINE take' #-}

-- | /O(1)/ - Get the vector with the first @n@ elements. Throws an error size is less than @n@
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array as A
-- >>> takeM 5 (0 ..: 10)
-- Array D Seq (Sz1 5)
--   [ 0, 1, 2, 3, 4 ]
-- >>> maybe 0 A.sum $ takeM 5 (0 ..: 10)
-- 10
-- >>> maybe (-1) A.sum $ takeM 15 (0 ..: 10)
-- -1
-- >>> takeM 15 (0 ..: 10)
-- *** Exception: SizeSubregionException: (Sz1 10) is to small for 0 (Sz1 15)
--
-- @since 0.5.0
takeM :: forall r e m. (Source r e, MonadThrow m) => Sz1 -> Vector r e -> m (Vector r e)
takeM k v = do
  let sz = size v
  when (k > sz) $ throwM $ SizeSubregionException sz 0 k
  pure $ unsafeTake k v
{-# INLINE takeM #-}

-- | /O(1)/ - Get a `Stream` vector with the first @n@ elements. Never fails
--
-- ==== __Examples__
--
-- @since 0.5.0
stake :: forall r e. Stream r Ix1 e => Sz1 -> Vector r e -> Vector DS e
stake n = fromSteps . S.take n . S.toStream
{-# INLINE stake #-}

-- |
--
-- ==== __Examples__
--
-- @since 0.5.0
drop :: forall r e. Source r e => Sz1 -> Vector r e -> Vector r e
drop k = snd . sliceAt k
{-# INLINE drop #-}


-- | Slice a manifest vector in such a way that it will not contain all initial elements
-- that satisfy the supplied predicate.
--
-- @since 0.5.5
dropWhile :: forall r e. Manifest r e => (e -> Bool) -> Vector r e -> Vector r e
dropWhile f v = drop (go 0) v
  where
    !k = elemsCount v
    go !i
      | i < k && f (unsafeLinearIndex v i) = go (i + 1)
      | otherwise = SafeSz i
{-# INLINE dropWhile #-}


-- | Keep all but the first @n@ elements from the delayed stream vector.
--
-- ==== __Examples__
--
-- @since 0.5.0
sdrop :: forall r e. Stream r Ix1 e => Sz1 -> Vector r e -> Vector DS e
sdrop n = fromSteps . S.drop n . S.toStream
{-# INLINE sdrop #-}

-- |
--
-- ==== __Examples__
--
-- @since 0.5.0
drop' :: forall r e. (HasCallStack, Source r e) => Sz1 -> Vector r e -> Vector r e
drop' k = throwEither . dropM k
{-# INLINE drop' #-}

-- |
--
-- ==== __Examples__
--
-- @since 0.5.0
dropM :: forall r e m. (Source r e, MonadThrow m) => Sz1 -> Vector r e -> m (Vector r e)
dropM k@(Sz d) v = do
  let sz@(Sz n) = size v
  when (k > sz) $ throwM $ SizeSubregionException sz d (SafeSz (n - d))
  pure $ unsafeLinearSlice d (SafeSz (n - d)) v
{-# INLINE dropM #-}


-- | Samel as `sliceAt`, except it never fails.
--
--
-- ==== __Examples__
--
-- @since 0.5.0
sliceAt :: forall r e. Source r e => Sz1 -> Vector r e -> (Vector r e, Vector r e)
sliceAt (Sz k) v = (unsafeTake d v, unsafeDrop d v)
  where
    !n = coerce (size v)
    !d = SafeSz (min k n)
{-# INLINE sliceAt #-}

-- | Same as `Data.Massiv.Array.splitAt'`, except for a flat vector.
--
-- ==== __Examples__
--
-- @since 0.5.0
sliceAt' :: (HasCallStack, Source r e) => Sz1 -> Vector r e -> (Vector r e, Vector r e)
sliceAt' k = throwEither . sliceAtM k
{-# INLINE sliceAt' #-}

-- | Same as `Data.Massiv.Array.splitAtM`, except for a flat vector.
--
-- ==== __Examples__
--
-- @since 0.5.0
sliceAtM :: forall r e m. (Source r e, MonadThrow m) => Sz1 -> Vector r e -> m (Vector r e, Vector r e)
sliceAtM k v = do
  l <- takeM k v
  pure (l, unsafeDrop k v)
{-# INLINE sliceAtM #-}


-- | Create an empty delayed stream vector
--
-- ==== __Examples__
--
-- @since 0.5.0
sempty :: Vector DS e
sempty = DSArray S.empty
{-# INLINE sempty #-}

-- | Create a delayed stream vector with a single element
--
-- ==== __Examples__
--
-- @since 0.5.0
ssingleton :: e -> Vector DS e
ssingleton = DSArray . S.singleton
{-# INLINE ssingleton #-}

-- | /O(1)/ - Add an element to the vector from the left side
--
-- @since 0.3.0
cons :: forall r e. (Size r, Load r Ix1 e) => e -> Vector r e -> Vector DL e
cons e v =
  let dv = toLoadArray v
      load scheduler startAt uWrite uSet =
        uWrite startAt e >> dlLoad dv scheduler (startAt + 1) uWrite uSet
      {-# INLINE load #-}
   in dv {dlSize = SafeSz (1 + unSz (dlSize dv)), dlLoad = load}
{-# INLINE cons #-}

-- | /O(1)/ - Add an element to the vector from the right side
--
-- @since 0.3.0
snoc :: forall r e. (Size r, Load r Ix1 e) => Vector r e -> e -> Vector DL e
snoc v e =
  let dv = toLoadArray v
      !k = unSz (size dv)
      load scheduler startAt uWrite uSet =
        dlLoad dv scheduler startAt uWrite uSet >> uWrite (k + startAt) e
      {-# INLINE load #-}
   in dv {dlSize = SafeSz (1 + k), dlLoad = load}
{-# INLINE snoc #-}



-- | Replicate the same element @n@ times
--
-- ==== __Examples__
--
-- @since 0.5.0
sreplicate :: Sz1 -> e -> Vector DS e
sreplicate n = DSArray . S.replicate n
{-# INLINE sreplicate #-}

-- | Create a delayed vector of length @n@ with a function that maps an index to an
-- element. Same as `makeLinearArray`
--
-- ==== __Examples__
--
-- @since 0.5.0
generate :: Comp -> Sz1 -> (Ix1 -> e) -> Vector D e
generate = makeArrayLinear
{-# INLINE generate #-}

-- | Create a delayed stream vector of length @n@ with a function that maps an index to an
-- element. Same as `makeLinearArray`
--
-- ==== __Examples__
--
-- @since 0.5.0
sgenerate :: Sz1 -> (Ix1 -> e) -> Vector DS e
sgenerate n = DSArray . S.generate n
{-# INLINE sgenerate #-}


-- | Create a delayed stream vector of infinite length by repeatedly applying a function to the
-- initial value.
--
-- ==== __Examples__
--
-- >>> stake 10 $ siterate succ 'a'
-- Array DS Seq (Sz1 10)
--   [ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j' ]
--
-- @since 0.5.2
siterate :: (e -> e) -> e -> Vector DS e
siterate f = fromSteps . S.unfoldr (\a -> Just (a, f a))
{-# INLINE siterate #-}

-- | Create a delayed stream vector of length @n@ by repeatedly applying a function to the
-- initial value.
--
-- ==== __Examples__
--
-- >>> siterateN 10 succ 'a'
-- Array DS Seq (Sz1 10)
--   [ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j' ]
--
-- @since 0.5.0
siterateN :: Sz1 -> (e -> e) -> e -> Vector DS e
siterateN n f a = fromSteps $ S.iterateN n f a
{-# INLINE siterateN #-}


-- | Create a vector by using the same monadic action @n@ times
--
-- ==== __Examples__
--
-- @since 0.5.0
sreplicateM :: forall e m. Monad m => Sz1 -> m e -> m (Vector DS e)
sreplicateM n f = fromStepsM $ S.replicateM n f
{-# INLINE sreplicateM #-}


-- | Create a delayed stream vector of length @n@ with a monadic action that from an index
-- generates an element.
--
-- ==== __Examples__
--
-- @since 0.5.0
sgenerateM :: forall e m. Monad m => Sz1 -> (Ix1 -> m e) -> m (Vector DS e)
sgenerateM n f = fromStepsM $ S.generateM n f
{-# INLINE sgenerateM #-}


-- | Create a delayed stream vector of length @n@ by repeatedly apply a monadic action to
-- the initial value.
--
-- ==== __Examples__
--
-- @since 0.5.0
siterateNM :: forall e m. Monad m => Sz1 -> (e -> m e) -> e -> m (Vector DS e)
siterateNM n f a = fromStepsM $ S.iterateNM n f a
{-# INLINE siterateNM #-}




-- | Right unfolding function. Useful when it is unknown ahead of time how many
-- elements a vector will have.
--
-- ====__Example__
--
-- >>> import Data.Massiv.Array as A
-- >>> sunfoldr (\i -> if i < 9 then Just (i * i, i + 1) else Nothing) (0 :: Int)
-- Array DS Seq (Sz1 9)
--   [ 0, 1, 4, 9, 16, 25, 36, 49, 64 ]
--
-- @since 0.5.0
sunfoldr :: forall e s. (s -> Maybe (e, s)) -> s -> Vector DS e
sunfoldr f = DSArray . S.unfoldr f
{-# INLINE sunfoldr #-}



-- | /O(n)/ - Right unfolding function with at most @n@ number of elements.
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Array as A
-- >>> sunfoldrN 9 (\i -> Just (i*i, i + 1)) (0 :: Int)
-- Array DS Seq (Sz1 9)
--   [ 0, 1, 4, 9, 16, 25, 36, 49, 64 ]
--
-- @since 0.5.0
sunfoldrN ::
     forall e s.
     Sz1
  -- ^ @n@ - maximum number of elements that the vector will have
  -> (s -> Maybe (e, s))
  -- ^ Unfolding function. Stops when `Nothing` is returned or maximum number of elements
  -- is reached.
  -> s -- ^ Inititial element.
  -> Vector DS e
sunfoldrN n f = DSArray . S.unfoldrN n f
{-# INLINE sunfoldrN #-}

-- | /O(n)/ - Same as `unfoldr`, but with monadic generating function.
--
-- ==== __Examples__
--
-- >>> import Control.Monad (when, guard)
-- >>> sunfoldrM (\i -> when (i == 0) (Left "Zero denominator") >> Right (guard (i < 5) >> Just (100 `div` i, i + 1))) (-10 :: Int)
-- Left "Zero denominator"
-- >>> sunfoldrM (\i -> when (i == 0) (Left "Zero denominator") >> Right (guard (i < -5) >> Just (100 `div` i, i + 1))) (-10 :: Int)
-- Right (Array DS Seq (Sz1 5)
--   [ -10, -12, -13, -15, -17 ]
-- )
--
-- @since 0.5.0
sunfoldrM :: forall e s m. Monad m => (s -> m (Maybe (e, s))) -> s -> m (Vector DS e)
sunfoldrM f = fromStepsM . S.unfoldrM f
{-# INLINE sunfoldrM #-}

-- | /O(n)/ - Same as `unfoldrN`, but with monadic generating function.
--
-- ==== __Examples__
--
-- >>> import Control.Monad (guard)
-- >>> sunfoldrNM 6 (\i -> print i >> pure (guard (i < 5) >> Just (i * i, i + 1))) (10 :: Int)
-- 10
-- Array DS Seq (Sz1 0)
--   [  ]
-- >>> sunfoldrNM 6 (\i -> print i >> pure (guard (i < 15) >> Just (i * i, i + 1))) (10 :: Int)
-- 10
-- 11
-- 12
-- 13
-- 14
-- 15
-- Array DS Seq (Sz1 5)
--   [ 100, 121, 144, 169, 196 ]
--
--
-- @since 0.5.0
sunfoldrNM :: forall e s m. Monad m => Sz1 -> (s -> m (Maybe (e, s))) -> s -> m (Vector DS e)
sunfoldrNM (Sz n) f = fromStepsM . S.unfoldrNM n f
{-# INLINE sunfoldrNM #-}


-- | /O(n)/ - Similar to `unfoldrN`, except the length of the resulting vector will be exactly @n@
--
-- ==== __Examples__
--
-- >>> sunfoldrExactN 10 (\i -> (i * i, i + 1)) (10 :: Int)
-- Array DS Seq (Sz1 10)
--   [ 100, 121, 144, 169, 196, 225, 256, 289, 324, 361 ]
--
-- @since 0.5.0
sunfoldrExactN :: forall e s. Sz1 -> (s -> (e, s)) -> s -> Vector DS e
sunfoldrExactN n f = fromSteps . S.unfoldrExactN n f
{-# INLINE sunfoldrExactN #-}

-- | /O(n)/ - Similar to `unfoldrNM`, except the length of the resulting vector will be exactly @n@
--
-- ==== __Examples__
--
-- λ> sunfoldrExactNM 11 (\i -> pure (100 `div` i, i + 1)) (-10 :: Int)
-- Array DS *** Exception: divide by zero
-- λ> sunfoldrExactNM 11 (\i -> guard (i /= 0) >> Just (100 `div` i, i + 1)) (-10 :: Int)
-- Nothing
-- λ> sunfoldrExactNM 9 (\i -> guard (i /= 0) >> Just (100 `div` i, i + 1)) (-10 :: Int)
-- Just (Array DS Seq (Sz1 9)
--   [ -10, -12, -13, -15, -17, -20, -25, -34, -50 ]
-- )
--
-- @since 0.5.0
sunfoldrExactNM :: forall e s m. Monad m => Sz1 -> (s -> m (e, s)) -> s -> m (Vector DS e)
sunfoldrExactNM n f = fromStepsM . S.unfoldrExactNM n f
{-# INLINE sunfoldrExactNM #-}


-- | /O(n)/ - Enumerate from a starting number @x@ exactly @n@ times with a step @1@.
--
-- /Related/: `senumFromStepN`, `enumFromN`, `enumFromStepN`, `rangeSize`,
-- `rangeStepSize`, `range`, `rangeStep'`
--
-- ==== __Examples__
--
-- >>> senumFromN (10 :: Int) 9
-- Array DS Seq (Sz1 9)
--   [ 10, 11, 12, 13, 14, 15, 16, 17, 18 ]
--
-- /__Similar__/:
--
-- [@Prelude.`Prelude.enumFromTo`@] Very similar to @[x .. x + n - 1]@, except that
-- `senumFromN` is faster and it only works for `Num` and not for `Enum` elements
--
-- [@Data.Vector.Generic.`Data.Vector.Generic.enumFromN`@] Uses exactly the same
-- implementation underneath.
--
-- @since 0.5.0
senumFromN ::
     Num e
  => e -- ^ @x@ - starting number
  -> Sz1 -- ^ @n@ - length of resulting vector
  -> Vector DS e
senumFromN x n = DSArray $ S.enumFromStepN x 1 n
{-# INLINE senumFromN #-}

-- | /O(n)/ - Enumerate from a starting number @x@ exactly @n@ times with a custom step value @dx@
--
-- ==== __Examples__
--
-- >>> senumFromStepN (5 :: Int) 2 10
-- Array DS Seq (Sz1 10)
--   [ 5, 7, 9, 11, 13, 15, 17, 19, 21, 23 ]
--
-- __/Similar/__:
--
-- [@Prelude.`Prelude.enumFrom`@] Just like @take n [x, x + dx ..]@, except that
-- `senumFromN` is faster and it only works for `Num` and not for `Enum` elements
--
-- [@Data.Vector.Generic.`Data.Vector.Generic.enumFromStepN`@] Uses exactly the same
-- implementation underneath.
--
-- @since 0.5.0
senumFromStepN ::
     Num e
  => e -- ^ @x@ - starting number
  -> e -- ^ @dx@ - Step
  -> Sz1 -- ^ @n@ - length of resulting vector
  -> Vector DS e
senumFromStepN x step n = DSArray $ S.enumFromStepN x step n
{-# INLINE senumFromStepN #-}



-- | Append two vectors together
--
-- /Related/: `appendM`, `appendOuterM`,
--
-- ==== __Examples__
--
-- λ> sappend (1 ..: 6) (senumFromStepN 6 (-1) 6)
-- Array DS Seq (Sz1 11)
--   [ 1, 2, 3, 4, 5, 6, 5, 4, 3, 2, 1 ]
--
-- __/Similar/__:
--
-- [@Data.Semigroup.`Data.Semigroup.<>`@] `DS` and `DL` arrays have instances for
-- `Semigroup`, so they will work in a similar fashion. `sappend` differs in that it accepts
-- `Stream` arrays with possibly different representations.
--
-- [@Data.List.`Data.List.++`@] Same operation, but for lists.
--
-- [@Data.Vector.Generic.`Data.Vector.Generic.++`@] Uses exactly the same implementation
-- underneath as `sappend`, except that it cannot append two vectors with different
-- memory representations.
--
-- @since 0.5.0
sappend ::
     forall r1 r2 e. (Stream r1 Ix1 e, Stream r2 Ix1 e)
  => Vector r1 e
  -> Vector r2 e
  -> Vector DS e
sappend a1 a2 = fromSteps (toStream a1 `S.append` toStream a2)
{-# INLINE sappend #-}


-- | Concat vectors together
--
-- /Related/: `concatM`, `concatOuterM`,
--
-- ==== __Examples__
--
-- >>> sconcat [2 ... 6, empty, singleton 1, generate Seq 5 id]
-- Array DS Seq (Sz1 11)
--   [ 2, 3, 4, 5, 6, 1, 0, 1, 2, 3, 4 ]
-- >>> sconcat [senumFromN 2 5, sempty, ssingleton 1, sgenerate 5 id]
-- Array DS Seq (Sz1 11)
--   [ 2, 3, 4, 5, 6, 1, 0, 1, 2, 3, 4 ]
--
-- __/Similar/__:
--
-- [@Data.Monoid.`Data.Monoid.mconcat`@] `DS` and `DL` arrays have instances for `Monoid`, so
-- they will work in a similar fashion. `sconcat` differs in that it accepts `Stream`
-- arrays of other representations.
--
-- [@Data.List.`Data.List.concat`@] Same operation, but for lists.
--
-- [@Data.Vector.Generic.`Data.Vector.Generic.concat`@] Uses exactly the same
-- implementation underneath as `sconcat`.
--
-- @since 0.5.0
sconcat :: forall r e. Stream r Ix1 e => [Vector r e] -> Vector DS e
sconcat = DSArray . foldMap toStream
{-# INLINE sconcat #-}

-- | Convert a list to a delayed stream vector
--
-- /Related/: `fromList`, `fromListN`, `sfromListN`
--
-- ==== __Examples__
--
-- >>> sfromList ([] :: [Int])
-- Array DS Seq (Sz1 0)
--   [  ]
-- >>> sfromList ([1,2,3] :: [Int])
-- Array DS Seq (Sz1 3)
--   [ 1, 2, 3 ]
--
-- @since 0.5.0
sfromList :: [e] -> Vector DS e
sfromList = fromSteps . S.fromList
{-# INLINE sfromList #-}

-- | Convert a list to a delayed stream vector. Length of the resulting vector will be at
-- most @n@. This version isn't really more efficient then `sfromList`, but there is
-- `Data.Massiv.Array.Unsafe.unsafeFromListN`
--
-- /Related/: `fromList`, `fromListN`, `sfromList`
--
-- ==== __Examples__
--
-- >>> sfromListN 10 [1 :: Int ..]
-- Array DS Seq (Sz1 10)
--   [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
-- >>> sfromListN 10 [1 :: Int .. 5]
-- Array DS Seq (Sz1 5)
--   [ 1, 2, 3, 4, 5 ]
--
-- @since 0.5.1
sfromListN :: Sz1 -> [e] -> Vector DS e
sfromListN (Sz n) = fromSteps . S.fromListN n
{-# INLINE sfromListN #-}

-- | Convert an array to a list by the means of a delayed stream vector.
--
-- /Related/: `toList`
--
-- ==== __Examples__
--
-- @since 0.5.0
stoList :: forall r ix e. Stream r ix e => Array r ix e -> [e]
stoList = S.toList . toStream
{-# INLINE stoList #-}



-- | Sequentially filter out elements from the array according to the supplied predicate.
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Array as A
-- >>> arr = makeArrayR D Seq (Sz2 3 4) fromIx2
-- >>> arr
-- Array D Seq (Sz (3 :. 4))
--   [ [ (0,0), (0,1), (0,2), (0,3) ]
--   , [ (1,0), (1,1), (1,2), (1,3) ]
--   , [ (2,0), (2,1), (2,2), (2,3) ]
--   ]
-- >>> sfilter (even . fst) arr
-- Array DS Seq (Sz1 8)
--   [ (0,0), (0,1), (0,2), (0,3), (2,0), (2,1), (2,2), (2,3) ]
--
-- @since 0.5.0
sfilter :: forall r ix e. S.Stream r ix e => (e -> Bool) -> Array r ix e -> Vector DS e
sfilter f = DSArray . S.filter f . S.toStream
{-# INLINE sfilter #-}


-- | Similar to `sfilter`, but filter with an index aware function.
--
-- ==== __Examples__
--
-- @since 0.5.0
sifilter :: forall r ix e. Stream r ix e => (ix -> e -> Bool) -> Array r ix e -> Vector DS e
sifilter f =
  simapMaybe $ \ix e ->
    if f ix e
      then Just e
      else Nothing
{-# INLINE sifilter #-}


-- | Sequentially filter out elements from the array according to the supplied applicative predicate.
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Array as A
-- >>> arr = makeArrayR D Seq (Sz2 3 4) fromIx2
-- >>> arr
-- Array D Seq (Sz (3 :. 4))
--   [ [ (0,0), (0,1), (0,2), (0,3) ]
--   , [ (1,0), (1,1), (1,2), (1,3) ]
--   , [ (2,0), (2,1), (2,2), (2,3) ]
--   ]
-- >>> sfilterM (Just . odd . fst) arr
-- Just (Array DS Seq (Sz1 4)
--   [ (1,0), (1,1), (1,2), (1,3) ]
-- )
-- >>> sfilterM (\ix@(_, j) -> print ix >> return (even j)) arr
-- (0,0)
-- (0,1)
-- (0,2)
-- (0,3)
-- (1,0)
-- (1,1)
-- (1,2)
-- (1,3)
-- (2,0)
-- (2,1)
-- (2,2)
-- (2,3)
-- Array DS Seq (Sz1 6)
--   [ (0,0), (0,2), (1,0), (1,2), (2,0), (2,2) ]
--
-- @since 0.5.0
sfilterM ::
     forall r ix e f. (S.Stream r ix e, Applicative f)
  => (e -> f Bool)
  -> Array r ix e
  -> f (Vector DS e)
sfilterM f arr = DSArray <$> S.filterA f (S.toStream arr)
{-# INLINE sfilterM #-}


-- | Similar to `filterM`, but filter with an index aware function.
--
-- Corresponds to: @`filterM` (uncurry f) . `simap` (,)@
--
-- @since 0.5.0
sifilterM ::
     forall r ix e f. (Stream r ix e, Applicative f)
  => (ix -> e -> f Bool)
  -> Array r ix e
  -> f (Vector DS e)
sifilterM f =
  simapMaybeM $ \ix e ->
    (\p ->
       if p
         then Just e
         else Nothing) <$>
    f ix e
{-# INLINE sifilterM #-}


-- | Apply a function to each element of the array, while discarding `Nothing` and
-- keeping the `Maybe` result.
--
-- ==== __Examples__
--
-- @since 0.5.0
smapMaybe :: forall r ix a b. S.Stream r ix a => (a -> Maybe b) -> Array r ix a -> Vector DS b
smapMaybe f = DSArray . S.mapMaybe f . S.toStream
{-# INLINE smapMaybe #-}


-- | Similar to `smapMaybe`, but map with an index aware function.
--
-- ==== __Examples__
--
-- @since 0.5.0
simapMaybe ::
     forall r ix a b. Stream r ix a
  => (ix -> a -> Maybe b)
  -> Array r ix a
  -> Vector DS b
simapMaybe f = DSArray . S.mapMaybe (uncurry f) . toStreamIx
{-# INLINE simapMaybe #-}

-- | Similar to `smapMaybeM`, but map with an index aware function.
--
-- ==== __Examples__
--
-- @since 0.5.0
simapMaybeM ::
     forall r ix a b f. (Stream r ix a, Applicative f)
  => (ix -> a -> f (Maybe b))
  -> Array r ix a
  -> f (Vector DS b)
simapMaybeM f = fmap DSArray . S.mapMaybeA (uncurry f) . toStreamIx
{-# INLINE simapMaybeM #-}


-- | Keep all `Maybe`s and discard the `Nothing`s.
--
-- ==== __Examples__
--
-- @since 0.5.0
scatMaybes :: forall r ix a. S.Stream r ix (Maybe a) => Array r ix (Maybe a) -> Vector DS a
scatMaybes = smapMaybe id
{-# INLINE scatMaybes #-}


-- | Similar to `smapMaybe`, but with the `Applicative` function.
--
-- Similar to @mapMaybe id <$> mapM f arr@
--
-- ==== __Examples__
--
-- @since 0.5.0
smapMaybeM ::
     forall r ix a b f. (S.Stream r ix a, Applicative f)
  => (a -> f (Maybe b))
  -> Array r ix a
  -> f (Vector DS b)
smapMaybeM f = fmap DSArray . S.mapMaybeA f . S.toStream
{-# INLINE smapMaybeM #-}



-- | Map a function over a stream vector
--
-- ==== __Examples__
--
-- @since 0.5.0
smap ::
     forall r ix a b. S.Stream r ix a
  => (a -> b)
  -> Array r ix a
  -> Vector DS b
smap f = fromSteps . S.map f . S.toStream
{-# INLINE smap #-}

-- | Map an index aware function over a stream vector
--
-- ==== __Examples__
--
-- @since 0.5.0
simap ::
     forall r ix a b. S.Stream r ix a
  => (ix -> a -> b)
  -> Array r ix a
  -> Vector DS b
simap f = fromSteps . S.map (uncurry f) . S.toStreamIx
{-# INLINE simap #-}


-- | Traverse a stream vector with an applicative function.
--
-- ==== __Examples__
--
-- @since 0.5.0
straverse ::
     forall r ix a b f. (S.Stream r ix a, Applicative f)
  => (a -> f b)
  -> Array r ix a
  -> f (Vector DS b)
straverse f = fmap fromSteps . S.traverse f . S.toStream
{-# INLINE straverse #-}


-- | Traverse a stream vector with an index aware applicative function.
--
-- ==== __Examples__
--
-- @since 0.5.0
sitraverse ::
     forall r ix a b f. (S.Stream r ix a, Applicative f)
  => (ix -> a -> f b)
  -> Array r ix a
  -> f (Vector DS b)
sitraverse f = fmap fromSteps . S.traverse (uncurry f) . S.toStreamIx
{-# INLINE sitraverse #-}


-- | Traverse a stream vector with a monadic function.
--
-- ==== __Examples__
--
-- @since 0.5.0
smapM ::
     forall r ix a b m. (S.Stream r ix a, Monad m)
  => (a -> m b)
  -> Array r ix a
  -> m (Vector DS b)
smapM f = fromStepsM . S.mapM f . S.transStepsId . S.toStream
{-# INLINE smapM #-}

-- | Traverse a stream vector with a monadic index aware function.
--
-- Corresponds to: @mapM (uncurry f) . imap (,) v@
--
-- ==== __Examples__
--
-- @since 0.5.0
simapM ::
     forall r ix a b m. (S.Stream r ix a, Monad m)
  => (ix -> a -> m b)
  -> Array r ix a
  -> m (Vector DS b)
simapM f = fromStepsM . S.mapM (uncurry f) . S.transStepsId . S.toStreamIx
{-# INLINE simapM #-}

-- | Traverse a stream vector with a monadic function, while discarding the result
--
-- ==== __Examples__
--
-- @since 0.5.0
smapM_ ::
     forall r ix a b m. (S.Stream r ix a, Monad m)
  => (a -> m b)
  -> Array r ix a
  -> m ()
smapM_ f = S.mapM_ f . S.transStepsId . S.toStream
{-# INLINE smapM_ #-}

-- | Traverse a stream vector with a monadic index aware function, while discarding the result
--
-- ==== __Examples__
--
-- @since 0.5.0
simapM_ ::
     forall r ix a b m. (S.Stream r ix a, Monad m)
  => (ix -> a -> m b)
  -> Array r ix a
  -> m ()
simapM_ f = S.mapM_ (uncurry f) . S.transStepsId . S.toStreamIx
{-# INLINE simapM_ #-}


-- | Same as `smapM`, but with arguments flipped.
--
-- ==== __Examples__
--
-- @since 0.5.0
sforM ::
     forall r ix a b m. (S.Stream r ix a, Monad m)
  => Array r ix a
  -> (a -> m b)
  -> m (Vector DS b)
sforM = flip smapM
{-# INLINE sforM #-}

-- | Same as `simapM`, but with arguments flipped.
--
-- ==== __Examples__
--
-- @since 0.5.0
siforM ::
     forall r ix a b m. (S.Stream r ix a, Monad m)
  => Array r ix a
  -> (ix -> a -> m b)
  -> m (Vector DS b)
siforM = flip simapM
{-# INLINE siforM #-}

-- | Same as `smapM_`, but with arguments flipped.
--
-- ==== __Examples__
--
-- @since 0.5.0
sforM_ :: (S.Stream r ix a, Monad m) => Array r ix a -> (a -> m b) -> m ()
sforM_ = flip smapM_
{-# INLINE sforM_ #-}

-- | Same as `simapM_`, but with arguments flipped.
--
-- ==== __Examples__
--
-- @since 0.5.0
siforM_ ::
     forall r ix a b m. (S.Stream r ix a, Monad m)
  => Array r ix a
  -> (ix -> a -> m b)
  -> m ()
siforM_ = flip simapM_
{-# INLINE siforM_ #-}



-- | Zip two arrays in a row-major order together together into a flat vector. Resulting
-- length of a vector will be the smallest number of elements of the supplied arrays.
--
-- ==== __Examples__
--
-- @since 0.5.0
szip ::
     forall ra rb a b. (S.Stream ra Ix1 a, S.Stream rb Ix1 b)
  => Vector ra a
  -> Vector rb b
  -> Vector DS (a, b)
szip = szipWith (,)
{-# INLINE szip #-}

-- |
--
-- @since 0.5.0
szip3 ::
     forall ra rb rc a b c. (S.Stream ra Ix1 a, S.Stream rb Ix1 b, S.Stream rc Ix1 c)
  => Vector ra a
  -> Vector rb b
  -> Vector rc c
  -> Vector DS (a, b, c)
szip3 = szipWith3 (,,)
{-# INLINE szip3 #-}

-- |
--
-- @since 0.5.0
szip4 ::
     forall ra rb rc rd a b c d.
     (S.Stream ra Ix1 a, S.Stream rb Ix1 b, S.Stream rc Ix1 c, S.Stream rd Ix1 d)
  => Vector ra a
  -> Vector rb b
  -> Vector rc c
  -> Vector rd d
  -> Vector DS (a, b, c, d)
szip4 = szipWith4 (,,,)
{-# INLINE szip4 #-}

-- |
--
-- @since 0.5.0
szip5 ::
     forall ra rb rc rd re a b c d e.
     (S.Stream ra Ix1 a, S.Stream rb Ix1 b, S.Stream rc Ix1 c, S.Stream rd Ix1 d, S.Stream re Ix1 e)
  => Vector ra a
  -> Vector rb b
  -> Vector rc c
  -> Vector rd d
  -> Vector re e
  -> Vector DS (a, b, c, d, e)
szip5 = szipWith5 (,,,,)
{-# INLINE szip5 #-}

-- |
--
-- @since 0.5.0
szip6 ::
     forall ra rb rc rd re rf a b c d e f.
     ( S.Stream ra Ix1 a
     , S.Stream rb Ix1 b
     , S.Stream rc Ix1 c
     , S.Stream rd Ix1 d
     , S.Stream re Ix1 e
     , S.Stream rf Ix1 f
     )
  => Vector ra a
  -> Vector rb b
  -> Vector rc c
  -> Vector rd d
  -> Vector re e
  -> Vector rf f
  -> Vector DS (a, b, c, d, e, f)
szip6 = szipWith6 (,,,,,)
{-# INLINE szip6 #-}






-- |
--
-- ==== __Examples__
--
-- @since 0.5.0
szipWith ::
     forall ra rb a b c.
     (S.Stream ra Ix1 a, S.Stream rb Ix1 b)
  => (a -> b -> c)
  -> Vector ra a
  -> Vector rb b
  -> Vector DS c
szipWith f v1 v2 = fromSteps $ S.zipWith f (S.toStream v1) (S.toStream v2)
{-# INLINE szipWith #-}

-- |
--
-- @since 0.5.0
szipWith3 ::
     forall ra rb rc a b c d.
     (S.Stream ra Ix1 a, S.Stream rb Ix1 b, S.Stream rc Ix1 c)
  => (a -> b -> c -> d)
  -> Vector ra a
  -> Vector rb b
  -> Vector rc c
  -> Vector DS d
szipWith3 f v1 v2 v3 = fromSteps $ S.zipWith3 f (S.toStream v1) (S.toStream v2) (S.toStream v3)
{-# INLINE szipWith3 #-}

-- |
--
-- @since 0.5.0
szipWith4 ::
     forall ra rb rc rd a b c d e.
     (S.Stream ra Ix1 a, S.Stream rb Ix1 b, S.Stream rc Ix1 c, S.Stream rd Ix1 d)
  => (a -> b -> c -> d -> e)
  -> Vector ra a
  -> Vector rb b
  -> Vector rc c
  -> Vector rd d
  -> Vector DS e
szipWith4 f v1 v2 v3 v4 =
  fromSteps $ S.zipWith4 f (S.toStream v1) (S.toStream v2) (S.toStream v3) (S.toStream v4)
{-# INLINE szipWith4 #-}

-- |
--
-- @since 0.5.0
szipWith5 ::
     forall ra rb rc rd re a b c d e f.
     (S.Stream ra Ix1 a, S.Stream rb Ix1 b, S.Stream rc Ix1 c, S.Stream rd Ix1 d, S.Stream re Ix1 e)
  => (a -> b -> c -> d -> e -> f)
  -> Vector ra a
  -> Vector rb b
  -> Vector rc c
  -> Vector rd d
  -> Vector re e
  -> Vector DS f
szipWith5 f v1 v2 v3 v4 v5 =
  fromSteps $
  S.zipWith5 f (S.toStream v1) (S.toStream v2) (S.toStream v3) (S.toStream v4) (S.toStream v5)
{-# INLINE szipWith5 #-}

-- |
--
-- @since 0.5.0
szipWith6 ::
     forall ra rb rc rd re rf a b c d e f g.
     ( S.Stream ra Ix1 a
     , S.Stream rb Ix1 b
     , S.Stream rc Ix1 c
     , S.Stream rd Ix1 d
     , S.Stream re Ix1 e
     , S.Stream rf Ix1 f
     )
  => (a -> b -> c -> d -> e -> f -> g)
  -> Vector ra a
  -> Vector rb b
  -> Vector rc c
  -> Vector rd d
  -> Vector re e
  -> Vector rf f
  -> Vector DS g
szipWith6 f v1 v2 v3 v4 v5 v6 =
  fromSteps $
  S.zipWith6
    f
    (S.toStream v1)
    (S.toStream v2)
    (S.toStream v3)
    (S.toStream v4)
    (S.toStream v5)
    (S.toStream v6)
{-# INLINE szipWith6 #-}

-- |
--
-- ==== __Examples__
--
-- @since 0.5.0
sizipWith ::
     forall ra rb a b c.
     (S.Stream ra Ix1 a, S.Stream rb Ix1 b)
  => (Ix1 -> a -> b -> c)
  -> Vector ra a
  -> Vector rb b
  -> Vector DS c
sizipWith f v1 v2 = fromSteps $ S.zipWith (uncurry f) (S.toStreamIx v1) (S.toStream v2)
{-# INLINE sizipWith #-}

-- |
--
-- @since 0.5.0
sizipWith3 ::
     forall ra rb rc a b c d.
     (S.Stream ra Ix1 a, S.Stream rb Ix1 b, S.Stream rc Ix1 c)
  => (Ix1 -> a -> b -> c -> d)
  -> Vector ra a
  -> Vector rb b
  -> Vector rc c
  -> Vector DS d
sizipWith3 f v1 v2 v3 =
  fromSteps $ S.zipWith3 (uncurry f) (S.toStreamIx v1) (S.toStream v2) (S.toStream v3)
{-# INLINE sizipWith3 #-}

-- |
--
-- @since 0.5.0
sizipWith4 ::
     forall ra rb rc rd a b c d e.
     (S.Stream ra Ix1 a, S.Stream rb Ix1 b, S.Stream rc Ix1 c, S.Stream rd Ix1 d)
  => (Ix1 -> a -> b -> c -> d -> e)
  -> Vector ra a
  -> Vector rb b
  -> Vector rc c
  -> Vector rd d
  -> Vector DS e
sizipWith4 f v1 v2 v3 v4 =
  fromSteps $
  S.zipWith4 (uncurry f) (S.toStreamIx v1) (S.toStream v2) (S.toStream v3) (S.toStream v4)
{-# INLINE sizipWith4 #-}

-- |
--
-- @since 0.5.0
sizipWith5 ::
     forall ra rb rc rd re a b c d e f.
     (S.Stream ra Ix1 a, S.Stream rb Ix1 b, S.Stream rc Ix1 c, S.Stream rd Ix1 d, S.Stream re Ix1 e)
  => (Ix1 -> a -> b -> c -> d -> e -> f)
  -> Vector ra a
  -> Vector rb b
  -> Vector rc c
  -> Vector rd d
  -> Vector re e
  -> Vector DS f
sizipWith5 f v1 v2 v3 v4 v5 =
  fromSteps $
  S.zipWith5
    (uncurry f)
    (S.toStreamIx v1)
    (S.toStream v2)
    (S.toStream v3)
    (S.toStream v4)
    (S.toStream v5)
{-# INLINE sizipWith5 #-}

-- |
--
-- @since 0.5.0
sizipWith6 ::
     forall ra rb rc rd re rf a b c d e f g.
     ( S.Stream ra Ix1 a
     , S.Stream rb Ix1 b
     , S.Stream rc Ix1 c
     , S.Stream rd Ix1 d
     , S.Stream re Ix1 e
     , S.Stream rf Ix1 f
     )
  => (Ix1 -> a -> b -> c -> d -> e -> f -> g)
  -> Vector ra a
  -> Vector rb b
  -> Vector rc c
  -> Vector rd d
  -> Vector re e
  -> Vector rf f
  -> Vector DS g
sizipWith6 f v1 v2 v3 v4 v5 v6 =
  fromSteps $
  S.zipWith6
    (uncurry f)
    (S.toStreamIx v1)
    (S.toStream v2)
    (S.toStream v3)
    (S.toStream v4)
    (S.toStream v5)
    (S.toStream v6)
{-# INLINE sizipWith6 #-}


-- |
--
-- ==== __Examples__
--
-- @since 0.5.0
szipWithM ::
     forall ra rb a b c m.
     (S.Stream ra Ix1 a, S.Stream rb Ix1 b, Monad m)
  => (a -> b -> m c)
  -> Vector ra a
  -> Vector rb b
  -> m (Vector DS c)
szipWithM f v1 v2 = fromStepsM $ S.zipWithM f (toStreamM v1) (toStreamM v2)
{-# INLINE szipWithM #-}

-- |
--
-- @since 0.5.0
szipWith3M ::
     forall ra rb rc a b c d m.
     (S.Stream ra Ix1 a, S.Stream rb Ix1 b, S.Stream rc Ix1 c, Monad m)
  => (a -> b -> c -> m d)
  -> Vector ra a
  -> Vector rb b
  -> Vector rc c
  -> m (Vector DS d)
szipWith3M f v1 v2 v3 = fromStepsM $ S.zipWith3M f (toStreamM v1) (toStreamM v2) (toStreamM v3)
{-# INLINE szipWith3M #-}

-- |
--
-- @since 0.5.0
szipWith4M ::
     forall ra rb rc rd a b c d e m.
     (S.Stream ra Ix1 a, S.Stream rb Ix1 b, S.Stream rc Ix1 c, S.Stream rd Ix1 d, Monad m)
  => (a -> b -> c -> d -> m e)
  -> Vector ra a
  -> Vector rb b
  -> Vector rc c
  -> Vector rd d
  -> m (Vector DS e)
szipWith4M f v1 v2 v3 v4 =
  fromStepsM $ S.zipWith4M f (toStreamM v1) (toStreamM v2) (toStreamM v3) (toStreamM v4)
{-# INLINE szipWith4M #-}

-- |
--
-- @since 0.5.0
szipWith5M ::
     forall ra rb rc rd re a b c d e f m.
     ( S.Stream ra Ix1 a
     , S.Stream rb Ix1 b
     , S.Stream rc Ix1 c
     , S.Stream rd Ix1 d
     , S.Stream re Ix1 e
     , Monad m
     )
  => (a -> b -> c -> d -> e -> m f)
  -> Vector ra a
  -> Vector rb b
  -> Vector rc c
  -> Vector rd d
  -> Vector re e
  -> m (Vector DS f)
szipWith5M f v1 v2 v3 v4 v5 =
  fromStepsM $
  S.zipWith5M f (toStreamM v1) (toStreamM v2) (toStreamM v3) (toStreamM v4) (toStreamM v5)
{-# INLINE szipWith5M #-}

-- |
--
-- @since 0.5.0
szipWith6M ::
     forall ra rb rc rd re rf a b c d e f g m.
     ( S.Stream ra Ix1 a
     , S.Stream rb Ix1 b
     , S.Stream rc Ix1 c
     , S.Stream rd Ix1 d
     , S.Stream re Ix1 e
     , S.Stream rf Ix1 f
     , Monad m
     )
  => (a -> b -> c -> d -> e -> f -> m g)
  -> Vector ra a
  -> Vector rb b
  -> Vector rc c
  -> Vector rd d
  -> Vector re e
  -> Vector rf f
  -> m (Vector DS g)
szipWith6M f v1 v2 v3 v4 v5 v6 =
  fromStepsM $
  S.zipWith6M
    f
    (toStreamM v1)
    (toStreamM v2)
    (toStreamM v3)
    (toStreamM v4)
    (toStreamM v5)
    (toStreamM v6)
{-# INLINE szipWith6M #-}


-- |
--
-- ==== __Examples__
--
-- @since 0.5.0
sizipWithM ::
     forall ra rb a b c m.
     (S.Stream ra Ix1 a, S.Stream rb Ix1 b, Monad m)
  => (Ix1 -> a -> b -> m c)
  -> Vector ra a
  -> Vector rb b
  -> m (Vector DS c)
sizipWithM f v1 v2 = fromStepsM $ S.zipWithM (uncurry f) (toStreamIxM v1) (toStreamM v2)
{-# INLINE sizipWithM #-}


-- |
--
-- @since 0.5.0
sizipWith3M ::
     forall ra rb rc a b c d m.
     (S.Stream ra Ix1 a, S.Stream rb Ix1 b, S.Stream rc Ix1 c, Monad m)
  => (Ix1 -> a -> b -> c -> m d)
  -> Vector ra a
  -> Vector rb b
  -> Vector rc c
  -> m (Vector DS d)
sizipWith3M f v1 v2 v3 =
  fromStepsM $ S.zipWith3M (uncurry f) (toStreamIxM v1) (toStreamM v2) (toStreamM v3)
{-# INLINE sizipWith3M #-}

-- |
--
-- @since 0.5.0
sizipWith4M ::
     forall ra rb rc rd a b c d e m.
     (S.Stream ra Ix1 a, S.Stream rb Ix1 b, S.Stream rc Ix1 c, S.Stream rd Ix1 d, Monad m)
  => (Ix1 -> a -> b -> c -> d -> m e)
  -> Vector ra a
  -> Vector rb b
  -> Vector rc c
  -> Vector rd d
  -> m (Vector DS e)
sizipWith4M f v1 v2 v3 v4 =
  fromStepsM $
  S.zipWith4M (uncurry f) (toStreamIxM v1) (toStreamM v2) (toStreamM v3) (toStreamM v4)
{-# INLINE sizipWith4M #-}

-- |
--
-- @since 0.5.0
sizipWith5M ::
     forall ra rb rc rd re a b c d e f m.
     ( S.Stream ra Ix1 a
     , S.Stream rb Ix1 b
     , S.Stream rc Ix1 c
     , S.Stream rd Ix1 d
     , S.Stream re Ix1 e
     , Monad m
     )
  => (Ix1 -> a -> b -> c -> d -> e -> m f)
  -> Vector ra a
  -> Vector rb b
  -> Vector rc c
  -> Vector rd d
  -> Vector re e
  -> m (Vector DS f)
sizipWith5M f v1 v2 v3 v4 v5 =
  fromStepsM $
  S.zipWith5M
    (uncurry f)
    (toStreamIxM v1)
    (toStreamM v2)
    (toStreamM v3)
    (toStreamM v4)
    (toStreamM v5)
{-# INLINE sizipWith5M #-}

-- |
--
-- ==== __Examples__
--
-- @since 0.5.0
sizipWith6M ::
     forall ra rb rc rd re rf a b c d e f g m.
     ( S.Stream ra Ix1 a
     , S.Stream rb Ix1 b
     , S.Stream rc Ix1 c
     , S.Stream rd Ix1 d
     , S.Stream re Ix1 e
     , S.Stream rf Ix1 f
     , Monad m
     )
  => (Ix1 -> a -> b -> c -> d -> e -> f -> m g)
  -> Vector ra a
  -> Vector rb b
  -> Vector rc c
  -> Vector rd d
  -> Vector re e
  -> Vector rf f
  -> m (Vector DS g)
sizipWith6M f v1 v2 v3 v4 v5 v6 =
  fromStepsM $
  S.zipWith6M
    (uncurry f)
    (toStreamIxM v1)
    (toStreamM v2)
    (toStreamM v3)
    (toStreamM v4)
    (toStreamM v5)
    (toStreamM v6)
{-# INLINE sizipWith6M #-}


-- |
--
-- ==== __Examples__
--
-- @since 0.5.0
szipWithM_ ::
     forall ra rb a b c m. (S.Stream ra Ix1 a, S.Stream rb Ix1 b, Monad m)
  => (a -> b -> m c)
  -> Vector ra a
  -> Vector rb b
  -> m ()
szipWithM_ f v1 v2 = S.zipWithM_ f (toStreamM v1) (toStreamM v2)
{-# INLINE szipWithM_ #-}

-- |
--
-- @since 0.5.0
szipWith3M_ ::
     forall ra rb rc a b c d m.
     (S.Stream ra Ix1 a, S.Stream rb Ix1 b, S.Stream rc Ix1 c, Monad m)
  => (a -> b -> c -> m d)
  -> Vector ra a
  -> Vector rb b
  -> Vector rc c
  -> m ()
szipWith3M_ f v1 v2 v3 = S.zipWith3M_ f (toStreamM v1) (toStreamM v2) (toStreamM v3)
{-# INLINE szipWith3M_ #-}

-- |
--
-- @since 0.5.0
szipWith4M_ ::
     forall ra rb rc rd a b c d e m.
     (S.Stream ra Ix1 a, S.Stream rb Ix1 b, S.Stream rc Ix1 c, S.Stream rd Ix1 d, Monad m)
  => (a -> b -> c -> d -> m e)
  -> Vector ra a
  -> Vector rb b
  -> Vector rc c
  -> Vector rd d
  -> m ()
szipWith4M_ f v1 v2 v3 v4 =
  S.zipWith4M_ f (toStreamM v1) (toStreamM v2) (toStreamM v3) (toStreamM v4)
{-# INLINE szipWith4M_ #-}

-- |
--
-- @since 0.5.0
szipWith5M_ ::
     forall ra rb rc rd re a b c d e f m.
     ( S.Stream ra Ix1 a
     , S.Stream rb Ix1 b
     , S.Stream rc Ix1 c
     , S.Stream rd Ix1 d
     , S.Stream re Ix1 e
     , Monad m
     )
  => (a -> b -> c -> d -> e -> m f)
  -> Vector ra a
  -> Vector rb b
  -> Vector rc c
  -> Vector rd d
  -> Vector re e
  -> m ()
szipWith5M_ f v1 v2 v3 v4 v5 =
  S.zipWith5M_ f (toStreamM v1) (toStreamM v2) (toStreamM v3) (toStreamM v4) (toStreamM v5)
{-# INLINE szipWith5M_ #-}

-- |
--
-- @since 0.5.0
szipWith6M_ ::
     forall ra rb rc rd re rf a b c d e f g m.
     ( S.Stream ra Ix1 a
     , S.Stream rb Ix1 b
     , S.Stream rc Ix1 c
     , S.Stream rd Ix1 d
     , S.Stream re Ix1 e
     , S.Stream rf Ix1 f
     , Monad m
     )
  => (a -> b -> c -> d -> e -> f -> m g)
  -> Vector ra a
  -> Vector rb b
  -> Vector rc c
  -> Vector rd d
  -> Vector re e
  -> Vector rf f
  -> m ()
szipWith6M_ f v1 v2 v3 v4 v5 v6 =
  S.zipWith6M_
    f
    (toStreamM v1)
    (toStreamM v2)
    (toStreamM v3)
    (toStreamM v4)
    (toStreamM v5)
    (toStreamM v6)
{-# INLINE szipWith6M_ #-}




-- |
--
-- ==== __Examples__
--
-- @since 0.5.0
sizipWithM_ ::
     forall ra rb a b c m. (S.Stream ra Ix1 a, S.Stream rb Ix1 b, Monad m)
  => (Ix1 -> a -> b -> m c)
  -> Vector ra a
  -> Vector rb b
  -> m ()
sizipWithM_ f v1 v2 = S.zipWithM_ (uncurry f) (toStreamIxM v1) (toStreamM v2)
{-# INLINE sizipWithM_ #-}


-- |
--
-- @since 0.5.0
sizipWith3M_ ::
     forall ra rb rc a b c d m.
     (S.Stream ra Ix1 a, S.Stream rb Ix1 b, S.Stream rc Ix1 c, Monad m)
  => (Ix1 -> a -> b -> c -> m d)
  -> Vector ra a
  -> Vector rb b
  -> Vector rc c
  -> m ()
sizipWith3M_ f v1 v2 v3 = S.zipWith3M_ (uncurry f) (toStreamIxM v1) (toStreamM v2) (toStreamM v3)
{-# INLINE sizipWith3M_ #-}

-- |
--
-- @since 0.5.0
sizipWith4M_ ::
     forall ra rb rc rd a b c d e m.
     (S.Stream ra Ix1 a, S.Stream rb Ix1 b, S.Stream rc Ix1 c, S.Stream rd Ix1 d, Monad m)
  => (Ix1 -> a -> b -> c -> d -> m e)
  -> Vector ra a
  -> Vector rb b
  -> Vector rc c
  -> Vector rd d
  -> m ()
sizipWith4M_ f v1 v2 v3 v4 =
  S.zipWith4M_ (uncurry f) (toStreamIxM v1) (toStreamM v2) (toStreamM v3) (toStreamM v4)
{-# INLINE sizipWith4M_ #-}

-- |
--
-- @since 0.5.0
sizipWith5M_ ::
     forall ra rb rc rd re a b c d e f m.
     ( S.Stream ra Ix1 a
     , S.Stream rb Ix1 b
     , S.Stream rc Ix1 c
     , S.Stream rd Ix1 d
     , S.Stream re Ix1 e
     , Monad m
     )
  => (Ix1 -> a -> b -> c -> d -> e -> m f)
  -> Vector ra a
  -> Vector rb b
  -> Vector rc c
  -> Vector rd d
  -> Vector re e
  -> m ()
sizipWith5M_ f v1 v2 v3 v4 v5 =
  S.zipWith5M_
    (uncurry f)
    (toStreamIxM v1)
    (toStreamM v2)
    (toStreamM v3)
    (toStreamM v4)
    (toStreamM v5)
{-# INLINE sizipWith5M_ #-}

-- |
--
-- @since 0.5.0
sizipWith6M_ ::
     forall ra rb rc rd re rf a b c d e f g m.
     ( S.Stream ra Ix1 a
     , S.Stream rb Ix1 b
     , S.Stream rc Ix1 c
     , S.Stream rd Ix1 d
     , S.Stream re Ix1 e
     , S.Stream rf Ix1 f
     , Monad m
     )
  => (Ix1 -> a -> b -> c -> d -> e -> f -> m g)
  -> Vector ra a
  -> Vector rb b
  -> Vector rc c
  -> Vector rd d
  -> Vector re e
  -> Vector rf f
  -> m ()
sizipWith6M_ f v1 v2 v3 v4 v5 v6 =
  S.zipWith6M_
    (uncurry f)
    (toStreamIxM v1)
    (toStreamM v2)
    (toStreamM v3)
    (toStreamM v4)
    (toStreamM v5)
    (toStreamM v6)
{-# INLINE sizipWith6M_ #-}





-- | Strict left fold sequentially over a streamed array.
--
-- ==== __Examples__
--
-- @since 0.5.0
sfoldl ::
     forall r ix e a. Stream r ix e
  => (a -> e -> a)
  -> a
  -> Array r ix e
  -> a
sfoldl f acc = S.unId . S.foldl f acc . toStream
{-# INLINE sfoldl #-}

-- |
--
-- ==== __Examples__
--
-- @since 0.5.0
sfoldlM ::
     forall r ix e a m. (Stream r ix e, Monad m)
  => (a -> e -> m a)
  -> a
  -> Array r ix e
  -> m a
sfoldlM f acc = S.foldlM f acc . S.transStepsId . toStream
{-# INLINE sfoldlM #-}

-- |
--
-- ==== __Examples__
--
-- @since 0.5.0
sfoldlM_ ::
     forall r ix e a m. (Stream r ix e, Monad m)
  => (a -> e -> m a)
  -> a
  -> Array r ix e
  -> m ()
sfoldlM_ f acc = void . sfoldlM f acc
{-# INLINE sfoldlM_ #-}


-- |
--
-- ==== __Examples__
--
-- @since 0.5.0
sfoldl1' ::
     forall r ix e. (HasCallStack, Stream r ix e)
  => (e -> e -> e)
  -> Array r ix e
  -> e
sfoldl1' f = throwEither . sfoldl1M (\e -> pure . f e)
{-# INLINE sfoldl1' #-}

-- |
--
-- ==== __Examples__
--
-- @since 0.5.0
sfoldl1M ::
     forall r ix e m. (Stream r ix e, MonadThrow m)
  => (e -> e -> m e)
  -> Array r ix e
  -> m e
sfoldl1M f arr = do
  let str = S.transStepsId $ toStream arr
  isNullStream <- S.null str
  when isNullStream $ throwM $ SizeEmptyException (outerSize arr)
  S.foldl1M f str
{-# INLINE sfoldl1M #-}

-- |
--
-- ==== __Examples__
--
-- @since 0.5.0
sfoldl1M_ ::
     forall r ix e m. (Stream r ix e, MonadThrow m)
  => (e -> e -> m e)
  -> Array r ix e
  -> m ()
sfoldl1M_ f = void . sfoldl1M f
{-# INLINE sfoldl1M_ #-}



-- |
--
-- ==== __Examples__
--
-- @since 0.5.0
sifoldl ::
     forall r ix e a. Stream r ix e
  => (a -> ix -> e -> a)
  -> a
  -> Array r ix e
  -> a
sifoldl f acc = S.unId . S.foldl (\a (ix, e) -> f a ix e) acc . toStreamIx
{-# INLINE sifoldl #-}

-- |
--
-- ==== __Examples__
--
-- @since 0.5.0
sifoldlM ::
     forall r ix e a m. (Stream r ix e, Monad m)
  => (a -> ix -> e -> m a)
  -> a
  -> Array r ix e
  -> m a
sifoldlM f acc = S.foldlM (\a (ix, e) -> f a ix e) acc . S.transStepsId . toStreamIx
{-# INLINE sifoldlM #-}

-- |
--
-- ==== __Examples__
--
-- @since 0.5.0
sifoldlM_ ::
     forall r ix e a m. (Stream r ix e, Monad m)
  => (a -> ix -> e -> m a)
  -> a
  -> Array r ix e
  -> m ()
sifoldlM_ f acc = void . sifoldlM f acc
{-# INLINE sifoldlM_ #-}


-- |
--
-- ==== __Examples__
--
-- @since 0.5.0
sor ::
     forall r ix. Stream r ix Bool
  => Array r ix Bool
  -> Bool
sor = S.unId . S.or . toStream
{-# INLINE sor #-}


-- |
--
-- ==== __Examples__
--
-- @since 0.5.0
sand :: forall r ix. Stream r ix Bool => Array r ix Bool -> Bool
sand = S.unId . S.and . toStream
{-# INLINE sand #-}


-- |
--
-- ==== __Examples__
--
-- @since 0.5.0
sany :: forall r ix e. Stream r ix e => (e -> Bool) -> Array r ix e -> Bool
sany f = S.unId . S.or . S.map f . toStream
{-# INLINE sany #-}


-- |
--
-- ==== __Examples__
--
-- @since 0.5.0
sall :: forall r ix e. Stream r ix e => (e -> Bool) -> Array r ix e -> Bool
sall f = S.unId . S.and . S.map f . toStream
{-# INLINE sall #-}



-- | Add all elements of the array together
--
-- /Related/: `sum`.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Vector as V
-- >>> V.ssum $ V.sfromList [10, 3, 70, 5 :: Int]
-- 88
--
-- @since 0.5.0
ssum :: forall r ix e. (Num e, Stream r ix e) => Array r ix e -> e
ssum = sfoldl (+) 0
{-# INLINE ssum #-}

-- | Multiply all elements of the array together
--
-- /Related/: `product`.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Vector as V
-- >>> V.sproduct $ V.sfromList [10, 3, 70, 5 :: Int]
-- 10500
--
-- @since 0.5.0
sproduct :: forall r ix e. (Num e, Stream r ix e) => Array r ix e -> e
sproduct = sfoldl (*) 1
{-# INLINE sproduct #-}


-- | /O(n)/ - Find the largest value in the array. Throws an error on empty.
--
-- /Related/: `smaximumM`, `maximum`, `maximumM`.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Vector as V
-- >>> V.smaximum' $ V.sfromList [10, 3, 70, 5 :: Int]
-- 70
--
-- @since 0.5.0
smaximum' :: forall r ix e. (HasCallStack, Ord e, Stream r ix e) => Array r ix e -> e
smaximum' = sfoldl1' max
{-# INLINE smaximum' #-}

-- | /O(n)/ - Find the largest value in the array.
--
-- /Related/: `smaximum`, `maximum`, `maximumM`.
--
-- /__Throws Exceptions__/: `SizeEmptyException` when array is empty
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Vector as V
-- >>> V.smaximumM $ V.sfromList [10, 3, 70, 5 :: Int]
-- 70
-- >>> V.smaximumM (V.empty :: Vector D Int) :: Maybe Int
-- Nothing
--
-- @since 0.5.0
smaximumM :: forall r ix e m. (Ord e, Stream r ix e, MonadThrow m) => Array r ix e -> m e
smaximumM = sfoldl1M (\e acc -> pure (max e acc))
{-# INLINE smaximumM #-}


-- | /O(n)/ - Find the smallest value in the array. Throws an error on empty.
--
-- /Related/: `sminimumM`, `minimum`, `minimumM`.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Vector as V
-- >>> V.sminimum' $ V.sfromList [10, 3, 70, 5 :: Int]
-- 3
--
-- @since 0.5.0
sminimum' :: forall r ix e. (HasCallStack, Ord e, Stream r ix e) => Array r ix e -> e
sminimum' = sfoldl1' min
{-# INLINE sminimum' #-}

-- | /O(n)/ - Find the smallest value in the array.
--
-- /Related/: `sminimum'`, `minimum`, `minimumM`.
--
-- /__Throws Exceptions__/: `SizeEmptyException` when array is empty
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Vector as V
-- >>> V.sminimumM $ V.sfromList [10, 3, 70, 5 :: Int]
-- 3
-- >>> V.sminimumM (V.empty :: Array D Ix2 Int) :: Maybe Int
-- Nothing
--
-- @since 0.5.0
sminimumM :: forall r ix e m. (Ord e, Stream r ix e, MonadThrow m) => Array r ix e -> m e
sminimumM = sfoldl1M (\e acc -> pure (min e acc))
{-# INLINE sminimumM #-}
