{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Data.Massiv.Vector
-- Copyright   : (c) Alexey Kuleshevich 2020
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
  , length
  , snull
  -- *** Indexing
  , (!)
  , (!?)
  , head'
  , shead'
  , last'
  -- *** Monadic Indexing
  , indexM
  , headM
  , lastM
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
  , stake
  -- *** Drop
  , drop
  , drop'
  , dropM
  , sdrop
  -- * Construction
  -- ** Initialization
  , empty
  , sempty
  , singleton
  , ssingleton
  , A.replicate
  , sreplicate
  , generate
  , sgenerate
  -- , iterateN
  -- , iiterateN
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
  -- -- ** Enumeration
  , senumFromN
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
  -- -- ** Mutable updates
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
  -- -- ** Zipping
  -- , szipWith
  -- , szipWith2
  -- , szipWith3
  -- , szipWith4
  -- , szipWith5
  -- , szipWith6
  -- , sizipWith
  -- , sizipWith2
  -- , sizipWith3
  -- , sizipWith4
  -- , sizipWith5
  -- , sizipWith6
  -- -- ** Monadic zipping
  -- , szipWithM
  -- , szipWithM_
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
  -- , takeWhile
  -- , dropWhile
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
  -- , findIndex
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
  , sfromList
  , sfromListN
  -- -- ** Other vector types
  -- , convert
  -- -- ** Mutable vectors
  -- , freeze
  -- , thaw
  -- , copy
  -- , unsafeFreeze
  -- , unsafeThaw
  -- , unsafeCopy
  -- * Deprecated
  , takeS
  , dropS
  , unfoldr
  , unfoldrN
  , filterS
  , ifilterS
  , filterM
  , ifilterM
  , mapMaybeS
  , imapMaybeS
  , mapMaybeM
  , imapMaybeM
  , catMaybesS
  , traverseS
  ) where

import Control.Monad hiding (filterM, replicateM)
import Data.Coerce
import Data.Massiv.Array.Delayed.Pull
import Data.Massiv.Array.Delayed.Stream
import qualified Data.Massiv.Array.Ops.Construct as A (makeArrayR, replicate)
import Data.Massiv.Core.Common
import qualified Data.Massiv.Vector.Stream as S
import Data.Massiv.Vector.Unsafe
import Prelude hiding (drop, init, length, null, replicate, splitAt, tail, take)

-- ========= --
-- Accessors --
-- ========= --


------------------------
-- Length information --
------------------------

-- |
--
-- /Note/ - calling this function on the `DS` type of vector might result in the whole
-- stream being traversed.
--
-- @since 0.5.0
length :: Load r Ix1 e => Vector r e -> Sz1
length = size
{-# INLINE length #-}

-- |
--
-- @since 0.5.0
snull :: Stream r ix e => Array r ix e -> Bool
snull = S.unId . S.null . toStream
{-# INLINE snull #-}

--------------
-- Indexing --
--------------

-- TODO: Add to vector: headMaybe

-- |
--
-- @since 0.5.0
head' :: Source r Ix1 e => Vector r e -> e
head' = (`evaluate'` 0)
{-# INLINE head' #-}

-- |
--
-- @since 0.5.0
shead' :: Stream r Ix1 e => Vector r e -> e
shead' = either throw id . sheadM
{-# INLINE shead' #-}

-- |
--
-- @since 0.5.0
sheadM :: (Stream r Ix1 e, MonadThrow m) => Vector r e -> m e
sheadM v =
  case S.unId (S.headMaybe (toStream v)) of
    Nothing -> throwM $ SizeEmptyException (size v)
    Just e -> pure e
{-# INLINE sheadM #-}

-- |
--
-- @since 0.5.0
last' :: Source r Ix1 e => Vector r e -> e
last' v = evaluate' v (max 0 (unSz (size v) - 1))
{-# INLINE last' #-}


----------------------
-- Monadic indexing --
----------------------


-- |
--
-- @since 0.5.0
headM :: (Source r Ix1 e, MonadThrow m) => Vector r e -> m e
headM v
  | isEmpty v = throwM $ SizeEmptyException (size v)
  | otherwise = pure $ unsafeLinearIndex v 0
{-# INLINE headM #-}

-- |
--
-- @since 0.5.0
lastM :: (Source r Ix1 e, MonadThrow m) => Vector r e -> m e
lastM v
  | k == 0 = throwM $ SizeEmptyException (size v)
  | otherwise = pure $ unsafeLinearIndex v (k - 1)
  where k = unSz (size v)
{-# INLINE lastM #-}


-- |
--
-- @since 0.5.0
slice :: Source r Ix1 e => Ix1 -> Sz1 -> Vector r e -> Vector r e
slice !i (Sz k) v = unsafeLinearSlice i' newSz v
  where
    !i' = min n (max 0 i)
    !newSz = SafeSz (min (n - i') k)
    Sz n = size v
{-# INLINE slice #-}

-- |
--
-- @since 0.5.0
sslice :: Stream r Ix1 e => Ix1 -> Sz1 -> Vector r e -> Vector DS e
sslice !i (Sz k) = fromSteps . S.slice i k . S.toStream
{-# INLINE sslice #-}


-- |
--
-- @since 0.5.0
slice' :: Source r Ix1 e => Ix1 -> Sz1 -> Vector r e -> Vector r e
slice' i k = either throw id . sliceM i k
{-# INLINE slice' #-}

-- |
--
-- @since 0.5.0
sliceM :: (Source r Ix1 e, MonadThrow m) => Ix1 -> Sz1 -> Vector r e -> m (Vector r e)
sliceM i newSz@(Sz k) v
  | i >= 0 && k <= n - i = pure $ unsafeLinearSlice i newSz v
  | otherwise = throwM $ SizeSubregionException sz i newSz
  where
    sz@(Sz n) = size v
{-# INLINE sliceM #-}

-- |
--
-- @since 0.5.0
init :: Source r Ix1 e => Vector r e -> Vector r e
init v = unsafeLinearSlice 0 (Sz (coerce (size v) - 1)) v
{-# INLINE init #-}

-- |
--
-- @since 0.5.0
init' :: Source r Ix1 e => Vector r e -> Vector r e
init' = either throw id . initM
{-# INLINE init' #-}

-- |
--
-- @since 0.5.0
initM :: (Source r Ix1 e, MonadThrow m) => Vector r e -> m (Vector r e)
initM v = do
  when (isEmpty v) $ throwM $ SizeEmptyException $ size v
  pure $ unsafeInit v
{-# INLINE initM #-}



-- |
--
-- @since 0.5.0
tail :: Source r Ix1 e => Vector r e -> Vector r e
tail = drop 1
{-# INLINE tail #-}


-- |
--
-- @since 0.5.0
tail' :: Source r Ix1 e => Vector r e -> Vector r e
tail' = either throw id . tailM
{-# INLINE tail' #-}


-- |
--
-- @since 0.5.0
tailM :: (Source r Ix1 e, MonadThrow m) => Vector r e -> m (Vector r e)
tailM v = do
  when (isEmpty v) $ throwM $ SizeEmptyException $ size v
  pure $ unsafeTail v
{-# INLINE tailM #-}



-- |
--
-- @since 0.5.0
take :: Source r Ix1 e => Sz1 -> Vector r e -> Vector r e
take k = fst . sliceAt k
{-# INLINE take #-}

-- |
--
-- @since 0.5.0
take' :: Source r Ix1 e => Sz1 -> Vector r e -> Vector r e
take' k = either throw id . takeM k
{-# INLINE take' #-}

-- |
--
-- @since 0.5.0
takeM :: (Source r Ix1 e, MonadThrow m) => Sz1 -> Vector r e -> m (Vector r e)
takeM k v = do
  let sz = size v
  when (k > sz) $ throwM $ SizeSubregionException sz 0 k
  pure $ unsafeTake k v
{-# INLINE takeM #-}

-- | Extract first @n@ elements from the delayed stream vector
--
-- @since 0.5.0
stake :: Stream r ix e => Sz1 -> Array r ix e -> Array DS Ix1 e
stake n = fromSteps . S.take (unSz n) . S.toStream
{-# INLINE stake #-}

-- |
--
-- @since 0.5.0
drop :: Source r Ix1 e => Sz1 -> Vector r e -> Vector r e
drop k = snd . sliceAt k
{-# INLINE drop #-}

-- | Keep all but the first @n@ elements from the delayed stream vector.
--
-- @since 0.5.0
sdrop :: Stream r ix e => Sz1 -> Array r ix e -> Array DS Ix1 e
sdrop n = fromSteps . S.drop (unSz n) . S.toStream
{-# INLINE sdrop #-}

-- |
--
-- @since 0.5.0
drop' :: Source r Ix1 e => Sz1 -> Vector r e -> Vector r e
drop' k = either throw id . dropM k
{-# INLINE drop' #-}

-- |
--
-- @since 0.5.0
dropM :: (Source r Ix1 e, MonadThrow m) => Sz1 -> Vector r e -> m (Vector r e)
dropM k@(Sz d) v = do
  let sz@(Sz n) = size v
  when (k > sz) $ throwM $ SizeSubregionException sz d (sz - k)
  pure $ unsafeLinearSlice d (SafeSz (n - d)) v
{-# INLINE dropM #-}


-- |
--
--
--
-- @since 0.5.0
sliceAt :: Source r Ix1 e => Sz1 -> Vector r e -> (Vector r e, Vector r e)
sliceAt (Sz k) v = (unsafeTake d v, unsafeDrop d v)
  where
    !n = coerce (size v)
    !d = SafeSz (min k n)
{-# INLINE sliceAt #-}

-- |
--
-- @since 0.5.0
sliceAt' :: Source r Ix1 e => Sz1 -> Vector r e -> (Vector r e, Vector r e)
sliceAt' k = either throw id . sliceAtM k
{-# INLINE sliceAt' #-}

-- |
--
-- @since 0.5.0
sliceAtM :: (Source r Ix1 e, MonadThrow m) => Sz1 -> Vector r e -> m (Vector r e, Vector r e)
sliceAtM k v = do
  l <- takeM k v
  pure (l, unsafeDrop k v)
{-# INLINE sliceAtM #-}


-- |
--
-- @since 0.5.0
sempty :: Vector DS e
sempty = DSArray S.empty
{-# INLINE sempty #-}

-- |
--
-- @since 0.5.0
ssingleton :: e -> Vector DS e
ssingleton = DSArray . S.singleton
{-# INLINE ssingleton #-}

-- | Replicate the same element
--
-- @since 0.5.0
sreplicate :: Sz1 -> e -> Vector DS e
sreplicate (Sz n) = DSArray . S.replicate n
{-# INLINE sreplicate #-}

-- |
--
-- @since 0.5.0
generate :: Comp -> Sz1 -> (Ix1 -> e) -> Vector D e
generate = makeArrayLinear
{-# INLINE generate #-}

-- |
--
-- @since 0.5.0
sgenerate :: Sz1 -> (Ix1 -> e) -> Vector DS e
sgenerate (Sz n) = DSArray . S.generate n
{-# INLINE sgenerate #-}


-- |
--
-- @since 0.5.0
siterateN :: Sz1 -> (e -> e) -> e -> Vector DS e
siterateN n f a = fromSteps $ S.iterateN (unSz n) f a
{-# INLINE siterateN #-}


-- |
--
-- @since 0.5.0
sreplicateM :: Monad m => Sz1 -> m e -> m (Vector DS e)
sreplicateM n f = fromStepsM $ S.replicateM (unSz n) f
{-# INLINE sreplicateM #-}


-- |
--
-- @since 0.5.0
sgenerateM :: Monad m => Sz1 -> (Ix1 -> m e) -> m (Vector DS e)
sgenerateM n f = fromStepsM $ S.generateM (unSz n) f
{-# INLINE sgenerateM #-}


-- |
--
-- @since 0.5.0
siterateNM :: Monad m => Sz1 -> (e -> m e) -> e -> m (Vector DS e)
siterateNM n f a = fromStepsM $ S.iterateNM (unSz n) f a
{-# INLINE siterateNM #-}




-- | Right unfolding function. Useful when it is unknown ahead of time on how many
-- elements the vector will have.
--
-- ====__Example__
--
-- >>> import Data.Massiv.Array as A
-- >>> sunfoldr (\i -> if i < 9 then Just (i * i, i + 1) else Nothing) (0 :: Int)
-- Array DS Seq (Sz1 9)
--   [ 0, 1, 4, 9, 16, 25, 36, 49, 64 ]
--
-- @since 0.5.0
sunfoldr :: (s -> Maybe (e, s)) -> s -> Vector DS e
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
     Sz1
  -- ^ @n@ - maximum number of elements that the vector will have
  -> (s -> Maybe (e, s))
  -- ^ Unfolding function. Stops when `Nothing` is returned or maximum number of elements
  -- is reached.
  -> s -- ^ Inititial element.
  -> Vector DS e
sunfoldrN (Sz n) f = DSArray . S.unfoldrN n f
{-# INLINE sunfoldrN #-}

-- |
--
-- @since 0.5.0
sunfoldrM :: Monad m => (s -> m (Maybe (e, s))) -> s -> m (Vector DS e)
sunfoldrM f = fromStepsM . S.unfoldrM f
{-# INLINE sunfoldrM #-}

-- |
--
-- @since 0.5.0
sunfoldrNM :: Monad m => Sz1 -> (s -> m (Maybe (e, s))) -> s -> m (Vector DS e)
sunfoldrNM (Sz n) f = fromStepsM . S.unfoldrNM n f
{-# INLINE sunfoldrNM #-}


-- |
--
-- @since 0.5.0
sunfoldrExactN :: Sz1 -> (s -> (e, s)) -> s -> Vector DS e
sunfoldrExactN (Sz n) f = fromSteps . S.unfoldrExactN n f
{-# INLINE sunfoldrExactN #-}

-- |
--
-- @since 0.5.0
sunfoldrExactNM :: Monad m => Sz1 -> (s -> m (e, s)) -> s -> m (Vector DS e)
sunfoldrExactNM (Sz n) f = fromStepsM . S.unfoldrExactNM n f
{-# INLINE sunfoldrExactNM #-}


-- |
--
-- @since 0.5.0
senumFromN :: Num e => e -> Sz1 -> Vector DS e
senumFromN x (Sz n) = DSArray $ S.enumFromStepN x 1 n
{-# INLINE senumFromN #-}

-- |
--
-- @since 0.5.0
senumFromStepN :: Num e => e -> e -> Sz1 -> Vector DS e
senumFromStepN x step (Sz n) = DSArray $ S.enumFromStepN x step n
{-# INLINE senumFromStepN #-}



-- |
--
-- @since 0.5.0
sappend :: (Stream r1 Ix1 e, Stream r2 Ix1 e) => Vector r1 e -> Vector r2 e -> Vector DS e
sappend a1 a2 = fromSteps (toStream a1 `S.append` toStream a2)
{-# INLINE sappend #-}


-- |
--
-- @since 0.5.0
sconcat :: Stream r Ix1 e => [Vector r e] -> Vector DS e
sconcat = DSArray . foldMap toStream
{-# INLINE sconcat #-}

-- |
--
-- @since 0.5.0
sfromList :: [e] -> Vector DS e
sfromList = fromSteps . S.fromList
{-# INLINE sfromList #-}

-- |
--
-- @since 0.5.0
sfromListN :: Int -> [e] -> Vector DS e
sfromListN n = fromSteps . S.fromListN n
{-# INLINE sfromListN #-}

-- |
--
-- @since 0.5.0
stoList :: Stream r ix e => Array r ix e -> [e]
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
sfilter :: S.Stream r ix e => (e -> Bool) -> Array r ix e -> Vector DS e
sfilter f = DSArray . S.filter f . S.toStream
{-# INLINE sfilter #-}


-- | Similar to `sfilter`, but map with an index aware function.
--
-- @since 0.5.0
sifilter :: Stream r ix a => (ix -> a -> Bool) -> Array r ix a -> Vector DS a
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
sfilterM :: (S.Stream r ix e, Applicative f) => (e -> f Bool) -> Array r ix e -> f (Vector DS e)
sfilterM f arr = DSArray <$> S.filterA f (S.toStream arr)
{-# INLINE sfilterM #-}


-- | Similar to `filterM`, but map with an index aware function.
--
-- @since 0.5.0
sifilterM ::
     (Stream r ix a, Applicative f) => (ix -> a -> f Bool) -> Array r ix a -> f (Vector DS a)
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
-- @since 0.5.0
smapMaybe :: S.Stream r ix a => (a -> Maybe b) -> Array r ix a -> Vector DS b
smapMaybe f = DSArray . S.mapMaybe f . S.toStream
{-# INLINE smapMaybe #-}


-- | Similar to `smapMaybe`, but map with an index aware function.
--
-- @since 0.5.0
simapMaybe :: Stream r ix a => (ix -> a -> Maybe b) -> Array r ix a -> Vector DS b
simapMaybe f = DSArray . S.mapMaybe (uncurry f) . toStreamIx
{-# INLINE simapMaybe #-}

-- | Similar to `smapMaybeM`, but map with an index aware function.
--
-- @since 0.5.0
simapMaybeM ::
     (Stream r ix a, Applicative f) => (ix -> a -> f (Maybe b)) -> Array r ix a -> f (Vector DS b)
simapMaybeM f = fmap DSArray . S.mapMaybeA (uncurry f) . toStreamIx
{-# INLINE simapMaybeM #-}


-- | Keep all `Maybe`s and discard the `Nothing`s.
--
-- @since 0.5.0
scatMaybes :: S.Stream r ix (Maybe a) => Array r ix (Maybe a) -> Vector DS a
scatMaybes = smapMaybe id
{-# INLINE scatMaybes #-}


-- | Similar to `smapMaybe`, but with the use of `Applicative`
--
-- @since 0.5.0
smapMaybeM ::
     (S.Stream r ix a, Applicative f) => (a -> f (Maybe b)) -> Array r ix a -> f (Vector DS b)
smapMaybeM f = fmap DSArray . S.mapMaybeA f . S.toStream
{-# INLINE smapMaybeM #-}



-- |
--
-- @since 0.5.0
smap :: (S.Stream r ix a) => (a -> b) -> Array r ix a -> Vector DS b
smap f = fromSteps . S.map f . S.toStream
{-# INLINE smap #-}

-- |
--
-- @since 0.5.0
simap :: (S.Stream r ix a) => (ix -> a -> b) -> Array r ix a -> Vector DS b
simap f = fromSteps . S.map (uncurry f) . S.toStreamIx
{-# INLINE simap #-}


-- | Traverse a stream with an applicative function.
--
-- @since 0.5.0
straverse :: (S.Stream r ix a, Applicative f) => (a -> f b) -> Array r ix a -> f (Vector DS b)
straverse f = fmap fromSteps . S.traverse f . S.toStream
{-# INLINE straverse #-}


-- | Traverse a stream with an index aware applicative function.
--
-- @since 0.5.0
sitraverse :: (S.Stream r ix a, Applicative f) => (ix -> a -> f b) -> Array r ix a -> f (Vector DS b)
sitraverse f = fmap fromSteps . S.traverse (uncurry f) . S.toStreamIx
{-# INLINE sitraverse #-}


-- |
--
-- @since 0.5.0
smapM :: (S.Stream r ix a, Monad m) => (a -> m b) -> Array r ix a -> m (Vector DS b)
smapM f = fromStepsM . S.mapM f . S.transStepsId . S.toStream
{-# INLINE smapM #-}

-- |
--
-- @since 0.5.0
simapM :: (S.Stream r ix a, Monad m) => (ix -> a -> m b) -> Array r ix a -> m (Vector DS b)
simapM f = fromStepsM . S.mapM (uncurry f) . S.transStepsId . S.toStreamIx
{-# INLINE simapM #-}

-- |
--
-- @since 0.5.0
smapM_ :: (S.Stream r ix a, Monad m) => (a -> m b) -> Array r ix a -> m ()
smapM_ f = S.mapM_ f . S.transStepsId . S.toStream
{-# INLINE smapM_ #-}

-- |
--
-- @since 0.5.0
simapM_ :: (S.Stream r ix a, Monad m) => (ix -> a -> m b) -> Array r ix a -> m ()
simapM_ f = S.mapM_ (uncurry f) . S.transStepsId . S.toStreamIx
{-# INLINE simapM_ #-}


-- |
--
-- @since 0.5.0
sforM :: (S.Stream r ix a, Monad m) => Array r ix a -> (a -> m b) -> m (Vector DS b)
sforM = flip smapM
{-# INLINE sforM #-}

-- |
--
-- @since 0.5.0
siforM :: (S.Stream r ix a, Monad m) => Array r ix a -> (ix -> a -> m b) -> m (Vector DS b)
siforM = flip simapM
{-# INLINE siforM #-}

-- |
--
-- @since 0.5.0
sforM_ :: (S.Stream r ix a, Monad m) => Array r ix a -> (a -> m b) -> m ()
sforM_ = flip smapM_
{-# INLINE sforM_ #-}

-- |
--
-- @since 0.5.0
siforM_ :: (S.Stream r ix a, Monad m) => Array r ix a -> (ix -> a -> m b) -> m ()
siforM_ = flip simapM_
{-# INLINE siforM_ #-}


-- |
--
-- @since 0.5.0
sfoldl :: Stream r ix e => (a -> e -> a) -> a -> Array r ix e -> a
sfoldl f acc = S.unId . S.foldl f acc . toStream
{-# INLINE sfoldl #-}

-- |
--
-- @since 0.5.0
sfoldlM :: (Stream r ix e, Monad m) => (a -> e -> m a) -> a -> Array r ix e -> m a
sfoldlM f acc = S.foldlM f acc . S.transStepsId . toStream
{-# INLINE sfoldlM #-}

-- |
--
-- @since 0.5.0
sfoldlM_ :: (Stream r ix e, Monad m) => (a -> e -> m a) -> a -> Array r ix e -> m ()
sfoldlM_ f acc = void . sfoldlM f acc
{-# INLINE sfoldlM_ #-}


-- |
--
-- @since 0.5.0
sfoldl1' :: Stream r ix e => (e -> e -> e) -> Array r ix e -> e
sfoldl1' f = either throw id . sfoldl1M (\e -> pure . f e)
{-# INLINE sfoldl1' #-}

-- |
--
-- @since 0.5.0
sfoldl1M :: (Stream r ix e, MonadThrow m) => (e -> e -> m e) -> Array r ix e -> m e
sfoldl1M f arr = do
  let str = S.transStepsId $ toStream arr
  nullStream <- S.null str
  when nullStream $ throwM $ SizeEmptyException (size arr)
  S.foldl1M f str
{-# INLINE sfoldl1M #-}

-- |
--
-- @since 0.5.0
sfoldl1M_ :: (Stream r ix e, MonadThrow m) => (e -> e -> m e) -> Array r ix e -> m ()
sfoldl1M_ f = void . sfoldl1M f
{-# INLINE sfoldl1M_ #-}



-- |
--
-- @since 0.5.0
sifoldl :: Stream r ix e => (a -> ix -> e -> a) -> a -> Array r ix e -> a
sifoldl f acc = S.unId . S.foldl (\a (ix, e) -> f a ix e) acc . toStreamIx
{-# INLINE sifoldl #-}

-- |
--
-- @since 0.5.0
sifoldlM :: (Stream r ix e, Monad m) => (a -> ix -> e -> m a) -> a -> Array r ix e -> m a
sifoldlM f acc = S.foldlM (\a (ix, e) -> f a ix e) acc . S.transStepsId . toStreamIx
{-# INLINE sifoldlM #-}

-- |
--
-- @since 0.5.0
sifoldlM_ :: (Stream r ix e, Monad m) => (a -> ix -> e -> m a) -> a -> Array r ix e -> m ()
sifoldlM_ f acc = void . sifoldlM f acc
{-# INLINE sifoldlM_ #-}


-- |
--
-- @since 0.5.0
sor :: Stream r ix Bool => Array r ix Bool -> Bool
sor = S.unId . S.or . toStream
{-# INLINE sor #-}


-- |
--
-- @since 0.5.0
sand :: Stream r ix Bool => Array r ix Bool -> Bool
sand = S.unId . S.and . toStream
{-# INLINE sand #-}


-- |
--
-- @since 0.5.0
sany :: Stream r ix e => (e -> Bool) -> Array r ix e -> Bool
sany f = S.unId . S.or . S.map f . toStream
{-# INLINE sany #-}


-- |
--
-- @since 0.5.0
sall :: Stream r ix e => (e -> Bool) -> Array r ix e -> Bool
sall f = S.unId . S.and . S.map f . toStream
{-# INLINE sall #-}



-- |
--
-- @since 0.5.0
ssum :: (Num e, Stream r ix e) => Array r ix e -> e
ssum = sfoldl (+) 0
{-# INLINE ssum #-}

-- |
--
-- @since 0.5.0
sproduct :: (Num e, Stream r ix e) => Array r ix e -> e
sproduct = sfoldl (*) 1
{-# INLINE sproduct #-}


-- |
--
-- @since 0.5.0
smaximum' :: (Ord e, Stream r ix e) => Array r ix e -> e
smaximum' = sfoldl1' max
{-# INLINE smaximum' #-}

-- |
--
-- @since 0.5.0
smaximumM :: (Ord e, Stream r ix e, MonadThrow m) => Array r ix e -> m e
smaximumM = sfoldl1M (\e acc -> pure (max e acc))
{-# INLINE smaximumM #-}



-- |
--
-- @since 0.5.0
sminimum' :: (Ord e, Stream r ix e) => Array r ix e -> e
sminimum' = sfoldl1' min
{-# INLINE sminimum' #-}

-- |
--
-- @since 0.5.0
sminimumM :: (Ord e, Stream r ix e, MonadThrow m) => Array r ix e -> m e
sminimumM = sfoldl1M (\e acc -> pure (min e acc))
{-# INLINE sminimumM #-}



-- | See `stake`.
--
-- @since 0.4.1
takeS :: Stream r ix e => Sz1 -> Array r ix e -> Array DS Ix1 e
takeS = stake
{-# INLINE takeS #-}
{-# DEPRECATED takeS "In favor of `stake`" #-}

-- | See `sdrop`.
--
-- @since 0.4.1
dropS :: Stream r ix e => Sz1 -> Array r ix e -> Array DS Ix1 e
dropS = sdrop
{-# INLINE dropS #-}
{-# DEPRECATED dropS "In favor of `sdrop`" #-}

-- | See `sunfoldr`
--
-- @since 0.4.1
unfoldr :: (s -> Maybe (e, s)) -> s -> Vector DS e
unfoldr = sunfoldr
{-# INLINE unfoldr #-}
{-# DEPRECATED unfoldr "In favor of `sunfoldr`" #-}


-- | See `sunfoldrN`
--
-- @since 0.4.1
unfoldrN :: Sz1 -> (s -> Maybe (e, s)) -> s -> Vector DS e
unfoldrN = unfoldrN
{-# INLINE unfoldrN #-}
{-# DEPRECATED unfoldrN "In favor of `sunfoldrN`" #-}


-- | See `sfilterM`
--
-- @since 0.4.1
filterM :: (S.Stream r ix e, Applicative f) => (e -> f Bool) -> Array r ix e -> f (Vector DS e)
filterM f arr = DSArray <$> S.filterA f (S.toStream arr)
{-# INLINE filterM #-}
{-# DEPRECATED filterM "In favor of `sfilterM`" #-}

-- | See `sfilter`
--
-- @since 0.4.1
filterS :: S.Stream r ix e => (e -> Bool) -> Array r ix e -> Array DS Ix1 e
filterS = sfilter
{-# INLINE filterS #-}
{-# DEPRECATED filterS "In favor of `sfilter`" #-}


-- | See `smapMaybe`
--
-- @since 0.4.1
mapMaybeS :: S.Stream r ix a => (a -> Maybe b) -> Array r ix a -> Vector DS b
mapMaybeS = smapMaybe
{-# INLINE mapMaybeS #-}
{-# DEPRECATED mapMaybeS "In favor of `smapMaybe`" #-}

-- | See `scatMaybes`
--
-- @since 0.4.4
catMaybesS :: S.Stream r ix (Maybe a) => Array r ix (Maybe a) -> Vector DS a
catMaybesS = scatMaybes
{-# INLINE catMaybesS #-}
{-# DEPRECATED catMaybesS "In favor of `scatMaybes`" #-}

-- | See `smapMaybeM`
--
-- @since 0.4.1
mapMaybeM ::
     (S.Stream r ix a, Applicative f) => (a -> f (Maybe b)) -> Array r ix a -> f (Vector DS b)
mapMaybeM = smapMaybeM
{-# INLINE mapMaybeM #-}
{-# DEPRECATED mapMaybeM "In favor of `smapMaybeM`" #-}

-- | See `traverseS`
--
-- @since 0.4.5
traverseS :: (S.Stream r ix a, Applicative f) => (a -> f b) -> Array r ix a -> f (Vector DS b)
traverseS = straverse
{-# INLINE traverseS #-}
{-# DEPRECATED traverseS "In favor of `straverse`" #-}

-- | See `simapMaybe`
--
-- @since 0.4.1
imapMaybeS :: Source r ix a => (ix -> a -> Maybe b) -> Array r ix a -> Array DS Ix1 b
imapMaybeS f arr =
  mapMaybeS (uncurry f) $ A.makeArrayR D (getComp arr) (size arr) $ \ix -> (ix, unsafeIndex arr ix)
{-# INLINE imapMaybeS #-}
{-# DEPRECATED imapMaybeS "In favor of `simapMaybe`" #-}

-- | See `simapMaybeM`
--
-- @since 0.4.1
imapMaybeM ::
     (Source r ix a, Applicative f) => (ix -> a -> f (Maybe b)) -> Array r ix a -> f (Array DS Ix1 b)
imapMaybeM f arr =
  mapMaybeM (uncurry f) $ A.makeArrayR D (getComp arr) (size arr) $ \ix -> (ix, unsafeIndex arr ix)
{-# INLINE imapMaybeM #-}
{-# DEPRECATED imapMaybeM "In favor of `simapMaybeM`" #-}

-- | Similar to `filterS`, but map with an index aware function.
--
-- @since 0.4.1
ifilterS :: Source r ix a => (ix -> a -> Bool) -> Array r ix a -> Array DS Ix1 a
ifilterS f =
  imapMaybeS $ \ix e ->
    if f ix e
      then Just e
      else Nothing
{-# INLINE ifilterS #-}
{-# DEPRECATED ifilterS "In favor of `sifilter`" #-}


-- | Similar to `filterM`, but map with an index aware function.
--
-- @since 0.4.1
ifilterM ::
     (Source r ix a, Applicative f) => (ix -> a -> f Bool) -> Array r ix a -> f (Array DS Ix1 a)
ifilterM f =
  imapMaybeM $ \ix e ->
    (\p ->
       if p
         then Just e
         else Nothing) <$>
    f ix e
{-# INLINE ifilterM #-}
{-# DEPRECATED ifilterM "In favor of `sifilterM`" #-}
