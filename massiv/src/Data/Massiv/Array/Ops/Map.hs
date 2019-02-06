{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- |
-- Module      : Data.Massiv.Array.Ops.Map
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Ops.Map
  ( map
  , imap
  -- ** Traversing
  -- *** Applicative
  , traverseA
  , itraverseA
  , traverseAR
  , itraverseAR
  -- *** PrimMonad
  , traversePrim
  , itraversePrim
  , traversePrimR
  , itraversePrimR
  -- ** Monadic mapping
  -- *** Sequential
  , mapM
  , mapMR
  , forM
  , forMR
  , imapM
  , imapMR
  , iforM
  , iforMR
  , mapM_
  , forM_
  , imapM_
  , iforM_
  -- *** Parallelizable
  , mapIO
  , mapIO_
  , imapIO
  , imapIO_
  , forIO
  , forIO_
  , iforIO
  , iforIO_
  -- ** Zipping
  , zip
  , zip3
  , unzip
  , unzip3
  , zipWith
  , zipWith3
  , izipWith
  , izipWith3
  , liftArray2
  ) where

import           Control.Monad                       (void)
import           Control.Monad.Primitive             (PrimMonad)
import           Data.Coerce
import           Data.Massiv.Array.Delayed.Pull
import           Data.Massiv.Array.Mutable
import           Data.Massiv.Array.Ops.Construct     (makeArrayA)
import           Data.Massiv.Core.Common
import           Data.Massiv.Core.Index.Internal     (Sz (..))
import           Data.Massiv.Scheduler
import           Prelude                             hiding (map, mapM, mapM_,
                                                      traverse, unzip, unzip3,
                                                      zip, zip3, zipWith,
                                                      zipWith3)

--------------------------------------------------------------------------------
-- map -------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Map a function over an array
map :: Source r ix e' => (e' -> e) -> Array r ix e' -> Array D ix e
map f = imap (const f)
{-# INLINE map #-}

-- | Map an index aware function over an array
imap :: Source r ix e' => (ix -> e' -> e) -> Array r ix e' -> Array D ix e
imap f !arr = DArray (getComp arr) (size arr) (\ !ix -> f ix (unsafeIndex arr ix))
{-# INLINE imap #-}

--------------------------------------------------------------------------------
-- zip -------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Zip two arrays
zip :: (Source r1 ix e1, Source r2 ix e2)
    => Array r1 ix e1 -> Array r2 ix e2 -> Array D ix (e1, e2)
zip = zipWith (,)
{-# INLINE zip #-}

-- | Zip three arrays
zip3 :: (Source r1 ix e1, Source r2 ix e2, Source r3 ix e3)
     => Array r1 ix e1 -> Array r2 ix e2 -> Array r3 ix e3 -> Array D ix (e1, e2, e3)
zip3 = zipWith3 (,,)
{-# INLINE zip3 #-}

-- | Unzip two arrays
unzip :: Source r ix (e1, e2) => Array r ix (e1, e2) -> (Array D ix e1, Array D ix e2)
unzip arr = (map fst arr, map snd arr)
{-# INLINE unzip #-}

-- | Unzip three arrays
unzip3 :: Source r ix (e1, e2, e3)
       => Array r ix (e1, e2, e3) -> (Array D ix e1, Array D ix e2, Array D ix e3)
unzip3 arr = (map (\ (e, _, _) -> e) arr, map (\ (_, e, _) -> e) arr, map (\ (_, _, e) -> e) arr)
{-# INLINE unzip3 #-}

--------------------------------------------------------------------------------
-- zipWith ---------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Zip two arrays with a function. Resulting array will be an intersection of
-- source arrays in case their dimensions do not match.
zipWith :: (Source r1 ix e1, Source r2 ix e2)
        => (e1 -> e2 -> e) -> Array r1 ix e1 -> Array r2 ix e2 -> Array D ix e
zipWith f = izipWith (\ _ e1 e2 -> f e1 e2)
{-# INLINE zipWith #-}


-- | Just like `zipWith`, except with an index aware function.
izipWith :: (Source r1 ix e1, Source r2 ix e2)
         => (ix -> e1 -> e2 -> e) -> Array r1 ix e1 -> Array r2 ix e2 -> Array D ix e
izipWith f arr1 arr2 =
  DArray
    (getComp arr1 <> getComp arr2)
    (SafeSz (liftIndex2 min (coerce (size arr1)) (coerce (size arr2)))) $ \ !ix ->
    f ix (unsafeIndex arr1 ix) (unsafeIndex arr2 ix)
{-# INLINE izipWith #-}


-- | Just like `zipWith`, except zip three arrays with a function.
zipWith3 :: (Source r1 ix e1, Source r2 ix e2, Source r3 ix e3)
         => (e1 -> e2 -> e3 -> e) -> Array r1 ix e1 -> Array r2 ix e2 -> Array r3 ix e3 -> Array D ix e
zipWith3 f = izipWith3 (\ _ e1 e2 e3 -> f e1 e2 e3)
{-# INLINE zipWith3 #-}


-- | Just like `zipWith3`, except with an index aware function.
izipWith3
  :: (Source r1 ix e1, Source r2 ix e2, Source r3 ix e3)
  => (ix -> e1 -> e2 -> e3 -> e)
  -> Array r1 ix e1
  -> Array r2 ix e2
  -> Array r3 ix e3
  -> Array D ix e
izipWith3 f arr1 arr2 arr3 =
  DArray
    (getComp arr1 <> getComp arr2 <> getComp arr3)
    (SafeSz
       (liftIndex2
          min
          (liftIndex2 min (coerce (size arr1)) (coerce (size arr2)))
          (coerce (size arr3)))) $ \ !ix ->
    f ix (unsafeIndex arr1 ix) (unsafeIndex arr2 ix) (unsafeIndex arr3 ix)
{-# INLINE izipWith3 #-}

--------------------------------------------------------------------------------
-- traverse --------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Traverse with an `Applicative` action over an array sequentially.
--
-- /Note/ - using `traversePrim` will always be faster, althought not always possible.
--
-- @since 0.2.6
--
traverseA ::
     (Source r' ix a, Mutable r ix e, Applicative f)
  => (a -> f e)
  -> Array r' ix a
  -> f (Array r ix e)
traverseA f arr = makeArrayA (getComp arr) (size arr) (f . unsafeIndex arr)
{-# INLINE traverseA #-}

-- | Traverse with an `Monad` index aware action over an array sequentially.
--
-- @since 0.2.6
--
itraverseA ::
     (Source r' ix a, Mutable r ix e, Applicative f)
  => (ix -> a -> f e)
  -> Array r' ix a
  -> f (Array r ix e)
itraverseA f arr = makeArrayA (getComp arr) (size arr) $ \ !ix -> f ix (unsafeIndex arr ix)
{-# INLINE itraverseA #-}



-- | Same as `traverseA`, except with ability to specify representation.
--
-- @since 0.2.6
--
traverseAR ::
     (Source r' ix a, Mutable r ix b, Applicative f)
  => r
  -> (a -> f b)
  -> Array r' ix a
  -> f (Array r ix b)
traverseAR _ = traverseA
{-# INLINE traverseAR #-}

-- | Same as `itraverseA`, except with ability to specify representation.
--
-- @since 0.2.6
--
itraverseAR ::
     (Source r' ix a, Mutable r ix b, Applicative f)
  => r
  -> (ix -> a -> f b)
  -> Array r' ix a
  -> f (Array r ix b)
itraverseAR _ = itraverseA
{-# INLINE itraverseAR #-}



-- | Traverse sequentially within `PrimMonad` over an array with an action.
--
-- @since 0.3.0
--
traversePrim ::
     (Source r' ix a, Mutable r ix b, PrimMonad m)
  => (a -> m b)
  -> Array r' ix a
  -> m (Array r ix b)
traversePrim f arr = generateArrayS (getComp arr) (size arr) (f . unsafeIndex arr)
{-# INLINE traversePrim #-}

-- | Same as `traversePrim`, but traverse with index aware action.
--
-- @since 0.3.0
--
itraversePrim ::
     (Source r' ix a, Mutable r ix b, PrimMonad m)
  => (ix -> a -> m b)
  -> Array r' ix a
  -> m (Array r ix b)
itraversePrim f arr = generateArrayS (getComp arr) (size arr) (\ !ix -> f ix (unsafeIndex arr ix))
{-# INLINE itraversePrim #-}


-- | Same as `traverseP`, but with ability to specify the desired representation.
--
-- @since 0.3.0
--
traversePrimR ::
     (Source r' ix a, Mutable r ix b, PrimMonad m)
  => r
  -> (a -> m b)
  -> Array r' ix a
  -> m (Array r ix b)
traversePrimR _ = traversePrim
{-# INLINE traversePrimR #-}

-- | Same as `itraverseP`, but with ability to specify the desired representation.
--
-- @since 0.3.0
--
itraversePrimR ::
     (Source r' ix a, Mutable r ix b, PrimMonad m)
  => r
  -> (ix -> a -> m b)
  -> Array r' ix a
  -> m (Array r ix b)
itraversePrimR _ = itraversePrim
{-# INLINE itraversePrimR #-}


--------------------------------------------------------------------------------
-- mapM ------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Map a monadic action over an array sequentially.
--
-- @since 0.2.6
--
mapM ::
     (Source r' ix a, Mutable r ix b, Monad m)
  => (a -> m b)
  -> Array r' ix a
  -> m (Array r ix b)
mapM = traverseA
{-# INLINE mapM #-}


-- | Same as `mapM`, except with ability to specify result representation.
--
-- @since 0.2.6
--
mapMR ::
     (Source r' ix a, Mutable r ix b, Monad m)
  => r
  -> (a -> m b)
  -> Array r' ix a
  -> m (Array r ix b)
mapMR _ = traverseA
{-# INLINE mapMR #-}


-- | Same as `mapM` except with arguments flipped.
--
-- @since 0.2.6
--
forM ::
     (Source r' ix a, Mutable r ix b, Monad m)
  => Array r' ix a
  -> (a -> m b)
  -> m (Array r ix b)
forM = flip traverseA
{-# INLINE forM #-}


-- | Same as `forM`, except with ability to specify result representation.
--
-- @since 0.2.6
--
forMR ::
     (Source r' ix a, Mutable r ix b, Monad m)
  => r
  -> Array r' ix a
  -> (a -> m b)
  -> m (Array r ix b)
forMR _ = flip traverseA
{-# INLINE forMR #-}



-- | Map a monadic action over an array sequentially.
--
-- @since 0.2.6
--
imapM ::
     (Source r' ix a, Mutable r ix b, Monad m)
  => (ix -> a -> m b)
  -> Array r' ix a
  -> m (Array r ix b)
imapM = itraverseA
{-# INLINE imapM #-}


-- | Same as `imapM`, except with ability to specify result representation.
--
-- @since 0.2.6
--
imapMR ::
     (Source r' ix a, Mutable r ix b, Monad m)
  => r
  -> (ix -> a -> m b)
  -> Array r' ix a
  -> m (Array r ix b)
imapMR _ = itraverseA
{-# INLINE imapMR #-}



-- | Same as `forM`, except map an index aware action.
--
-- @since 0.2.6
--
iforM ::
     (Source r' ix a, Mutable r ix b, Monad m)
  => (ix -> a -> m b)
  -> Array r' ix a
  -> m (Array r ix b)
iforM = itraverseA
{-# INLINE iforM #-}


-- | Same as `iforM`, except with ability to specify result representation.
--
-- @since 0.2.6
--
iforMR ::
     (Source r' ix a, Mutable r ix b, Monad m)
  => r
  -> (ix -> a -> m b)
  -> Array r' ix a
  -> m (Array r ix b)
iforMR _ = itraverseA
{-# INLINE iforMR #-}


-- | Map a monadic function over an array sequentially, while discarding the result.
--
-- ==== __Examples__
--
-- >>> mapM_ print $ rangeStep 10 12 60
-- 10
-- 22
-- 34
-- 46
-- 58
--
mapM_ :: (Source r ix a, Monad m) => (a -> m b) -> Array r ix a -> m ()
mapM_ f !arr = iterM_ zeroIndex (unSz (size arr)) (pureIndex 1) (<) (f . unsafeIndex arr)
{-# INLINE mapM_ #-}


-- | Just like `mapM_`, except with flipped arguments.
--
-- ==== __Examples__
--
-- Here is a common way of iterating N times using a for loop in an imperative
-- language with mutation being an obvious side effect:
--
-- >>> import Data.IORef
-- >>> ref <- newIORef 0 :: IO (IORef Int)
-- >>> forM_ (range Seq (Ix1 0) 1000) $ \ i -> modifyIORef' ref (+i)
-- >>> readIORef ref
-- 499500
--
forM_ :: (Source r ix a, Monad m) => Array r ix a -> (a -> m b) -> m ()
forM_ = flip mapM_
{-# INLINE forM_ #-}


-- | Just like `imapM_`, except with flipped arguments.
iforM_ :: (Source r ix a, Monad m) => Array r ix a -> (ix -> a -> m b) -> m ()
iforM_ = flip imapM_
{-# INLINE iforM_ #-}


-- | Map an `IO` action over an `Array`. Underlying computation strategy is respected and will be
-- parallelized when requested. Unfortunately no fusion is possible and new array will be create
-- upon each call.
--
-- @since 0.2.6
mapIO ::
     (Source r' ix a, Mutable r ix b, MonadUnliftIO m, PrimMonad m)
  => (a -> m b)
  -> Array r' ix a
  -> m (Array r ix b)
mapIO action = imapIO (const action)
{-# INLINE mapIO #-}

-- | Similar to `mapIO`, but ignores the result of mapping action and does not create a resulting
-- array, therefore it is faster. Use this instead of `mapIO` when result is irrelevant.
--
-- @since 0.2.6
mapIO_ :: (Source r b e, MonadUnliftIO m) => (e -> m a) -> Array r b e -> m ()
mapIO_ action = imapIO_ (const action)
{-# INLINE mapIO_ #-}

-- | Same as `mapIO_`, but map an index aware action instead.
--
-- @since 0.2.6
imapIO_ :: (Source r ix e, MonadUnliftIO m) => (ix -> e -> m a) -> Array r ix e -> m ()
imapIO_ action arr = do
  let sz = size arr
  withScheduler_ (getComp arr) $ \scheduler ->
    splitLinearlyWith_
      (numWorkers scheduler)
      (scheduleWork scheduler)
      (totalElem sz)
      (unsafeLinearIndex arr)
      (\i -> void . action (fromLinearIndex sz i))
{-# INLINE imapIO_ #-}


-- | Same as `mapIO` but map an index aware action instead.
--
-- @since 0.2.6
imapIO ::
     (Source r' ix a, Mutable r ix b, MonadUnliftIO m, PrimMonad m)
  => (ix -> a -> m b)
  -> Array r' ix a
  -> m (Array r ix b)
imapIO action arr = generateArray (getComp arr) (size arr) $ \ix -> action ix (unsafeIndex arr ix)
{-# INLINE imapIO #-}

-- | Same as `mapIO` but with arguments flipped.
--
-- @since 0.2.6
forIO ::
     (Source r' ix a, Mutable r ix b, MonadUnliftIO m, PrimMonad m)
  => Array r' ix a
  -> (a -> m b)
  -> m (Array r ix b)
forIO = flip mapIO
{-# INLINE forIO #-}

-- | Same as `mapIO_` but with arguments flipped.
--
-- ==== __Example__
--
-- This is the same example as in `forM_`, with important difference that accumulator `ref` will be
-- modified concurrently by as many threads as there are capabilities.
--
-- >>> import Data.IORef
-- >>> ref <- newIORef 0 :: IO (IORef Int)
-- >>> forIO_ (range Par (Ix1 0) 1000) $ \ i -> atomicModifyIORef' ref (\v -> (v+i, ()))
-- >>> readIORef ref
-- 499500
--
-- @since 0.2.6
forIO_ :: (Source r ix e, MonadUnliftIO m) => Array r ix e -> (e -> m a) -> m ()
forIO_ = flip mapIO_
{-# INLINE forIO_ #-}

-- | Same as `imapIO` but with arguments flipped.
--
-- @since 0.2.6
iforIO ::
     (Source r' ix a, Mutable r ix b, MonadUnliftIO m, PrimMonad m)
  => Array r' ix a
  -> (ix -> a -> m b)
  -> m (Array r ix b)
iforIO = flip imapIO
{-# INLINE iforIO #-}

-- | Same as `imapIO_` but with arguments flipped.
--
-- @since 0.2.6
iforIO_ :: (Source r ix a, MonadUnliftIO m) => Array r ix a -> (ix -> a -> m b) -> m ()
iforIO_ = flip imapIO_
{-# INLINE iforIO_ #-}
