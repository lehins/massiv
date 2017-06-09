{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : Data.Array.Massiv.Ops.Map
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Ops.Map
  ( -- * Mapping
    map
  , imap
  -- * Monadic Mapping
  , mapM_
  , imapM_
  , forM_
  , iforM_
  , mapP_
  , imapP_
  -- * Zipping
  , zip
  , zip3
  , unzip
  , unzip3
  , zipWith
  , zipWith3
  , izipWith
  , izipWith3
  ) where

-- import           Control.DeepSeq             (NFData, deepseq)
import           Control.Monad               (void, when)
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Delayed
import           Data.Array.Massiv.Scheduler
import           Prelude                     hiding (map, mapM_, unzip, unzip3,
                                              zip, zip3, zipWith, zipWith3)


-- | Map a function over an array
map :: Source r ix e' => (e' -> e) -> Array r ix e' -> Array D ix e
map f = imap (const f)
{-# INLINE map #-}

-- | Map an index aware function over an array
imap :: Source r ix e' => (ix -> e' -> e) -> Array r ix e' -> Array D ix e
imap f !arr = DArray (getComp arr) (size arr) (\ !ix -> f ix (unsafeIndex arr ix))
{-# INLINE imap #-}

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
  DArray (getComp arr1) (liftIndex2 min (size arr1) (size arr1)) $ \ !ix ->
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
    (getComp arr1)
    (liftIndex2 min (liftIndex2 min (size arr1) (size arr1)) (size arr3)) $ \ !ix ->
    f ix (unsafeIndex arr1 ix) (unsafeIndex arr2 ix) (unsafeIndex arr3 ix)
{-# INLINE izipWith3 #-}



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
mapM_ f !arr = iterM_ zeroIndex (size arr) 1 (<) (f . unsafeIndex arr)
{-# INLINE mapM_ #-}


-- | Just like `mapM_`, except with flipped arguments.
--
-- ==== __Examples__
--
-- Here is a common way of iterating N times using a for loop in an imperative
-- language with mutation being an obvious side effect:
--
-- >>> :m + Data.IORef
-- >>> var <- newIORef 0 :: IO (IORef Int)
-- >>> forM_ (range 0 1000) $ \ i -> modifyIORef' var (+i)
-- >>> readIORef var
-- 499500
--
forM_ :: (Source r ix a, Monad m) => Array r ix a -> (a -> m b) -> m ()
forM_ = flip mapM_
{-# INLINE forM_ #-}


-- | Map a monadic index aware function over an array sequentially, while discarding the result.
--
-- ==== __Examples__
--
-- >>> imapM_ (curry print) $ range 10 15
-- (0,10)
-- (1,11)
-- (2,12)
-- (3,13)
-- (4,14)
--
imapM_ :: (Source r ix a, Monad m) => (ix -> a -> m b) -> Array r ix a -> m ()
imapM_ f !arr =
  iterM_ zeroIndex (size arr) 1 (<) $ \ !ix -> f ix (unsafeIndex arr ix)
{-# INLINE imapM_ #-}

-- | Just like `imapM_`, except with flipped arguments.
iforM_ :: (Source r ix a, Monad m) => Array r ix a -> (ix -> a -> m b) -> m ()
iforM_ = flip imapM_
{-# INLINE iforM_ #-}



-- | Map an IO action, over an array in parallel, while discarding the result.
mapP_ :: Source r ix a => (a -> IO b) -> Array r ix a -> IO ()
mapP_ f = imapP_ (const f)
{-# INLINE mapP_ #-}


-- | Map an IO action, that is index aware, over an array in parallel, while
-- discarding the result.
imapP_ :: Source r ix a => (ix -> a -> IO b) -> Array r ix a -> IO ()
imapP_ f arr = do
  let sz = size arr
      wIds =
        case getComp arr of
          ParOn ids -> ids
          _         -> []
  splitWork_ wIds sz $ \ !scheduler !chunkLength !totalLength !slackStart -> do
    loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
      submitRequest scheduler $
      JobRequest $
      iterLinearM_ sz start (start + chunkLength) 1 (<) $ \ !i ix -> do
        void $ f ix (unsafeLinearIndex arr i)
    when (slackStart < totalLength) $
      submitRequest scheduler $
      JobRequest $
      iterLinearM_ sz slackStart totalLength 1 (<) $ \ !i ix -> do
        void $ f ix (unsafeLinearIndex arr i)
{-# INLINE imapP_ #-}



-- -- | Map an IO action, that is index aware, over an array in parallel, while
-- -- discarding the result.
-- imapP_ :: (NFData b, Source r ix a) => (ix -> a -> IO b) -> Array r ix a -> IO ()
-- imapP_ f !arr = do
--   let !sz = size arr
--   splitWork_ sz $ \ !scheduler !chunkLength !totalLength !slackStart -> do
--     loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
--       submitRequest scheduler $
--       JobRequest 0 $
--       iterLinearM_ sz start (start + chunkLength) 1 (<) $ \ !i ix -> do
--         res <- f ix (unsafeLinearIndex arr i)
--         res `deepseq` return ()
--     when (slackStart < totalLength) $
--       submitRequest scheduler $
--       JobRequest 0 $
--       iterLinearM_ sz slackStart totalLength 1 (<) $ \ !i ix -> do
--         res <- f ix (unsafeLinearIndex arr i)
--         res `deepseq` return ()
-- {-# INLINE imapP_ #-}
