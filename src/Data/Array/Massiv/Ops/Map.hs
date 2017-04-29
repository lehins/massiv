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
  , imapM_
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

import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Delayed
import           Prelude                   hiding (map, unzip, unzip3, zip,
                                            zip3, zipWith, zipWith3)


map :: Source r ix b => (b -> e) -> Array r ix b -> Array D ix e
map = liftArray
{-# INLINE map #-}

imap :: Source r ix b => (ix -> b -> e) -> Array r ix b -> Array D ix e
imap f !arr = DArray (size arr) (\ !ix -> f ix (unsafeIndex arr ix))
{-# INLINE imap #-}


zip
  :: (Source r1 ix a1, Source r2 ix a2)
  => Array r1 ix a1 -> Array r2 ix a2 -> Array D ix (a1, a2)
zip = zipWith (,)
{-# INLINE zip #-}

zip3
  :: (Source r1 ix a1, Source r2 ix a2, Source r3 ix a3)
  => Array r1 ix a1 -> Array r2 ix a2 -> Array r3 ix a3 -> Array D ix (a1, a2, a3)
zip3 = zipWith3 (,,)
{-# INLINE zip3 #-}


unzip
  :: Source r ix (a1, a2)
  => Array r ix (a1, a2) -> (Array D ix a1, Array D ix a2)
unzip arr = (map fst arr, map snd arr)
{-# INLINE unzip #-}


unzip3
  :: Source r ix (a1, a2, a3)
  => Array r ix (a1, a2, a3) -> (Array D ix a1, Array D ix a2, Array D ix a3)
unzip3 arr = (map (\ (e, _, _) -> e) arr, map (\ (_, e, _) -> e) arr, map (\ (_, _, e) -> e) arr)
{-# INLINE unzip3 #-}


zipWith
  :: (Source r1 ix a1, Source r2 ix a2)
  => (a1 -> a2 -> e) -> Array r1 ix a1 -> Array r2 ix a2 -> Array D ix e
zipWith f !arr1 !arr2 =
  DArray (liftIndex2 min (size arr1) (size arr1)) $ \ !ix ->
    f (unsafeIndex arr1 ix) (unsafeIndex arr2 ix)
{-# INLINE zipWith #-}


zipWith3
  :: (Source r1 ix a1, Source r2 ix a2, Source r3 ix a3)
  => (a1 -> a2 -> a3 -> e) -> Array r1 ix a1 -> Array r2 ix a2 -> Array r3 ix a3 -> Array D ix e
zipWith3 f !arr1 !arr2 !arr3 =
  DArray (liftIndex2 min (liftIndex2 min (size arr1) (size arr1)) (size arr3)) $ \ !ix ->
    f (unsafeIndex arr1 ix) (unsafeIndex arr2 ix) (unsafeIndex arr3 ix)
{-# INLINE zipWith3 #-}


izipWith
  :: (Source r1 ix a1, Source r2 ix a2)
  => (ix -> a1 -> a2 -> e) -> Array r1 ix a1 -> Array r2 ix a2 -> Array D ix e
izipWith f !arr1 !arr2 =
  DArray (liftIndex2 min (size arr1) (size arr1)) $ \ !ix ->
    f ix (unsafeIndex arr1 ix) (unsafeIndex arr2 ix)
{-# INLINE izipWith #-}


izipWith3
  :: (Source r1 ix a1, Source r2 ix a2, Source r3 ix a3)
  => (ix -> a1 -> a2 -> a3 -> e)
  -> Array r1 ix a1
  -> Array r2 ix a2
  -> Array r3 ix a3
  -> Array D ix e
izipWith3 f !arr1 !arr2 !arr3 =
  DArray (liftIndex2 min (liftIndex2 min (size arr1) (size arr1)) (size arr3)) $ \ !ix ->
    f ix (unsafeIndex arr1 ix) (unsafeIndex arr2 ix) (unsafeIndex arr3 ix)
{-# INLINE izipWith3 #-}



imapM_ :: (Source r ix a, Monad m) => (ix -> a -> m b) -> Array r ix a -> m ()
imapM_ f !arr =
  iterM_ zeroIndex (size arr) 1 (<) $ \ !ix -> f ix (unsafeIndex arr ix)
{-# INLINE imapM_ #-}
