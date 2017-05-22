{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- |
-- Module      : Data.Array.Massiv.Common.Ops
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Common.Ops
  ( foldlM
  , foldrM
  , foldlM_
  , foldrM_
  , ifoldlM
  , ifoldrM
  , ifoldlM_
  , ifoldrM_
  , foldlS
  , lazyFoldlS
  , foldrS
  , foldrFB
  , lazyFoldrS
  , ifoldlS
  , ifoldrS
  , mapM_
  , imapM_
  , forM_
  , iforM_
  ) where

import           Control.Monad            (void)
import           Data.Array.Massiv.Common
import           Data.Functor.Identity    (runIdentity)
import           Prelude                  hiding (mapM_)


-- | /O(n)/ - Monadic left fold.
foldlM :: (Source r ix e, Monad m) => (a -> e -> m a) -> a -> Array r ix e -> m a
foldlM f = ifoldlM (\ a _ b -> f a b)
{-# INLINE foldlM #-}


-- | /O(n)/ - Monadic left fold, that discards the result.
foldlM_ :: (Source r ix e, Monad m) => (a -> e -> m a) -> a -> Array r ix e -> m ()
foldlM_ f = ifoldlM_ (\ a _ b -> f a b)
{-# INLINE foldlM_ #-}


-- | /O(n)/ - Monadic left fold with an index aware function.
ifoldlM :: (Source r ix e, Monad m) => (a -> ix -> e -> m a) -> a -> Array r ix e -> m a
ifoldlM f !acc !arr =
  iterM zeroIndex (size arr) 1 (<) acc $ \ !ix !a -> f a ix (unsafeIndex arr ix)
{-# INLINE ifoldlM #-}


-- | /O(n)/ - Monadic left fold with an index aware function, that discards the result.
ifoldlM_ :: (Source r ix e, Monad m) => (a -> ix -> e -> m a) -> a -> Array r ix e -> m ()
ifoldlM_ f acc = void . ifoldlM f acc
{-# INLINE ifoldlM_ #-}


-- | /O(n)/ - Monadic right fold.
foldrM :: (Source r ix e, Monad m) => (e -> a -> m a) -> a -> Array r ix e -> m a
foldrM f = ifoldrM (\_ e a -> f e a)
{-# INLINE foldrM #-}


-- | /O(n)/ - Monadic right fold, that discards the result.
foldrM_ :: (Source r ix e, Monad m) => (e -> a -> m a) -> a -> Array r ix e -> m ()
foldrM_ f = ifoldrM_ (\_ e a -> f e a)
{-# INLINE foldrM_ #-}


-- | /O(n)/ - Monadic right fold with an index aware function.
ifoldrM :: (Source r ix e, Monad m) => (ix -> e -> a -> m a) -> a -> Array r ix e -> m a
ifoldrM f !acc !arr =
  iterM (liftIndex (subtract 1) (size arr)) zeroIndex (-1) (>=) acc $ \ !ix !acc0 ->
    f ix (unsafeIndex arr ix) acc0
{-# INLINE ifoldrM #-}


-- | /O(n)/ - Monadic right fold with an index aware function, that discards the result.
ifoldrM_ :: (Source r ix e, Monad m) => (ix -> e -> a -> m a) -> a -> Array r ix e -> m ()
ifoldrM_ f !acc !arr = void $ ifoldrM f acc arr
{-# INLINE ifoldrM_ #-}



-- | /O(n)/ - Left fold, computed sequentially with lazy accumulator.
lazyFoldlS :: Source r ix e => (a -> e -> a) -> a -> Array r ix e -> a
lazyFoldlS f initAcc arr = go initAcc 0 where
    len = totalElem (size arr)
    go acc k | k < len = go (f acc (unsafeLinearIndex arr k)) (k + 1)
             | otherwise = acc
{-# INLINE lazyFoldlS #-}


-- | /O(n)/ - Right fold, computed sequentially with lazy accumulator.
lazyFoldrS :: Source r ix e => (e -> a -> a) -> a -> Array r ix e -> a
lazyFoldrS = foldrFB
{-# INLINE lazyFoldrS #-}


-- | /O(n)/ - Left fold, computed sequentially.
foldlS :: Source r ix e => (a -> e -> a) -> a -> Array r ix e -> a
foldlS f = ifoldlS (\ a _ e -> f a e)
{-# INLINE foldlS #-}


-- | /O(n)/ - Left fold with an index aware function, computed sequentially.
ifoldlS :: Source r ix e
        => (a -> ix -> e -> a) -> a -> Array r ix e -> a
ifoldlS f acc = runIdentity . ifoldlM (\ a ix e -> return $ f a ix e) acc
{-# INLINE ifoldlS #-}


-- | /O(n)/ - Right fold, computed sequentially.
foldrS :: Source r ix e => (e -> a -> a) -> a -> Array r ix e -> a
foldrS f = ifoldrS (\_ e a -> f e a)
{-# INLINE foldrS #-}


-- | Version of foldr that supports foldr/build list fusion implemented by GHC.
foldrFB :: Source r ix e => (e -> b -> b) -> b -> Array r ix e -> b
foldrFB c n arr = go 0
  where
    !k = totalElem (size arr)
    go !i
      | i == k = n
      | otherwise = let !v = unsafeLinearIndex arr i in v `c` go (i + 1)
{-# INLINE [0] foldrFB #-}


-- foldrS :: (Source r ix e) => (e -> a -> a) -> a -> Array r ix e -> a
-- foldrS f !acc !arr =
--   iter (liftIndex (subtract 1) (size arr)) zeroIndex (-1) (>=) acc $ \ !ix !acc0 ->
--     f (unsafeIndex arr ix) acc0
-- {-# INLINE foldrS #-}

-- foldrS' :: Source r ix e => (e -> a -> a) -> a -> Array r ix e -> a
-- foldrS' f acc arr =
--   iter (totalElem sz) 0 (-1) (>=) acc $ \ !i !acc0 ->
--     f (unsafeLinearIndex arr i) acc0
--   where
--     !sz = size arr
-- {-# INLINE foldrS' #-}

-- foldlS' :: Source r ix e => (a -> e -> a) -> a -> Array r ix e -> a
-- foldlS' f !acc !arr =
--   iter zeroIndex (size arr) 1 (<) acc $ \ !ix !a -> f a (unsafeIndex arr ix)
--   -- iter 0 (totalElem (size arr)) 1 (<) acc $ \ !i !acc0 ->
--   --   f acc0 (unsafeLinearIndex arr i)
-- {-# INLINE foldlS' #-}




-- | /O(n)/ - Right fold with an index aware function, computed sequentially.
ifoldrS :: Source r ix e => (ix -> e -> a -> a) -> a -> Array r ix e -> a
ifoldrS f acc = runIdentity . ifoldrM (\ ix e a -> return $ f ix e a) acc
{-# INLINE ifoldrS #-}



-- | Map a monadic function over an array sequentially, while discarding the result.
mapM_ :: (Source r ix a, Monad m) => (a -> m b) -> Array r ix a -> m ()
mapM_ f !arr = iterM_ zeroIndex (size arr) 1 (<) (f . unsafeIndex arr)
{-# INLINE mapM_ #-}


-- | Just like `mapM_`, except with flipped arguments.
forM_ :: (Source r ix a, Monad m) => Array r ix a -> (a -> m b) -> m ()
forM_ = flip mapM_
{-# INLINE forM_ #-}


-- | Map a monadic index aware function over an array sequentially, while discarding the result.
imapM_ :: (Source r ix a, Monad m) => (ix -> a -> m b) -> Array r ix a -> m ()
imapM_ f !arr =
  iterM_ zeroIndex (size arr) 1 (<) $ \ !ix -> f ix (unsafeIndex arr ix)
{-# INLINE imapM_ #-}

-- | Just like `imapM_`, except with flipped arguments.
iforM_ :: (Source r ix a, Monad m) => Array r ix a -> (ix -> a -> m b) -> m ()
iforM_ = flip imapM_
{-# INLINE iforM_ #-}

