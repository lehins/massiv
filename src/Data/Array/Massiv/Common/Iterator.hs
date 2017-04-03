{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- |
-- Module      : Data.Array.Massiv.Common.Iterator
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Common.Iterator (Iterator(..), RowMajor(..)) where

import           Data.Array.Massiv.Common.Index hiding (iter)


  -- ifoldl :: Source r ix e => i -> (b -> ix -> e -> b) -> b -> Array r ix e -> b

  -- ifoldr :: Source r ix e => i -> (ix -> e -> b -> b) -> b -> Array r ix e -> b



class Index ix => Iterator i ix where

  iter :: i -> ix -> ix -> a -> (ix -> a -> a) -> a

  iterateM_
    :: Monad m
    => i -- ^ Iterator
    -> ix -- ^ From
    -> ix -- ^ To
    -> (ix -> m ())
    -> m ()
  iterateLinearM_
    :: Monad m
    => i -- ^ Iterator
    -> ix -- ^ Dimensions
    -> Int -- ^ From
    -> Int -- ^ To
    -> (Int -> ix -> m ())
    -> m ()
  iterateWithLinearM_
    :: Monad m
    => i -- ^ Iterator
    -> ix -- ^ Dimensions
    -> ix -- ^ From
    -> ix -- ^ To
    -> (Int -> ix -> m ())
    -> m ()
  iterateWithLinearM_ it !sz !start !end f =
    iterateM_ it start end $ \ !ix -> f (toLinearIndex sz ix) ix
  {-# INLINE iterateWithLinearM_ #-}




-- -- | Very efficient loop with an accumulator
-- loop :: Int -> (Int -> Bool) -> (Int -> Int) -> a -> (Int -> a -> a) -> a
-- loop !init' condition increment !initAcc f = go init' initAcc where
--   go !step !acc =
--     case condition step of
--       False -> acc
--       True  -> go (increment step) (f step acc)
-- {-# INLINE loop #-}


-- -- | Very efficient monadic loop
-- loopM_ :: Monad m => Int -> (Int -> Bool) -> (Int -> Int) -> (Int -> m a) -> m ()
-- loopM_ !init' condition increment f = go init' where
--   go !step =
--     case condition step of
--       False -> return ()
--       True  -> f step >> go (increment step)
-- {-# INLINE loopM_ #-}


-- -- | Very efficient monadic loop with an accumulator
-- loopM :: Monad m => Int -> (Int -> Bool) -> (Int -> Int) -> a -> (Int -> a -> m a) -> m a
-- loopM !init' condition increment !initAcc f = go init' initAcc where
--   go !step acc =
--     case condition step of
--       False -> return acc
--       True  -> f step acc >>= go (increment step)
-- {-# INLINE loopM #-}




data RowMajor = RowMajor


instance Iterator RowMajor DIM1 where

  iter _ k0 k1 acc = loop k0 (<k1) (+1) acc
  {-# INLINE iter #-}
  -- ifoldr _ f acc arr =
  --   let !k = size arr
  --   in loop 0 (< k) (+ 1) acc $ \ !ix !accO -> f ix (unsafeIndex arr ix) accO
  -- {-# INLINE ifoldr #-}
  -- ifoldl _ f acc arr =
  --   let !k = size arr
  --   in loop (k - 1) (>= 0) (subtract 1) acc $ \ !ix !accO ->
  --        f accO ix (unsafeIndex arr ix)
  -- {-# INLINE ifoldl #-}
  iterateM_ _ !k0 !k1 f = loopM_ k0 (< k1) (+ 1) $ \ !i -> f i
  {-# INLINE iterateM_ #-}
  iterateLinearM_ = iterateWithLinearM_
  {-# INLINE iterateLinearM_ #-}


instance Iterator RowMajor DIM2 where
  -- ifoldr _ f acc arr =
  --   let !(m, n) = size arr
  --   in loop 0 (< m) (+ 1) acc $ \ !i !accO ->
  --        loop 0 (< n) (+ 1) accO $ \ !j !accI ->
  --          let !ix = (i, j)
  --          in f ix (unsafeIndex arr ix) accI
  -- {-# INLINE ifoldr #-}
  -- ifoldl _ f acc arr =
  --   let !(m, n) = size arr
  --   in loop (m - 1) (>= 0) (subtract 1) acc $ \ !i !accO ->
  --        loop (n - 1) (>= 0) (subtract 1) accO $ \ !j !accI ->
  --          let !ix = (i, j)
  --          in f accI ix (unsafeIndex arr ix)
  -- {-# INLINE ifoldl #-}
  iter _ (m0, n0) (m1, n1) acc f =
    loop m0 (< m1) (+ 1) acc $ \ !i !acc0 ->
      loop n0 (< n1) (+ 1) acc0 $ \ !j !acc1 ->
        f (i, j) acc1
  {-# INLINE iter #-}
  iterateM_ _ !(m0, n0) !(m1, n1) f =
    loopM_ m0 (< m1) (+ 1) $ \ !i -> loopM_ n0 (< n1) (+ 1) $ \ !j -> f (i, j)
  {-# INLINE iterateM_ #-}
  iterateLinearM_ _ !sz@(_, n) !k0 !k1 f = do
    let !(si, sj) = fromLinearIndex sz k0
    let !(ei, ej) = fromLinearIndex sz (k1 - 1)
    if si == ei || k0 >= k1
      then loopM_ k0 (< k1) (+ 1) $ \ !k -> f k (si, sj + k - k0)
      else do
        loopM_ sj (< n) (+ 1) $ \ !j ->
          f (k0 + j - sj) (si, j)
        loopM_ (si + 1) (< ei) (+ 1) $ \ !i ->
          loopM_ 0 (< n) (+ 1) $ \ !j ->
            let !ix = (i, j)
            in f (toLinearIndex sz ix) ix
        let !ej' = ej + 1
        loopM_ 0 (< ej') (+ 1) $ \ !j -> f (k1 + j - ej') (ei, j)
  {-# INLINE iterateLinearM_ #-}
  -- iterateLinearM_ _ !sz !k0 !k1 f = do
  --   loopM_ k0 (<k1) (+1) $ \ !i ->
  --     f i (fromLinearIndex sz i)
  -- {-# INLINE iterateLinearM_ #-}


instance Iterator RowMajor DIM3 where
  iter it sIx eIx acc f =
    loop k0 (< k1) (+ 1) acc $ \ !i !acc0 ->
      iter it sIxL eIxL acc0 $ \ !ix acc1 -> f (consDim i ix) acc1
    where
      !(k0, sIxL) = unconsDim sIx
      !(k1, eIxL) = unconsDim eIx
  {-# INLINE iter #-}
  iterateM_ it !sIx !eIx f = do
    let (k0, sIxL) = unconsDim sIx
        (k1, eIxL) = unconsDim eIx
    loopM_ k0 (< k1) (+ 1) $ \ !i ->
      iterateM_ it sIxL eIxL $ \ !ix ->
        f (consDim i ix)
  {-# INLINE iterateM_ #-}
  iterateLinearM_ _ !sz !k0 !k1 f = do
    loopM_ k0 (<k1) (+1) $ \ !i ->
      f i (fromLinearIndex sz i)
  {-# INLINE iterateLinearM_ #-}
  -- iterateLinearM_ _ !sz !k0 !k1 f = do
  --   loopM_ k0 (<k1) (+1) $ \ !i ->
  --     f i (fromLinearIndex sz i)
  -- {-# INLINE iterateLinearM_ #-}



-- data ColumnMajor = ColumnMajor



-- instance Iterator ColumnMajor DIM2 where
--   -- ifoldr _ f acc arr =
--   --   let !(m, n) = size arr
--   --   in loop 0 (< n) (+ 1) acc $ \ !j !acc0 ->
--   --        loop 0 (< m) (+ 1) acc0 $ \ !i !acc1 ->
--   --          let !ix = (i, j)
--   --          in f ix (unsafeIndex arr ix) acc1
--   -- {-# INLINE ifoldr #-}
--   -- ifoldl _ f acc arr =
--   --   let !(m, n) = size arr
--   --   in loop (n - 1) (>= 0) (subtract 1) acc $ \ !j !acc0 ->
--   --        loop (m - 1) (>= 0) (subtract 1) acc0 $ \ !i !acc1 ->
--   --          let !ix = (i, j)
--   --          in f acc1 ix (unsafeIndex arr ix)
--   -- {-# INLINE ifoldl #-}
--   iterateM_ _ !(m0, n0) !(m1, n1) f =
--     loopM_ n0 (< n1) (+ 1) $ \ !i ->
--       loopM_ m0 (< m1) (+ 1) $ \ !j ->
--         f (i, j)
--   {-# INLINE iterateM_ #-}
--   iterateLinearM_  = undefined
--   --   let !(si, sj) = fromLinearIndex sz k0
--   --   let !(ei, ej) = fromLinearIndex sz (k1 - 1)
--   --   if si == ei || k0 >= k1
--   --     then loopM_ k0 (< k1) (+ 1) $ \ !k -> f k (si, sj + k - k0)
--   --     else do
--   --       loopM_ sj (< n) (+ 1) $ \ !j -> f (k0 + j - sj) (si, j)
--   --       loopM_ (si + 1) (< ei) (+ 1) $ \ !i ->
--   --         loopM_ 0 (< n) (+ 1) $ \ !j ->
--   --           let !ix = (i, j)
--   --           in f (toLinearIndex sz ix) ix
--   --       let !ej' = ej + 1
--   --       loopM_ 0 (< ej') (+ 1) $ \ !j -> f (k1 + j - ej') (ei, j)
--   -- {-# INLINE iterateLinearM_ #-}
