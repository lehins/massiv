{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
-- |
-- Module      : Data.Array.Massiv.Manifest
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Manifest where

import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Common.Shape

import           Data.Foldable


-- | Manifest arrays are backed by actual memory and values are looked up versus
-- computed as it is with delayed arrays. Because of this fact indexing functions
-- `(!)`, `(!?)`, etc. are constrained to manifest arrays only.
class Source r ix e => Manifest r ix e where



-- | Manifest representation
data M

data instance Array M ix e = MArray { mSize :: !ix
                                    , mUnsafeLinearIndex :: Int -> e }

instance Index ix => Massiv M ix where
  size = mSize
  {-# INLINE size #-}

instance Massiv M ix => Foldable (Array M ix) where
  foldr f acc (MArray sz g) =
    loop (totalElem sz - 1) (>= 0) (subtract 1) acc $ \i accI -> f (g i) accI
  {-# INLINE foldr #-}
  foldr' f !acc (MArray sz g) =
    loop (totalElem sz - 1) (>= 0) (subtract 1) acc $ \ !i !accI -> f (g i) accI
  {-# INLINE foldr' #-}
  foldl f acc (MArray sz g) =
    loop 0 (< totalElem sz) (+ 1) acc $ \i accI -> f accI (g i)
  {-# INLINE foldl #-}
  foldl' f !acc (MArray sz g) =
    loop 0 (< totalElem sz) (+ 1) acc $ \ !i !accI -> f accI (g i)
  {-# INLINE foldl' #-}
  length = totalElem . size
  {-# INLINE length #-}



instance Index ix => Source M ix e where
  unsafeLinearIndex = mUnsafeLinearIndex
  {-# INLINE unsafeLinearIndex #-}


instance Index ix => Manifest M ix e


instance Index ix => Shape M ix e where

  unsafeReshape !sz !arr = arr { mSize = sz }
  {-# INLINE unsafeReshape #-}

  unsafeExtract !sIx !newSz !arr =
    MArray newSz $ \ !i ->
      unsafeIndex arr (liftIndex2 (+) (fromLinearIndex newSz i) sIx)
  {-# INLINE unsafeExtract #-}


instance (Index ix, Index (Lower ix)) => Slice M ix e where

  (!?>) !arr !i
    | isSafeIndex m i = Just (MArray szL (\ !k -> unsafeLinearIndex arr (k + kStart)))
    | otherwise = Nothing
    where
      !sz = size arr
      !(m, szL) = unconsDim sz
      !kStart = toLinearIndex sz (consDim i (zeroIndex :: Lower ix))
  {-# INLINE (!?>) #-}

  (<!?) !arr !i
    | isSafeIndex m i = Just (MArray szL (\ !k -> unsafeLinearIndex arr (k * m + kStart)))
    | otherwise = Nothing
    where
      !sz = size arr
      !(szL, m) = unsnocDim sz
      !kStart = toLinearIndex sz (snocDim (zeroIndex :: Lower ix) i)
  {-# INLINE (<!?) #-}


-- | _O(1)_ Conversion of manifest arrays to `M` representation.
toManifest :: Manifest r ix e => Array r ix e -> Array M ix e
toManifest arr = MArray (size arr) (unsafeLinearIndex arr) where
{-# INLINE toManifest #-}


(!) :: Manifest r ix e => Array r ix e -> ix -> e
(!) = index
{-# INLINE (!) #-}

(!?) :: Manifest r ix e => Array r ix e -> ix -> Maybe e
(!?) = maybeIndex
{-# INLINE (!?) #-}

(?) :: Manifest r ix e => Maybe (Array r ix e) -> ix -> Maybe e
(?) Nothing _      = Nothing
(?) (Just arr) !ix = arr !? ix
{-# INLINE (?) #-}



maybeIndex :: Manifest r ix e => Array r ix e -> ix -> Maybe e
maybeIndex !arr = handleBorderIndex (Fill Nothing) (size arr) (Just . unsafeIndex arr)
{-# INLINE maybeIndex #-}


defaultIndex :: Manifest r ix e => e -> Array r ix e -> ix -> e
defaultIndex defVal = borderIndex (Fill defVal)
{-# INLINE defaultIndex #-}


borderIndex :: Manifest r ix e => Border e -> Array r ix e -> ix -> e
borderIndex border !arr = handleBorderIndex border (size arr) (unsafeIndex arr)
{-# INLINE borderIndex #-}

index :: Manifest r ix e => Array r ix e -> ix -> e
index !arr !ix = borderIndex (Fill (errorIx "index" arr ix)) arr ix
{-# INLINE index #-}



-- instance Source M (Int, Int, Int, Int) where
--   type Elt M (Int, Int, Int, Int) e = Array D DIM3 e
--   unsafeLinearIndex = mUnsafeLinearIndex
--   {-# INLINE unsafeLinearIndex #-}
--   (!?) !arr !i
--     | isSafeIndex m i = Just (DArray szL (\ !ixL -> unsafeIndex arr (consDim i ixL)))
--     | otherwise = Nothing
--     where
--       !(m, szL) = unconsDim (size arr)
--   {-# INLINE (!?) #-}



-- makeA :: (Int, Int) -> ((Int, Int) -> e) -> Array D DIM2 e
-- makeA !(m, n) = DArray2D m n
-- {-# INLINE makeA #-}


-- sizeA :: Array D DIM2 e -> (Int, Int)
-- sizeA (DArray2D m n _) = (m, n)
-- {-# INLINE sizeA #-}



-- indexA :: Array D DIM2 e -> (Int, Int) -> e
-- indexA !arr !ix
--   | isValidIx (sizeA arr) ix = unsafeIndexA arr ix
--   | otherwise = error $ "Index out of bounds: " ++ show ix ++ " for array: " ++ show arr
-- {-# INLINE indexA #-}


-- mapA :: (b -> e) -> Array D DIM2 b -> Array D DIM2 e
-- mapA f (DArray2D m n g) = DArray2D m n (f . g)
-- {-# INLINE mapA #-}


-- imapA :: ((Int, Int) -> a -> e) -> Array D DIM2 a -> Array D DIM2 e
-- imapA f (DArray2D m n g) = DArray2D m n (\ !ix -> f ix (g ix))
-- {-# INLINE imapA #-}


-- zipWithA :: (a -> b -> e) -> Array D DIM2 a -> Array D DIM2 b -> Array D DIM2 e
-- zipWithA f (DArray2D m1 n1 g1) (DArray2D m2 n2 g2) =
--   DArray2D (min m1 m2) (min n1 n2) (\ !ix -> f (g1 ix) (g2 ix))
-- {-# INLINE zipWithA #-}


-- izipWithA :: ((Int, Int) -> a -> b -> e)
--           -> Array D DIM2 a -> Array D DIM2 b -> Array D DIM2 e
-- izipWithA f (DArray2D m1 n1 g1) (DArray2D m2 n2 g2) =
--   DArray2D (min m1 m2) (min n1 n2) (\ !ix -> f ix (g1 ix) (g2 ix))
-- {-# INLINE izipWithA #-}


-- unsafeTraverseA :: (Int, Int)
--                 -> (((Int, Int) -> a) -> (Int, Int) -> e)
--                 -> Array D DIM2 a
--                 -> Array D DIM2 e
-- unsafeTraverseA !(m, n) f (DArray2D _ _ g) = DArray2D m n $ f g
-- {-# INLINE unsafeTraverseA #-}


-- traverseA :: (Int, Int)
--           -> (((Int, Int) -> e1) -> (Int, Int) -> e)
--           -> Array D DIM2 e1
--           -> Array D DIM2 e
-- traverseA !(m, n) f arr = DArray2D m n (f (indexA arr))
-- {-# INLINE traverseA #-}


-- foldrA :: (a -> b -> b) -> b -> Array D DIM2 a -> b
-- foldrA f !acc (DArray2D m n g) =
--   loop 0 (<m) (+1) acc $ \ !i !accO ->
--     loop 0 (<n) (+1) accO $ \ !j !accI -> f (g (i, j)) accI
-- {-# INLINE foldrA #-}


-- foldlA :: (b -> a -> b) -> b -> Array D DIM2 a -> b
-- foldlA f !acc (DArray2D m n g) =
--   loop (m-1) (>=0) (subtract 1) acc $ \ !i !accO ->
--     loop (n-1) (>=0) (subtract 1) accO $ \ !j !accI -> f accI (g (i, j))
-- {-# INLINE foldlA #-}


-- sumA :: Num a => Array D DIM2 a -> a
-- sumA = foldrA (+) 0
-- {-# INLINE sumA #-}



-- -- fromListsA :: VG.Vector v p => [[p]] -> Array2D p
-- -- fromListsA !ls =
-- --   if all (== n) (fmap length ls)
-- --     then MArray2D (m, n) . VG.fromList . concat $ ls
-- --     else errorA "fromListsVG" "Inner lists are of different lengths."
-- --   where
-- --     (m, n) =
-- --       checkDimsA "fromListsVG" (length ls, maybe 0 length $ listToMaybe ls)
-- -- {-# INLINE fromListsA #-}



-- intersectIx :: (Int, Int) -> (Int, Int) -> (Int, Int)
-- intersectIx !(i1, j1) !(i2, j2) = (min i1 i2, min j1 j2)
-- {-# INLINE intersectIx #-}

-- fromIx :: Int -- ^ @n@ columns
--        -> (Int, Int) -- ^ @(i, j)@ row, column index
--        -> Int -- ^ Flat vector index
-- fromIx !n !(i, j) = n * i + j
-- {-# INLINE fromIx #-}

-- toIx :: Int -- ^ @n@ columns
--      -> Int -- ^ Flat vector index
--      -> (Int, Int) -- ^ @(i, j)@ row, column index
-- toIx !n !k = divMod k n
-- {-# INLINE toIx #-}

-- isValidIx :: (Int, Int) -> (Int, Int) -> Bool
-- isValidIx !(m, n) !(i, j) = 0 < i && 0 < j && i <= m && j <= n
-- {-# INLINE isValidIx #-}


-- -- | Very efficient loop
-- loop :: t -> (t -> Bool) -> (t -> t) -> a -> (t -> a -> a) -> a
-- loop !init' condition increment !initAcc f = go init' initAcc where
--   go !step !acc =
--     case condition step of
--       False -> acc
--       True  -> go (increment step) (f step acc)
-- {-# INLINE loop #-}



-- errorA :: String -> String -> a
-- errorA fName errMsg =
--   error $ "Graphics.Image.Repr.Native.Generic." ++ fName ++ ": " ++ errMsg
