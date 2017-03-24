{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Data.Array.Massiv.Delayed.DIM2
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Delayed.DIM2 where

import           Data.Array.Massiv.Common



make2D :: (Int, Int) -> ((Int, Int) -> e) -> Array D DIM2 e
make2D !(m, n) = DArray2D m n
{-# INLINE make2D #-}



map2D :: Source r DIM2 => (b -> e) -> Array r DIM2 b -> Array D DIM2 e
map2D f !arr = DArray2D m n (f . g)
  where
    !(m, n) = size arr
    g = unsafeIndex arr
{-# INLINE map2D #-}


imap2D :: Source r DIM2 => ((Int, Int) -> a -> e) -> Array r DIM2 a -> Array D DIM2 e
imap2D f !arr = DArray2D m n (\ !ix -> f ix (g ix))
  where
    !(m, n) = size arr
    g = unsafeIndex arr
{-# INLINE imap2D #-}


zipWith2D :: (Source r1 DIM2, Source r2 DIM2)
          => (a -> b -> e) -> Array r1 DIM2 a -> Array r2 DIM2 b -> Array D DIM2 e
zipWith2D f !arr1 !arr2 =
  DArray2D
    (min m1 m2)
    (min n1 n2)
    (\ !ix -> f ((unsafeIndex arr1) ix) ((unsafeIndex arr2) ix))
  where
    !(m1, n1) = size arr1
    !(m2, n2) = size arr2
{-# INLINE zipWith2D #-}


izipWith2D :: (Source r1 DIM2, Source r2 DIM2)
          => ((Int, Int) -> a -> b -> e)
          -> Array r1 DIM2 a -> Array r2 DIM2 b -> Array D DIM2 e
izipWith2D f !arr1 !arr2 =
  DArray2D
    (min m1 m2)
    (min n1 n2)
    (\ !ix -> f ix ((unsafeIndex arr1) ix) ((unsafeIndex arr2) ix))
  where
    !(m1, n1) = size arr1
    !(m2, n2) = size arr2
{-# INLINE izipWith2D #-}



--unsafeTraverse sz f arr = makeA sz (f (unsafeIndex arr))

unsafeTraverse2D :: Source r DIM2 =>
                   (Int, Int)
                -> (((Int, Int) -> a) -> (Int, Int) -> e)
                -> Array r DIM2 a
                -> Array D DIM2 e
unsafeTraverse2D !(m, n) f arr = DArray2D m n $ f (unsafeIndex arr)
{-# INLINE unsafeTraverse2D #-}


traverse2D :: Source r DIM2 => (Int, Int)
          -> (((Int, Int) -> e1) -> (Int, Int) -> e)
          -> Array r DIM2 e1
          -> Array D DIM2 e
traverse2D !(m, n) f arr = DArray2D m n (f (index arr))
{-# INLINE traverse2D #-}


foldr2D :: Source r DIM2 => (a -> b -> b) -> b -> Array r DIM2 a -> b
foldr2D f !acc !arr =
  loop 0 (<m) (+1) acc $ \ !i !accO ->
    loop 0 (<n) (+1) accO $ \ !j !accI -> f ((unsafeIndex arr) (i, j)) accI
  where !(m, n) = size arr
{-# INLINE foldr2D #-}


foldl2D :: Source r DIM2 => (b -> a -> b) -> b -> Array r DIM2 a -> b
foldl2D f !acc !arr =
  loop (m-1) (>=0) (subtract 1) acc $ \ !i !accO ->
    loop (n-1) (>=0) (subtract 1) accO $ \ !j !accI -> f accI (g (i, j))
  where !(m, n) = size arr
        g = unsafeIndex arr
{-# INLINE foldl2D #-}


sum2D :: (Source r DIM2, Num a) => Array r DIM2 a -> a
sum2D = foldr2D (+) 0
{-# INLINE sum2D #-}

-- computeS :: forall v e . VG.Vector v e => Array D DIM2 e -> Array D DIM2 e
-- computeS (DArray2D sz f) =
--   MArray2D sz $ VG.create generateArray
--   where
--     !(m, n) = checkDims2D "makeArrayVG" sz
--     generateArray :: ST s ((VG.Mutable v) s e)
--     generateArray = do
--       mv <- MVG.unsafeNew (m * n)
--       loopM_ 0 (< m) (+ 1) $ \ !i -> do
--         loopM_ 0 (< n) (+ 1) $ \ !j -> do
--           MVG.unsafeWrite mv (fromIx n (i, j)) (f (i, j))
--       return mv
--     {-# INLINE generateArray #-}
-- computeS !arr = arr
-- {-# INLINE computeS #-}


-- fromLists2D :: VG.Vector v p => [[p]] -> Array2D p
-- fromLists2D !ls =
--   if all (== n) (fmap length ls)
--     then MArray2D (m, n) . VG.fromList . concat $ ls
--     else error2D "fromListsVG" "Inner lists are of different lengths."
--   where
--     (m, n) =
--       checkDims2D "fromListsVG" (length ls, maybe 0 length $ listToMaybe ls)
-- {-# INLINE fromLists2D #-}



error2D :: String -> String -> a
error2D fName errMsg =
  error $ "Graphics.Image.Repr.Native.Generic." ++ fName ++ ": " ++ errMsg

checkDims2D :: String -> (Int, Int) -> (Int, Int)
checkDims2D fName = checkDims ("Graphics.Image.Repr.Native.Generic." ++ fName)
{-# INLINE checkDims2D #-}


checkDims :: String -> (Int, Int) -> (Int, Int)
checkDims err !sz@(m, n)
  | m <= 0 || n <= 0 =
    error $
    show err ++ ": dimensions are expected to be positive: " ++ show sz
  | otherwise = sz
{-# INLINE checkDims #-}



-- zipWith2D :: Ix ix =>
--             (t1 -> t -> e) -> Array ix t1 -> Array ix t -> Array ix e
-- zipWith2D f (Array sz1 g1) (Array sz2 g2) = Array (intersectIx sz1 sz2) (\ix -> f (g1 ix) (g2 ix))


-- traverse2D :: (ix1 -> ix) -> ((ix1 -> e1) -> ix -> e) -> Array ix1 e1 -> Array ix e
-- traverse2D ixF f (Array sz g) = Array (ixF sz) $ f g



-- class Ix ix where

--   type Lower ix :: *

--   type Higher ix :: *

--   intersectIx :: ix -> ix -> ix

--   lower :: ix -> (Int, Lower ix)

--   higher :: ix -> Int -> Higher ix

--   -- | Size -> Index -> Flat index
--   toFlat :: ix -> ix -> Int

--   -- | Size -> FLat Index -> Index

-- data Z = Z

-- instance Ix Int where

--   type Lower Int = Z

--   type Higher Int = (Int, Int)

--   intersectIx k1 k2 = min k1 k2

--   lower k = (k, Z)

--   higher i j = (i, j)

--   toFlat _ = id


-- instance Ix (Int, Int) where

--   type Lower (Int, Int) = Int

--   type Higher (Int, Int) = (Int, Int, Int)

--   intersectIx (i1, j1) (i2, j2) = (min i1 i2, min j1 j2)

--   lower = id

--   toFlat (_, n) (i, j) = n*i + j

-- data RGB e = PixelRGB e e e

-- type family Pixel (p :: * -> *) e :: *

-- type instance Pixel RGB e = e




-- -- | Immutable, shape polymorphic array representation.
-- class IArray a i p e where

--   makeWindowedI2D :: i -> i -> i -> (i -> p e) -> (i -> p e) -> a i p e

--   scalarI2D :: e -> a i p e

--   mapE :: IArray a i p e1 => (e1 -> e) -> a i p1 e1 -> a i p e

--   mapI2D :: IArray a i p1 e1 => (p1 e1 -> p2 e) -> a i p1 e1 -> a i p e

--   imapI2D :: IArray a i p1 e1 => (i -> p1 e1 -> p e) -> a i p1 e1 -> a i p e

--   zipWithE :: (IArray a i p e1, IArray a i p e2) =>
--     (e1 -> e2 -> e) -> a i p e1 -> a i p e2 -> a i p e

--   zipWithI2D :: (IArray a i p1 e1, IArray a i p2 e2) =>
--     (p1 e1 -> p2 e2 -> p e) -> a i p1 e1 -> a i p2 e2 -> a i p e

--   foldlI2D :: (b -> p e -> b) -> b -> a i p e -> p e

-- class NumArray a i p e where

--   width :: a i p e -> Int

--   plus2D :: e -> a i p e -> a i p e

--   plusA2 :: a i p e -> a i p e -> a i p e

--   stencil :: a i p e



-- data Array3 ix p e = Array3 (Array ix e) (Array ix e) (Array ix e)


-- data family Array repr ix e :: *

-- data D

-- data VG (v :: * -> *)

-- data instance Array D Int e = DArray1D Int (Int -> e)

-- data instance Array (VG v) Int e = MArray1D (v e)

-- size :: Array repr ix e -> ix
-- size (MArray1D v) = VG.length v
