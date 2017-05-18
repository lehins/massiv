{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Array.Massiv.Delayed
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Delayed where

import           Control.Monad                  (void)
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Common.Shape
import           Data.Array.Massiv.Ops.Fold
import           Data.Array.Massiv.Scheduler
import           Data.Foldable
import           GHC.Base                       (build)



-- | Delayed representation.
data D


data instance Array D ix e = DArray { dSize :: !ix
                                    , dUnsafeIndex :: ix -> e }



-- | /O(1)/ Conversion from a source array to `D` representation.
delay :: Source r ix e => Array r ix e -> Array D ix e
delay arr = DArray (size arr) (unsafeIndex arr)
{-# INLINE delay #-}


instance Index ix => Massiv D ix e where
  size = dSize
  {-# INLINE size #-}

  makeArray = DArray . liftIndex (max 0)
  {-# INLINE makeArray #-}


instance Index ix => Source D ix e where
  unsafeIndex = dUnsafeIndex
  {-# INLINE unsafeIndex #-}

instance Index ix => Shape D ix e where
  unsafeReshape !sz !arr =
    DArray sz $ \ !ix ->
      unsafeIndex arr (fromLinearIndex (size arr) (toLinearIndex sz ix))
  {-# INLINE unsafeReshape #-}

  unsafeExtract !sIx !newSz !arr =
    DArray newSz $ \ !ix ->
      unsafeIndex arr (liftIndex2 (+) ix sIx)
  {-# INLINE unsafeExtract #-}


instance (Index ix, Index (Lower ix)) => Slice D ix e where

  (!?>) !arr !i
    | isSafeIndex m i = Just (DArray szL (\ !ix -> unsafeIndex arr (consDim i ix)))
    | otherwise = Nothing
    where
      !(m, szL) = unconsDim (size arr)
  {-# INLINE (!?>) #-}

  (<!?) !arr !i
    | isSafeIndex m i = Just (DArray szL (\ !ix -> unsafeIndex arr (snocDim ix i)))
    | otherwise = Nothing
    where
      !(szL, m) = unsnocDim (size arr)
  {-# INLINE (<!?) #-}


instance (Eq e, Index ix) => Eq (Array D ix e) where
  (==) (DArray sz1 uIndex1) (DArray sz2 uIndex2) =
    sz1 == sz2 && foldlS (&&) True (DArray sz1 (\ !ix -> uIndex1 ix == uIndex2 ix))


instance Functor (Array D ix) where
  fmap f (DArray sz g) = DArray sz (f . g)
  {-# INLINE fmap #-}


instance Index ix => Applicative (Array D ix) where
  pure a = DArray (liftIndex (+ 1) zeroIndex) (const a)
  (<*>) (DArray sz1 uIndex1) (DArray sz2 uIndex2) =
    DArray (liftIndex2 (*) sz1 sz2) $ \ !ix ->
      (uIndex1 (liftIndex2 mod ix sz1)) (uIndex2 (liftIndex2 mod ix sz2))


-- | Row-major folding over a delayed array.
instance Index ix => Foldable (Array D ix) where
  foldl = lazyFoldlS
  {-# INLINE foldl #-}
  foldl' = foldlS
  {-# INLINE foldl' #-}
  foldr = foldrFB
  {-# INLINE foldr #-}
  foldr' = foldrS
  {-# INLINE foldr' #-}
  null (DArray sz _) = totalElem sz == 0
  {-# INLINE null #-}
  sum = foldl' (+) 0
  {-# INLINE sum #-}
  product = foldl' (*) 1
  {-# INLINE product #-}
  length = totalElem . size
  {-# INLINE length #-}
  toList arr = build (\ c n -> foldrFB c n arr)
  {-# INLINE toList #-}


instance Index ix => Load D ix where
  loadS (DArray sz f) _ unsafeWrite =
    iterM_ zeroIndex sz 1 (<) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (f ix)
  {-# INLINE loadS #-}
  loadP wIds (DArray sz f) _ unsafeWrite = do
    void $ splitWork wIds sz $ \ !scheduler !chunkLength !totalLength !slackStart -> do
      loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
        submitRequest scheduler $
        JobRequest 0 $
        iterLinearM_ sz start (start + chunkLength) 1 (<) $ \ !k !ix -> do
          unsafeWrite k $ f ix
      submitRequest scheduler $
        JobRequest 0 $
        iterLinearM_ sz slackStart totalLength 1 (<) $ \ !k !ix -> do
          unsafeWrite k (f ix)
  {-# INLINE loadP #-}



singleton :: Index ix => e -> Array D ix e
singleton !e = DArray (liftIndex (+ 1) zeroIndex) (const e)
{-# INLINE singleton #-}

liftArray :: Source r ix b => (b -> e) -> Array r ix b -> Array D ix e
liftArray f !arr = DArray (size arr) (f . unsafeIndex arr)
{-# INLINE liftArray #-}

-- | Similar to @zipWith@, except dimensions of both arrays either have to be the
-- same, or at least one of two array must be a singleton array, in which
-- case it will behave as @fmap@.
liftArray2
  :: (Source r1 ix a, Source r2 ix b)
  => (a -> b -> e) -> Array r1 ix a -> Array r2 ix b -> Array D ix e
liftArray2 f !arr1 !arr2
  | sz1 == oneIndex = liftArray (f (unsafeIndex arr1 zeroIndex)) arr2
  | sz2 == oneIndex = liftArray (`f` (unsafeIndex arr2 zeroIndex)) arr1
  | sz1 == sz2 =
    DArray sz1 (\ !ix -> f (unsafeIndex arr1 ix) (unsafeIndex arr2 ix))
  | otherwise =
    error $
    "Array dimensions must be the same, instead got: " ++
    show arr1 ++ " and " ++ show arr2
  where
    sz1 = size arr1
    sz2 = size arr2
    !oneIndex = liftIndex (+ 1) zeroIndex
{-# INLINE liftArray2 #-}


instance (Index ix, Num e) => Num (Array D ix e) where
  (+)         = liftArray2 (+)
  {-# INLINE (+) #-}
  (-)         = liftArray2 (-)
  {-# INLINE (-) #-}
  (*)         = liftArray2 (*)
  {-# INLINE (*) #-}
  abs         = liftArray abs
  {-# INLINE abs #-}
  signum      = liftArray signum
  {-# INLINE signum #-}
  fromInteger = singleton . fromInteger
  {-# INLINE fromInteger #-}

instance (Index ix, Fractional e) => Fractional (Array D ix e) where
  (/)          = liftArray2 (/)
  {-# INLINE (/) #-}
  fromRational = singleton . fromRational
  {-# INLINE fromRational #-}


instance (Index ix, Floating e) => Floating (Array D ix e) where
  pi    = singleton pi
  {-# INLINE pi #-}
  exp   = liftArray exp
  {-# INLINE exp #-}
  log   = liftArray log
  {-# INLINE log #-}
  sin   = liftArray sin
  {-# INLINE sin #-}
  cos   = liftArray cos
  {-# INLINE cos #-}
  asin  = liftArray asin
  {-# INLINE asin #-}
  atan  = liftArray atan
  {-# INLINE atan #-}
  acos  = liftArray acos
  {-# INLINE acos #-}
  sinh  = liftArray sinh
  {-# INLINE sinh #-}
  cosh  = liftArray cosh
  {-# INLINE cosh #-}
  asinh = liftArray asinh
  {-# INLINE asinh #-}
  atanh = liftArray atanh
  {-# INLINE atanh #-}
  acosh = liftArray acosh
  {-# INLINE acosh #-}
