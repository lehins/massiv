{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Data.Massiv.Array.Delayed.Internal
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Delayed.Internal
  ( D(..)
  , Array(..)
  , delay
  , eq
  , singleton
  , liftArray
  , liftArray2
  ) where

import           Data.Foldable              (Foldable (..))
import           Data.Massiv.Array.Ops.Fold as M
import           Data.Massiv.Core
import           Data.Massiv.Core.Scheduler
import           Data.Monoid                ((<>))
import           GHC.Base                   (build)
import           Prelude                    hiding (zipWith)

-- | Delayed representation.
data D = D deriving Show


data instance Array D ix e = DArray { dComp :: !Comp
                                    , dSize :: !ix
                                    , dUnsafeIndex :: ix -> e }


instance Index ix => Construct D ix e where
  size = dSize
  {-# INLINE size #-}

  getComp = dComp
  {-# INLINE getComp #-}

  setComp c arr = arr { dComp = c }
  {-# INLINE setComp #-}

  unsafeMakeArray = DArray
  {-# INLINE unsafeMakeArray #-}


instance Index ix => Source D ix e where
  unsafeIndex = dUnsafeIndex
  {-# INLINE unsafeIndex #-}

instance Index ix => Shape D ix e where
  unsafeReshape !sz !arr =
    DArray (getComp arr) sz $ \ !ix ->
      unsafeIndex arr (fromLinearIndex (size arr) (toLinearIndex sz ix))
  {-# INLINE unsafeReshape #-}

  unsafeExtract !sIx !newSz !arr =
    DArray (getComp arr) newSz $ \ !ix ->
      unsafeIndex arr (liftIndex2 (+) ix sIx)
  {-# INLINE unsafeExtract #-}


instance (Index ix, Index (Lower ix)) => Slice D ix e where

  (!?>) !arr !i
    | isSafeIndex m i = Just (DArray (getComp arr) szL (\ !ix -> unsafeIndex arr (consDim i ix)))
    | otherwise = Nothing
    where
      !(m, szL) = unconsDim (size arr)
  {-# INLINE (!?>) #-}

  (<!?) !arr !i
    | isSafeIndex m i = Just (DArray (getComp arr) szL (\ !ix -> unsafeIndex arr (snocDim ix i)))
    | otherwise = Nothing
    where
      !(szL, m) = unsnocDim (size arr)
  {-# INLINE (<!?) #-}


instance (Eq e, Index ix) => Eq (Array D ix e) where
  (==) = eq (==)
  {-# INLINE (==) #-}


instance Functor (Array D ix) where
  fmap f (DArray c sz g) = DArray c sz (f . g)
  {-# INLINE fmap #-}


instance Index ix => Applicative (Array D ix) where
  pure a = DArray Seq (liftIndex (+ 1) zeroIndex) (const a)
  {-# INLINE pure #-}
  (<*>) (DArray c1 sz1 uIndex1) (DArray c2 sz2 uIndex2) =
    DArray (c1 <> c2) (liftIndex2 min sz1 sz2) $ \ !ix ->
      (uIndex1 ix) (uIndex2 ix)
  {-# INLINE (<*>) #-}


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
  null (DArray _ sz _) = totalElem sz == 0
  {-# INLINE null #-}
  sum = foldl' (+) 0
  {-# INLINE sum #-}
  product = foldl' (*) 1
  {-# INLINE product #-}
  length = totalElem . size
  {-# INLINE length #-}
  toList arr = build (\ c n -> foldrFB c n arr)
  {-# INLINE toList #-}


instance Index ix => Load D ix e where
  loadS (DArray _ sz f) _ unsafeWrite =
    iterM_ zeroIndex sz 1 (<) $ \ !ix ->
      unsafeWrite (toLinearIndex sz ix) (f ix)
  {-# INLINE loadS #-}
  loadP wIds (DArray _ sz f) _ unsafeWrite = do
    divideWork_ wIds sz $ \ !scheduler !chunkLength !totalLength !slackStart -> do
      loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
        scheduleWork scheduler $
        iterLinearM_ sz start (start + chunkLength) 1 (<) $ \ !k !ix -> do
          unsafeWrite k $ f ix
      scheduleWork scheduler $
        iterLinearM_ sz slackStart totalLength 1 (<) $ \ !k !ix -> do
          unsafeWrite k (f ix)
  {-# INLINE loadP #-}


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



-- | /O(1)/ Conversion from a source array to `D` representation.
delay :: Source r ix e => Array r ix e -> Array D ix e
delay arr = DArray (getComp arr) (size arr) (unsafeIndex arr)
{-# INLINE delay #-}


-- | /O(n1 + n2)/ - Compute array equality by applying a comparing function to each element.
eq :: (Source r1 ix e1, Source r2 ix e2) =>
      (e1 -> e2 -> Bool) -> Array r1 ix e1 -> Array r2 ix e2 -> Bool
eq f arr1 arr2 =
  (size arr1 == size arr2) &&
  M.fold
    (&&)
    True
    (DArray (getComp arr1 <> getComp arr2) (size arr1) $ \ix ->
       f (unsafeIndex arr1 ix) (unsafeIndex arr2 ix))
{-# INLINE eq #-}



singleton :: Index ix => e -> Array D ix e
singleton !e = DArray Seq (liftIndex (+ 1) zeroIndex) (const e)
{-# INLINE singleton #-}

liftArray :: Source r ix b => (b -> e) -> Array r ix b -> Array D ix e
liftArray f !arr = DArray (getComp arr) (size arr) (f . unsafeIndex arr)
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
    DArray (getComp arr1) sz1 (\ !ix -> f (unsafeIndex arr1 ix) (unsafeIndex arr2 ix))
  | otherwise =
    error $
    "Array dimensions must be the same, instead got: " ++
    show (size arr1) ++ " and " ++ show (size arr2)
  where
    sz1 = size arr1
    sz2 = size arr2
{-# INLINE liftArray2 #-}
