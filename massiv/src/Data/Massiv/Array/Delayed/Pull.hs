{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Massiv.Array.Delayed.Pull
-- Copyright   : (c) Alexey Kuleshevich 2018-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Delayed.Pull
  ( D(..)
  , Array(..)
  , delay
  , eqArrays
  , compareArrays
  , imap
  , liftArray2Matching
  , unsafeExtract
  , unsafeSlice
  , unsafeInnerSlice
  ) where

import           Control.Applicative
import qualified Data.Foldable as F
import           Data.Massiv.Array.Ops.Fold.Internal as A
import           Data.Massiv.Core.Common
import           Data.Massiv.Core.List (L, showArrayList, showsArrayPrec)
import           Data.Massiv.Core.Operations
import           Data.Massiv.Vector.Stream as S (steps)
import           GHC.Base (build)
import           Prelude hiding (zipWith)

#include "massiv.h"


-- | Delayed representation.
data D = D deriving Show


data instance Array D ix e = DArray { dComp :: !Comp
                                    , dSize :: !(Sz ix)
                                    , dIndex :: ix -> e }

instance (Ragged L ix e, Show e) => Show (Array D ix e) where
  showsPrec = showsArrayPrec id
  showList = showArrayList

instance Index ix => Shape D ix where
  maxLinearSize = Just . SafeSz . elemsCount
  {-# INLINE maxLinearSize #-}

instance Size D where
  size = dSize
  {-# INLINE size #-}

instance Resize D where
  unsafeResize !sz !arr =
    DArray (dComp arr) sz $ \ !ix ->
      unsafeIndex arr (fromLinearIndex (size arr) (toLinearIndex sz ix))
  {-# INLINE unsafeResize #-}

instance Strategy D where
  setComp c arr = arr { dComp = c }
  {-# INLINE setComp #-}
  getComp = dComp
  {-# INLINE getComp #-}

instance Source D e where
  unsafeIndex = INDEX_CHECK("(Source D ix e).unsafeIndex", size, dIndex)
  {-# INLINE unsafeIndex #-}

  unsafeOuterSlice !arr !szL !i = DArray (dComp arr) szL (unsafeIndex arr . consDim i)
  {-# INLINE unsafeOuterSlice #-}

  unsafeLinearSlice !o !sz arr =
    DArray (dComp arr) sz $ \ !i -> unsafeIndex arr (fromLinearIndex (size arr) (i + o))
  {-# INLINE unsafeLinearSlice #-}


-- | /O(1)/ - Extract a portion of an array. Staring index and new size are
-- not validated.
unsafeExtract :: (Source r e, Index ix) => ix -> Sz ix -> Array r ix e -> Array D ix e
unsafeExtract !sIx !newSz !arr =
  DArray (getComp arr) newSz (unsafeIndex arr . liftIndex2 (+) sIx)
{-# INLINE unsafeExtract #-}

-- | /O(1)/ - Take a slice out of an array from within
unsafeSlice :: (Source r e, Index ix, Index (Lower ix), Raises m) =>
  Array r ix e -> ix -> Sz ix -> Dim -> m (Array D (Lower ix) e)
unsafeSlice arr start cut@(SafeSz cutSz) dim = do
  newSz <- dropDimM cutSz dim
  return $ unsafeResize (SafeSz newSz) (unsafeExtract start cut arr)
{-# INLINE unsafeSlice #-}

-- | /O(1)/ - Take a slice out of an array from the inside
unsafeInnerSlice ::
     (Source r e, Index ix) => Array r ix e -> Sz (Lower ix) -> Int -> Array D (Lower ix) e
unsafeInnerSlice !arr szL !i = DArray (getComp arr) szL (unsafeIndex arr . (`snocDim` i))
{-# INLINE unsafeInnerSlice #-}



instance (Eq e, Index ix) => Eq (Array D ix e) where
  (==) = eqArrays (==)
  {-# INLINE (==) #-}

instance (Ord e, Index ix) => Ord (Array D ix e) where
  compare = compareArrays compare
  {-# INLINE compare #-}

instance Functor (Array D ix) where
  fmap f (DArray c sz g) = DArray c sz (f . g)
  {-# INLINE fmap #-}
  (<$) e (DArray c sz _) = DArray c sz (const e)
  {-# INLINE (<$) #-}


instance Index ix => Applicative (Array D ix) where
  pure = singleton
  {-# INLINE pure #-}
  (<*>) = liftArray2Matching id
  {-# INLINE (<*>) #-}
#if MIN_VERSION_base(4,10,0)
  liftA2 = liftArray2Matching
  {-# INLINE liftA2 #-}
#endif

-- | Row-major sequential folding over a Delayed array.
instance Index ix => Foldable (Array D ix) where
  fold = A.fold
  {-# INLINE fold #-}
  foldMap = A.foldMono
  {-# INLINE foldMap #-}
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
  length = totalElem . size
  {-# INLINE length #-}
  elem e = A.any (e ==)
  {-# INLINE elem #-}
  toList arr = build (\ c n -> foldrFB c n arr)
  {-# INLINE toList #-}


instance Index ix => Load D ix e where
  makeArray = DArray
  {-# INLINE makeArray #-}
  loadArrayWithST !scheduler !arr =
    splitLinearlyWith_ scheduler (elemsCount arr) (unsafeLinearIndex arr)
  {-# INLINE loadArrayWithST #-}

instance Index ix => StrideLoad D ix e

instance Index ix => Stream D ix e where
  toStream = S.steps
  {-# INLINE toStream #-}
  toStreamIx = S.steps . imap (,)
  {-# INLINE toStreamIx #-}

-- | Map an index aware function over an array
--
-- @since 0.1.0
imap :: forall r ix e a. (Index ix, Source r e) => (ix -> e -> a) -> Array r ix e -> Array D ix a
imap f !arr = DArray (getComp arr) (size arr) (\ !ix -> f ix (unsafeIndex arr ix))
{-# INLINE imap #-}


instance Num e => FoldNumeric D e where
  unsafeDotProduct = defaultUnsafeDotProduct
  {-# INLINE unsafeDotProduct #-}
  powerSumArray = defaultPowerSumArray
  {-# INLINE powerSumArray #-}
  foldArray = defaultFoldArray
  {-# INLINE foldArray #-}

instance Num e => Numeric D e where
  unsafeLiftArray f arr = arr {dIndex = f . dIndex arr}
  {-# INLINE unsafeLiftArray #-}
  unsafeLiftArray2 f a1 a2 = -- TODO: possibly use the first size, it is unsafe anyways.
    DArray (dComp a1 <> dComp a2) (SafeSz (liftIndex2 min (unSz (dSize a1)) (unSz (dSize a2)))) $ \i ->
      f (dIndex a1 i) (dIndex a2 i)
  {-# INLINE unsafeLiftArray2 #-}


instance Floating e => NumericFloat D e



-- | /O(1)/ Conversion from a source array to `D` representation.
delay :: (Index ix, Source r e) => Array r ix e -> Array D ix e
delay arr = DArray (getComp arr) (size arr) (unsafeIndex arr)
{-# INLINE [1] delay #-}

{-# RULES
"delay" [~1] forall (arr :: Array D ix e) . delay arr = arr
 #-}

-- | Compute array equality by applying a comparing function to each element.
--
-- @since 0.5.7
eqArrays :: (Index ix, Source r1 e1, Source r2 e2) =>
            (e1 -> e2 -> Bool) -> Array r1 ix e1 -> Array r2 ix e2 -> Bool
eqArrays f arr1 arr2 =
  (size arr1 == size arr2) &&
  not (A.any not
       (DArray (getComp arr1 <> getComp arr2) (size arr1) $ \ix ->
           f (unsafeIndex arr1 ix) (unsafeIndex arr2 ix)))
{-# INLINE eqArrays #-}

-- | Compute array ordering by applying a comparing function to each element.
-- The exact ordering is unspecified so this is only intended for use in maps and the like where
-- you need an ordering but do not care about which one is used.
--
-- @since 0.5.7
compareArrays :: (Index ix, Source r1 e1, Source r2 e2) =>
       (e1 -> e2 -> Ordering) -> Array r1 ix e1 -> Array r2 ix e2 -> Ordering
compareArrays f arr1 arr2 =
  compare (size arr1) (size arr2) <>
  A.fold
    (DArray (getComp arr1 <> getComp arr2) (size arr1) $ \ix ->
       f (unsafeIndex arr1 ix) (unsafeIndex arr2 ix))
{-# INLINE compareArrays #-}


liftArray2Matching
  :: (HasCallStack, Index ix, Source r1 a, Source r2 b)
  => (a -> b -> e) -> Array r1 ix a -> Array r2 ix b -> Array D ix e
liftArray2Matching f !arr1 !arr2
  | sz1 == sz2 =
    DArray
      (getComp arr1 <> getComp arr2)
      sz1
      (\ !ix -> f (unsafeIndex arr1 ix) (unsafeIndex arr2 ix))
  | otherwise = raiseImprecise $ SizeMismatchException (size arr1) (size arr2)
  where
    sz1 = size arr1
    sz2 = size arr2
{-# INLINE liftArray2Matching #-}

