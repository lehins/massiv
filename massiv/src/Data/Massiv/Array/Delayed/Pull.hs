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
  ) where

import Control.Applicative
import qualified Data.Foldable as F
import Data.Massiv.Array.Ops.Fold.Internal as A
import Data.Massiv.Vector.Stream as S (steps)
import Data.Massiv.Core.Common
import Data.Massiv.Core.Operations
import Data.Massiv.Core.List (L, showArrayList, showsArrayPrec)
import GHC.Base (build)
import Prelude hiding (zipWith)

#include "massiv.h"

-- | Delayed representation.
data D = D deriving Show

data instance Array D ix e = DArray { dComp :: !Comp
                                    , dSize :: !(Sz ix)
                                    , dIndex :: ix -> e }

instance (Ragged L ix e, Show e) => Show (Array D ix e) where
  showsPrec = showsArrayPrec id
  showList = showArrayList

instance Index ix => Resize D ix where
  unsafeResize !sz !arr =
    DArray (dComp arr) sz $ \ !ix ->
      unsafeIndex arr (fromLinearIndex (size arr) (toLinearIndex sz ix))
  {-# INLINE unsafeResize #-}

instance Index ix => Extract D ix e where
  unsafeExtract !sIx !newSz !arr =
    DArray (dComp arr) newSz $ \ !ix ->
      unsafeIndex arr (liftIndex2 (+) ix sIx)
  {-# INLINE unsafeExtract #-}


instance Index ix => Construct D ix e where
  setComp c arr = arr { dComp = c }
  {-# INLINE setComp #-}

  makeArray = DArray
  {-# INLINE makeArray #-}


instance Index ix => Source D ix e where
  unsafeIndex = INDEX_CHECK("(Source D ix e).unsafeIndex", size, dIndex)
  {-# INLINE unsafeIndex #-}
  unsafeLinearSlice !o !sz arr =
    DArray (dComp arr) sz $ \ !i -> unsafeIndex arr (fromLinearIndex (size arr) (i + o))
  {-# INLINE unsafeLinearSlice #-}


instance ( Index ix
         , Index (Lower ix)
         , Elt D ix e ~ Array D (Lower ix) e
         ) =>
         Slice D ix e where
  unsafeSlice arr start cut@(SafeSz cutSz) dim = do
    newSz <- dropDimM cutSz dim
    return $ unsafeResize (SafeSz newSz) (unsafeExtract start cut arr)
  {-# INLINE unsafeSlice #-}


instance (Elt D ix e ~ Array D (Lower ix) e, Index ix) => OuterSlice D ix e where

  unsafeOuterSlice !arr !i =
    DArray (dComp arr) (snd (unconsSz (size arr))) (\ !ix -> unsafeIndex arr (consDim i ix))
  {-# INLINE unsafeOuterSlice #-}

instance (Elt D ix e ~ Array D (Lower ix) e, Index ix) => InnerSlice D ix e where

  unsafeInnerSlice !arr (szL, _) !i =
    DArray (dComp arr) szL (\ !ix -> unsafeIndex arr (snocDim ix i))
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
  liftA2 = liftArray2Matching
  {-# INLINE liftA2 #-}


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
  size = dSize
  {-# INLINE size #-}
  getComp = dComp
  {-# INLINE getComp #-}
  loadArrayM !scheduler !arr = splitLinearlyWith_ scheduler (elemsCount arr) (unsafeLinearIndex arr)
  {-# INLINE loadArrayM #-}

instance Index ix => StrideLoad D ix e

instance Index ix => Stream D ix e where
  toStream = S.steps
  {-# INLINE toStream #-}
  toStreamIx = S.steps . imap (,)
  {-# INLINE toStreamIx #-}

-- | Map an index aware function over an array
imap :: Source r ix e' => (ix -> e' -> e) -> Array r ix e' -> Array D ix e
imap f !arr = DArray (getComp arr) (size arr) (\ !ix -> f ix (unsafeIndex arr ix))
{-# INLINE imap #-}

instance (Index ix, Num e) => Num (Array D ix e) where
  (+)         = liftArray2Matching (+)
  {-# INLINE (+) #-}
  (-)         = liftArray2Matching (-)
  {-# INLINE (-) #-}
  (*)         = liftArray2Matching (*)
  {-# INLINE (*) #-}
  abs         = unsafeLiftArray abs
  {-# INLINE abs #-}
  signum      = unsafeLiftArray signum
  {-# INLINE signum #-}
  fromInteger = singleton . fromInteger
  {-# INLINE fromInteger #-}

instance (Index ix, Fractional e) => Fractional (Array D ix e) where
  (/)          = liftArray2Matching (/)
  {-# INLINE (/) #-}
  fromRational = singleton . fromRational
  {-# INLINE fromRational #-}


instance (Index ix, Floating e) => Floating (Array D ix e) where
  pi    = singleton pi
  {-# INLINE pi #-}
  exp   = unsafeLiftArray exp
  {-# INLINE exp #-}
  log   = unsafeLiftArray log
  {-# INLINE log #-}
  sin   = unsafeLiftArray sin
  {-# INLINE sin #-}
  cos   = unsafeLiftArray cos
  {-# INLINE cos #-}
  asin  = unsafeLiftArray asin
  {-# INLINE asin #-}
  atan  = unsafeLiftArray atan
  {-# INLINE atan #-}
  acos  = unsafeLiftArray acos
  {-# INLINE acos #-}
  sinh  = unsafeLiftArray sinh
  {-# INLINE sinh #-}
  cosh  = unsafeLiftArray cosh
  {-# INLINE cosh #-}
  asinh = unsafeLiftArray asinh
  {-# INLINE asinh #-}
  atanh = unsafeLiftArray atanh
  {-# INLINE atanh #-}
  acosh = unsafeLiftArray acosh
  {-# INLINE acosh #-}


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
delay :: Source r ix e => Array r ix e -> Array D ix e
delay arr = DArray (getComp arr) (size arr) (unsafeIndex arr)
{-# INLINE [1] delay #-}

{-# RULES
"delay" [~1] forall (arr :: Array D ix e) . delay arr = arr
 #-}

-- | /O(min (n1, n2))/ - Compute array equality by applying a comparing function to each element.
--
-- @since 0.5.7
eqArrays :: (Source r1 ix e1, Source r2 ix e2) =>
      (e1 -> e2 -> Bool) -> Array r1 ix e1 -> Array r2 ix e2 -> Bool
eqArrays f arr1 arr2 =
  (size arr1 == size arr2) &&
  not (A.any not
       (DArray (getComp arr1 <> getComp arr2) (size arr1) $ \ix ->
           f (unsafeIndex arr1 ix) (unsafeIndex arr2 ix)))
{-# INLINE eqArrays #-}

-- | /O(min (n1, n2))/ - Compute array ordering by applying a comparing function to each element.
-- The exact ordering is unspecified so this is only intended for use in maps and the like where
-- you need an ordering but do not care about which one is used.
--
-- @since 0.5.7
compareArrays :: (Source r1 ix e1, Source r2 ix e2) =>
       (e1 -> e2 -> Ordering) -> Array r1 ix e1 -> Array r2 ix e2 -> Ordering
compareArrays f arr1 arr2 =
  compare (size arr1) (size arr2) <>
  A.fold
    (DArray (getComp arr1 <> getComp arr2) (size arr1) $ \ix ->
       f (unsafeIndex arr1 ix) (unsafeIndex arr2 ix))
{-# INLINE compareArrays #-}


liftArray2Matching
  :: (Source r1 ix a, Source r2 ix b)
  => (a -> b -> e) -> Array r1 ix a -> Array r2 ix b -> Array D ix e
liftArray2Matching f !arr1 !arr2
  | sz1 == sz2 =
    DArray
      (getComp arr1 <> getComp arr2)
      sz1
      (\ !ix -> f (unsafeIndex arr1 ix) (unsafeIndex arr2 ix))
  | otherwise = throw $ SizeMismatchException (size arr1) (size arr2)
  where
    sz1 = size arr1
    sz2 = size arr2
{-# INLINE liftArray2Matching #-}


-- -- | The usual map.
-- liftArray :: Source r ix b => (b -> e) -> Array r ix b -> Array D ix e
-- liftArray f !arr = DArray (getComp arr) (size arr) (f . unsafeIndex arr)
-- {-# INLINE liftArray #-}

-- -- | Similar to `Data.Massiv.Array.zipWith`, except dimensions of both arrays either have to be the
-- -- same, or at least one of the two array must be a singleton array, in which case it will behave as
-- -- a `Data.Massiv.Array.map`.
-- --
-- -- @since 0.1.4
-- liftArray2
--   :: (Source r1 ix a, Source r2 ix b)
--   => (a -> b -> e) -> Array r1 ix a -> Array r2 ix b -> Array D ix e
-- liftArray2 f !arr1 !arr2
--   | sz1 == oneSz = liftArray (f (unsafeIndex arr1 zeroIndex)) arr2
--   | sz2 == oneSz = liftArray (`f` unsafeIndex arr2 zeroIndex) arr1
--   | sz1 == sz2 =
--     DArray (getComp arr1 <> getComp arr2) sz1 (\ !ix -> f (unsafeIndex arr1 ix) (unsafeIndex arr2 ix))
--   | otherwise = throw $ SizeMismatchException (size arr1) (size arr2)
--   where
--     sz1 = size arr1
--     sz2 = size arr2
-- {-# INLINE liftArray2 #-}


