{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Data.Massiv.Array.Delayed.Pull
-- Copyright   : (c) Alexey Kuleshevich 2018-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
module Data.Massiv.Array.Delayed.Pull
  ( D(..)
  , Array(..)
  , delay
  , eqArrays
  , compareArrays
  , imap
  , liftArray2'
  , liftArray2M
  , unsafeExtract
  , unsafeSlice
  , unsafeInnerSlice
  , zipWithInternal
  ) where

import Control.Applicative
import qualified Data.Foldable as F
import Data.Massiv.Array.Ops.Fold.Internal as A
import Data.Massiv.Core.Common as A
import Data.Massiv.Core.List (L, showArrayList, showsArrayPrec)
import Data.Massiv.Core.Operations
import qualified Data.Massiv.Vector.Stream as S
import GHC.Base (build)
import Prelude hiding (zipWith)

#include "massiv.h"

-- | Delayed representation.
data D =
  D
  deriving (Show)


data instance  Array D ix e =
  DArray { dComp :: !Comp
         , dSize :: !(Sz ix)
         , dPrefIndex :: !(PrefIndex ix e)
         }

instance (Ragged L ix e, Show e) => Show (Array D ix e) where
  showsPrec = showsArrayPrec id
  showList = showArrayList

instance Index ix => Shape D ix where
  maxLinearSize = Just . SafeSz . elemsCount
  {-# INLINE maxLinearSize #-}

instance Size D where
  size = dSize
  {-# INLINE size #-}
  unsafeResize !sz !arr =
    makeArrayLinear (dComp arr) sz (unsafeIndex arr . fromLinearIndex (size arr))
  {-# INLINE unsafeResize #-}

instance Strategy D where
  setComp c arr = arr {dComp = c}
  {-# INLINE setComp #-}
  getComp = dComp
  {-# INLINE getComp #-}
  repr = D

instance Source D e where
  unsafeIndex arr =
    case dPrefIndex arr of
      PrefIndex f -> f
      PrefIndexLinear f -> f . toLinearIndex (size arr)
  {-# INLINE unsafeIndex #-}
  unsafeLinearIndex arr =
    case dPrefIndex arr of
      PrefIndex f -> f . fromLinearIndex (size arr)
      PrefIndexLinear f -> f
  {-# INLINE unsafeLinearIndex #-}
  unsafePrefIndex = dPrefIndex
  {-# INLINE unsafePrefIndex #-}
  unsafeOuterSlice !arr !szL !i =
    makeArray (dComp arr) szL (unsafeIndex arr . consDim i)
  {-# INLINE unsafeOuterSlice #-}
  unsafeLinearSlice !o !sz arr =
    makeArrayLinear (dComp arr) sz $ \ !i -> unsafeLinearIndex arr (i + o)
  {-# INLINE unsafeLinearSlice #-}

-- | /O(1)/ - Extract a portion of an array. Staring index and new size are
-- not validated.
unsafeExtract :: (Source r e, Index ix) => ix -> Sz ix -> Array r ix e -> Array D ix e
unsafeExtract !sIx !newSz !arr =
  makeArray (getComp arr) newSz (unsafeIndex arr . liftIndex2 (+) sIx)
{-# INLINE unsafeExtract #-}

-- | /O(1)/ - Take a slice out of an array from within
unsafeSlice ::
     (Source r e, Index ix, Index (Lower ix), MonadThrow m)
  => Array r ix e
  -> ix
  -> Sz ix
  -> Dim
  -> m (Array D (Lower ix) e)
unsafeSlice arr start cut@(SafeSz cutSz) dim = do
  newSz <- dropDimM cutSz dim
  return $ unsafeResize (SafeSz newSz) (unsafeExtract start cut arr)
{-# INLINE unsafeSlice #-}

-- | /O(1)/ - Take a slice out of an array from the inside
unsafeInnerSlice ::
     (Source r e, Index ix) => Array r ix e -> Sz (Lower ix) -> Int -> Array D (Lower ix) e
unsafeInnerSlice !arr szL !i =
  DArray (getComp arr) szL $ PrefIndex (unsafeIndex arr . (`snocDim` i))
{-# INLINE unsafeInnerSlice #-}

instance (Eq e, Index ix) => Eq (Array D ix e) where
  (==) = eqArrays (==)
  {-# INLINE (==) #-}

instance (Ord e, Index ix) => Ord (Array D ix e) where
  compare = compareArrays compare
  {-# INLINE compare #-}

instance Functor (Array D ix) where
  fmap f (DArray c sz g) = DArray c sz (fmap f g)
  {-# INLINE fmap #-}
  (<$) e (DArray c sz g) = DArray c sz (e <$ g)
  {-# INLINE (<$) #-}

instance Index ix => Applicative (Array D ix) where
  pure = singleton
  {-# INLINE pure #-}
  (<*>) = liftArray2' id
  {-# INLINE (<*>) #-}
#if MIN_VERSION_base(4,10,0)
  liftA2 = liftArray2'
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
  toList arr = build (\c n -> foldrFB c n arr)
  {-# INLINE toList #-}

instance Index ix => Load D ix e where
  makeArray comp sz = DArray comp sz . PrefIndex
  {-# INLINE makeArray #-}
  makeArrayLinear comp sz = DArray comp sz . PrefIndexLinear
  {-# INLINE makeArrayLinear #-}
  iterArrayLinearST_ !scheduler DArray {..} uWrite =
    case dPrefIndex of
      PrefIndex f ->
        iterTargetFullST_ defRowMajor scheduler 0 dSize $ \ !i -> uWrite i . f
      PrefIndexLinear f ->
        iterTargetFullST_ defRowMajorLinear scheduler 0 dSize $ \ !i _ -> uWrite i (f i)
  {-# INLINE iterArrayLinearST_ #-}

instance Index ix => StrideLoad D ix e where
  iterArrayLinearWithStrideST_ !scheduler !stride sz DArray {..} uWrite =
    case dPrefIndex of
      PrefIndex f ->
        iterTargetFullWithStrideST_ defRowMajor scheduler 0 sz stride $ \i ->
          uWrite i . f
      PrefIndexLinear f -> do
        iterTargetFullWithStrideST_ defRowMajor scheduler 0 sz stride $ \i ->
          uWrite i . f . toLinearIndex dSize
  {-# INLINE iterArrayLinearWithStrideST_ #-}

instance Index ix => Stream D ix e where
  toStream = S.steps
  {-# INLINE toStream #-}
  toStreamIx = S.steps . imap (,)
  {-# INLINE toStreamIx #-}

-- | Map an index aware function over an array
--
-- @since 0.1.0
imap ::
     forall r ix e a. (Index ix, Source r e)
  => (ix -> e -> a)
  -> Array r ix e
  -> Array D ix a
imap f !arr =
  case unsafePrefIndex arr of
    PrefIndex gix -> DArray (getComp arr) sz $ PrefIndex (\ !ix -> f ix (gix ix))
    PrefIndexLinear gi ->
      DArray (getComp arr) sz $ PrefIndex (\ !ix -> f ix (gi (toLinearIndex sz ix)))
  where
    !sz = size arr
{-# INLINE imap #-}

instance Num e => FoldNumeric D e where
  unsafeDotProduct = defaultUnsafeDotProduct
  {-# INLINE unsafeDotProduct #-}
  powerSumArray = defaultPowerSumArray
  {-# INLINE powerSumArray #-}
  foldArray = defaultFoldArray
  {-# INLINE foldArray #-}

instance Num e => Numeric D e where
  unsafeLiftArray f arr = arr {dPrefIndex = f <$> dPrefIndex arr}
  {-# INLINE unsafeLiftArray #-}
  unsafeLiftArray2 f a1 a2 = zipWithInternal (size a1) f a1 a2
  {-# INLINE unsafeLiftArray2 #-}

instance Floating e => NumericFloat D e

-- | /O(1)/ Conversion from a source array to `D` representation.
delay :: (Index ix, Source r e) => Array r ix e -> Array D ix e
delay arr =
  case unsafePrefIndex arr of
    PrefIndex gix -> makeArray (getComp arr) (size arr) gix
    PrefIndexLinear gi -> makeArrayLinear (getComp arr) (size arr) gi
{-# INLINE [1] delay #-}
{-# RULES
"delay" [~1] forall (arr :: Array D ix e) . delay arr = arr
 #-}

-- | Compute array equality by applying a comparing function to each
-- element. Empty arrays are always equal, regardless of their size.
--
-- @since 0.5.7
eqArrays ::
     (Index ix, Source r1 e1, Source r2 e2)
  => (e1 -> e2 -> Bool)
  -> Array r1 ix e1
  -> Array r2 ix e2
  -> Bool
eqArrays f arr1 arr2 =
  let sz1 = size arr1
      sz2 = size arr2
   in (sz1 == sz2 &&
       not
         (A.any
            not
            (makeArray @D (getComp arr1 <> getComp arr2) (size arr1) $ \ix ->
               f (unsafeIndex arr1 ix) (unsafeIndex arr2 ix)))) ||
      (isZeroSz sz1 && isZeroSz sz2)
{-# INLINE eqArrays #-}

-- | Compute array ordering by applying a comparing function to each element.
-- The exact ordering is unspecified so this is only intended for use in maps and the like where
-- you need an ordering but do not care about which one is used.
--
-- @since 0.5.7
compareArrays ::
     (Index ix, Source r1 e1, Source r2 e2)
  => (e1 -> e2 -> Ordering)
  -> Array r1 ix e1
  -> Array r2 ix e2
  -> Ordering
compareArrays f arr1 arr2 =
  compare (size arr1) (size arr2) <>
  A.fold
    (makeArray @D (getComp arr1 <> getComp arr2) (size arr1) $ \ix ->
       f (unsafeIndex arr1 ix) (unsafeIndex arr2 ix))
{-# INLINE compareArrays #-}

-- | Same as `liftArray2M`, but throws an imprecise exception on mismatched
-- sizes.
--
-- @since 1.0.0
liftArray2' ::
     (HasCallStack, Index ix, Source r1 a, Source r2 b)
  => (a -> b -> e)
  -> Array r1 ix a
  -> Array r2 ix b
  -> Array D ix e
liftArray2' f arr1 arr2 = throwEither $ liftArray2M f arr1 arr2
{-# INLINE liftArray2' #-}

-- | Similar to `Data.Massiv.Array.zipWith`, except dimensions of both arrays
-- have to be the same, otherwise it throws `SizeMismatchException`.
--
-- @since 1.0.0
liftArray2M ::
     (Index ix, Source r1 a, Source r2 b, MonadThrow m)
  => (a -> b -> e)
  -> Array r1 ix a
  -> Array r2 ix b
  -> m (Array D ix e)
liftArray2M f !arr1 !arr2
  | sz1 == sz2 = pure $ zipWithInternal sz1 f arr1 arr2
  | isZeroSz sz1 && isZeroSz sz2 = pure A.empty
  | otherwise = throwM $ SizeMismatchException (size arr1) (size arr2)
  where
    sz1 = size arr1
    sz2 = size arr2
{-# INLINE liftArray2M #-}

zipWithInternal ::
     (Index ix, Source r1 e1, Source r2 e2)
  => Sz ix
  -> (e1 -> e2 -> e3)
  -> Array r1 ix e1
  -> Array r2 ix e2
  -> Array D ix e3
zipWithInternal sz f arr1 arr2 =
  case unsafePrefIndex arr1 of
    PrefIndexLinear gi1
      | PrefIndexLinear gi2 <- unsafePrefIndex arr2 ->
        makeArrayLinear comp sz (\ !i -> f (gi1 i) (gi2 i))
    _ -> makeArray comp sz (\ !ix -> f (unsafeIndex arr1 ix) (unsafeIndex arr2 ix))
  where
    comp = getComp arr1 <> getComp arr2
{-# INLINE zipWithInternal #-}
