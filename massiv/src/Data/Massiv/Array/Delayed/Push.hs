{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Data.Massiv.Array.Delayed.Internal
-- Copyright   : (c) Alexey Kuleshevich 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Delayed.Push
  ( DP(..)
  , Array(..)
  ) where

import           Control.Monad.Primitive
import           Data.Foldable                       (Foldable (..))
import           Data.Massiv.Array.Ops.Fold.Internal as A
import           Data.Massiv.Core.Common
import           Data.Massiv.Core.Scheduler
import           Data.Monoid                         ((<>))
import           GHC.Base                            (build)
import           Prelude                             hiding (zipWith)

#include "massiv.h"

-- | Delayed representation.
data DP = DP deriving Show


data instance Array DP ix e = DPArray
  { dpComp :: !Comp
  , dpSize :: !ix
  , dpLoad :: forall m . PrimMonad m => (ix -> m e) -> (ix -> e -> m ()) -> m () }
type instance EltRepr DP ix = DP

instance Index ix => Construct DP ix e where
  getComp = dpComp
  {-# INLINE getComp #-}

  setComp c arr = arr { dpComp = c }
  {-# INLINE setComp #-}

  unsafeMakeArray comp sz f = DPArray comp sz $ \ _dpRead dpWrite ->
    iterM_ zeroIndex sz (pureIndex 1) (<) (\ix -> dpWrite ix (f ix))
  {-# INLINE unsafeMakeArray #-}


-- instance Index ix => Source D ix e where
--   unsafeIndex = INDEX_CHECK("(Source D ix e).unsafeIndex", size, dIndex)
--   {-# INLINE unsafeIndex #-}

instance Index ix => Size DP ix e where
  size = dpSize
  {-# INLINE size #-}

  unsafeResize !sz DPArray {..} =
    DPArray dpComp sz $ \ dpRead dpWrite ->
      let toIx = fromLinearIndex sz . toLinearIndex dpSize
      in dpLoad (\ ix -> dpRead (toIx ix)) (\ix e -> dpWrite (toIx ix) e)
  {-# INLINE unsafeResize #-}

  unsafeExtract !sIx !newSz DPArray {..} =
    DPArray dpComp newSz $ \ dpRead dpWrite ->
      let toIx ix = liftIndex2 (+) ix sIx
      in dpLoad (dpRead . toIx) (\ix e -> dpWrite (toIx ix) e)
  {-# INLINE unsafeExtract #-}

-- instance ( Index ix
--          , Index (Lower ix)
--          , Elt D ix e ~ Array D (Lower ix) e
--          ) =>
--          Slice D ix e where
--   unsafeSlice arr start cutSz dim = do
--     newSz <- dropDim cutSz dim
--     return $ unsafeResize newSz (unsafeExtract start cutSz arr)
--   {-# INLINE unsafeSlice #-}


-- instance (Elt D ix e ~ Array D (Lower ix) e, Index ix) => OuterSlice D ix e where

--   unsafeOuterSlice !arr !i =
--     DArray (getComp arr) (tailDim (size arr)) (\ !ix -> unsafeIndex arr (consDim i ix))
--   {-# INLINE unsafeOuterSlice #-}

-- instance (Elt D ix e ~ Array D (Lower ix) e, Index ix) => InnerSlice D ix e where

--   unsafeInnerSlice !arr !(szL, _) !i =
--     DArray (getComp arr) szL (\ !ix -> unsafeIndex arr (snocDim ix i))
--   {-# INLINE unsafeInnerSlice #-}


-- instance (Eq e, Index ix) => Eq (Array D ix e) where
--   (==) = eq (==)
--   {-# INLINE (==) #-}

-- instance (Ord e, Index ix) => Ord (Array D ix e) where
--   compare = ord compare
--   {-# INLINE compare #-}

-- instance Functor (Array D ix) where
--   fmap f (DArray c sz g) = DArray c sz (f . g)
--   {-# INLINE fmap #-}


-- instance Index ix => Applicative (Array D ix) where
--   pure a = DArray Seq (liftIndex (+ 1) zeroIndex) (const a)
--   {-# INLINE pure #-}
--   (<*>) (DArray c1 sz1 uIndex1) (DArray c2 sz2 uIndex2) =
--     DArray (c1 <> c2) (liftIndex2 min sz1 sz2) $ \ !ix ->
--       (uIndex1 ix) (uIndex2 ix)
--   {-# INLINE (<*>) #-}


-- -- | Row-major sequential folding over a Delayed array.
-- instance Index ix => Foldable (Array D ix) where
--   foldl = lazyFoldlS
--   {-# INLINE foldl #-}
--   foldl' = foldlS
--   {-# INLINE foldl' #-}
--   foldr = foldrFB
--   {-# INLINE foldr #-}
--   foldr' = foldrS
--   {-# INLINE foldr' #-}
--   null (DArray _ sz _) = totalElem sz == 0
--   {-# INLINE null #-}
--   sum = foldl' (+) 0
--   {-# INLINE sum #-}
--   product = foldl' (*) 1
--   {-# INLINE product #-}
--   length = totalElem . size
--   {-# INLINE length #-}
--   toList arr = build (\ c n -> foldrFB c n arr)
--   {-# INLINE toList #-}


-- instance Index ix => Load D ix e where
--   loadS (DArray _ sz f) _ unsafeWrite =
--     iterM_ zeroIndex sz (pureIndex 1) (<) $ \ !ix -> unsafeWrite (toLinearIndex sz ix) (f ix)
--   {-# INLINE loadS #-}
--   loadP wIds (DArray _ sz f) _ unsafeWrite =
--     divideWork_ wIds sz $ \ !scheduler !chunkLength !totalLength !slackStart -> do
--       loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
--         scheduleWork scheduler $
--         iterLinearM_ sz start (start + chunkLength) 1 (<) $ \ !k !ix -> unsafeWrite k (f ix)
--       scheduleWork scheduler $
--         iterLinearM_ sz slackStart totalLength 1 (<) $ \ !k !ix -> unsafeWrite k (f ix)
--   {-# INLINE loadP #-}


-- instance (Index ix, Num e) => Num (Array D ix e) where
--   (+)         = liftArray2 (+)
--   {-# INLINE (+) #-}
--   (-)         = liftArray2 (-)
--   {-# INLINE (-) #-}
--   (*)         = liftArray2 (*)
--   {-# INLINE (*) #-}
--   abs         = liftArray abs
--   {-# INLINE abs #-}
--   signum      = liftArray signum
--   {-# INLINE signum #-}
--   fromInteger = singleton Seq . fromInteger
--   {-# INLINE fromInteger #-}

-- instance (Index ix, Fractional e) => Fractional (Array D ix e) where
--   (/)          = liftArray2 (/)
--   {-# INLINE (/) #-}
--   fromRational = singleton Seq . fromRational
--   {-# INLINE fromRational #-}


-- instance (Index ix, Floating e) => Floating (Array D ix e) where
--   pi    = singleton Seq pi
--   {-# INLINE pi #-}
--   exp   = liftArray exp
--   {-# INLINE exp #-}
--   log   = liftArray log
--   {-# INLINE log #-}
--   sin   = liftArray sin
--   {-# INLINE sin #-}
--   cos   = liftArray cos
--   {-# INLINE cos #-}
--   asin  = liftArray asin
--   {-# INLINE asin #-}
--   atan  = liftArray atan
--   {-# INLINE atan #-}
--   acos  = liftArray acos
--   {-# INLINE acos #-}
--   sinh  = liftArray sinh
--   {-# INLINE sinh #-}
--   cosh  = liftArray cosh
--   {-# INLINE cosh #-}
--   asinh = liftArray asinh
--   {-# INLINE asinh #-}
--   atanh = liftArray atanh
--   {-# INLINE atanh #-}
--   acosh = liftArray acosh
--   {-# INLINE acosh #-}



-- -- | /O(1)/ Conversion from a source array to `D` representation.
-- delay :: Source r ix e => Array r ix e -> Array D ix e
-- delay arr = DArray (getComp arr) (size arr) (unsafeIndex arr)
-- {-# INLINE delay #-}


-- -- | /O(n1 + n2)/ - Compute array equality by applying a comparing function to each element.
-- eq :: (Source r1 ix e1, Source r2 ix e2) =>
--       (e1 -> e2 -> Bool) -> Array r1 ix e1 -> Array r2 ix e2 -> Bool
-- eq f arr1 arr2 =
--   (size arr1 == size arr2) &&
--   A.fold
--     (&&)
--     True
--     (DArray (getComp arr1 <> getComp arr2) (size arr1) $ \ix ->
--        f (unsafeIndex arr1 ix) (unsafeIndex arr2 ix))
-- {-# INLINE eq #-}

-- -- | /O(n1 + n2)/ - Compute array ordering by applying a comparing function to each element.
-- -- The exact ordering is unspecified so this is only intended for use in maps and the like where
-- -- you need an ordering but do not care about which one is used.
-- ord :: (Source r1 ix e1, Source r2 ix e2) =>
--        (e1 -> e2 -> Ordering) -> Array r1 ix e1 -> Array r2 ix e2 -> Ordering
-- ord f arr1 arr2 =
--   (compare (size arr1) (size arr2)) <>
--   A.fold
--     (<>)
--     mempty
--     (DArray (getComp arr1 <> getComp arr2) (size arr1) $ \ix ->
--        f (unsafeIndex arr1 ix) (unsafeIndex arr2 ix))
-- {-# INLINE ord #-}

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
--   | sz1 == oneIndex = liftArray (f (unsafeIndex arr1 zeroIndex)) arr2
--   | sz2 == oneIndex = liftArray (`f` (unsafeIndex arr2 zeroIndex)) arr1
--   | sz1 == sz2 =
--     DArray (getComp arr1) sz1 (\ !ix -> f (unsafeIndex arr1 ix) (unsafeIndex arr2 ix))
--   | otherwise = errorSizeMismatch "liftArray2" (size arr1) (size arr2)
--   where
--     oneIndex = pureIndex 1
--     sz1 = size arr1
--     sz2 = size arr2
-- {-# INLINE liftArray2 #-}


