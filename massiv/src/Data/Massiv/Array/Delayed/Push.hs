{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Massiv.Array.Delayed.Push
-- Copyright   : (c) Alexey Kuleshevich 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Delayed.Push
  ( DL(..)
  , Array(..)
  ) where

import           Control.Monad.Primitive
import           Data.Foldable                       (Foldable (..))
import           Data.Massiv.Array.Ops.Fold.Internal as A
import           Data.Massiv.Core.Common
import           Data.Monoid                         ((<>))
import           GHC.Base                            (build)
import           Prelude                             hiding (map, zipWith)

import           System.IO.Unsafe
-- import Control.Monad (void)
-- import Control.Monad.ST

-- import               Data.List (foldl')
import           Data.Massiv.Array                   as MA
-- import               Prelude hiding (map)


#include "massiv.h"

-- | Delayed load representation. Also known as Push array.
data DL = DL deriving Show


data instance Array DL ix e = DLArray
  { dlComp :: !Comp
  , dlSize :: !ix
  , dlLoad :: forall m . Monad m
           => Int
           -> (m () -> m ())
           -> (Int -> m e)
           -> (Int -> e -> m ())
           -> m ()
  }

type instance EltRepr DL ix = DL

instance Index ix => Construct DL ix e where
  getComp = dlComp
  {-# INLINE getComp #-}
  setComp c arr = arr {dlComp = c}
  {-# INLINE setComp #-}
  unsafeMakeArray comp sz f =
    DLArray comp sz $ \numWorkers scheduleWith _dlRead dlWrite ->
      splitWith_ numWorkers scheduleWith (totalElem sz) (f . fromLinearIndex sz) dlWrite
  {-# INLINE unsafeMakeArray #-}

instance {-# OVERLAPPING #-} (Show (Array B ix e), Index ix) => Show (Array DL ix e) where
  show = show . computeAs B

instance Index ix => Source DL ix e where


-- downsample
--   :: Index ix => Stride ix -> Array DL ix e -> Array DL ix e
-- downsample stride@(Stride str) arr@DLArray {dlSize, dlLoad} =
--   arr
--     { dlSize = strideSize stride dlSize
--     , dlLoad =
--         \numWorkers scheduleWith _dlRead dlWrite ->
--           dlLoad numWorkers scheduleWith undefined (\ix -> dlWrite (applyStride ix))
--     }
--   where
--     applyStride ix = liftIndex2 div ix str
--     {-# INLINE applyStride #-}



upsample
  :: Index ix => e -> Stride ix -> Array DL ix e -> Array DL ix e
upsample fillWith (Stride stride) arr@DLArray {dlSize, dlLoad} =
  arr
    { dlSize = sz
    , dlLoad =
        \numWorkers scheduleWith dlRead dlWrite -> do
          dlLoad
            numWorkers
            scheduleWith
            (dlRead . (* linearStride))
            (\i e -> do
                let i' = i * linearStride
                dlWrite i' e
                -- TODO: Benchmark against fast initialization with single value first
                loopM_ (i' + 1) (< min (i' + linearStride) k) (+1) (`dlWrite` fillWith)
            )
    }
  where
    sz = liftIndex2 (*) dlSize stride
    k = totalElem sz
    linearStride = toLinearIndex dlSize stride


instance Index ix => Size DL ix e where
  size = dlSize
  {-# INLINE size #-}

  -- unsafeResize !sz DLArray {..} =
  --   DLArray dlComp sz $ \ dlRead dlWrite ->
  --     let toIx = fromLinearIndex sz . toLinearIndex dlSize
  --     in dlLoad (\ ix -> dlRead (toIx ix)) (\ix e -> dlWrite (toIx ix) e)
  -- {-# INLINE unsafeResize #-}

  -- unsafeExtract !sIx !newSz DLArray {..} =
  --   DLArray dlComp newSz $ \ dlRead dlWrite ->
  --     let toIx ix = liftIndex2 (+) ix sIx
  --     in dlLoad (dlRead . toIx) (\ix e -> dlWrite (toIx ix) e)
  -- {-# INLINE unsafeExtract #-}


toLoadArray :: Load r ix e => Array r ix e -> Array DL ix e
toLoadArray arr =
  DLArray (getComp arr) (size arr) $ \numWorkers scheduleWith dlRead dlWrite ->
    loadArray numWorkers scheduleWith arr dlRead dlWrite
{-# INLINE toLoadArray #-}

instance (Index ix) => Load DL ix e where
  loadArray numWorkers scheduleWith DLArray {dlLoad} uRead uWrite =
    dlLoad numWorkers scheduleWith uRead uWrite


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
