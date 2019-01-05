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
  , toLoadArray
  , makeLoadArray
  ) where

import           Data.Massiv.Array.Delayed.Internal
import           Data.Massiv.Array.Manifest.Boxed
import           Data.Massiv.Array.Manifest.Internal
import           Data.Massiv.Array.Ops.Construct     (makeArrayR)
import           Data.Massiv.Core.Common
import           Prelude                             hiding (map, zipWith)


#include "massiv.h"

-- | Delayed load representation. Also known as Push array.
data DL = DL deriving Show


data instance Array DL ix e = DLArray
  { dlComp :: !Comp
  , dlSize :: !ix
  , dlLoad :: forall m . Monad m
           => Int
           -> (m () -> m ())
           -> (Int -> e -> m ())
           -> m ()
  }

type instance EltRepr DL ix = DL

instance Index ix => Construct DL ix e where
  setComp c arr = arr {dlComp = c}
  {-# INLINE setComp #-}
  unsafeMakeArray comp sz f =
    DLArray comp sz $ \numWorkers scheduleWith dlWrite ->
      splitWith_ numWorkers scheduleWith (totalElem sz) (f . fromLinearIndex sz) dlWrite
  {-# INLINE unsafeMakeArray #-}

instance {-# OVERLAPPING #-} (Show (Array B ix e), Index ix) => Show (Array DL ix e) where
  show = show . computeAs B

instance Index ix => Source DL ix e where


-- | Specify how an array can be loaded/computed through creation of a `DL` array.
--
-- @since 0.3.0
makeLoadArray ::
     Comp
  -> ix
  -> (forall m. Monad m =>
                  Int -> (m () -> m ()) -> (Int -> e -> m ()) -> m ())
  -> Array DL ix e
makeLoadArray comp sz f = DLArray comp sz f
{-# INLINE makeLoadArray #-}

instance Index ix => Resize Array DL ix where
  unsafeResize !sz arr = arr { dlSize = sz }
  {-# INLINE unsafeResize #-}


-- | Convert any `Load`able array into `DL` representation.
--
-- @since 0.3.0
toLoadArray :: Load r ix e => Array r ix e -> Array DL ix e
toLoadArray arr =
  DLArray (getComp arr) (size arr) $ \numWorkers scheduleWith dlWrite ->
    loadArray numWorkers scheduleWith arr dlWrite
{-# INLINE toLoadArray #-}

instance Index ix => Load DL ix e where
  unsafeSize = dlSize
  {-# INLINE unsafeSize #-}
  getComp = dlComp
  {-# INLINE getComp #-}
  loadArray numWorkers scheduleWith DLArray {dlLoad} = dlLoad numWorkers scheduleWith
  {-# INLINE loadArray #-}

instance Functor (Array DL ix) where
  fmap f arr =
    arr
      { dlLoad =
          \numWorkers scheduleWork uWrite ->
            (dlLoad arr) numWorkers scheduleWork (\i e -> uWrite i (f e))
      }
  {-# INLINE fmap #-}


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
