{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Massiv.Array.Manifest.Vector.Stream
-- Copyright   : (c) Alexey Kuleshevich 2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Manifest.Vector.Stream
  ( -- | __Important__ - This module is still experimental, as such it is considered
    -- internal and exported for the curious users only.
    Steps(..)
  , Stream(..)
  -- * Conversion
  , steps
  , isteps
  , fromStream
  , fromStreamM
  , fromStreamExactM
  , unstreamExact
  , unstreamMax
  , unstreamMaxM
  , unstreamUnknown
  , unstreamUnknownM
  , unstreamIntoM
  -- * Bundle
  , toBundle
  , fromBundle
  , fromBundleM
  -- * Operations on Steps
  , length
  , empty
  , singleton
  , generate
  , cons
  , uncons
  , snoc
  , drop
  , take
  , slice
  , iterateN
  , iterateNM
  , replicateM
  , generateM
  , traverse
  , mapM
  , concatMap
  , append
  , zipWith
  , zipWithM
  -- ** Folding
  , foldl
  , foldr
  , foldlM
  , foldrM
  -- ** Unfolding
  , unfoldr
  , unfoldrN
  -- * Lists
  , toList
  , fromList
  , fromListN
  -- ** Filter
  , mapMaybe
  , mapMaybeA
  , mapMaybeM
  , filter
  , filterA
  , filterM
  -- * Transformations
  , transSteps
  , transStepsId
  -- * Useful re-exports
  , module Data.Vector.Fusion.Bundle.Size
  , module Data.Vector.Fusion.Util
  ) where

import Data.Maybe (catMaybes)
import qualified Control.Monad as M
import Control.Monad.ST
import Data.Massiv.Core.Common hiding (empty, singleton)
import qualified Data.Traversable as Traversable (traverse)
import qualified Data.Vector.Fusion.Bundle.Monadic as B
import Data.Vector.Fusion.Bundle.Size
import qualified Data.Vector.Fusion.Stream.Monadic as S
import Data.Vector.Fusion.Util
import Prelude hiding (zipWith, mapM, traverse, length, foldl, foldr, filter, concatMap, drop, take)


-- TODO: benchmark: `fmap snd . isteps`
steps :: forall r ix e m . (Monad m, Source r ix e) => Array r ix e -> Steps m e
steps arr = k `seq` arr `seq` Steps (S.Stream step 0) (Exact k)
  where
    k = totalElem $ size arr
    step i
      | i < k =
        let e = unsafeLinearIndex arr i
         in e `seq` return $ S.Yield e (i + 1)
      | otherwise = return S.Done
    {-# INLINE step #-}
{-# INLINE steps #-}


isteps :: forall r ix e m . (Monad m, Source r ix e) => Array r ix e -> Steps m (ix, e)
isteps arr = k `seq` arr `seq` Steps (S.Stream step 0) (Exact k)
  where
    sz = size arr
    k = totalElem sz
    step i
      | i < k =
        let e = unsafeLinearIndex arr i
         in e `seq` return $ S.Yield (fromLinearIndex sz i, e) (i + 1)
      | otherwise = return S.Done
    {-# INLINE step #-}
{-# INLINE isteps #-}

toBundle :: (Monad m, Source r ix e) => Array r ix e -> B.Bundle m v e
toBundle arr =
  let Steps str k = steps arr
   in B.fromStream str k
{-# INLINE toBundle #-}

fromBundle :: Mutable r Ix1 e => B.Bundle Id v e -> Array r Ix1 e
fromBundle bundle = fromStream (B.sSize bundle) (B.sElems bundle)
{-# INLINE fromBundle #-}


fromBundleM :: (Monad m, Mutable r Ix1 e) => B.Bundle m v e -> m (Array r Ix1 e)
fromBundleM bundle = fromStreamM (B.sSize bundle) (B.sElems bundle)
{-# INLINE fromBundleM #-}


fromStream :: forall r e . Mutable r Ix1 e => Size -> S.Stream Id e -> Array r Ix1 e
fromStream sz str =
  case upperBound sz of
    Nothing -> unstreamUnknown str
    Just k  -> unstreamMax k str
{-# INLINE fromStream #-}

fromStreamM :: forall r e m. (Monad m, Mutable r Ix1 e) => Size -> S.Stream m e -> m (Array r Ix1 e)
fromStreamM sz str = do
  xs <- S.toList str
  case upperBound sz of
    Nothing -> pure $! unstreamUnknown (S.fromList xs)
    Just k  -> pure $! unstreamMax k (S.fromList xs)
{-# INLINE fromStreamM #-}

fromStreamExactM ::
     forall r ix e m. (Monad m, Mutable r ix e)
  => Sz ix
  -> S.Stream m e
  -> m (Array r ix e)
fromStreamExactM sz str = do
  xs <- S.toList str
  pure $! unstreamExact sz (S.fromList xs)
{-# INLINE fromStreamExactM #-}


unstreamIntoM ::
     (Mutable r Ix1 a, PrimMonad m)
  => MArray (PrimState m) r Ix1 a
  -> Size
  -> S.Stream Id a
  -> m (MArray (PrimState m) r Ix1 a)
unstreamIntoM marr sz str =
  case sz of
    Exact _ -> marr <$ unstreamMaxM marr str
    Max _ -> unsafeLinearShrink marr . SafeSz =<< unstreamMaxM marr str
    Unknown  -> unstreamUnknownM marr str
{-# INLINE unstreamIntoM #-}



unstreamMax ::
     forall r e. (Mutable r Ix1 e)
  => Int
  -> S.Stream Id e
  -> Array r Ix1 e
unstreamMax kMax str =
  runST $ do
    marr <- unsafeNew (SafeSz kMax)
    k <- unstreamMaxM marr str
    unsafeLinearShrink marr (SafeSz k) >>= unsafeFreeze Seq
{-# INLINE unstreamMax #-}


unstreamMaxM ::
     (Mutable r ix a, PrimMonad m) => MArray (PrimState m) r ix a -> S.Stream Id a -> m Int
unstreamMaxM marr (S.Stream step s) = stepLoad s 0
  where
    stepLoad t i =
      case unId (step t) of
        S.Yield e' t' -> do
          unsafeLinearWrite marr i e'
          stepLoad t' (i + 1)
        S.Skip t' -> stepLoad t' i
        S.Done -> return i
    {-# INLINE stepLoad #-}
{-# INLINE unstreamMaxM #-}


unstreamUnknown :: Mutable r Ix1 a => S.Stream Id a -> Array r Ix1 a
unstreamUnknown str =
  runST $ do
    marr <- unsafeNew zeroSz
    unstreamUnknownM marr str >>= unsafeFreeze Seq
{-# INLINE unstreamUnknown #-}


unstreamUnknownM ::
     (Mutable r Ix1 a, PrimMonad m)
  => MArray (PrimState m) r Ix1 a
  -> S.Stream Id a
  -> m (MArray (PrimState m) r Ix1 a)
unstreamUnknownM marrInit (S.Stream step s) = stepLoad s 0 (unSz (msize marrInit)) marrInit
  where
    stepLoad t i kMax marr
      | i < kMax =
        case unId (step t) of
          S.Yield e' t' -> do
            unsafeLinearWrite marr i e'
            stepLoad t' (i + 1) kMax marr
          S.Skip t' -> stepLoad t' i kMax marr
          S.Done -> unsafeLinearShrink marr (SafeSz i)
      | otherwise = do
        let kMax' = max 1 (kMax * 2)
        marr' <- unsafeLinearGrow marr (SafeSz kMax')
        stepLoad t i kMax' marr'
    {-# INLINE stepLoad #-}
{-# INLINE unstreamUnknownM #-}


unstreamExact ::
     forall r ix e. (Mutable r ix e)
  => Sz ix
  -> S.Stream Id e
  -> Array r ix e
unstreamExact sz str =
  runST $ do
    marr <- unsafeNew sz
    _ <- unstreamMaxM marr str
    unsafeFreeze Seq marr
{-# INLINE unstreamExact #-}

length :: Steps Id a -> Int
length (Steps str sz) =
  case sz of
    Exact k -> k
    _       -> unId (S.length str)
{-# INLINE length #-}

empty :: Monad m => Steps m e
empty = Steps S.empty (Exact 0)
{-# INLINE empty #-}

singleton :: Monad m => e -> Steps m e
singleton e = Steps (S.singleton e) (Exact 1)
{-# INLINE singleton #-}

generate :: Monad m => Int -> (Int -> e) -> Steps m e
generate k f = Steps (S.generate k f) (Exact k)
{-# INLINE generate #-}

cons :: Monad m => e -> Steps m e -> Steps m e
cons e (Steps str k) = Steps (S.cons e str) (k + 1)
{-# INLINE cons #-}

uncons :: Monad m => Steps m e -> m (Maybe (e, Steps m e))
uncons sts@(Steps str _) = do
  mx <- str S.!? 0
  pure $ fmap (, drop 1 sts) mx
{-# INLINE uncons #-}

snoc :: Monad m => Steps m e -> e -> Steps m e
snoc (Steps str k) e = Steps (S.snoc str e) (k + 1)
{-# INLINE snoc #-}

traverse :: (Monad m, Applicative f) => (e -> f a) -> Steps Id e -> f (Steps m a)
traverse f (Steps str k) = (`Steps` k) <$> liftListA (Traversable.traverse f) str
{-# INLINE traverse #-}

append :: Monad m => Steps m e -> Steps m e -> Steps m e
append (Steps str1 k1) (Steps str2 k2) = Steps (str1 S.++ str2) (k1 + k2)
{-# INLINE append #-}

mapM :: Monad m => (e -> m a) -> Steps m e -> Steps m a
mapM f (Steps str k) = Steps (S.mapM f str) k
{-# INLINE mapM #-}

zipWith :: Monad m => (a -> b -> e) -> Steps m a -> Steps m b -> Steps m e
zipWith f (Steps str1 k1) (Steps str2 k2) = Steps (S.zipWith f str1 str2) (smaller k1 k2)
{-# INLINE zipWith #-}

zipWithM :: Monad m => (a -> b -> m c) -> Steps m a -> Steps m b -> Steps m c
zipWithM f (Steps str1 k1) (Steps str2 k2) = Steps (S.zipWithM f str1 str2) (smaller k1 k2)
{-# INLINE zipWithM #-}

transStepsId :: Monad m => Steps Id e -> Steps m e
transStepsId (Steps sts k) = Steps (S.trans (pure . unId) sts) k
{-# INLINE transStepsId #-}

transSteps :: (Monad m, Monad n) => Steps m e -> m (Steps n e)
transSteps (Steps strM sz@(Exact _)) = (`Steps` sz) <$> liftListM strM
transSteps (Steps strM _) = do
  (n, strN) <- liftListNM strM
  pure (Steps strN (Exact n))
{-# INLINE transSteps #-}


foldr :: (a -> b -> b) -> b -> Steps Id a -> b
foldr f acc sts = unId (S.foldr f acc (stepsStream sts))
{-# INLINE foldr #-}


foldl :: (b -> a -> b) -> b -> Steps Id a -> b
foldl f acc sts = unId (S.foldl f acc (stepsStream sts))
{-# INLINE foldl #-}


foldlM :: Monad m => (a -> b -> m a) -> a -> Steps m b -> m a
foldlM f acc (Steps sts _) = S.foldlM f acc sts
{-# INLINE foldlM #-}


foldrM :: Monad m => (b -> a -> m a) -> a -> Steps m b -> m a
foldrM f acc (Steps sts _) = S.foldrM f acc sts
{-# INLINE foldrM #-}


mapMaybe :: Monad m => (a -> Maybe e) -> Steps m a -> Steps m e
mapMaybe f (Steps str k) = Steps (S.mapMaybe f str) (toMax k)
{-# INLINE mapMaybe #-}

concatMap :: Monad m => (a -> Steps m e) -> Steps m a -> Steps m e
concatMap f (Steps str _) = Steps (S.concatMap (stepsStream . f) str) Unknown
{-# INLINE concatMap #-}


mapMaybeA :: (Monad m, Applicative f) => (a -> f (Maybe e)) -> Steps Id a -> f (Steps m e)
mapMaybeA f (Steps str k) = (`Steps` toMax k) <$> liftListA (mapMaybeListA f) str
{-# INLINE mapMaybeA #-}

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> Steps m a -> Steps m b
mapMaybeM f (Steps str k) = Steps (mapMaybeStreamM f str) (toMax k)
{-# INLINE mapMaybeM #-}

mapMaybeListA :: Applicative f => (a -> f (Maybe b)) -> [a] -> f [b]
mapMaybeListA f = fmap catMaybes . Traversable.traverse f
{-# INLINE mapMaybeListA #-}

mapMaybeStreamM :: Monad m => (a -> m (Maybe b)) -> S.Stream m a -> S.Stream m b
mapMaybeStreamM f (S.Stream step t) = S.Stream step' t
  where
    step' s = do
      r <- step s
      case r of
        S.Yield x s' -> do
          b <- f x
          return $
            case b of
              Nothing -> S.Skip s'
              Just b' -> S.Yield b' s'
        S.Skip s' -> return $ S.Skip s'
        S.Done -> return S.Done
    {-# INLINE step' #-}
{-# INLINE mapMaybeStreamM #-}

filter :: Monad m => (a -> Bool) -> Steps m a -> Steps m a
filter f (Steps str k) = Steps (S.filter f str) (toMax k)
{-# INLINE filter #-}


filterA :: (Monad m, Applicative f) => (e -> f Bool) -> Steps Id e -> f (Steps m e)
filterA f (Steps str k) = (`Steps` toMax k) <$> liftListA (M.filterM f) str
{-# INLINE filterA #-}

filterM :: Monad m => (e -> m Bool) -> Steps m e -> Steps m e
filterM f (Steps str k) = Steps (S.filterM f str) (toMax k)
{-# INLINE filterM #-}

take :: Monad m => Int -> Steps m a -> Steps m a
take n (Steps str _) = Steps (S.take n str) (Max n)
{-# INLINE take #-}

drop :: Monad m => Int -> Steps m a -> Steps m a
drop n (Steps str k) = Steps (S.drop n str) (k `clampedSubtract` Exact n)
{-# INLINE drop #-}

slice :: Monad m => Int -> Int -> Steps m a -> Steps m a
slice i k (Steps str _) = Steps (S.slice i k str) (Max k)
{-# INLINE slice #-}

iterateN :: Monad m => Int -> (a -> a) -> a -> Steps m a
iterateN n f a = Steps (S.iterateN n f a) (Exact n)
{-# INLINE iterateN #-}

iterateNM :: Monad m => Int -> (a -> m a) -> a -> Steps m a
iterateNM n f a = Steps (S.iterateNM n f a) (Exact n)
{-# INLINE iterateNM #-}

replicateM :: Monad m => Int -> m a -> Steps m a
replicateM n f = Steps (S.replicateM n f) (Exact n)
{-# INLINE replicateM #-}


generateM :: Monad m => Int -> (Int -> m a) -> Steps m a
generateM n f = Steps (S.generateM n f) (Exact n)
{-# INLINE generateM #-}


unfoldr :: Monad m => (s -> Maybe (e, s)) -> s -> Steps m e
unfoldr f e0 = Steps (S.unfoldr f e0) Unknown
{-# INLINE unfoldr #-}

unfoldrN :: Monad m => Sz1 -> (s -> Maybe (e, s)) -> s -> Steps m e
unfoldrN n f e0 = Steps (S.unfoldrN (unSz n) f e0) (Max (unSz n))
{-# INLINE unfoldrN #-}

toList :: Steps Id e -> [e]
toList (Steps str _) = unId (S.toList str)
{-# INLINE toList #-}

fromList :: Monad m => [e] -> Steps m e
fromList = (`Steps` Unknown) . S.fromList
{-# INLINE fromList #-}

fromListN :: Monad m => Int -> [e] -> Steps m e
fromListN n  = (`Steps` Exact n) . S.fromListN n
{-# INLINE fromListN #-}

liftListA :: (Monad m, Functor f) => ([a] -> f [b]) -> S.Stream Id a -> f (S.Stream m b)
liftListA f str = S.fromList <$> f (unId (S.toList str))
{-# INLINE liftListA #-}

liftListM :: (Monad m, Monad n) => S.Stream m a -> m (S.Stream n a)
liftListM str = do
  xs <- S.toList str
  pure $ S.fromList xs
{-# INLINE liftListM #-}

liftListNM :: (Monad m, Monad n) => S.Stream m a -> m (Int, S.Stream n a)
liftListNM str = do
  (n, xs) <- toListN str
  pure (n, S.fromList xs)
{-# INLINE liftListNM #-}


toListN :: Monad m => S.Stream m a -> m (Int, [a])
toListN = S.foldr (\x (i, xs) -> (i + 1, x:xs)) (0, [])
{-# INLINE toListN #-}

