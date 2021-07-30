{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide, not-home #-}
-- |
-- Module      : Data.Massiv.Vector.Stream
-- Copyright   : (c) Alexey Kuleshevich 2019-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Vector.Stream
  ( -- | This module has a similar purpose as the 'Data.Vector.Fusion.Bundle.Monadic', but
    -- quite a bit simpler.
    --
    -- __Important__ - This module is still experimental, as such it is considered
    -- internal and exported for the curious users only.
    Steps(..)
  , Stream(..)
  -- * Conversion
  , steps
  , isteps
  , consume
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
  , null
  , empty
  , singleton
  , generate
  , headMaybe
  , last
  , cons
  , uncons
  , snoc
  , drop
  , take
  , slice
  , iterateN
  , iterateNM
  , replicate
  , replicateM
  , generateM
  , traverse
  , map
  , mapM
  , mapM_
  , indexed
  , concatMap
  , append
  , zipWith
  , zipWith3
  , zipWith4
  , zipWith5
  , zipWith6
  , zipWithM
  , zipWith3M
  , zipWith4M
  , zipWith5M
  , zipWith6M
  , zipWithM_
  , zipWith3M_
  , zipWith4M_
  , zipWith5M_
  , zipWith6M_
  -- ** Folding
  , foldl
  , foldl1
  , foldlM
  , foldl1M
  , foldlLazy
  , foldl1Lazy
  , foldlLazyM
  , foldl1LazyM
  , foldrLazy
  , foldr1Lazy
  , foldrLazyM
  , foldr1LazyM

  , or
  , and
  -- ** Unfolding
  , unfoldr
  , unfoldrN
  , unsafeUnfoldrN
  , unfoldrM
  , unfoldrNM
  , unsafeUnfoldrNM
  , unfoldrExactN
  , unfoldrExactNM
  -- ** Enumeration
  , enumFromStepN
  -- * Lists
  , toList
  , fromList
  , fromListN
  , unsafeFromListN
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
  , module Data.Vector.Fusion.Util
  , Id(..)
  ) where

import qualified Control.Monad as M
import Control.Monad.ST
import qualified Data.Foldable as F
import Data.Massiv.Core.Common hiding (empty, singleton, replicate)
import Data.Coerce
import Data.Maybe (catMaybes)
import qualified Data.Traversable as Traversable (traverse)
import qualified Data.Vector.Fusion.Bundle.Monadic as B
import qualified Data.Vector.Fusion.Bundle.Size as B
import qualified Data.Vector.Fusion.Stream.Monadic as S
import Data.Vector.Fusion.Util
import Prelude hiding (and, concatMap, drop, filter, foldl, foldl1, foldr,
                foldr1, length, map, mapM, mapM_, null, or, replicate, take,
                traverse, zipWith, zipWith3)
import qualified GHC.Exts (IsList(..))

instance Monad m => Functor (Steps m) where
  fmap f str = str {stepsStream = S.map f (stepsStream str)}
  {-# INLINE fmap #-}
  (<$) e str =
    case stepsSize str of
      LengthExact n -> str {stepsStream = S.replicate (coerce n) e}
      _             -> fmap (const e) str
  {-# INLINE (<$) #-}

instance Monad m => Semigroup (Steps m e) where
  (<>) = append
  {-# INLINE (<>) #-}

instance Monad m => Monoid (Steps m e) where
  mempty = empty
  {-# INLINE mempty #-}
  mappend = append
  {-# INLINE mappend #-}


instance GHC.Exts.IsList (Steps Id e) where
  type Item (Steps Id e) = e
  toList = toList
  {-# INLINE toList #-}
  fromList = fromList
  {-# INLINE fromList #-}
  fromListN n = (`Steps` LengthMax (Sz n)) . S.fromListN n
  {-# INLINE fromListN #-}

instance Foldable (Steps Id) where
  foldr f acc = unId . foldrLazy f acc
  {-# INLINE foldr #-}
  foldl f acc = unId . foldlLazy f acc
  {-# INLINE foldl #-}
  foldl' f acc = unId . foldl f acc
  {-# INLINE foldl' #-}
  foldr1 f = unId . foldr1Lazy f
  {-# INLINE foldr1 #-}
  foldl1 f = unId . foldl1Lazy f
  {-# INLINE foldl1 #-}
  toList = toList
  {-# INLINE toList #-}
  length = unId . length
  {-# INLINE length #-}
  null = unId . null
  {-# INLINE null #-}
  sum = unId . foldl (+) 0
  {-# INLINE sum #-}
  product = unId . foldl (*) 1
  {-# INLINE product #-}
  maximum = unId . foldl1 max
  {-# INLINE maximum #-}
  minimum = unId . foldl1 min
  {-# INLINE minimum #-}


-- TODO: benchmark: `fmap snd . isteps`
steps :: forall r ix e m . (Monad m, Index ix, Source r e) => Array r ix e -> Steps m e
steps arr = k `seq` arr `seq` Steps (S.Stream step 0) (LengthExact (coerce k))
  where
    k = totalElem $ size arr
    step i
      | i < k =
        let e = unsafeLinearIndex arr i
         in e `seq` return $ S.Yield e (i + 1)
      | otherwise = return S.Done
    {-# INLINE step #-}
{-# INLINE steps #-}


isteps :: forall r ix e m . (Monad m, Index ix, Source r e) => Array r ix e -> Steps m (ix, e)
isteps arr = k `seq` arr `seq` Steps (S.Stream step 0) (LengthExact (coerce k))
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

toBundle :: (Monad m, Index ix, Source r e) => Array r ix e -> B.Bundle m v e
toBundle arr =
  let Steps str k = steps arr
   in B.fromStream str (sizeHintToBundleSize k)
{-# INLINE toBundle #-}

fromBundle :: Manifest r e => B.Bundle Id v e -> Vector r e
fromBundle bundle = fromStream (B.sSize bundle) (B.sElems bundle)
{-# INLINE fromBundle #-}


fromBundleM :: (Monad m, Manifest r e) => B.Bundle m v e -> m (Vector r e)
fromBundleM bundle = fromStreamM (B.sSize bundle) (B.sElems bundle)
{-# INLINE fromBundleM #-}


fromStream :: forall r e . Manifest r e => B.Size -> S.Stream Id e -> Vector r e
fromStream sz str =
  case B.upperBound sz of
    Nothing -> unstreamUnknown str
    Just k  -> unstreamMax k str
{-# INLINE fromStream #-}

fromStreamM :: forall r e m. (Monad m, Manifest r e) => B.Size -> S.Stream m e -> m (Vector r e)
fromStreamM sz str = do
  xs <- S.toList str
  case B.upperBound sz of
    Nothing -> pure $! unstreamUnknown (S.fromList xs)
    Just k  -> pure $! unstreamMax k (S.fromList xs)
{-# INLINE fromStreamM #-}

fromStreamExactM ::
     forall r ix e m. (Monad m, Manifest r e, Index ix)
  => Sz ix
  -> S.Stream m e
  -> m (Array r ix e)
fromStreamExactM sz str = do
  xs <- S.toList str
  pure $! unstreamExact sz (S.fromList xs)
{-# INLINE fromStreamExactM #-}


unstreamIntoM ::
     (Manifest r a, PrimMonad m)
  => MVector (PrimState m) r a
  -> LengthHint
  -> S.Stream Id a
  -> m (MVector (PrimState m) r a)
unstreamIntoM marr sz str =
  case sz of
    LengthExact _ -> marr <$ unstreamMaxM marr str
    LengthMax _   -> unsafeLinearShrink marr . SafeSz =<< unstreamMaxM marr str
    LengthUnknown -> unstreamUnknownM marr str
{-# INLINE unstreamIntoM #-}



unstreamMax ::
     forall r e. (Manifest r e)
  => Int
  -> S.Stream Id e
  -> Vector r e
unstreamMax kMax str =
  runST $ do
    marr <- unsafeNew (SafeSz kMax)
    k <- unstreamMaxM marr str
    unsafeLinearShrink marr (SafeSz k) >>= unsafeFreeze Seq
{-# INLINE unstreamMax #-}


unstreamMaxM ::
     (Manifest r a, Index ix, PrimMonad m) => MArray (PrimState m) r ix a -> S.Stream Id a -> m Int
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


unstreamUnknown :: Manifest r a => S.Stream Id a -> Vector r a
unstreamUnknown str =
  runST $ do
    marr <- unsafeNew zeroSz
    unstreamUnknownM marr str >>= unsafeFreeze Seq
{-# INLINE unstreamUnknown #-}


unstreamUnknownM ::
     (Manifest r a, PrimMonad m)
  => MVector (PrimState m) r a
  -> S.Stream Id a
  -> m (MVector (PrimState m) r a)
unstreamUnknownM marrInit (S.Stream step s) = stepLoad s 0 (unSz (sizeOfMArray marrInit)) marrInit
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
     forall r ix e. (Manifest r e, Index ix)
  => Sz ix
  -> S.Stream Id e
  -> Array r ix e
unstreamExact sz str =
  runST $ do
    marr <- unsafeNew sz
    _ <- unstreamMaxM marr str
    unsafeFreeze Seq marr
{-# INLINE unstreamExact #-}

length :: Monad m => Steps m a -> m Int
length (Steps str sz) =
  case sz of
    LengthExact k -> pure $ coerce k
    _             -> S.length str
{-# INLINE length #-}


null :: Monad m => Steps m a -> m Bool
null (Steps str sz) =
  case sz of
    LengthExact k -> pure (k == zeroSz)
    _         -> S.null str
{-# INLINE null #-}

empty :: Monad m => Steps m e
empty = Steps S.empty (LengthExact zeroSz)
{-# INLINE empty #-}

singleton :: Monad m => e -> Steps m e
singleton e = Steps (S.singleton e) (LengthExact oneSz)
{-# INLINE singleton #-}

generate :: Monad m => Sz1 -> (Int -> e) -> Steps m e
generate k f = Steps (S.generate (coerce k) f) (LengthExact k)
{-# INLINE generate #-}

-- | First element of the 'Stream' or error if empty
headMaybe :: Monad m => Steps m a -> m (Maybe a)
headMaybe (Steps (S.Stream step t) _) = headMaybeLoop S.SPEC t
  where
    headMaybeLoop !_ s = do
      r <- step s
      case r of
        S.Yield x _ -> pure $ Just x
        S.Skip s'   -> headMaybeLoop S.SPEC s'
        S.Done      -> pure Nothing
    {-# INLINE [0] headMaybeLoop #-}
{-# INLINE headMaybe #-}


cons :: Monad m => e -> Steps m e -> Steps m e
cons e (Steps str k) = Steps (S.cons e str) (k `addInt` 1)
{-# INLINE cons #-}

-- | First element of the `Steps` or `Nothing` if empty
uncons :: Monad m => Steps m e -> m (Maybe (e, Steps m e))
uncons sts = (\mx -> (, drop oneSz sts) <$> mx) <$> headMaybe sts
{-# INLINE uncons #-}

snoc :: Monad m => Steps m e -> e -> Steps m e
snoc (Steps str k) e = Steps (S.snoc str e) (k `addInt` 1)
{-# INLINE snoc #-}

traverse :: (Monad m, Applicative f) => (e -> f a) -> Steps Id e -> f (Steps m a)
traverse f (Steps str k) = (`Steps` k) <$> liftListA (Traversable.traverse f) str
{-# INLINE traverse #-}

append :: Monad m => Steps m e -> Steps m e -> Steps m e
append (Steps str1 k1) (Steps str2 k2) = Steps (str1 S.++ str2) (k1 `addLengthHint` k2)
{-# INLINE append #-}

map :: Monad m => (e -> a) -> Steps m e -> Steps m a
map f (Steps str k) = Steps (S.map f str) k
{-# INLINE map #-}

indexed :: Monad m => Steps m e -> Steps m (Int, e)
indexed (Steps str k) = Steps (S.indexed str) k
{-# INLINE indexed #-}

mapM :: Monad m => (e -> m a) -> Steps m e -> Steps m a
mapM f (Steps str k) = Steps (S.mapM f str) k
{-# INLINE mapM #-}

mapM_ :: Monad m => (e -> m a) -> Steps m e -> m ()
mapM_ f (Steps str _) = S.mapM_ f str
{-# INLINE mapM_ #-}

zipWith :: Monad m => (a -> b -> e) -> Steps m a -> Steps m b -> Steps m e
zipWith f (Steps sa ka) (Steps sb kb) = Steps (S.zipWith f sa sb) (minLengthHint ka kb)
{-# INLINE zipWith #-}

zipWith3 :: Monad m => (a -> b -> c -> d) -> Steps m a -> Steps m b -> Steps m c -> Steps m d
zipWith3 f (Steps sa ka) (Steps sb kb) (Steps sc kc) =
  Steps (S.zipWith3 f sa sb sc) (minLengthHint ka (minLengthHint kb kc))
{-# INLINE zipWith3 #-}

zipWith4 ::
  Monad m => (a -> b -> c -> d -> e) -> Steps m a -> Steps m b -> Steps m c -> Steps m d -> Steps m e
zipWith4 f (Steps sa ka) (Steps sb kb) (Steps sc kc) (Steps sd kd) =
  Steps (S.zipWith4 f sa sb sc sd) (minLengthHint ka (minLengthHint kb (minLengthHint kc kd)))
{-# INLINE zipWith4 #-}

zipWith5 ::
     Monad m
  => (a -> b -> c -> d -> e -> f)
  -> Steps m a
  -> Steps m b
  -> Steps m c
  -> Steps m d
  -> Steps m e
  -> Steps m f
zipWith5 f (Steps sa ka) (Steps sb kb) (Steps sc kc) (Steps sd kd) (Steps se ke) =
  Steps (S.zipWith5 f sa sb sc sd se) (minLengthHint ka (minLengthHint kb (minLengthHint kc (minLengthHint kd ke))))
{-# INLINE zipWith5 #-}

zipWith6 ::
     Monad m
  => (a -> b -> c -> d -> e -> f -> g)
  -> Steps m a
  -> Steps m b
  -> Steps m c
  -> Steps m d
  -> Steps m e
  -> Steps m f
  -> Steps m g
zipWith6 f (Steps sa ka) (Steps sb kb) (Steps sc kc) (Steps sd kd) (Steps se ke) (Steps sf kf) =
  Steps
    (S.zipWith6 f sa sb sc sd se sf)
    (minLengthHint ka (minLengthHint kb (minLengthHint kc (minLengthHint kd (minLengthHint ke kf)))))
{-# INLINE zipWith6 #-}

zipWithM :: Monad m => (a -> b -> m c) -> Steps m a -> Steps m b -> Steps m c
zipWithM f (Steps sa ka) (Steps sb kb) = Steps (S.zipWithM f sa sb) (minLengthHint ka kb)
{-# INLINE zipWithM #-}


zipWith3M :: Monad m => (a -> b -> c -> m d) -> Steps m a -> Steps m b -> Steps m c -> Steps m d
zipWith3M f (Steps sa ka) (Steps sb kb) (Steps sc kc) =
  Steps (S.zipWith3M f sa sb sc) (minLengthHint ka (minLengthHint kb kc))
{-# INLINE zipWith3M #-}

zipWith4M ::
     Monad m
  => (a -> b -> c -> d -> m e)
  -> Steps m a
  -> Steps m b
  -> Steps m c
  -> Steps m d
  -> Steps m e
zipWith4M f (Steps sa ka) (Steps sb kb) (Steps sc kc) (Steps sd kd) =
  Steps (S.zipWith4M f sa sb sc sd) (minLengthHint ka (minLengthHint kb (minLengthHint kc kd)))
{-# INLINE zipWith4M #-}

zipWith5M ::
     Monad m
  => (a -> b -> c -> d -> e -> m f)
  -> Steps m a
  -> Steps m b
  -> Steps m c
  -> Steps m d
  -> Steps m e
  -> Steps m f
zipWith5M f (Steps sa ka) (Steps sb kb) (Steps sc kc) (Steps sd kd) (Steps se ke) =
  Steps (S.zipWith5M f sa sb sc sd se) (minLengthHint ka (minLengthHint kb (minLengthHint kc (minLengthHint kd ke))))
{-# INLINE zipWith5M #-}

zipWith6M ::
     Monad m
  => (a -> b -> c -> d -> e -> f -> m g)
  -> Steps m a
  -> Steps m b
  -> Steps m c
  -> Steps m d
  -> Steps m e
  -> Steps m f
  -> Steps m g
zipWith6M f (Steps sa ka) (Steps sb kb) (Steps sc kc) (Steps sd kd) (Steps se ke) (Steps sf kf) =
  Steps
    (S.zipWith6M f sa sb sc sd se sf)
    (minLengthHint ka (minLengthHint kb (minLengthHint kc (minLengthHint kd (minLengthHint ke kf)))))
{-# INLINE zipWith6M #-}


zipWithM_ :: Monad m => (a -> b -> m c) -> Steps m a -> Steps m b -> m ()
zipWithM_ f (Steps str1 _) (Steps str2 _) = S.zipWithM_ f str1 str2
{-# INLINE zipWithM_ #-}

zipWith3M_ :: Monad m => (a -> b -> c -> m d) -> Steps m a -> Steps m b -> Steps m c -> m ()
zipWith3M_ f sa sb sc = consume $ zipWith3M f sa sb sc
{-# INLINE zipWith3M_ #-}


zipWith4M_ ::
     Monad m
  => (a -> b -> c -> d -> m e)
  -> Steps m a
  -> Steps m b
  -> Steps m c
  -> Steps m d
  -> m ()
zipWith4M_ f sa sb sc sd = consume $ zipWith4M f sa sb sc sd
{-# INLINE zipWith4M_ #-}

zipWith5M_ ::
     Monad m
  => (a -> b -> c -> d -> e -> m f)
  -> Steps m a
  -> Steps m b
  -> Steps m c
  -> Steps m d
  -> Steps m e
  -> m ()
zipWith5M_ f sa sb sc sd se = consume $ zipWith5M f sa sb sc sd se
{-# INLINE zipWith5M_ #-}

zipWith6M_ ::
     Monad m
  => (a -> b -> c -> d -> e -> f -> m g)
  -> Steps m a
  -> Steps m b
  -> Steps m c
  -> Steps m d
  -> Steps m e
  -> Steps m f
  -> m ()
zipWith6M_ f sa sb sc sd se sf = consume $ zipWith6M f sa sb sc sd se sf
{-# INLINE zipWith6M_ #-}



consume :: Monad m => Steps m a -> m ()
consume (Steps (S.Stream step t) _) = consumeLoop S.SPEC t
  where
    consumeLoop !_ s = do
      r <- step s
      case r of
        S.Yield _ s' -> consumeLoop S.SPEC s'
        S.Skip s' -> consumeLoop S.SPEC s'
        S.Done -> pure ()
{-# INLINE consume #-}

transStepsId :: Monad m => Steps Id e -> Steps m e
transStepsId (Steps sts k) = Steps (S.trans (pure . unId) sts) k
{-# INLINE transStepsId #-}

transSteps :: (Monad m, Monad n) => Steps m e -> m (Steps n e)
transSteps (Steps strM sz@(LengthExact _)) = (`Steps` sz) <$> transListM strM
transSteps (Steps strM _) = do
  (n, strN) <- transListNM strM
  pure (Steps strN (LengthExact n))
{-# INLINE transSteps #-}


foldl :: Monad m => (b -> a -> b) -> b -> Steps m a -> m b
foldl f acc = S.foldl' f acc . stepsStream
{-# INLINE foldl #-}

foldl1 :: Monad m => (a -> a -> a) -> Steps m a -> m a
foldl1 f = S.foldl1' f . stepsStream
{-# INLINE foldl1 #-}


foldlM :: Monad m => (a -> b -> m a) -> a -> Steps m b -> m a
foldlM f acc = S.foldlM' f acc . stepsStream
{-# INLINE foldlM #-}


foldl1M :: Monad m => (a -> a -> m a) -> Steps m a -> m a
foldl1M f (Steps sts _) = S.foldl1M' f sts
{-# INLINE foldl1M #-}


foldrLazy :: Monad m => (a -> b -> b) -> b -> Steps m a -> m b
foldrLazy f acc = S.foldr f acc . stepsStream
{-# INLINE foldrLazy #-}

foldr1Lazy :: Monad m => (a -> a -> a) -> Steps m a -> m a
foldr1Lazy f = S.foldr1 f . stepsStream
{-# INLINE foldr1Lazy #-}

foldlLazy :: Monad m => (b -> a -> b) -> b -> Steps m a -> m b
foldlLazy f acc = S.foldl f acc . stepsStream
{-# INLINE foldlLazy #-}

foldl1Lazy :: Monad m => (a -> a -> a) -> Steps m a -> m a
foldl1Lazy f = S.foldl1 f . stepsStream
{-# INLINE foldl1Lazy #-}


foldlLazyM :: Monad m => (a -> b -> m a) -> a -> Steps m b -> m a
foldlLazyM f acc = S.foldlM f acc . stepsStream
{-# INLINE foldlLazyM #-}


foldl1LazyM :: Monad m => (a -> a -> m a) -> Steps m a -> m a
foldl1LazyM f (Steps sts _) = S.foldl1M f sts
{-# INLINE foldl1LazyM #-}


foldrLazyM :: Monad m => (b -> a -> m a) -> a -> Steps m b -> m a
foldrLazyM f acc (Steps sts _) = S.foldrM f acc sts
{-# INLINE foldrLazyM #-}


foldr1LazyM :: Monad m => (a -> a -> m a) -> Steps m a -> m a
foldr1LazyM f = S.foldr1M f . stepsStream
{-# INLINE foldr1LazyM #-}


or :: Monad m => Steps m Bool -> m Bool
or = S.or . stepsStream
{-# INLINE or #-}

and :: Monad m => Steps m Bool -> m Bool
and = S.and . stepsStream
{-# INLINE and #-}


mapMaybe :: Monad m => (a -> Maybe e) -> Steps m a -> Steps m e
mapMaybe f (Steps str k) = Steps (S.mapMaybe f str) (toLengthMax k)
{-# INLINE mapMaybe #-}

concatMap :: Monad m => (a -> Steps m e) -> Steps m a -> Steps m e
concatMap f (Steps str _) = Steps (S.concatMap (stepsStream . f) str) LengthUnknown
{-# INLINE concatMap #-}


mapMaybeA :: (Monad m, Applicative f) => (a -> f (Maybe e)) -> Steps Id a -> f (Steps m e)
mapMaybeA f (Steps str k) = (`Steps` toLengthMax k) <$> liftListA (mapMaybeListA f) str
{-# INLINE mapMaybeA #-}

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> Steps m a -> Steps m b
mapMaybeM f (Steps str k) = Steps (mapMaybeStreamM f str) (toLengthMax k)
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
    {-# INLINE [0] step' #-}
{-# INLINE mapMaybeStreamM #-}

filter :: Monad m => (a -> Bool) -> Steps m a -> Steps m a
filter f (Steps str k) = Steps (S.filter f str) (toLengthMax k)
{-# INLINE filter #-}


filterA :: (Monad m, Applicative f) => (e -> f Bool) -> Steps Id e -> f (Steps m e)
filterA f (Steps str k) = (`Steps` toLengthMax k) <$> liftListA (M.filterM f) str
{-# INLINE filterA #-}

filterM :: Monad m => (e -> m Bool) -> Steps m e -> Steps m e
filterM f (Steps str k) = Steps (S.filterM f str) (toLengthMax k)
{-# INLINE filterM #-}

take :: Monad m => Sz1 -> Steps m a -> Steps m a
take n (Steps str sz) =
  Steps (S.take (coerce n) str) $!
  case sz of
    LengthExact k -> LengthExact (inline0 min n k)
    LengthMax k -> LengthMax (inline0 min n k)
    LengthUnknown -> LengthUnknown
{-# INLINE take #-}

drop :: Monad m => Sz1 -> Steps m a -> Steps m a
drop n (Steps str k) = Steps (S.drop (coerce n) str) (k `subtractLengthHint` LengthExact n)
{-# INLINE drop #-}

slice :: Monad m => Int -> Sz1 -> Steps m a -> Steps m a
slice i k (Steps str _) = Steps (S.slice i (coerce k) str) (LengthMax k)
{-# INLINE slice #-}

iterateN :: Monad m => Sz1 -> (a -> a) -> a -> Steps m a
iterateN n f a = Steps (S.iterateN (coerce n) f a) (LengthExact n)
{-# INLINE iterateN #-}

iterateNM :: Monad m => Sz1 -> (a -> m a) -> a -> Steps m a
iterateNM n f a = Steps (S.iterateNM (coerce n) f a) (LengthExact n)
{-# INLINE iterateNM #-}

replicate :: Monad m => Sz1 -> a -> Steps m a
replicate n a = Steps (S.replicate (coerce n) a) (LengthExact n)
{-# INLINE replicate #-}

replicateM :: Monad m => Sz1 -> m a -> Steps m a
replicateM n f = Steps (S.replicateM (coerce n) f) (LengthExact n)
{-# INLINE replicateM #-}


generateM :: Monad m => Sz1 -> (Int -> m a) -> Steps m a
generateM n f = Steps (S.generateM (coerce n) f) (LengthExact n)
{-# INLINE generateM #-}


unfoldr :: Monad m => (s -> Maybe (e, s)) -> s -> Steps m e
unfoldr f e0 = Steps (S.unfoldr f e0) LengthUnknown
{-# INLINE unfoldr #-}

unfoldrN :: Monad m => Sz1 -> (s -> Maybe (e, s)) -> s -> Steps m e
unfoldrN n f e0 = Steps (S.unfoldrN (coerce n) f e0) LengthUnknown
{-# INLINE unfoldrN #-}

unsafeUnfoldrN :: Monad m => Sz1 -> (s -> Maybe (e, s)) -> s -> Steps m e
unsafeUnfoldrN n f e0 = Steps (S.unfoldrN (coerce n) f e0) (LengthMax n)
{-# INLINE unsafeUnfoldrN #-}

unfoldrM :: Monad m => (s -> m (Maybe (e, s))) -> s -> Steps m e
unfoldrM f e0 = Steps (S.unfoldrM f e0) LengthUnknown
{-# INLINE unfoldrM #-}

unfoldrNM :: Monad m => Int -> (s -> m (Maybe (e, s))) -> s -> Steps m e
unfoldrNM n f e0 = Steps (S.unfoldrNM n f e0) LengthUnknown
{-# INLINE unfoldrNM #-}

unsafeUnfoldrNM :: Monad m => Sz1 -> (s -> m (Maybe (e, s))) -> s -> Steps m e
unsafeUnfoldrNM n f e0 = Steps (S.unfoldrNM (coerce n) f e0) (LengthMax n)
{-# INLINE unsafeUnfoldrNM #-}

unfoldrExactN :: Monad m => Sz1 -> (s -> (a, s)) -> s -> Steps m a
unfoldrExactN n f = unfoldrExactNM n (pure . f)
{-# INLINE unfoldrExactN #-}

unfoldrExactNM :: Monad m => Sz1 -> (s -> m (a, s)) -> s -> Steps m a
unfoldrExactNM n f t = Steps (S.Stream step (t, unSz n)) (LengthExact n)
  where
    step (s, i)
      | i <= 0 = pure S.Done
      | otherwise = fmap (\(x, s') -> S.Yield x (s', i - 1)) (f s)
    {-# INLINE [0] step #-}
{-# INLINE unfoldrExactNM #-}


enumFromStepN :: (Num a, Monad m) => a -> a -> Sz1 -> Steps m a
enumFromStepN x step k = Steps (S.enumFromStepN x step (coerce k)) (LengthExact k)
{-# INLINE enumFromStepN #-}




toList :: Steps Id e -> [e]
toList (Steps str _) = unId (S.toList str)
{-# INLINE toList #-}

fromList :: Monad m => [e] -> Steps m e
fromList = (`Steps` LengthUnknown) . S.fromList
{-# INLINE fromList #-}

fromListN :: Monad m => Int -> [e] -> Steps m e
fromListN n  = (`Steps` LengthUnknown) . S.fromListN n
{-# INLINE fromListN #-}

unsafeFromListN :: Monad m => Sz1 -> [e] -> Steps m e
unsafeFromListN n  = (`Steps` LengthMax n) . S.fromListN (coerce n)
{-# INLINE unsafeFromListN #-}

liftListA :: (Monad m, Functor f) => ([a] -> f [b]) -> S.Stream Id a -> f (S.Stream m b)
liftListA f str = S.fromList <$> f (unId (S.toList str))
{-# INLINE liftListA #-}


transListM :: (Monad m, Monad n) => S.Stream m a -> m (S.Stream n a)
transListM str = do
  xs <- S.toList str
  pure $ S.fromList xs
{-# INLINE transListM #-}

transListNM :: (Monad m, Monad n) => S.Stream m a -> m (Sz1, S.Stream n a)
transListNM str = do
  (n, xs) <- toListN str
  pure (coerce n, S.fromList xs)
{-# INLINE transListNM #-}


toListN :: Monad m => S.Stream m a -> m (Int, [a])
toListN = S.foldr (\x (i, xs) -> (i + 1, x:xs)) (0, [])
{-# INLINE toListN #-}


sizeHintToBundleSize :: LengthHint -> B.Size
sizeHintToBundleSize =
  \case
    LengthExact k -> B.Exact (coerce k)
    LengthMax k   -> B.Max (coerce k)
    LengthUnknown -> B.Unknown
{-# INLINE sizeHintToBundleSize #-}

addHint :: (Sz1 -> LengthHint) -> Int -> Int -> LengthHint
addHint hint m n
  | k == coerce sz = hint sz
  | otherwise = LengthUnknown -- overflow
  where
    k = m + n
    sz = Sz k
{-# INLINE addHint #-}



addInt :: LengthHint -> Int -> LengthHint
addInt (LengthExact m) n = addHint LengthExact (coerce m) (coerce n)
addInt (LengthMax   m) n = addHint LengthExact (coerce m) n
addInt _               _ = LengthUnknown
{-# INLINE addInt #-}

addLengthHint :: LengthHint -> LengthHint -> LengthHint
addLengthHint (LengthExact m) (LengthExact n) = addHint LengthExact (coerce m) (coerce n)
addLengthHint (LengthMax   m) (LengthExact n) = addHint LengthMax (coerce m) (coerce n)
addLengthHint (LengthExact m) (LengthMax   n) = addHint LengthMax (coerce m) (coerce n)
addLengthHint (LengthMax   m) (LengthMax   n) = addHint LengthMax (coerce m) (coerce n)
addLengthHint _               _               = LengthUnknown
{-# INLINE addLengthHint #-}

subtractLengthHint :: LengthHint -> LengthHint -> LengthHint
subtractLengthHint (LengthExact m) (LengthExact n) = LengthExact (m - n)
subtractLengthHint (LengthMax   m) (LengthExact n) = LengthMax (m - n)
subtractLengthHint (LengthExact m) (LengthMax   _) = LengthMax m
subtractLengthHint (LengthMax   m) (LengthMax   _) = LengthMax m
subtractLengthHint _               _               = LengthUnknown
{-# INLINE subtractLengthHint #-}


minLengthHint :: LengthHint -> LengthHint -> LengthHint
minLengthHint (LengthExact m) (LengthExact n) = LengthExact (inline0 min m n)
minLengthHint (LengthExact m) (LengthMax   n) = LengthMax   (inline0 min m n)
minLengthHint (LengthExact m) LengthUnknown   = LengthMax   m
minLengthHint (LengthMax   m) (LengthExact n) = LengthMax   (inline0 min m n)
minLengthHint (LengthMax   m) (LengthMax   n) = LengthMax   (inline0 min m n)
minLengthHint (LengthMax   m) LengthUnknown   = LengthMax   m
minLengthHint LengthUnknown   (LengthExact n) = LengthMax   n
minLengthHint LengthUnknown   (LengthMax   n) = LengthMax   n
minLengthHint LengthUnknown   LengthUnknown   = LengthUnknown
{-# INLINE minLengthHint #-}

toLengthMax :: LengthHint -> LengthHint
toLengthMax (LengthExact n) = LengthMax n
toLengthMax (LengthMax   n) = LengthMax n
toLengthMax LengthUnknown   = LengthUnknown
{-# INLINE toLengthMax #-}


