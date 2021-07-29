{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Massiv.Core.List
-- Copyright   : (c) Alexey Kuleshevich 2018-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Core.List
  ( LN
  , L(..)
  , Array(..)
  , toListArray
  , showsArrayPrec
  , showArrayList
  , ListItem
  ) where

import Control.Monad (unless, when)
import Control.Scheduler
import Data.Coerce
import Data.Functor.Identity
import Data.Kind
import qualified Data.List as L
import Data.Massiv.Core.Common
import qualified Data.Massiv.Vector.Stream as S
import Data.Monoid
import Data.Typeable
import GHC.Exts
import GHC.TypeLits
import System.IO.Unsafe (unsafePerformIO)

data LN

type family ListItem ix e :: Type where
  ListItem Ix1 e = e
  ListItem ix  e = [ListItem (Lower ix) e]

newtype instance Array LN ix e = List { unList :: [Elt LN ix e] }

--TODO remove
instance Strategy LN where
  getComp _ = Seq
  setComp _ = id


instance Coercible (Elt LN ix e) (ListItem ix e) => IsList (Array LN ix e) where
  type Item (Array LN ix e) = ListItem ix e
  fromList = coerce
  {-# INLINE fromList #-}
  toList = coerce
  {-# INLINE toList #-}


data L = L

data instance Array L ix e = LArray { lComp :: Comp
                                    , lData :: !(Array LN ix e)
                                    }


instance Coercible (Elt LN ix e) (ListItem ix e) => IsList (Array L ix e) where
  type Item (Array L ix e) = ListItem ix e
  fromList = LArray Seq . coerce
  {-# INLINE fromList #-}
  toList = coerce . lData
  {-# INLINE toList #-}

lengthHintList :: [a] -> LengthHint
lengthHintList =
  \case
    [] -> LengthExact zeroSz
    _  -> LengthUnknown
{-# INLINE lengthHintList #-}

instance Shape LN Ix1 where
  linearSize = SafeSz . length . unList
  {-# INLINE linearSize #-}
  linearSizeHint = lengthHintList . unList
  {-# INLINE linearSizeHint #-}
  isNull = null . unList
  {-# INLINE isNull #-}
  outerSize = linearSize
  {-# INLINE outerSize #-}

instance Shape L Ix1 where
  linearSize = linearSize . lData
  {-# INLINE linearSize #-}
  linearSizeHint = linearSizeHint . lData
  {-# INLINE linearSizeHint #-}
  isNull = isNull . lData
  {-# INLINE isNull #-}
  outerSize = linearSize
  {-# INLINE outerSize #-}

instance Shape LN Ix2 where
  linearSize = SafeSz . getSum . foldMap (Sum . length . unList) . unList
  {-# INLINE linearSize #-}
  linearSizeHint = lengthHintList . unList
  {-# INLINE linearSizeHint #-}
  isNull = getAll . foldMap (All . null . unList) . unList
  {-# INLINE isNull #-}
  outerSize arr =
    case unList arr of
      []     -> zeroSz
      (x:xs) -> SafeSz ((1 + length xs) :. length (unList x))
  {-# INLINE outerSize #-}

instance Shape L Ix2 where
  linearSize = linearSize . lData
  {-# INLINE linearSize #-}
  linearSizeHint = linearSizeHint . lData
  {-# INLINE linearSizeHint #-}
  isNull = isNull . lData
  {-# INLINE isNull #-}
  outerSize = outerSize . lData
  {-# INLINE outerSize #-}

instance (Shape LN (Ix (n - 1)), Index (IxN n)) => Shape LN (IxN n) where
  linearSize = SafeSz . getSum . foldMap (Sum . unSz . linearSize) . unList
  {-# INLINE linearSize #-}
  linearSizeHint = lengthHintList . unList
  {-# INLINE linearSizeHint #-}
  isNull = getAll . foldMap (All . isNull) . unList
  {-# INLINE isNull #-}
  outerSize arr =
    case unList arr of
      []     -> zeroSz
      (x:xs) -> SafeSz ((1 + length xs) :> unSz (outerSize x))
  {-# INLINE outerSize #-}


instance (Index (IxN n), Shape LN (IxN n)) => Shape L (IxN n) where
  linearSize = linearSize . lData
  {-# INLINE linearSize #-}
  linearSizeHint = linearSizeHint . lData
  {-# INLINE linearSizeHint #-}
  isNull = isNull . lData
  {-# INLINE isNull #-}
  outerSize = outerSize . lData
  {-# INLINE outerSize #-}


outerLength :: Array L ix e -> Sz Int
outerLength = SafeSz . length . unList . lData


instance Ragged L Ix1 e where
  emptyR comp = LArray comp (List [])
  {-# INLINE emptyR #-}
  consR x arr = arr {lData = coerce (x : coerce (lData arr))}
  {-# INLINE consR #-}
  unconsR LArray {..} =
    case L.uncons $ coerce lData of
      Nothing -> Nothing
      Just (x, xs) -> Just (x, LArray lComp (coerce xs))
  {-# INLINE unconsR #-}
  flattenRagged = id
  {-# INLINE flattenRagged #-}
  generateRaggedM !comp !k f = do
    xs <-
      loopDeepM 0 (< coerce k) (+ 1) [] $ \i acc -> do
        e <- f i
        return (e : acc)
    return $ LArray comp $ coerce xs
  {-# INLINE generateRaggedM #-}
  loadRaggedST _scheduler xs uWrite start end sz = go (unList (lData xs)) start
    where
      go (y:ys) i
        | i < end = uWrite i y >> go ys (i + 1)
        | otherwise = throwM (DimTooShortException sz (outerLength xs))
      go [] i = when (i /= end) $ throwM DimTooLongException
  {-# INLINE loadRaggedST #-}
  raggedFormat f _ arr = L.concat $ "[ " : L.intersperse ", " (map f (coerce (lData arr))) ++ [" ]"]


instance (Shape L ix, Ragged L ix e) => Load L ix e where
  makeArray comp sz f = runIdentity $ generateRaggedM comp sz (pure . f)
  {-# INLINE makeArray #-}
  iterArrayLinearST_ scheduler arr uWrite =
    loadRaggedST scheduler arr uWrite 0 (totalElem sz) sz
    where !sz = outerSize arr
  {-# INLINE iterArrayLinearST_ #-}

instance Ragged L Ix2 e where
  emptyR comp = LArray comp (List [])
  {-# INLINE emptyR #-}
  consR (LArray _ x) arr = newArr
    where
      newArr = arr {lData = coerce (x : coerce (lData arr))}
  {-# INLINE consR #-}
  unconsR LArray {..} =
    case L.uncons (coerce lData) of
      Nothing -> Nothing
      Just (x, xs) ->
        let newArr = LArray lComp (coerce xs)
            newX = LArray lComp x
         in Just (newX, newArr)
  {-# INLINE unconsR #-}
  generateRaggedM = unsafeGenerateParM
  {-# INLINE generateRaggedM #-}
  flattenRagged arr = LArray {lComp = lComp arr, lData = coerce xs}
    where
      xs = concatMap (unList . lData . flattenRagged . LArray (lComp arr)) (unList (lData arr))
  {-# INLINE flattenRagged #-}
  loadRaggedST scheduler xs uWrite start end sz
    | isZeroSz sz = when (isNotNull (flattenRagged xs)) (throwM DimTooLongException)
    | otherwise = do
      let (k, szL) = unconsSz sz
          step = totalElem szL
      leftOver <-
        loopM start (< end) (+ step) (coerce (lData xs)) $ \i zs ->
          case zs of
            [] -> throwM (DimTooShortException k (outerLength xs))
            (y:ys) -> do
              scheduleWork_ scheduler $
                let end' = i + step
                    go (a:as) j
                      | j < end' = uWrite j a >> go as (j + 1)
                      | otherwise = throwM (DimTooShortException szL (Sz (length y)))
                    go [] j = when (j /= end') $ throwM DimTooLongException
                 in go y i
              pure ys
      unless (null leftOver) (throwM DimTooLongException)
  {-# INLINE loadRaggedST #-}
  raggedFormat f sep (LArray comp xs) =
    showN (\s y -> raggedFormat f s (LArray comp y :: Array L Ix1 e)) sep (coerce xs)

instance ( Shape L (IxN n)
         , Shape LN (Ix (n - 1))
         , Ragged L (Ix (n - 1)) e
         , Coercible (Elt LN (Ix (n - 1)) e) (ListItem (Ix (n - 1)) e)
         ) =>
         Ragged L (IxN n) e where
  emptyR comp = LArray comp (List [])
  {-# INLINE emptyR #-}
  consR (LArray _ x) arr = newArr
    where
      newArr = arr {lData = coerce (x : coerce (lData arr))}
  {-# INLINE consR #-}
  unconsR LArray {..} =
    case L.uncons (coerce lData) of
      Nothing -> Nothing
      Just (x, xs) ->
        let newArr = LArray lComp (coerce xs)
            newX = LArray lComp x
         in Just (newX, newArr)
  {-# INLINE unconsR #-}
  generateRaggedM = unsafeGenerateParM
  {-# INLINE generateRaggedM #-}
  flattenRagged arr = LArray {lComp = lComp arr, lData = coerce xs}
    where
      xs = concatMap (unList . lData . flattenRagged . LArray (lComp arr)) (unList (lData arr))
  {-# INLINE flattenRagged #-}
  loadRaggedST scheduler xs uWrite start end sz
    | isZeroSz sz = when (isNotNull (flattenRagged xs)) (throwM DimTooLongException)
    | otherwise = do
      let (k, szL) = unconsSz sz
          step = totalElem szL
          subScheduler
            | end - start < numWorkers scheduler * step = scheduler
            | otherwise = trivialScheduler_
      leftOver <-
        loopM start (< end) (+ step) xs $ \i zs ->
          case unconsR zs of
            Nothing -> throwM (DimTooShortException k (outerLength xs))
            Just (y, ys) ->
              ys <$ scheduleWork_ scheduler (loadRaggedST subScheduler y uWrite i (i + step) szL)
      unless (isNull leftOver) (throwM DimTooLongException)
  {-# INLINE loadRaggedST #-}
  raggedFormat f sep (LArray comp xs) =
    showN (\s y -> raggedFormat f s (LArray comp y :: Array L (Ix (n - 1)) e)) sep (coerce xs)

unsafeGenerateParM ::
     (Elt LN ix e ~ Array LN (Lower ix) e, Index ix, Monad m, Ragged L (Lower ix) e)
  => Comp
  -> Sz ix
  -> (ix -> m e)
  -> m (Array L ix e)
unsafeGenerateParM comp !sz f = do
  res <- sequence $ unsafePerformIO $ do
    let !(ksz, szL) = unconsSz sz
        !k = unSz ksz
    withScheduler comp $ \ scheduler ->
      splitLinearly (numWorkers scheduler) k $ \ chunkLength slackStart -> do
        loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
          scheduleWork scheduler $ do
            res <- loopDeepM start (< (start + chunkLength)) (+ 1) [] $ \i acc ->
              return (fmap lData (generateRaggedM Seq szL (\ !ixL -> f (consDim i ixL))):acc)
            return $! sequence res
        when (slackStart < k) $
          scheduleWork scheduler $ do
            res <- loopDeepM slackStart (< k) (+ 1) [] $ \i acc ->
              return (fmap lData (generateRaggedM Seq szL (\ !ixL -> f (consDim i ixL))):acc)
            return $! sequence res
  return $ LArray comp $ List $ concat res
{-# INLINE unsafeGenerateParM #-}

instance Strategy L where
  setComp c arr = arr {lComp = c}
  {-# INLINE setComp #-}
  getComp = lComp
  {-# INLINE getComp #-}

-- -- TODO: benchmark against using unsafeGenerateM directly
-- unsafeGenerateN ::
--   ( Ragged r ix e
--   , Ragged r (Lower ix) e
--   , Elt r ix e ~ Array r (Lower ix) e )
--   => Comp
--   -> Sz ix
--   -> (ix -> e)
--   -> Array r ix e
-- unsafeGenerateN comp sz f = unsafePerformIO $ do
--   let !(m, szL) = unconsSz sz
--   xs <- withScheduler comp $ \scheduler ->
--     loopM_ 0 (< coerce m) (+ 1) $ \i -> scheduleWork scheduler $
--       generateRaggedM comp szL $ \ix -> return $ f (consDim i ix)
--   return $! foldr' consR (emptyR comp) xs
-- {-# INLINE unsafeGenerateN #-}


-- | Construct an array backed by linked lists from any source array
--
-- @since 0.4.0
toListArray :: (Ragged L ix e, Shape r ix, Source r e) => Array r ix e -> Array L ix e
toListArray !arr = makeArray (getComp arr) (outerSize arr) (unsafeIndex arr)
{-# INLINE toListArray #-}



instance (Ragged L ix e, Show e) => Show (Array L ix e) where
  showsPrec = showsArrayLAsPrec (Proxy :: Proxy L)

instance (Ragged L ix e, Show e) => Show (Array LN ix e) where
  show arr = "  " ++ raggedFormat show "\n  " arrL
    where arrL = LArray Seq arr :: Array L ix e


showN :: (String -> a -> String) -> String -> [a] -> String
showN _     _        [] = "[  ]"
showN fShow lnPrefix ls =
  L.concat
    (["[ "] ++
     L.intersperse (lnPrefix ++ ", ") (map (fShow (lnPrefix ++ "  ")) ls) ++ [lnPrefix, "]"])


showsArrayLAsPrec ::
     forall r ix e. (Ragged L ix e, Typeable r, Show e)
  => Proxy r
  -> Int
  -> Array L ix e -- Array to show
  -> ShowS
showsArrayLAsPrec pr n arr =
  opp .
  ("Array " ++) .
  showsTypeRep (typeRep pr) .
  (' ':) .
  showsPrec 1 (getComp arr) . (" (" ++) . shows (outerSize arr) . (")\n" ++) . shows lnarr . clp
  where
    (opp, clp) =
      if n == 0
        then (id, id)
        else (('(':), ("\n)" ++))
    lnarr = lData arr

-- | Helper function for declaring `Show` instances for arrays
--
-- @since 0.4.0
showsArrayPrec ::
     forall r r' ix e. (Ragged L ix e, Load r ix e, Load r' ix e, Source r' e, Show e)
  => (Array r ix e -> Array r' ix e) -- ^ Modifier
  -> Int
  -> Array r ix e -- Array to show
  -> ShowS
showsArrayPrec f n arr = showsArrayLAsPrec (Proxy :: Proxy r) n larr
  where
    arr' = f arr
    larr = makeArray (getComp arr') (size arr') (evaluate' arr') :: Array L ix e


-- | Helper function for declaring `Show` instances for arrays
--
-- @since 0.4.0
showArrayList
  :: Show arr => [arr] -> String -> String
showArrayList arrs = ('[':) . go arrs . (']':)
  where
    go []     = id
    go [x]    = (' ':) . shows x . ('\n':)
    go (x:xs) = (' ':) . shows x . ("\n," ++) . go xs


instance Stream L Ix1 e where
  toStream = S.fromList . unList . lData
  {-# INLINE toStream #-}
  toStreamIx = S.indexed . S.fromList . unList . lData
  {-# INLINE toStreamIx #-}
