{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Massiv.Core.List
-- Copyright   : (c) Alexey Kuleshevich 2018-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Core.List
  ( L(..)
  , Array(..)
  , List(..)
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


type family ListItem ix e :: Type where
  ListItem Ix1 e = e
  ListItem ix  e = [ListItem (Lower ix) e]

type family Elt ix e :: Type where
  Elt Ix1 e = e
  Elt ix  e = List (Lower ix) e

newtype List ix e = List { unList :: [Elt ix e] }


instance Coercible (Elt ix e) (ListItem ix e) => IsList (List ix e) where
  type Item (List ix e) = ListItem ix e
  fromList = coerce
  {-# INLINE fromList #-}
  toList = coerce
  {-# INLINE toList #-}


data L = L

data instance Array L ix e = LArray { lComp :: Comp
                                    , lData :: !(List ix e)
                                    }


instance Coercible (Elt ix e) (ListItem ix e) => IsList (Array L ix e) where
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

instance Shape L Ix1 where
  linearSize = outerLength
  {-# INLINE linearSize #-}
  linearSizeHint = lengthHintList . unList . lData
  {-# INLINE linearSizeHint #-}
  isNull = null . unList . lData
  {-# INLINE isNull #-}
  outerSize = linearSize
  {-# INLINE outerSize #-}

instance Shape L Ix2 where
  linearSize = SafeSz . getSum . foldMap (Sum . length . unList) . unList . lData
  {-# INLINE linearSize #-}
  linearSizeHint = lengthHintList . unList . lData
  {-# INLINE linearSizeHint #-}
  isNull = getAll . foldMap (All . null . unList) . unList . lData
  {-# INLINE isNull #-}
  outerSize arr =
    case unList (lData arr) of
      []     -> zeroSz
      (x:xs) -> SafeSz ((1 + length xs) :. length (unList x))
  {-# INLINE outerSize #-}

instance (Shape L (Ix (n - 1)), Index (IxN n)) => Shape L (IxN n) where
  linearSize = SafeSz . getSum . foldMap (Sum . unSz . linearSize . LArray Seq) . unList . lData
  {-# INLINE linearSize #-}
  linearSizeHint = lengthHintList . unList . lData
  {-# INLINE linearSizeHint #-}
  isNull = getAll . foldMap (All . isNull . LArray Seq) . unList . lData
  {-# INLINE isNull #-}
  outerSize arr =
    case unList (lData arr) of
      []     -> zeroSz
      (x:xs) -> SafeSz ((1 + length xs) :> unSz (outerSize (LArray Seq x)))
  {-# INLINE outerSize #-}


outerLength :: Array L ix e -> Sz Int
outerLength = SafeSz . length . unList . lData
{-# INLINE outerLength #-}


instance Ragged L Ix1 e where
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
        | otherwise = throwM $ DimTooLongException 1 sz (outerLength xs)
      go [] i = when (i /= end) $ throwM $ DimTooShortException 1 sz (outerLength xs)
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
  generateRaggedM = unsafeGenerateParM
  {-# INLINE generateRaggedM #-}
  flattenRagged arr = LArray {lComp = lComp arr, lData = coerce xs}
    where
      xs = concatMap (unList . lData . flattenRagged . LArray (lComp arr)) (unList (lData arr))
  {-# INLINE flattenRagged #-}
  loadRaggedST scheduler xs uWrite start end sz
    | isZeroSz sz = when (isNotNull (flattenRagged xs)) (throwM ShapeNonEmpty)
    | otherwise = do
      let (k, szL) = unconsSz sz
          step = totalElem szL
      leftOver <-
        loopM start (< end) (+ step) (coerce (lData xs)) $ \i zs ->
          case zs of
            [] -> throwM (DimTooShortException 2 k (outerLength xs))
            (y:ys) -> do
              scheduleWork_ scheduler $
                let end' = i + step
                    go (a:as) j
                      | j < end' = uWrite j a >> go as (j + 1)
                      | otherwise = throwM $ DimTooLongException 1 szL (Sz (length y))
                    go [] j = when (j /= end') $ throwM (DimTooShortException 1 szL (Sz (length y)))
                 in go y i
              pure ys
      unless (null leftOver) $ throwM $ DimTooLongException 2 k (outerLength xs)
  {-# INLINE loadRaggedST #-}
  raggedFormat f sep (LArray comp xs) =
    showN (\s y -> raggedFormat f s (LArray comp y :: Array L Ix1 e)) sep (coerce xs)

instance ( Shape L (IxN n)
         , Ragged L (Ix (n - 1)) e
         , Coercible (Elt (Ix (n - 1)) e) (ListItem (Ix (n - 1)) e)
         ) =>
         Ragged L (IxN n) e where
  generateRaggedM = unsafeGenerateParM
  {-# INLINE generateRaggedM #-}
  flattenRagged arr = LArray {lComp = lComp arr, lData = coerce xs}
    where
      xs = concatMap (unList . lData . flattenRagged . LArray (lComp arr)) (unList (lData arr))
  {-# INLINE flattenRagged #-}
  loadRaggedST scheduler xs uWrite start end sz
    | isZeroSz sz = when (isNotNull (flattenRagged xs)) (throwM ShapeNonEmpty)
    | otherwise = do
      let (k, szL) = unconsSz sz
          step = totalElem szL
          subScheduler
            | end - start < numWorkers scheduler * step = scheduler
            | otherwise = trivialScheduler_
      leftOver <-
        loopM start (< end) (+ step) (unList (lData xs)) $ \i zs ->
          case zs of
            [] -> throwM (DimTooShortException (dimensions sz) k (outerLength xs))
            (y:ys) -> do
              scheduleWork_ scheduler $
                loadRaggedST subScheduler (LArray Seq y) uWrite i (i + step) szL
              pure ys
      unless (null leftOver) $ throwM $ DimTooLongException (dimensions sz) k (outerLength xs)
  {-# INLINE loadRaggedST #-}
  raggedFormat f sep (LArray comp xs) =
    showN (\s y -> raggedFormat f s (LArray comp y :: Array L (Ix (n - 1)) e)) sep (coerce xs)

unsafeGenerateParM ::
     (Elt ix e ~ List (Lower ix) e, Index ix, Monad m, Ragged L (Lower ix) e)
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
        loopA_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
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
  repr = L

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
  showsPrec n arr  = showsArrayLAsPrec (Proxy :: Proxy L) (outerSize arr) n arr

instance (Ragged L ix e, Show e) => Show (List ix e) where
  show xs = "  " ++ raggedFormat show "\n  " arrL
    where arrL = LArray Seq xs :: Array L ix e


showN :: (String -> a -> String) -> String -> [a] -> String
showN _     _        [] = "[  ]"
showN fShow lnPrefix ls =
  L.concat
    (["[ "] ++
     L.intersperse (lnPrefix ++ ", ") (map (fShow (lnPrefix ++ "  ")) ls) ++ [lnPrefix, "]"])


showsArrayLAsPrec ::
     forall r ix e. (Ragged L ix e, Typeable r, Show e)
  => Proxy r
  -> Sz ix
  -> Int
  -> Array L ix e -- Array to show
  -> ShowS
showsArrayLAsPrec pr sz n arr =
  opp .
  ("Array " ++) .
  showsTypeRep (typeRep pr) .
  (' ':) .
  showsPrec 1 (getComp arr) . (" (" ++) . shows sz . (")\n" ++) . shows lnarr . clp
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
showsArrayPrec f n arr = showsArrayLAsPrec (Proxy :: Proxy r) sz n larr
  where
    sz = size arr'
    arr' = f arr
    larr = makeArray (getComp arr') sz (evaluate' arr') :: Array L ix e


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
