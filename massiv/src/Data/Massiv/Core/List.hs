{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Massiv.Core.List
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
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

import Control.Exception
import Control.Monad (unless, when)
import Control.Scheduler
import Data.Coerce
import Data.Foldable (foldr')
import qualified Data.List as L
import Data.Massiv.Core.Common
import Data.Typeable
import GHC.Exts
import System.IO.Unsafe (unsafePerformIO)

data LN

type family ListItem ix e :: * where
  ListItem Ix1 e = e
  ListItem ix  e = [ListItem (Lower ix) e]

type instance NestedStruct LN ix e = [ListItem ix e]

newtype instance Array LN ix e = List { unList :: [Elt LN ix e] }


instance {-# OVERLAPPING #-} Nested LN Ix1 e where
  fromNested = coerce
  {-# INLINE fromNested #-}
  toNested = coerce
  {-# INLINE toNested #-}

instance ( Elt LN ix e ~ Array LN (Lower ix) e
         , ListItem ix e ~ [ListItem (Lower ix) e]
         , Coercible (Elt LN ix e) (ListItem ix e)
         ) =>
         Nested LN ix e where
  fromNested = coerce
  {-# INLINE fromNested #-}
  toNested = coerce
  {-# INLINE toNested #-}


instance Nested LN ix e => IsList (Array LN ix e) where
  type Item (Array LN ix e) = ListItem ix e
  fromList = fromNested
  {-# INLINE fromList #-}
  toList = toNested
  {-# INLINE toList #-}


data L = L

type instance NestedStruct L ix e = Array LN ix e

data instance Array L ix e = LArray { lComp :: Comp
                                    , lData :: !(Array LN ix e) }


instance Nested L ix e where
  fromNested = LArray Seq
  {-# INLINE fromNested #-}
  toNested = lData
  {-# INLINE toNested #-}


instance Nested LN ix e => IsList (Array L ix e) where
  type Item (Array L ix e) = ListItem ix e
  fromList = LArray Seq . fromNested
  {-# INLINE fromList #-}
  toList = toNested . lData
  {-# INLINE toList #-}

instance {-# OVERLAPPING #-} Ragged L Ix1 e where
  isNull = null . unList . lData
  {-# INLINE isNull #-}
  emptyR comp = LArray comp (List [])
  {-# INLINE emptyR #-}
  edgeSize = SafeSz . length . unList . lData
  {-# INLINE edgeSize #-}
  consR x arr = arr { lData = coerce (x : coerce (lData arr)) }
  {-# INLINE consR #-}
  unconsR LArray {..} =
    case L.uncons $ coerce lData of
      Nothing      -> Nothing
      Just (x, xs) -> Just (x, LArray lComp (coerce xs))
  {-# INLINE unconsR #-}
  flattenRagged = id
  {-# INLINE flattenRagged #-}
  generateRaggedM !comp !k f = do
    xs <- loopDeepM 0 (< coerce k) (+ 1) [] $ \i acc -> do
      e <- f i
      return (e:acc)
    return $ LArray comp $ coerce xs
  {-# INLINE generateRaggedM #-}
  loadRagged using uWrite start end sz xs =
    using $ do
      leftOver <-
        loopM start (< end) (+ 1) xs $ \i xs' ->
          case unconsR xs' of
            Nothing      -> return $! throw (DimTooShortException sz (outerLength xs))
            Just (y, ys) -> uWrite i y >> return ys
      unless (isNull leftOver) (return $! throw DimTooLongException)
  {-# INLINE loadRagged #-}
  raggedFormat f _ arr = L.concat $ "[ " : L.intersperse ", " (map f (coerce (lData arr))) ++ [" ]"]


instance (Index ix, Ragged L ix e) => Load L ix e where
  size = coerce . edgeSize
  {-# INLINE size #-}
  getComp = lComp
  {-# INLINE getComp #-}
  loadArrayM scheduler arr uWrite =
    loadRagged (scheduleWork scheduler) uWrite 0 (totalElem sz) sz arr
    where !sz = edgeSize arr
  {-# INLINE loadArrayM #-}


instance (Index ix, Load L ix e, Ragged L ix e) => Load LN ix e where
  size = edgeSize . LArray Seq
  {-# INLINE size #-}
  getComp _ = Seq
  {-# INLINE getComp #-}
  loadArrayM scheduler arr uWrite =
    loadRagged (scheduleWork scheduler) uWrite 0 (totalElem sz) sz arrL
    where
      !arrL = LArray Seq arr
      !sz = size arrL
  {-# INLINE loadArrayM #-}



outerLength :: Array L ix e -> Sz Int
outerLength = SafeSz . length . unList . lData

instance ( Index ix
         , Index (Lower ix)
         , Ragged L (Lower ix) e
         , Elt L ix e ~ Array L (Lower ix) e
         , Elt LN ix e ~ Array LN (Lower ix) e
         , Coercible (Elt LN ix e) [Elt LN (Lower ix) e]
         ) =>
         Ragged L ix e where
  isNull = null . unList . lData
  {-# INLINE isNull #-}
  emptyR comp = LArray comp (List [])
  {-# INLINE emptyR #-}
  edgeSize arr =
    SafeSz
      (consDim (length (unList (lData arr))) $
       case unconsR arr of
         Nothing     -> zeroIndex
         Just (x, _) -> coerce (edgeSize x))
  {-# INLINE edgeSize #-}
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
  -- generateRaggedM Seq !sz f = do
  --   let !(k, szL) = unconsSz sz
  --   loopDeepM 0 (< coerce k) (+ 1) (emptyR Seq) $ \i acc -> do
  --     e <- generateRaggedM Seq szL (\ !ixL -> f (consDim i ixL))
  --     return (cons e acc)
  generateRaggedM = unsafeGenerateParM
  {-# INLINE generateRaggedM #-}
  flattenRagged arr = LArray {lComp = lComp arr, lData = coerce xs}
    where
      xs = concatMap (unList . lData . flattenRagged . LArray (lComp arr)) (unList (lData arr))
  {-# INLINE flattenRagged #-}
  loadRagged using uWrite start end sz xs = do
    let (k, szL) = unconsSz sz
        step = totalElem szL
        isZero = totalElem sz == 0
    when (isZero && not (isNull (flattenRagged xs))) (return $! throw DimTooLongException)
    unless isZero $ do
      leftOver <-
        loopM start (< end) (+ step) xs $ \i zs ->
          case unconsR zs of
            Nothing -> return $! throw (DimTooShortException k (outerLength xs))
            Just (y, ys) -> do
              _ <- loadRagged using uWrite i (i + step) szL y
              return ys
      unless (isNull leftOver) (return $! throw DimTooLongException)
  {-# INLINE loadRagged #-}
  raggedFormat f sep (LArray comp xs) =
    showN (\s y -> raggedFormat f s (LArray comp y :: Array L (Lower ix) e)) sep (coerce xs)

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


instance {-# OVERLAPPING #-} Construct L Ix1 e where
  setComp c arr = arr { lComp = c }
  {-# INLINE setComp #-}
  makeArray comp sz f = LArray comp $ List $ unsafePerformIO $
    withScheduler comp $ \scheduler ->
      loopM_ 0 (< coerce sz) (+ 1) (scheduleWork scheduler . return . f)
  {-# INLINE makeArray #-}


instance ( Index ix
         , Ragged L ix e
         , Ragged L (Lower ix) e
         , Elt L ix e ~ Array L (Lower ix) e
         ) =>
         Construct L ix e where
  setComp c arr = arr {lComp = c}
  {-# INLINE setComp #-}
  makeArray = unsafeGenerateN
  {-# INLINE makeArray #-}

 -- TODO: benchmark against using unsafeGenerateM directly
unsafeGenerateN ::
  ( Ragged r ix e
  , Ragged r (Lower ix) e
  , Elt r ix e ~ Array r (Lower ix) e )
  => Comp
  -> Sz ix
  -> (ix -> e)
  -> Array r ix e
unsafeGenerateN comp sz f = unsafePerformIO $ do
  let !(m, szL) = unconsSz sz
  xs <- withScheduler comp $ \scheduler ->
    loopM_ 0 (< coerce m) (+ 1) $ \i -> scheduleWork scheduler $
      generateRaggedM comp szL $ \ix -> return $ f (consDim i ix)
  return $! foldr' consR (emptyR comp) xs
{-# INLINE unsafeGenerateN #-}


-- | Construct an array backed by linked lists
--
-- @since 0.3.7
toListArray :: (Construct L ix e, Source r ix e)
            => Array r ix e
            -> Array L ix e
toListArray !arr = makeArray (getComp arr) (size arr) (unsafeIndex arr)
{-# INLINE toListArray #-}



instance (Ragged L ix e, Show e) => Show (Array L ix e) where
  showsPrec = showsArrayLAsPrec (Proxy :: Proxy L)

instance (Ragged L ix e, Show e) => Show (Array LN ix e) where
  show arr = "  " ++ raggedFormat show "\n  " arrL
    where arrL = fromNested arr :: Array L ix e


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
  showsPrec 1 (getComp arr) . (" (" ++) . shows (size arr) . (")\n" ++) . shows lnarr . clp
  where
    (opp, clp) =
      if n == 0
        then (id, id)
        else (('(':), ("\n)" ++))
    lnarr = toNested arr

-- | Helper function for declaring `Show` instances for arrays
--
-- @since 0.3.7
showsArrayPrec ::
     forall r r' ix ix' e. (Ragged L ix' e, Load r ix e, Source r' ix' e, Show e)
  => (Array r ix e -> Array r' ix' e) -- ^ Modifier
  -> Int
  -> Array r ix e -- Array to show
  -> ShowS
showsArrayPrec f n arr = showsArrayLAsPrec (Proxy :: Proxy r) n larr
  where
    arr' = f arr
    larr = makeArray (getComp arr') (size arr') (evaluate' arr') :: Array L ix' e


-- | Helper function for declaring `Show` instances for arrays
--
-- @since 0.3.7
showArrayList
  :: Show arr => [arr] -> String -> String
showArrayList arrs = ('[':) . go arrs . (']':)
  where
    go []     = id
    go [x]    = (' ':) . shows x . ('\n':)
    go (x:xs) = (' ':) . shows x . ("\n," ++) . go xs


instance {-# OVERLAPPING #-} OuterSlice L Ix1 e where
  unsafeOuterSlice (LArray _ xs) = (coerce xs !!)
  {-# INLINE unsafeOuterSlice #-}


instance Ragged L ix e => OuterSlice L ix e where
  unsafeOuterSlice arr' i = go 0 arr'
    where
      go n arr =
        case unconsR arr of
          Nothing -> throw $ IndexOutOfBoundsException (Sz (headDim (unSz (size arr')))) i
          Just (x, _) | n == i -> x
          Just (_, xs) -> go (n + 1) xs
  {-# INLINE unsafeOuterSlice #-}
