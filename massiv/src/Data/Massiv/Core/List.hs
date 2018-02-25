{-# LANGUAGE BangPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Massiv.Core.List
-- Copyright   : (c) Alexey Kuleshevich 2018
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
  , ListItem
  , ShapeError(..)
  ) where

import           Control.Exception
import           Control.Monad              (unless, when)
import           Data.Coerce
import           Data.Foldable              (foldr')
import           Data.Functor.Identity
import qualified Data.List                  as L
import           Data.Massiv.Core.Common
import           Data.Massiv.Core.Scheduler
import           Data.Proxy
import           Data.Typeable
import           GHC.Exts
import           System.IO.Unsafe           (unsafePerformIO)

data LN

type instance EltRepr LN ix = LN

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
type instance EltRepr L ix = L

type instance NestedStruct L ix e = Array LN ix e

data instance Array L ix e = LArray { lComp :: Comp
                                    , lData :: !(Array LN ix e) }



data ShapeError = RowTooShortError
                | RowTooLongError
                deriving Show

instance Exception ShapeError


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
  empty comp = LArray comp (List [])
  {-# INLINE empty #-}
  edgeSize = length . unList . lData
  {-# INLINE edgeSize #-}
  cons x arr = arr { lData = coerce (x : coerce (lData arr)) }
  {-# INLINE cons #-}
  uncons LArray {..} =
    case L.uncons $ coerce lData of
      Nothing      -> Nothing
      Just (x, xs) -> Just (x, LArray lComp (coerce xs))
  {-# INLINE uncons #-}
  flatten = id
  {-# INLINE flatten #-}
  unsafeGenerateM !comp !k f = do
    xs <- loopM (k - 1) (>= 0) (subtract 1) [] $ \i acc -> do
      e <- f i
      return (e:acc)
    return $ LArray comp $ coerce xs
  {-# INLINE unsafeGenerateM #-}
  loadRagged using uWrite start end _ xs =
    using $ do
      leftOver <-
        loopM start (< end) (+ 1) xs $ \i xs' ->
          case uncons xs' of
            Nothing      -> throwIO RowTooShortError
            Just (y, ys) -> uWrite i y >> return ys
      unless (isNull leftOver) $ throwIO RowTooLongError
  {-# INLINE loadRagged #-}
  raggedFormat f _ arr = L.concat $ "[ " : (L.intersperse "," $ map f (coerce (lData arr))) ++ [" ]"]


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
  empty comp = LArray comp (List [])
  {-# INLINE empty #-}
  edgeSize arr =
    consDim (length (unList (lData arr))) $
    case uncons arr of
      Nothing     -> zeroIndex
      Just (x, _) -> edgeSize x
  {-# INLINE edgeSize #-}
  cons (LArray _ x) arr = newArr
    where
      newArr =
        arr {lData = coerce (x : coerce (lData arr))}
  {-# INLINE cons #-}
  uncons LArray {..} =
    case L.uncons (coerce lData) of
      Nothing -> Nothing
      Just (x, xs) ->
        let newArr = LArray lComp (coerce xs)
            newX = LArray lComp x
        in Just (newX, newArr)
  {-# INLINE uncons #-}
  unsafeGenerateM Seq !sz f = do
    let !(k, szL) = unconsDim sz
    loopM (k - 1) (>= 0) (subtract 1) (empty Seq) $ \i acc -> do
      e <- unsafeGenerateM Seq szL (\ !ixL -> f (consDim i ixL))
      return (cons e acc)
  unsafeGenerateM p@(ParOn wss) sz f = unsafeGenerateParM wss sz f
  {-# INLINE unsafeGenerateM #-}
  flatten arr = LArray {lComp = lComp arr, lData = coerce xs}
    where
      xs =
        concatMap
          (unList . lData . flatten . LArray (lComp arr))
          (unList (lData arr))
  {-# INLINE flatten #-}
  loadRagged using uWrite start end sz xs = do
    let step = totalElem sz
        szL = tailDim sz
    leftOver <-
      loopM start (< end) (+ step) xs $ \i zs ->
        case uncons zs of
          Nothing -> throwIO RowTooShortError
          Just (y, ys) -> do
            _ <- loadRagged using uWrite i (i + step) szL y
            return ys
    unless (isNull (flatten leftOver)) $ throwIO RowTooLongError
  {-# INLINE loadRagged #-}
  raggedFormat f sep (LArray comp xs) =
    showN
      (\s y -> raggedFormat f s (LArray comp y :: Array L (Lower ix) e))
      sep
      (coerce xs)

unsafeGenerateParM ::
     (Elt LN ix e ~ Array LN (Lower ix) e, Index ix, Monad m, Ragged L (Lower ix) e)
  => [Int]
  -> ix
  -> (ix -> m e)
  -> m (Array L ix e)
unsafeGenerateParM wws !sz f = do
  res <- sequence $ unsafePerformIO $ do
    let !(k, szL) = unconsDim sz
    resLs <- divideWork wws k $ \ !scheduler !chunkLength !totalLength !slackStart -> do
        when (slackStart < totalLength) $
          scheduleWork scheduler $ do
            res <- loopM (totalLength - 1) (>= slackStart) (subtract 1) [] $ \i acc -> do
              return (fmap lData (unsafeGenerateM Seq szL (\ !ixL -> f (consDim i ixL))):acc)
            return $! sequence res
        loopM_ slackStart (> 0) (subtract chunkLength) $ \ !start -> do
          let !end = start - chunkLength
          scheduleWork scheduler $ do
            res <- loopM (start - 1) (>= end) (subtract 1) [] $ \i acc -> do
              return (fmap lData (unsafeGenerateM Seq szL (\ !ixL -> f (consDim i ixL))):acc)
            return $! sequence res
    return resLs
  return $ LArray (ParOn wws) $ List $ concat res
{-# INLINE unsafeGenerateParM #-}



instance {-# OVERLAPPING #-} Construct L Ix1 e where
  getComp = lComp
  {-# INLINE getComp #-}
  setComp c arr = arr { lComp = c }
  {-# INLINE setComp #-}
  unsafeMakeArray Seq sz f = runIdentity $ unsafeGenerateM Seq sz (return . f)
  unsafeMakeArray (ParOn wss) sz f = LArray (ParOn wss) $ List $ unsafePerformIO $ do
    withScheduler' wss $ \scheduler ->
      loopM_ 0 (< sz) (+ 1) (scheduleWork scheduler . return . f)
  {-# INLINE unsafeMakeArray #-}


instance ( Index ix
         , Ragged L ix e
         , Ragged L (Lower ix) e
         , Elt L ix e ~ Array L (Lower ix) e
         ) =>
         Construct L ix e where
  getComp = lComp
  {-# INLINE getComp #-}
  setComp c arr = arr {lComp = c}
  {-# INLINE setComp #-}
  unsafeMakeArray comp sz f = unsafeGenerateN comp sz f
  {-# INLINE unsafeMakeArray #-}

 -- TODO: benchmark against using unsafeGenerateM directly
unsafeGenerateN ::
  ( Index ix
  , Ragged r ix e
  , Ragged r (Lower ix) e
  , Elt r ix e ~ Array r (Lower ix) e )
  => Comp
  -> ix
  -> (ix -> e)
  -> Array r ix e
unsafeGenerateN Seq sz f = runIdentity $ unsafeGenerateM Seq sz (return . f)
unsafeGenerateN c@(ParOn wss) sz f = unsafePerformIO $ do
  let !(m, szL) = unconsDim sz
  xs <- withScheduler' wss $ \scheduler -> do
    loopM_ 0 (< m) (+ 1) $ \i -> scheduleWork scheduler $ do
      unsafeGenerateM c szL $ \ix -> return $ f (consDim i ix)
  return $! foldr' cons (empty c) xs
{-# INLINE unsafeGenerateN #-}


toListArray :: (Construct L ix e, Source r ix e)
            => Array r ix e
            -> Array L ix e
toListArray !arr =
  unsafeMakeArray (getComp arr) (size arr) (unsafeIndex arr)
{-# INLINE toListArray #-}



instance {-# OVERLAPPING #-} (Ragged L ix e, Show e) => Show (Array L ix e) where
  show arr = "  " ++ raggedFormat show "\n  " arr

instance {-# OVERLAPPING #-} (Ragged L ix e, Nested LN ix e, Show e) =>
  Show (Array LN ix e) where
  show arr = show (fromNested arr :: Array L ix e)


showN :: (String -> a -> String) -> String -> [a] -> String
showN _     _        [] = "[  ]"
showN fShow lnPrefix ls =
  L.concat
    (["[ "] ++
     (L.intersperse (lnPrefix ++ ", ") $ map (fShow (lnPrefix ++ "  ")) ls) ++ [lnPrefix, "]"])

instance ( Ragged L ix e
         , Construct L ix e
         , Source r ix e
         , Show e
         ) =>
         Show (Array r ix e) where
  show arr =
    "(Array " ++ showsTypeRep (typeRep (Proxy :: Proxy r)) " " ++
    showComp (getComp arr) ++ " (" ++
    (show (size arr)) ++ ")\n" ++
    show (makeArray (getComp arr) (size arr) (evaluateAt arr) :: Array L ix e) ++ ")"
    where showComp Seq = "Seq"
          showComp Par = "Par"
          showComp c   = "(" ++ show c ++ ")"




instance {-# OVERLAPPING #-} OuterSlice L Ix1 e where
  unsafeOuterSlice (LArray _ xs) = (coerce xs !!)
  {-# INLINE unsafeOuterSlice #-}
  outerLength = length . (coerce :: Array LN Ix1 e -> [e]). lData
  {-# INLINE outerLength #-}


instance Ragged L ix e => OuterSlice L ix e where
  unsafeOuterSlice arr' i = go 0 arr'
    where
      go n arr =
        case uncons arr of
          Nothing -> errorIx "Data.Massiv.Core.List.unsafeOuterSlice" (outerLength arr') i
          Just (x, _) | n == i -> x
          Just (_, xs) -> go (n + 1) xs
  {-# INLINE unsafeOuterSlice #-}
  outerLength = length . (coerce :: Array LN ix e -> [Elt LN ix e]) . lData
  {-# INLINE outerLength #-}
