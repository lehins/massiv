{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Data.Massiv.Core.List
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Core.List where

import           Control.Monad                (unless)
import           Data.Coerce
import           Data.Foldable                (foldr')
import           Data.Functor.Identity
import qualified Data.List                    as L
import           Data.Massiv.Core.Array
import           Data.Massiv.Core.Computation
import           Data.Massiv.Core.Index
import           Data.Massiv.Core.Iterator
import           Data.Massiv.Core.Scheduler
import           GHC.Exts
import           System.IO.Unsafe             (unsafePerformIO)


data L = L
type instance EltRepr L ix = L

data instance Array L ix e = LArray { lComp :: Comp
                                    , lSize :: ix
                                    , lData :: ![NestedItem L ix e] }


instance {-# OVERLAPPING #-} Construct L Ix1 e where
  size = lSize
  {-# INLINE size #-}
  getComp = lComp
  {-# INLINE getComp #-}
  setComp c arr = arr { lComp = c }
  {-# INLINE setComp #-}
  unsafeMakeArray Seq sz f = runIdentity $ unsafeGenerateM Seq sz (return . f)
  unsafeMakeArray (ParOn wss) sz f = LArray (ParOn wss) sz $ unsafePerformIO $ do
    withScheduler' wss $ \scheduler ->
      loopM_ 0 (< sz) (+ 1) (scheduleWork scheduler . return . f)
  {-# INLINE unsafeMakeArray #-}


instance ( Index ix
         , Index (Lower ix)
         , Ragged L ix e
         , Ragged L (Lower ix) e
         , Elt L ix e ~ Array L (Lower ix) e
         ) =>
         Construct L ix e where
  size = lSize
  {-# INLINE size #-}
  getComp = lComp
  {-# INLINE getComp #-}
  setComp c arr = arr {lComp = c}
  {-# INLINE setComp #-}
  unsafeMakeArray comp sz f = unsafeGenerateN comp sz f
  {-# INLINE unsafeMakeArray #-}



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
unsafeGenerateN comp@(ParOn wss) sz f = unsafePerformIO $ do
  let (m, szL) = unconsDim sz
  xs <- withScheduler' wss $ \scheduler -> do
    loopM_ 0 (< m) (+ 1) $ \i -> scheduleWork scheduler $ do
      unsafeGenerateM comp szL $ \ix -> return $ f (consDim i ix)
  return $! foldr' cons (empty comp) xs
{-# INLINE unsafeGenerateN #-}


instance {-# OVERLAPPING #-} Ragged L Ix1 e where
  type Nested L Ix1 e = [NestedItem L Ix1 e]
  isNull (LArray _ _ xs) = null xs
  {-# INLINE isNull #-}
  empty comp = LArray comp 0 []
  {-# INLINE empty #-}
  edgeSize = outerLength
  {-# INLINE edgeSize #-}
  outerLength = length . lData
  {-# INLINE outerLength #-}
  cons x arr = newArr
    where
      newArr = arr { lData = x : lData arr, lSize = edgeSize newArr }
  {-# INLINE cons #-}
  uncons LArray {..} =
    case L.uncons lData of
      Nothing      -> Nothing
      Just (x, xs) -> let newArr = LArray lComp (edgeSize newArr) xs in Just (x, newArr)
  {-# INLINE uncons #-}
  unsafeGenerateM comp k f = do
    xs <- loopM (k - 1) (>= 0) (subtract 1) [] $ \i acc -> do
      e <- f i
      return (e:acc)
    return $ LArray comp k xs
  {-# INLINE unsafeGenerateM #-}
  loadRagged using uWrite start end _ xs =
    using $ do
      leftOver <-
        loopM start (< end) (+ 1) xs $ \i xs' ->
          case uncons xs' of
            Nothing      -> error $ "Row is too short"
            Just (y, ys) -> uWrite i y >> return ys
      unless (isNull leftOver) $ error "Row is too long"
  {-# INLINE loadRagged #-}
  fromNested comp xs = LArray comp (length xs) xs
  {-# INLINE fromNested #-}
  toNested = lData
  {-# INLINE toNested #-}
  raggedFormat f _ arr = L.concat $ "[ " : (L.intersperse "," $ map f (lData arr)) ++ [" ]"]


instance ( Index ix
         , Index (Lower ix)
         , Ragged L (Lower ix) e
         , Elt L ix e ~ Array L (Lower ix) e
         , NestedItem L ix e ~ [NestedItem L (Lower ix) e]
         , [NestedItem L (Lower ix) e] ~ NestedItem L ix [NestedItem L (Lower ix) e]
         ) =>
         Ragged L ix e where
  type Nested L ix e = [NestedItem L ix e]
  isNull (LArray _ _ xs) = null xs
  {-# INLINE isNull #-}
  empty comp = LArray comp zeroIndex []
  {-# INLINE empty #-}
  edgeSize arr =
    consDim (length (lData arr)) $
    case uncons arr of
      Nothing -> zeroIndex
      Just (x, _) -> edgeSize x
  {-# INLINE edgeSize #-}
  outerLength = length . lData
  {-# INLINE outerLength #-}
  cons (LArray _ _ x) arr = newArr
    where
      newArr = arr {lData = x : lData arr, lSize = edgeSize newArr}
  {-# INLINE cons #-}
  uncons LArray {..} =
    case L.uncons lData of
      Nothing -> Nothing
      Just (x, xs) ->
        let newArr = LArray lComp (edgeSize newArr) xs
            newX = LArray lComp (edgeSize newX) x
        in Just (newX, newArr)
  {-# INLINE uncons #-}
  unsafeGenerateM comp sz f = do
    let (k, szL) = unconsDim sz
    loopM (k - 1) (>= 0) (subtract 1) (empty comp) $ \i acc -> do
      e <- unsafeGenerateM comp szL (\ixL -> f (consDim i ixL))
      return (cons e acc)
  {-# INLINE unsafeGenerateM #-}
  loadRagged using uWrite = loadRaggedRec (loadRagged using uWrite)
  {-# INLINE loadRagged #-}
  fromNested comp xs =
    let newArr = LArray comp (edgeSize newArr) xs in newArr
  {-# INLINE fromNested #-}
  toNested = lData
  {-# INLINE toNested #-}
  raggedFormat f sep (LArray comp ix xs) =
    showN (\s y -> raggedFormat f s $ LArray comp (tailDim ix) y) sep xs

-- TODO: remove construct
instance (Construct L ix e, Ragged L ix e, Index ix) => Load L ix e where
  loadS arr _ unsafeWrite = let sz = edgeSize arr in
    loadRagged id unsafeWrite 0 (totalElem sz) (tailDim sz) arr
  {-# INLINE loadS #-}
  loadP wIds arr _ unsafeWrite = let sz = edgeSize arr in
    withScheduler_ wIds $ \scheduler ->
      loadRagged (scheduleWork scheduler) unsafeWrite 0 (totalElem sz) (tailDim sz) arr
  {-# INLINE loadP #-}


instance Ragged L ix e => IsList (Array L ix e) where
  type Item (Array L ix e) = NestedItem L ix e
  fromList xs = let newArr = LArray Seq (edgeSize newArr) (coerce xs) in newArr
  {-# INLINE fromList #-}
  toList = lData
  {-# INLINE toList #-}




showN :: (String -> a -> String) -> String -> [a] -> String
showN fShow lnPrefix ls =
  L.concat
    (["[ "] ++
     (L.intersperse (lnPrefix ++ ", ") $ map (fShow (lnPrefix ++ "  ")) ls) ++ [lnPrefix, "]"])


loadRaggedRec :: (Index ix1, Monad m, Ragged r ix e) =>
                 (Int -> Int -> Lower ix1 -> Elt r ix e -> m a)
              -> Int -> Int -> ix1 -> Array r ix e -> m ()
loadRaggedRec loadLower start end sz xs = do
  let step = totalElem sz
      szL = tailDim sz
  leftOver <-
    loopM start (< end) (+ step) xs $ \ i zs ->
      case uncons zs of
        Nothing -> error "Too short"
        Just (y, ys) -> do
          _ <- loadLower i (i + step) szL y
          return ys
  unless (isNull leftOver) $ error "Too long"
{-# INLINE loadRaggedRec #-}
