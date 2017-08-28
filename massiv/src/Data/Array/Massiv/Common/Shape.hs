{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Data.Array.Massiv.Common.Shape
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Common.Shape where

import           Control.Monad            (guard)
import           Data.Array.Massiv.Common



class (Source r ix e, Source (R r) ix e) => Shape r ix e where
  type R r :: *
  type R r = r

  unsafeReshape :: Index ix' => ix' -> Array r ix e -> Array r ix' e

  unsafeExtract :: r' ~ R r => ix -> ix -> Array r ix e -> Array r' ix e


extract :: Shape r ix e => ix -> ix -> Array r ix e -> Maybe (Array (R r) ix e)
extract !sIx !newSz !arr
  | isSafeIndex sz1 sIx && isSafeIndex eIx1 sIx && isSafeIndex sz1 eIx =
    Just $ unsafeExtract sIx newSz arr
  | otherwise = Nothing
  where
    sz1 = liftIndex (+1) (size arr)
    eIx1 = liftIndex (+1) eIx
    eIx = liftIndex2 (+) sIx newSz
{-# INLINE extract #-}

extractFromTo :: Shape r ix e => ix -> ix -> Array r ix e -> Maybe (Array (R r) ix e)
extractFromTo sIx eIx = extract sIx newSz
  where
    newSz = liftIndex2 (-) eIx sIx
{-# INLINE extractFromTo #-}


reshape :: (Index ix', Shape r ix e) => ix' -> Array r ix e -> Maybe (Array r ix' e)
reshape !sz !arr
  | totalElem sz == totalElem (size arr) = Just $ unsafeReshape sz arr
  | otherwise = Nothing
{-# INLINE reshape #-}

-- | Same as `reshape`, but raise an error if supplied dimensions are incorrect.
reshape' :: (Index ix', Shape r ix e) => ix' -> Array r ix e -> Array r ix' e
reshape' !sz !arr =
  maybe
    (error $
     "Total number of elements do not match: " ++
     show sz ++ " vs " ++ show (size arr))
    id $
  reshape sz arr
{-# INLINE reshape' #-}


class ( Index (Lower ix)
      , Shape r ix e
      , Shape (R r) ix e
      , Shape (R r) (Lower ix) e
      ) =>
      Slice r ix e where
  (!?>) :: Array r ix e -> Int -> Maybe (Array (R r) (Lower ix) e)
  (<!?) :: Array r ix e -> Int -> Maybe (Array (R r) (Lower ix) e)


(<!?>) :: Slice r ix e => Array r ix e -> (Dim, Int) -> Maybe (Array (R r) (Lower ix) e)
(<!?>) !arr !(dim, i) = do
  m <- getIndex (size arr) dim
  guard $ isSafeIndex m i
  start <- setIndex zeroIndex dim i
  cutSz <- setIndex (size arr) dim 1
  newSz <- dropDim cutSz dim
  return $ unsafeReshape newSz (unsafeExtract start cutSz arr)
{-# INLINE (<!?>) #-}


(?>) :: Slice r ix e => Maybe (Array r ix e) -> Int -> Maybe (Array (R r) (Lower ix) e)
(?>) Nothing      _ = Nothing
(?>) (Just arr) !ix = arr !?> ix
{-# INLINE (?>) #-}

(<?) :: Slice r ix e => Maybe (Array r ix e) -> Int -> Maybe (Array (R r) (Lower ix) e)
(<?) Nothing      _ = Nothing
(<?) (Just arr) !ix = arr <!? ix
{-# INLINE (<?) #-}

(<?>) :: Slice r ix e => Maybe (Array r ix e) -> (Dim, Int) -> Maybe (Array (R r) (Lower ix) e)
(<?>) Nothing      _ = Nothing
(<?>) (Just arr) !ix = arr <!?> ix
{-# INLINE (<?>) #-}

(!>) :: Slice r ix e => Array r ix e -> Int -> Array (R r) (Lower ix) e
(!>) !arr !ix =
  case arr !?> ix of
    Just res -> res
    Nothing  -> errorIx "(!>)" (size arr) ix
{-# INLINE (!>) #-}

(<!) :: Slice r ix e => Array r ix e -> Int -> Array (R r) (Lower ix) e
(<!) !arr !ix =
  case arr <!? ix of
    Just res -> res
    Nothing  -> errorIx "(<!)" (size arr) ix
{-# INLINE (<!) #-}

(<!>) :: Slice r ix e => Array r ix e -> (Dim, Int) -> Array (R r) (Lower ix) e
(<!>) !arr !(dim, i) =
  case arr <!?> (dim, i) of
    Just res -> res
    Nothing ->
      let arrRank = rank (size arr)
      in if dim < 1 || dim > arrRank
           then error $
                "(<!>): Invalid dimension: " ++
                show dim ++ " for Array of rank: " ++ show arrRank
           else errorIx "(<!>)" (size arr) (dim, i)
{-# INLINE (<!>) #-}
