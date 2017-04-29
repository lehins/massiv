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

  unsafeExtract :: ix -> ix -> Array r ix e -> Array (R r) ix e


extract :: Shape r ix e => ix -> ix -> Array r ix e -> Maybe (Array (R r) ix e)
extract !sIx !newSz !arr
  | isSafeIndex (size arr) sIx && isSafeIndex eIx sIx && eIx <= size arr =
    Just $ unsafeExtract sIx newSz arr
  | otherwise = Nothing
  where
    eIx = liftIndex2 (+) sIx newSz
{-# INLINE extract #-}

extractFromTo :: Shape r ix e => ix -> ix -> Array r ix e -> Maybe (Array (R r) ix e)
extractFromTo !sIx !eIx !arr = extract sIx newSz arr
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


(<!?>) :: Slice r ix e => Array r ix e -> (Int, Int) -> Maybe (Array (R r) (Lower ix) e)
(<!?>) !arr !(dim, i) = do
  m <- getIndex (size arr) dim
  guard $ isSafeIndex m i
  start <- setIndex zeroIndex dim i
  cutSz <- setIndex (size arr) dim 1
  newSz <- dropIndex cutSz dim
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

(<?>) :: Slice r ix e => Maybe (Array r ix e) -> (Int, Int) -> Maybe (Array (R r) (Lower ix) e)
(<?>) Nothing      _ = Nothing
(<?>) (Just arr) !ix = arr <!?> ix
{-# INLINE (<?>) #-}


(!>) :: Slice r ix e => Array r ix e -> Int -> Array (R r) (Lower ix) e
(!>) !arr !ix =
  case arr !?> ix of
    Just res -> res
    Nothing  -> errorIx "(!>)" arr ix
{-# INLINE (!>) #-}

(<!) :: Slice r ix e => Array r ix e -> Int -> Array (R r) (Lower ix) e
(<!) !arr !ix =
  case arr <!? ix of
    Just res -> res
    Nothing  -> errorIx "(<!)" arr ix
{-# INLINE (<!) #-}

(<!>) :: Slice r ix e => Array r ix e -> (Int, Int) -> Array (R r) (Lower ix) e
(<!>) !arr !(dim, i) =
  case arr <!?> (dim, i) of
    Just res -> res
    Nothing  -> if dim < 1 || dim > rank (size arr)
                then error $ "Invalid dimension: " ++ show dim ++ " for: " ++ show arr
                else errorIx "(<!>)" arr (dim, i)
{-# INLINE (<!>) #-}
