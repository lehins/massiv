{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Data.Massiv.Array.Ops.Slice
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Ops.Slice
  (
  -- ** From the outside
    (!>)
  , (!?>)
  , (?>)
  -- ** From the inside
  , (<!)
  , (<!?)
  , (<?)
  -- ** From within
  , (<!>)
  , (<!?>)
  , (<?>)
  ) where

import           Control.Monad    (guard)
import           Data.Massiv.Core.Common


infixl 4 !>, ?>, <!, <?, <!>, <!?>, <?>, !?>, <!?


(!?>) :: OuterSlice r ix e => Array r ix e -> Int -> Maybe (Elt r ix e)
(!?>) !arr !i
  | isSafeIndex m i = Just $ unsafeOuterSlice arr sz i
  | otherwise = Nothing
  where
    !sz@(m, _) = unconsDim (size arr)
{-# INLINE (!?>) #-}

(<!?) :: InnerSlice r ix e => Array r ix e -> Int -> Maybe (Elt r ix e)
(<!?) !arr !i
  | isSafeIndex m i = Just $ unsafeInnerSlice arr sz i
  | otherwise = Nothing
  where
    !sz@(_, m) = unsnocDim (size arr)
{-# INLINE (<!?) #-}



(<!?>) :: (Slice r ix e)
       => Array r ix e -> (Dim, Int) -> Maybe (Elt r ix e)
(<!?>) !arr !(dim, i) = do
  m <- getIndex (size arr) dim
  guard $ isSafeIndex m i
  start <- setIndex zeroIndex dim i
  cutSz <- setIndex (size arr) dim 1
  unsafeSlice arr start cutSz dim
{-# INLINE (<!?>) #-}


(?>) :: OuterSlice r ix e => Maybe (Array r ix e) -> Int -> Maybe (Elt r ix e)
(?>) Nothing      _ = Nothing
(?>) (Just arr) !ix = arr !?> ix
{-# INLINE (?>) #-}

(<?) :: InnerSlice r ix e => Maybe (Array r ix e) -> Int -> Maybe (Elt r ix e)
(<?) Nothing      _ = Nothing
(<?) (Just arr) !ix = arr <!? ix
{-# INLINE (<?) #-}

(<?>) :: Slice r ix e => Maybe (Array r ix e) -> (Dim, Int) -> Maybe (Elt r ix e)
(<?>) Nothing      _ = Nothing
(<?>) (Just arr) !ix = arr <!?> ix
{-# INLINE (<?>) #-}

(!>) :: OuterSlice r ix e => Array r ix e -> Int -> Elt r ix e
(!>) !arr !ix =
  case arr !?> ix of
    Just res -> res
    Nothing  -> errorIx "(!>)" (size arr) ix
{-# INLINE (!>) #-}

(<!) :: InnerSlice r ix e => Array r ix e -> Int -> Elt r ix e
(<!) !arr !ix =
  case arr <!? ix of
    Just res -> res
    Nothing  -> errorIx "(<!)" (size arr) ix
{-# INLINE (<!) #-}

(<!>) :: Slice r ix e => Array r ix e -> (Dim, Int) -> Elt r ix e
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
