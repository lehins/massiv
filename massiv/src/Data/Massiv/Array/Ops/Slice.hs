{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Data.Massiv.Array.Ops.Slice
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
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
  , (??>)
  -- ** From the inside
  , (<!)
  , (<!?)
  , (<??)
  -- ** From within
  , (<!>)
  , (<!?>)
  , (<??>)
  ) where

import           Control.Monad           (guard)
import           Data.Massiv.Core.Common


infixl 4 !>, !?>, ??>, <!, <!?, <??, <!>, <!?>, <??>


-- | /O(1)/ - Slices the array from the outside. For 2-dimensional array this will
-- be equivalent of taking a row. Throws an error when index is out of bounds.
--
-- ===__Examples__
--
-- You could say that slicing from outside is synonymous to slicing from the end or slicing at the
-- highermost dimension. For example with rank-3 arrays outer slice would be equivalent to getting a
-- page:
--
-- >>> let arr = makeArrayR U Seq (3 :> 2 :. 4) fromIx3
-- >>> arr
-- (Array U Seq (3 :> 2 :. 4)
--   [ [ [ (0,0,0),(0,0,1),(0,0,2),(0,0,3) ]
--     , [ (0,1,0),(0,1,1),(0,1,2),(0,1,3) ]
--     ]
--   , [ [ (1,0,0),(1,0,1),(1,0,2),(1,0,3) ]
--     , [ (1,1,0),(1,1,1),(1,1,2),(1,1,3) ]
--     ]
--   , [ [ (2,0,0),(2,0,1),(2,0,2),(2,0,3) ]
--     , [ (2,1,0),(2,1,1),(2,1,2),(2,1,3) ]
--     ]
--   ])
-- >>> arr !> 2
-- (Array M Seq (2 :. 4)
--   [ [ (2,0,0),(2,0,1),(2,0,2),(2,0,3) ]
--   , [ (2,1,0),(2,1,1),(2,1,2),(2,1,3) ]
--   ])
--
-- There is nothing wrong with chaining, mixing and matching slicing operators, or even using them
-- to index arrays:
--
-- >>> arr !> 2 !> 0 !> 3
-- (2,0,3)
-- >>> arr !> 2 <! 3 ! 0
-- (2,0,3)
-- >>> arr !> 2 !> 0 !> 3 == arr ! 2 :> 0 :. 3
-- True
--
(!>) :: OuterSlice r ix e => Array r ix e -> Int -> Elt r ix e
(!>) !arr !ix =
  case arr !?> ix of
    Just res -> res
    Nothing  -> errorIx "(!>)" (fst $ unconsSz (size arr)) ix
{-# INLINE (!>) #-}


-- | /O(1)/ - Just like `!>` slices the array from the outside, but returns
-- `Nothing` when index is out of bounds.
(!?>) :: OuterSlice r ix e => Array r ix e -> Int -> Maybe (Elt r ix e)
(!?>) !arr !i
  | isSafeIndex (fst (unconsSz (size arr))) i = Just $ unsafeOuterSlice arr i
  | otherwise = Nothing
{-# INLINE (!?>) #-}


-- | /O(1)/ - Safe slicing continuation from the outside. Similarly to (`!>`) slices the array from
-- the outside, but takes `Maybe` array as input and returns `Nothing` when index is out of bounds.
--
-- ===__Examples__
--
-- >>> let arr = makeArrayR U Seq (3 :> 2 :. 4) fromIx3
-- >>> arr !?> 2 ??> 0 ??> 3
-- Just (2,0,3)
-- >>> arr !?> 2 ??> 0 ??> -1
-- Nothing
-- >>> arr !?> -2 ??> 0 ?? 1
-- Nothing
--
(??>) :: OuterSlice r ix e => Maybe (Array r ix e) -> Int -> Maybe (Elt r ix e)
(??>) Nothing      _ = Nothing
(??>) (Just arr) !ix = arr !?> ix
{-# INLINE (??>) #-}


-- | /O(1)/ - Safe slice from the inside
(<!?) :: InnerSlice r ix e => Array r ix e -> Int -> Maybe (Elt r ix e)
(<!?) !arr !i
  | isSafeIndex m i = Just $ unsafeInnerSlice arr sz i
  | otherwise = Nothing
  where
    !sz@(_, m) = unsnocSz (size arr)
{-# INLINE (<!?) #-}


-- | /O(1)/ - Similarly to (`!>`) slice an array from an opposite direction.
(<!) :: InnerSlice r ix e => Array r ix e -> Int -> Elt r ix e
(<!) !arr !ix =
  case arr <!? ix of
    Just res -> res
    Nothing  -> errorIx "(<!)" (size arr) ix
{-# INLINE (<!) #-}


-- | /O(1)/ - Safe slicing continuation from the inside
(<??) :: InnerSlice r ix e => Maybe (Array r ix e) -> Int -> Maybe (Elt r ix e)
(<??) Nothing      _ = Nothing
(<??) (Just arr) !ix = arr <!? ix
{-# INLINE (<??) #-}


-- | /O(1)/ - Same as (`<!>`), but fails gracefully with a `Nothing`, instead of an error
(<!?>) :: Slice r ix e => Array r ix e -> (Dim, Int) -> Maybe (Elt r ix e)
(<!?>) !arr !(dim, i) = do
  (m, szl) <- pullOutSz (size arr) dim
  guard $ isSafeIndex m i
  start <- setDim zeroIndex dim i
  cutSz <- insertSz szl dim oneSz
  unsafeSlice arr start cutSz dim
{-# INLINE (<!?>) #-}


-- | /O(1)/ - Slices the array in any available dimension. Throws an error when
-- index is out of bounds or dimensions is invalid.
--
-- prop> arr !> i == arr <!> (dimensions (size arr), i)
-- prop> arr <! i == arr <!> (1,i)
--
(<!>) :: Slice r ix e => Array r ix e -> (Dim, Int) -> Elt r ix e
(<!>) !arr !(dim, i) =
  case arr <!?> (dim, i) of
    Just res -> res
    Nothing ->
      let arrDims = dimensions (size arr)
      in if dim < 1 || dim > arrDims
           then error $
                "(<!>): Invalid dimension: " ++
                show dim ++ " for Array of dimensions: " ++ show arrDims
           else errorIx "(<!>)" (size arr) (dim, i)
{-# INLINE (<!>) #-}


-- | /O(1)/ - Safe slicing continuation from within.
(<??>) :: Slice r ix e => Maybe (Array r ix e) -> (Dim, Int) -> Maybe (Elt r ix e)
(<??>) Nothing      _ = Nothing
(<??>) (Just arr) !ix = arr <!?> ix
{-# INLINE (<??>) #-}
