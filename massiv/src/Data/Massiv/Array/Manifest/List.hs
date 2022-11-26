{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Massiv.Array.Manifest.List
-- Copyright   : (c) Alexey Kuleshevich 2018-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
module Data.Massiv.Array.Manifest.List
  ( -- ** List
    fromList
  , fromListsM
  , fromLists'
  , toList
  , toLists
  , toLists2
  , toLists3
  , toLists4
  ) where

import Data.Massiv.Array.Manifest.Internal
import Data.Massiv.Array.Ops.Fold (foldrInner)
import Data.Massiv.Array.Ops.Fold.Internal (foldrFB)
import Data.Massiv.Core.Common
import Data.Massiv.Core.List
import qualified GHC.Exts as GHC (IsList (..), build)

-- | Convert a flat list into a vector
--
-- @since 0.1.0
fromList
  :: forall r e
   . Manifest r e
  => Comp
  -- ^ Computation startegy to use
  -> [e]
  -- ^ Flat list
  -> Vector r e
fromList = fromLists'
{-# INLINE fromList #-}

-- | /O(n)/ - Convert a nested list into an array. Nested list must be of a rectangular shape,
-- otherwise a runtime error will occur. Also, nestedness must match the rank of resulting array,
-- which should be specified through an explicit type signature.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array as A
-- >>> fromListsM Seq [[1,2,3],[4,5,6]] :: Maybe (Array U Ix2 Int)
-- Just (Array U Seq (Sz (2 :. 3))
--   [ [ 1, 2, 3 ]
--   , [ 4, 5, 6 ]
--   ]
-- )
--
-- >>> fromListsM Par [[[1,2,3]],[[4,5,6]]] :: Maybe (Array U Ix3 Int)
-- Just (Array U Par (Sz (2 :> 1 :. 3))
--   [ [ [ 1, 2, 3 ]
--     ]
--   , [ [ 4, 5, 6 ]
--     ]
--   ]
-- )
--
-- Elements of a boxed array could be lists themselves if necessary, but cannot be ragged:
--
-- >>> fromListsM Seq [[[1,2,3]],[[4,5]]] :: Maybe (Array B Ix2 [Int])
-- Just (Array B Seq (Sz (2 :. 1))
--   [ [ [1,2,3] ]
--   , [ [4,5] ]
--   ]
-- )
-- >>> fromListsM Seq [[[1,2,3]],[[4,5]]] :: Maybe (Array B Ix3 Integer)
-- Nothing
-- >>> fromListsM Seq [[[1,2,3]],[[4,5,6],[7,8,9]]] :: IO (Array B Ix3 Integer)
-- *** Exception: DimTooLongException for (Dim 2): expected (Sz1 1), got (Sz1 2)
-- >>> fromListsM Seq [[1,2,3,4],[5,6,7]] :: IO (Matrix B Integer)
-- *** Exception: DimTooShortException for (Dim 1): expected (Sz1 4), got (Sz1 3)
--
-- @since 0.3.0
fromListsM
  :: forall r ix e m
   . (Ragged L ix e, Manifest r e, MonadThrow m)
  => Comp
  -> [ListItem ix e]
  -> m (Array r ix e)
fromListsM comp = fromRaggedArrayM . setComp comp . fromListToListArray
{-# INLINE fromListsM #-}

fromListToListArray
  :: forall ix e
   . GHC.IsList (Array L ix e)
  => [ListItem ix e]
  -> Array L ix e
fromListToListArray = GHC.fromList
{-# INLINE fromListToListArray #-}

-- | Same as `fromListsM`, but will throw an error on irregular shaped lists.
--
-- __Note__: This function is the same as if you would turn on @{\-\# LANGUAGE OverloadedLists #-\}@
-- extension. For that reason you can also use `GHC.Exts.fromList`.
--
-- prop> \xs -> fromLists' Seq xs == (fromList Seq xs :: Vector P Int)
--
-- ====__Examples__
--
-- Convert a list of lists into a 2D Array
--
-- >>> import Data.Massiv.Array as A
-- >>> fromLists' Seq [[1,2,3],[4,5,6]] :: Array U Ix2 Int
-- Array U Seq (Sz (2 :. 3))
--   [ [ 1, 2, 3 ]
--   , [ 4, 5, 6 ]
--   ]
--
-- Above example implemented using GHC's `OverloadedLists` extension:
--
-- >>> :set -XOverloadedLists
-- >>> [[1,2,3],[4,5,6]] :: Array U Ix2 Int
-- Array U Seq (Sz (2 :. 3))
--   [ [ 1, 2, 3 ]
--   , [ 4, 5, 6 ]
--   ]
--
-- @since 0.1.0
fromLists'
  :: forall r ix e
   . (HasCallStack, Ragged L ix e, Manifest r e)
  => Comp
  -- ^ Computation startegy to use
  -> [ListItem ix e]
  -- ^ Nested list
  -> Array r ix e
fromLists' comp = fromRaggedArray' . setComp comp . fromListToListArray
{-# INLINE fromLists' #-}

-- | Convert any array to a flat list.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> toList $ makeArrayR U Seq (Sz (2 :. 3)) fromIx2
-- [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
--
-- @since 0.1.0
toList :: (Index ix, Source r e) => Array r ix e -> [e]
toList !arr = GHC.build (\c n -> foldrFB c n arr)
{-# INLINE toList #-}

-- | /O(n)/ - Convert an array into a nested list. Number of array dimensions and list nestedness
-- will always match, but you can use `toList`, `toLists2`, etc. if flattening of inner dimensions
-- is desired.
--
-- __Note__: This function is almost the same as `GHC.Exts.toList`.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> arr = makeArrayR U Seq (Sz (2 :> 1 :. 3)) id
-- >>> arr
-- Array U Seq (Sz (2 :> 1 :. 3))
--   [ [ [ 0 :> 0 :. 0, 0 :> 0 :. 1, 0 :> 0 :. 2 ]
--     ]
--   , [ [ 1 :> 0 :. 0, 1 :> 0 :. 1, 1 :> 0 :. 2 ]
--     ]
--   ]
-- >>> toLists arr
-- [[[0 :> 0 :. 0,0 :> 0 :. 1,0 :> 0 :. 2]],[[1 :> 0 :. 0,1 :> 0 :. 1,1 :> 0 :. 2]]]
--
-- @since 0.1.0
toLists
  :: (Ragged L ix e, Shape r ix, Source r e)
  => Array r ix e
  -- ^ Array to be converted to nested lists
  -> [ListItem ix e]
toLists = GHC.toList . toListArray
{-# INLINE toLists #-}

-- | Convert an array with at least 2 dimensions into a list of lists. Inner dimensions will get
-- flattened.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> toLists2 $ makeArrayR U Seq (Sz2 2 3) fromIx2
-- [[(0,0),(0,1),(0,2)],[(1,0),(1,1),(1,2)]]
-- >>> toLists2 $ makeArrayR U Seq (Sz3 2 1 3) fromIx3
-- [[(0,0,0),(0,0,1),(0,0,2)],[(1,0,0),(1,0,1),(1,0,2)]]
--
-- @since 0.1.0
toLists2 :: (Source r e, Index ix, Index (Lower ix)) => Array r ix e -> [[e]]
toLists2 = toList . foldrInner (:) []
{-# INLINE toLists2 #-}

-- | Convert an array with at least 3 dimensions into a 3 deep nested list. Inner dimensions will
-- get flattened.
--
-- @since 0.1.0
toLists3
  :: (Source r e, Index ix, Index (Lower ix), Index (Lower (Lower ix))) => Array r ix e -> [[[e]]]
toLists3 = toList . foldrInner (:) [] . foldrInner (:) []
{-# INLINE toLists3 #-}

-- | Convert an array with at least 4 dimensions into a 4 deep nested list. Inner dimensions will
-- get flattened.
--
-- @since 0.1.0
toLists4
  :: ( Source r e
     , Index ix
     , Index (Lower ix)
     , Index (Lower (Lower ix))
     , Index (Lower (Lower (Lower ix)))
     )
  => Array r ix e
  -> [[[[e]]]]
toLists4 = toList . foldrInner (:) [] . foldrInner (:) [] . foldrInner (:) []
{-# INLINE toLists4 #-}

-- $setup
-- >>> import Data.Massiv.Array as A
