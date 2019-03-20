{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module      : Data.Massiv.Array.Manifest.List
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Manifest.List
  (
  -- ** List
    fromList
  , fromListsM
  , fromLists'
  , fromLists
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
import GHC.Base (build)

-- | Convert a flat list into a vector
--
-- @since 0.1.0
fromList ::
     forall r e. Mutable r Ix1 e
  => Comp -- ^ Computation startegy to use
  -> [e] -- ^ Flat list
  -> Array r Ix1 e
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
-- >>> fromListsM Seq [[[1,2,3]],[[4,5]]] :: IO (Array B Ix3 Int)
-- *** Exception: DimTooShortException: expected (Sz1 3), got (Sz1 2)
--
-- @since 0.3.0
fromListsM :: forall r ix e m . (Nested LN ix e, Ragged L ix e, Mutable r ix e, MonadThrow m)
           => Comp -> [ListItem ix e] -> m (Array r ix e)
fromListsM comp = fromRaggedArrayM . setComp comp . throughNested
{-# INLINE fromListsM #-}

-- | Similar to `fromListsM`, but less general.
fromLists :: (Nested LN ix e, Ragged L ix e, Mutable r ix e)
         => Comp -> [ListItem ix e] -> Maybe (Array r ix e)
fromLists comp = fromRaggedArrayM . setComp comp . throughNested
{-# INLINE fromLists #-}
{-# DEPRECATED fromLists "In favor of a more general `fromListsM`" #-}

-- TODO: Figure out QuickCheck properties. Best guess idea so far IMHO is to add it as dependency
-- and move Arbitrary instances int the library
--
-- prop> fromLists' Seq xs == fromList xs
--
-- | Same as `fromLists`, but will throw an error on irregular shaped lists.
--
-- __Note__: This function is the same as if you would turn on @{-\# LANGUAGE OverloadedLists #-}@
-- extension. For that reason you can also use `GHC.Exts.fromList`.
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
-- Example of failure on conversion of an irregular nested list.
--
-- >>> fromLists' Seq [[1],[3,4]] :: Array U Ix2 Int
-- Array U *** Exception: DimTooLongException
--
-- @since 0.1.0
fromLists' :: forall r ix e . (Nested LN ix e, Ragged L ix e, Mutable r ix e)
         => Comp -- ^ Computation startegy to use
         -> [ListItem ix e] -- ^ Nested list
         -> Array r ix e
fromLists' comp = fromRaggedArray' . setComp comp . throughNested
{-# INLINE fromLists' #-}


throughNested :: forall ix e . Nested LN ix e => [ListItem ix e] -> Array L ix e
throughNested xs = fromNested (fromNested xs :: Array LN ix e)
{-# INLINE throughNested #-}



-- | Convert any array to a flat list.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> toList $ makeArrayR U Seq (Sz (2 :. 3)) fromIx2
-- [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
--
-- @since 0.1.0
toList :: Source r ix e => Array r ix e -> [e]
toList !arr = build (\ c n -> foldrFB c n arr)
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
toLists :: (Nested LN ix e, Construct L ix e, Source r ix e)
       => Array r ix e
       -> [ListItem ix e]
toLists = toNested . toNested . toListArray
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
toLists2 :: (Source r ix e, Index (Lower ix)) => Array r ix e -> [[e]]
toLists2 = toList . foldrInner (:) []
{-# INLINE toLists2 #-}


-- | Convert an array with at least 3 dimensions into a 3 deep nested list. Inner dimensions will
-- get flattened.
--
-- @since 0.1.0
toLists3 :: (Index (Lower (Lower ix)), Index (Lower ix), Source r ix e) => Array r ix e -> [[[e]]]
toLists3 = toList . foldrInner (:) [] . foldrInner (:) []
{-# INLINE toLists3 #-}

-- | Convert an array with at least 4 dimensions into a 4 deep nested list. Inner dimensions will
-- get flattened.
--
-- @since 0.1.0
toLists4 ::
     ( Index (Lower (Lower (Lower ix)))
     , Index (Lower (Lower ix))
     , Index (Lower ix)
     , Source r ix e
     )
  => Array r ix e
  -> [[[[e]]]]
toLists4 = toList . foldrInner (:) [] . foldrInner (:) [] . foldrInner (:) []
{-# INLINE toLists4 #-}
