{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- |
-- Module      : Data.Massiv.Array.Manifest.List
-- Copyright   : (c) Alexey Kuleshevich 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Manifest.List
  (
  -- ** List
    fromList
  , fromLists
  , fromLists'
  , toList
  , toLists
  , toLists2
  , toLists3
  , toLists4
  ) where

import           Data.Massiv.Array.Delayed.Internal  (D (..))
import           Data.Massiv.Array.Manifest.Internal
import           Data.Massiv.Array.Ops.Construct     (makeArrayR)
import           Data.Massiv.Array.Ops.Fold.Internal (foldrFB, foldrS)
import           Data.Massiv.Core.Common
import           Data.Massiv.Core.List
import           GHC.Base                            (build)

-- | Convert a flat list into a vector
fromList :: (Nested LN Ix1 e, Nested L Ix1 e, Ragged L Ix1 e, Mutable r Ix1 e)
         => Comp -- ^ Computation startegy to use
         -> [e] -- ^ Nested list
         -> Array r Ix1 e
fromList = fromLists'
{-# INLINE fromList #-}




-- | /O(n)/ - Convert a nested list into an array. Nested list must be of a rectangular shape,
-- otherwise a runtime error will occur. Also, nestedness must match the rank of resulting array,
-- which should be specified through an explicit type signature.
--
-- __Note__: This function is almost the same (modulo customizable computation strategy) if you
-- would turn on @{-# LANGUAGE OverloadedLists #-}@. For that reason you can also use
-- `GHC.Exts.fromList`.
--
-- ==== __Examples__
--
-- >>> fromLists Seq [[1,2],[3,4]] :: Maybe (Array U Ix2 Int)
-- Just (Array U Seq (2 :. 2)
--   [ [ 1,2 ]
--   , [ 3,4 ]
--   ])
--
-- >>> fromLists Par [[[1,2,3]],[[4,5,6]]] :: Maybe (Array U Ix3 Int)
-- Just (Array U Par (2 :> 1 :. 3)
--   [ [ [ 1,2,3 ]
--     ]
--   , [ [ 4,5,6 ]
--     ]
--   ])
--
-- Elements of a boxed array could be lists themselves if necessary, but cannot be ragged:
--
-- >>> fromLists Seq [[[1,2,3]],[[4,5]]] :: Maybe (Array B Ix2 [Int])
-- Just (Array B Seq (2 :. 1)
--   [ [ [1,2,3] ]
--   , [ [4,5] ]
--   ])
-- >>> fromLists Seq [[[1,2,3]],[[4,5]]] :: Maybe (Array B Ix3 Int)
-- Nothing
--
fromLists :: (Nested LN ix e, Nested L ix e, Ragged L ix e, Mutable r ix e)
         => Comp -> [ListItem ix e] -> Maybe (Array r ix e)
fromLists comp = either (const Nothing) Just . fromRaggedArray . setComp comp . throughNested
{-# INLINE fromLists #-}


-- | Same as `fromLists`, but will throw an error on irregular shaped lists.
--
-- ===__Examples__
--
-- Convert a list of lists into a 2D Array
--
-- >>> fromLists' Seq [[1,2],[3,4]] :: Array U Ix2 Int
-- (Array U Seq (2 :. 2)
--   [ [ 1,2 ]
--   , [ 3,4 ]
--   ])
--
-- Above example implemented using GHC's `OverloadedLists` extension:
--
-- >>> :set -XOverloadedLists
-- >>> [[1,2],[3,4]] :: Array U Ix2 Int
-- (Array U Seq (2 :. 2)
--   [ [ 1,2 ]
--   , [ 3,4 ]
--   ])
--
-- Example of failure on ceonversion of an irregular nested list.
--
-- >>> fromLists' Seq [[1],[3,4]] :: Array U Ix2 Int
-- (Array U *** Exception: Too many elements in a row
--
fromLists' :: (Nested LN ix e, Nested L ix e, Ragged L ix e, Mutable r ix e)
         => Comp -- ^ Computation startegy to use
         -> [ListItem ix e] -- ^ Nested list
         -> Array r ix e
fromLists' comp = fromRaggedArray' . setComp comp . throughNested
{-# INLINE fromLists' #-}


throughNested :: forall ix e . (Nested LN ix e, Nested L ix e) => [ListItem ix e] -> Array L ix e
throughNested xs = fromNested (fromNested xs :: Array LN ix e)
{-# INLINE throughNested #-}



-- | Convert any array to a flat list.
--
-- ==== __Examples__
--
-- >>> toList $ makeArrayR U Seq (2 :. 3) fromIx2
-- [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
--
toList :: Source r ix e => Array r ix e -> [e]
toList !arr = build (\ c n -> foldrFB c n arr)
{-# INLINE toList #-}


-- | /O(n)/ - Convert an array into a nested list. Array rank and list nestedness will always match,
-- but you can use `toList`, `toLists2`, etc. if flattening of inner dimensions is desired.
--
-- __Note__: This function is almost the same as `GHC.Exts.toList`.
--
-- ====__Examples__
--
-- >>> let arr = makeArrayR U Seq (2 :> 1 :. 3) fromIx3
-- >>> print arr
-- (Array U Seq (2 :> 1 :. 3)
--   [ [ [ (0,0,0),(0,0,1),(0,0,2) ]
--     ]
--   , [ [ (1,0,0),(1,0,1),(1,0,2) ]
--     ]
--   ])
-- >>> toList arr
-- [[[(0,0,0),(0,0,1),(0,0,2)]],[[(1,0,0),(1,0,1),(1,0,2)]]]
--
toLists :: (Nested LN ix e, Nested L ix e, Construct L ix e, Source r ix e)
       => Array r ix e
       -> [ListItem ix e]
toLists = toNested . toNested . toListArray
{-# INLINE toLists #-}



-- | Convert an array with at least 2 dimensions into a list of lists. Inner dimensions will get
-- flattened.
--
-- ==== __Examples__
--
-- >>> toList2 $ makeArrayR U Seq (2 :. 3) fromIx2
-- [[(0,0),(0,1),(0,2)],[(1,0),(1,1),(1,2)]]
-- >>> toList2 $ makeArrayR U Seq (2 :> 1 :. 3) fromIx3
-- [[(0,0,0),(0,0,1),(0,0,2)],[(1,0,0),(1,0,1),(1,0,2)]]
--
toLists2 :: (Source r ix e, Index (Lower ix)) => Array r ix e -> [[e]]
toLists2 = toList . foldrInner (:) []
{-# INLINE toLists2 #-}


-- | Convert an array with at least 3 dimensions into a 3 deep nested list. Inner dimensions will
-- get flattened.
toLists3 :: (Index (Lower (Lower ix)), Index (Lower ix), Source r ix e) => Array r ix e -> [[[e]]]
toLists3 = toList . foldrInner (:) [] . foldrInner (:) []
{-# INLINE toLists3 #-}

-- | Convert an array with at least 4 dimensions into a 4 deep nested list. Inner dimensions will
-- get flattened.
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


-- | Right fold with an index aware function of inner most dimension.
foldrInner :: (Source r ix e, Index (Lower ix)) =>
              (e -> a -> a) -> a -> Array r ix e -> Array D (Lower ix) a
foldrInner f !acc !arr =
  unsafeMakeArray (getComp arr) szL $ \ !ix ->
    foldrS f acc $ makeArrayR D Seq m (unsafeIndex arr . snocDim ix)
  where
    !(szL, m) = unsnocDim (size arr)
{-# INLINE foldrInner #-}
