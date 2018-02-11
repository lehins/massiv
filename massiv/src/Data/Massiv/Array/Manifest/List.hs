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
  , fromList'
  , toList
  , toListIx1
  , toListIx2
  , toListIx3
  , toListIx4
  ) where

import           Data.Massiv.Array.Delayed          (D (..))
import           Data.Massiv.Array.Manifest.Internal
import           Data.Massiv.Array.Ops.Construct    (makeArrayR)
import           Data.Massiv.Array.Ops.Fold         (foldrFB, foldrS)
import           Data.Massiv.Core.Common
import           Data.Massiv.Core.List
import           GHC.Base                           (build)

-- | Juts like `fromList'`, but will return `Nothing` instead of throwing an
-- error on irregular shaped lists.
fromList :: (Nested LN ix e, Nested L ix e, Ragged L ix e, Mutable r ix e)
         => Comp -> [ListItem ix e] -> Maybe (Array r ix e)
fromList comp = either (const Nothing) Just . fromRaggedArray . setComp comp . throughNested
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
-- >>> fromList' Seq [[1,2],[3,4]] :: Array U Ix2 Int
-- (Array U Seq (2 :. 2)
-- [ [ 1,2 ]
-- , [ 3,4 ]
-- ])
--
-- >>> fromList' Par [[[1,2,3]],[[4,5,6]]] :: Array U Ix3 Int
-- (Array U Par (2 :> 1 :. 3)
-- [ [ [ 1,2,3 ]
--   ]
-- , [ [ 4,5,6 ]
--   ]
-- ])
--
-- Elements of a boxed array could be lists themselves if necessary:
--
-- >>> fromList' Seq [[[1,2,3]],[[4,5]]] :: Array B Ix2 [Int]
-- (Array B Seq (2 :. 1)
-- [ [ [1,2,3] ]
-- , [ [4,5] ]
-- ])
--
-- Above example implemented using GHC's `OverloadedLists` extension:
--
-- >>> :set -XOverloadedLists
-- >>> [[[1,2,3]],[[4,5]]] :: Array B Ix2 [Int]
-- (Array B Seq (2 :. 1)
-- [ [ [1,2,3] ]
-- , [ [4,5] ]
-- ])

fromList' :: (Nested LN ix e, Nested L ix e, Ragged L ix e, Mutable r ix e)
         => Comp -- ^ Computation startegy to use
         -> [ListItem ix e] -- ^ Nested list
         -> Array r ix e
fromList' comp = fromRaggedArray' . setComp comp . throughNested
{-# INLINE fromList' #-}


throughNested :: forall ix e . (Nested LN ix e, Nested L ix e) => [ListItem ix e] -> Array L ix e
throughNested xs = fromNested (fromNested xs :: Array LN ix e)
{-# INLINE throughNested #-}


-- | /O(n)/ - Convert an array into a nested list. Array rank and list nestedness will always match,
-- but you can use `toListIx1`, `toListIx2`, etc. if flattening of inner dimensions is desired.
--
-- __Note__: This function is almost the same as `GHC.Exts.toList`.
--
-- ====__Examples__
--
-- >>> let arr = makeArrayR U Seq (2 :> 1 :. 3) fromIx3
-- >>> print arr
-- (Array U Seq (2 :> 1 :. 3)
-- [ [ [ (0,0,0),(0,0,1),(0,0,2) ]
--   ]
-- , [ [ (1,0,0),(1,0,1),(1,0,2) ]
--   ]
-- ])
-- >>> toList arr
-- [[[(0,0,0),(0,0,1),(0,0,2)]],[[(1,0,0),(1,0,1),(1,0,2)]]]
--
toList :: (Nested LN ix e, Nested L ix e, Construct L ix e, Source r ix e)
       => Array r ix e
       -> [ListItem ix e]
toList = toNested . toNested . toListArray
{-# INLINE toList #-}



-- | Convert an array into a list.
--
-- >>> toListIx1 $ makeArrayR U Seq (2 :. 3) fromIx2
-- [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
--
toListIx1 :: Source r ix e => Array r ix e -> [e]
toListIx1 !arr = build (\ c n -> foldrFB c n arr)
{-# INLINE toListIx1 #-}


-- | Convert an array with at least 2 dimensions into a list of lists. Inner
-- dimensions will get flattened into a list.
--
-- ==== __Examples__
--
-- >>> toListIx2 $ makeArrayR U Seq (2 :. 3) fromIx2
-- [[(0,0),(0,1),(0,2)],[(1,0),(1,1),(1,2)]]
-- >>> toListIx2 $ makeArrayR U Seq (2 :> 1 :. 3) fromIx3
-- [[(0,0,0),(0,0,1),(0,0,2)],[(1,0,0),(1,0,1),(1,0,2)]]
--
toListIx2 :: (Source r ix e, Index (Lower ix)) => Array r ix e -> [[e]]
toListIx2 = toListIx1 . foldrInner (:) []
{-# INLINE toListIx2 #-}


-- | Similar to `toListIx1` and `toListIx2`, but for 3 deep nested list.
toListIx3 :: (Index (Lower (Lower ix)), Index (Lower ix), Source r ix e) => Array r ix e -> [[[e]]]
toListIx3 = toListIx1 . foldrInner (:) [] . foldrInner (:) []
{-# INLINE toListIx3 #-}

-- | Convert an array with the `Rank` of at least `4` into 4 deep nested list.
toListIx4 ::
     ( Index (Lower (Lower (Lower ix)))
     , Index (Lower (Lower ix))
     , Index (Lower ix)
     , Source r ix e
     )
  => Array r ix e
  -> [[[[e]]]]
toListIx4 = toListIx1 . foldrInner (:) [] . foldrInner (:) [] . foldrInner (:) []
{-# INLINE toListIx4 #-}


-- | Right fold with an index aware function of inner most dimension.
foldrInner :: (Source r ix e, Index (Lower ix)) =>
              (e -> a -> a) -> a -> Array r ix e -> Array D (Lower ix) a
foldrInner f !acc !arr =
  unsafeMakeArray (getComp arr) szL $ \ !ix ->
    foldrS f acc $ makeArrayR D Seq m (unsafeIndex arr . snocDim ix)
  where
    !(szL, m) = unsnocDim (size arr)
{-# INLINE foldrInner #-}
