{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- |
-- Module      : Data.Massiv.Array.Manifest.List
-- Copyright   : (c) Alexey Kuleshevich 2017
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
  ) where

import           Data.Massiv.Core.Common
import           Data.Massiv.Core.List
import           Data.Massiv.Array.Manifest.Mutable


-- | Juts like `fromList'`, but will return `Nothing` instead of throwing an
-- error on irregular shaped lists.
fromList :: (Nested LN ix e, Nested L ix e, Ragged L ix e, Mutable r ix e)
         => Comp -> [ListItem ix e] -> Maybe (Array r ix e)
fromList comp = either (const Nothing) Just . fromRaggedArray . setComp comp . throughNested
{-# INLINE fromList #-}

-- | /O(n)/ - Convert a nested list into an array. Nested list must be of a rectangular
-- shape, otherwise a runtime error will occur. Also, nestedness must match the
-- dimensionality of resulting array, which must be specified through an
-- explicit type signature.
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
fromList' :: (Nested LN ix e, Nested L ix e, Ragged L ix e, Mutable r ix e)
         => Comp -- ^ Computation startegy to use
         -> [ListItem ix e] -- ^ Nested list
         -> Array r ix e
fromList' comp = fromRaggedArray' . setComp comp . throughNested
{-# INLINE fromList' #-}


throughNested :: forall ix e . (Nested LN ix e, Nested L ix e) => [ListItem ix e] -> Array L ix e
throughNested xs = fromNested (fromNested xs :: Array LN ix e)
{-# INLINE throughNested #-}


-- | /O(n)/ - Convert an array into a nested list.
--
-- ====__Examples__
--
-- >>> makeArrayR U Seq (2 :> 1 :. 3) fromIx3
-- (Array U Seq (2 :> 1 :. 3)
-- [ [ [ (0,0,0),(0,0,1),(0,0,2) ]
--   ]
-- , [ [ (1,0,0),(1,0,1),(1,0,2) ]
--   ]
-- ])
-- >>> toList $ makeArrayR U Seq (2 :> 1 :. 3) fromIx3
-- [[[(0,0,0),(0,0,1),(0,0,2)]],[[(1,0,0),(1,0,1),(1,0,2)]]]
--
toList :: (Nested LN ix e, Nested L ix e, Construct L ix e, Source r ix e)
       => Array r ix e
       -> [ListItem ix e]
toList = toNested . toNested . toListArray
{-# INLINE toList #-}
