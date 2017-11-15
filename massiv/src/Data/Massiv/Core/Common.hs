{-# LANGUAGE BangPatterns            #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
-- |
-- Module      : Data.Massiv.Core.Common
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Core.Common where

import           Data.Massiv.Core.Computation
import           Data.Massiv.Core.Index
import           Data.Typeable


-- | The array family. All array representations @r@ describe how data is
-- arranged. All arrays have a common property that each index @ix@ always maps
-- to the same unique element, even if that element does not exist in memory and
-- has to be computed upon lookup. Data is always arranged in a nested fasion,
-- depth of which is controlled by @`Rank` ix@.
data family Array r ix e :: *

type family EltRepr r ix :: *

type family Elt r ix e :: * where
  Elt r Ix1 e = e
  Elt r ix  e = Array (EltRepr r ix) (Lower ix) e

type family NestedStruct r ix e :: *

-- | Index polymorphic arrays.
class (Typeable r, Index ix) => Construct r ix e where

  size :: Array r ix e -> ix

  getComp :: Array r ix e -> Comp

  setComp :: Comp -> Array r ix e -> Array r ix e

  unsafeMakeArray :: Comp -> ix -> (ix -> e) -> Array r ix e


class Construct r ix e => Source r ix e where

  unsafeIndex :: Array r ix e -> ix -> e
  unsafeIndex !arr = unsafeLinearIndex arr . toLinearIndex (size arr)
  {-# INLINE unsafeIndex #-}

  unsafeLinearIndex :: Array r ix e -> Int -> e
  unsafeLinearIndex !arr = unsafeIndex arr . fromLinearIndex (size arr)
  {-# INLINE unsafeLinearIndex #-}


class Construct r ix e => Load r ix e where
  -- | Load an array into memory sequentially
  loadS
    :: Monad m =>
       Array r ix e -- ^ Array that is being loaded
    -> (Int -> m e) -- ^ Function that reads an element from target array
    -> (Int -> e -> m ()) -- ^ Function that writes an element into target array
    -> m ()

  -- | Load an array into memory in parallel
  loadP
    :: [Int] -- ^ List of capabilities to run workers on, as described in
             -- `Control.Concurrent.forkOn`. Empty list will imply all
             -- capabilities, i.e. run on all cores available through @+RTS -N@.
    -> Array r ix e -- ^ Array that is being loaded
    -> (Int -> IO e) -- ^ Function that reads an element from target array
    -> (Int -> e -> IO ()) -- ^ Function that writes an element into target array
    -> IO ()

class Construct r ix e => OuterSlice r ix e where
  unsafeOuterSlice :: Array r ix e -> (Int, Lower ix) -> Int -> Elt r ix e

class Construct r ix e => InnerSlice r ix e where
  unsafeInnerSlice :: Array r ix e -> (Lower ix, Int) -> Int -> Elt r ix e

class (InnerSlice r ix e, OuterSlice r ix e) => Slice r ix e where
  unsafeSlice :: Array r ix e -> ix -> ix -> Dim -> Maybe (Elt r ix e)


class Construct r ix e => Size r ix e where
  unsafeResize :: Index ix' => ix' -> Array r ix e -> Array r ix' e

  unsafeExtract :: ix -> ix -> Array r ix e -> Array (EltRepr r ix) ix e

class Nested r ix e where
  fromNested :: NestedStruct r ix e -> Array r ix e

  toNested :: Array r ix e -> NestedStruct r ix e


class Ragged r ix e where

  empty :: Comp -> Array r ix e

  isNull :: Array r ix e -> Bool

  cons :: Elt r ix e -> Array r ix e -> Array r ix e

  uncons :: Array r ix e -> Maybe (Elt r ix e, Array r ix e)

  unsafeGenerateM :: Monad m => Comp -> ix -> (ix -> m e) -> m (Array r ix e)

  edgeSize :: Array r ix e -> ix

  outerLength :: Array r ix e -> Int

  flatten :: Array r ix e -> Array r Ix1 e

  loadRagged ::
    (Monad m) =>
    (m () -> m ()) -> (Int -> e -> m a) -> Int -> Int -> Lower ix -> Array r ix e -> m ()

  -- TODO: test property:
  -- (read $ raggedFormat show "\n" (ls :: Array L (IxN n) Int)) == ls
  raggedFormat :: (e -> String) -> String -> Array r ix e -> String

