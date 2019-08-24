-- |
-- Module      : Data.Massiv.Array
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
--
-- Massiv is a library, that allows creation and manipulation of arrays in parallel and
-- sequentially. Depending on the representation (@__r__@), an @__`Array` r ix e__@ will have
-- certain properties that are unique to that particular representation, but all of them will share
-- the same trait, that an array is simply a mapping from an index (@__ix__@) of an arbitrary
-- dimension to an element (@__e__@) of some value. Which means that some representations describe
-- classic arrays and are backed by a contiguous chunk of memory reserved for the elements (or
-- pointers to elements), namely arrays with `Manifest` representations:
--
-- * `B` - The most basic type of array that can hold any type of element in a boxed form, i.e. each
--         element is a pointer to the actual value, therefore it is also the slowest
--         representation. Elements are kept in a Weak Head Normal Form (WHNF).
--
-- * `N` - Similar to `B`, is also a boxed type, except it's elements are always kept in a Normal
--         Form (NF). This property is very useful for parallel processing, i.e. when calling
--         `compute` you do want all of your elements to be fully evaluated.
--
-- * `S` - Is a type of array that is backed by pinned memory, therefore pointers to those arrays
--         can be passed to FFI calls, because Garbage Collector (GC) is guaranteed not to move
--         it. Elements must be an instance of `Storable` class. It is just as efficient as `P` and
--         `U` arrays, except it is subject to fragmentation.
--
-- * `U` - Unboxed representation. Elements must be an instance of `Unbox` class.
--
-- * `P` - Array that can hold Haskell primitives, such as `Int`, `Word`, `Double`, etc. Any element
--        must be an instance of `Prim` class.
--
-- * `M` - General manifest array type, that any of the above representations can be converted to in
--       constant time using `toManifest`.
--
-- There are also array representations that only describe how values for its elements can be
-- computed or loaded into memory, as such, they are represented by functions and do not impose the
-- memory overhead, that is normally associated with arrays. They are needed for proper fusion and
-- parallelization of computation.
--
-- * `D` - delayed array that is a mere function from an index to an element. Also known as /Pull/
--         array. Crucial representation for fusing computation. Use `computeAs` in order to load
--         array into `Manifest` representation.
--
-- * `DL` - delayed load array representation that describes how an array can be loaded. Also known
--         as /Push/ array. Useful for fusing various array combining functions. Use `computeAs` in
--         order to load array into `Manifest` representation.
--
-- * `DI` - delayed interleaved array. Same as `D`, but performs better with unbalanced
--         computation, when evaluation of one element takes much longer than of its neighbor.
--
-- * `DW` - delayed windowed array. This peculiar representation allows for very fast `Stencil`
--        computation.
--
-- Other Array types:
--
-- * `L` and `LN` - those types aren't particularly useful on their own, but because of their unique
--       ability to be converted to and from nested lists in constant time, provide a perfect
--       intermediary for lists \<-> array conversion.
--
-- Most of the `Manifest` arrays are capable of in-place mutation. Check out
-- "Data.Massiv.Array.Mutable" module for available functionality.
--
-- Many of the function names exported by this package will clash with the ones
-- from "Prelude", hence it can be more convenient to import like this:
--
-- @
-- import Prelude as P
-- import Data.Massiv.Array as A
-- @
--
module Data.Massiv.Array
  ( -- * Construct
    module Data.Massiv.Array.Ops.Construct
  -- * Compute
  , getComp
  , setComp
  , compute
  , computeS
  , computeAs
  , computeProxy
  , computeSource
  , computeWithStride
  , computeWithStrideAs
  , clone
  , convert
  , convertAs
  , convertProxy
  , fromRaggedArrayM
  , fromRaggedArray'
  -- * Size
  , size
  , elemsCount
  , isEmpty
  -- * Indexing
  , (!?)
  , (!)
  , (??)
  , indexM
  , index
  , index'
  , defaultIndex
  , borderIndex
  , evaluateM
  , evaluate'
  -- * Mapping
  , module Data.Massiv.Array.Ops.Map
  -- * Folding

  -- $folding

  , module Data.Massiv.Array.Ops.Fold
  -- * Transforming
  , module Data.Massiv.Array.Ops.Transform
  -- * Slicing
  , module Data.Massiv.Array.Ops.Slice
  -- * Algorithms
  -- ** Sorting
  , quicksort
  -- ** Iterations
  , iterateUntil
  -- * Conversion
  , module Data.Massiv.Array.Manifest.List
  -- * Mutable
  , module Data.Massiv.Array.Mutable
  -- * Core
  , module Data.Massiv.Core
  -- * Representations
  , module Data.Massiv.Array.Delayed
  , module Data.Massiv.Array.Manifest
  -- * Stencil
  , module Data.Massiv.Array.Stencil
  -- * Numeric Operations
  , module Data.Massiv.Array.Numeric
  ) where

import Data.Massiv.Array.Delayed
import Data.Massiv.Array.Manifest
import Data.Massiv.Array.Manifest.Internal
import Data.Massiv.Array.Manifest.List
import Data.Massiv.Array.Mutable
import Data.Massiv.Array.Numeric
import Data.Massiv.Array.Ops.Construct
import Data.Massiv.Array.Ops.Fold
import Data.Massiv.Array.Ops.Map
import Data.Massiv.Array.Ops.Slice
import Data.Massiv.Array.Ops.Sort (quicksort)
import Data.Massiv.Array.Ops.Transform
import Data.Massiv.Array.Stencil
import Data.Massiv.Core
import Data.Massiv.Core.Common
import Prelude as P hiding (all, and, any, enumFromTo, foldl, foldr, mapM,
                            mapM_, maximum, minimum, or, product, replicate, splitAt,
                            sum, zip)

{- $folding

All folding is done in a row-major order.

-}


