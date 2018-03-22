{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Massiv.Array.Ops.Fold
-- Copyright   : (c) Alexey Kuleshevich 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Ops.Fold
  (
  -- ** Unstructured folds

  -- $unstruct_folds

    fold
  , foldMono
  , minimum
  , maximum
  , sum
  , product
  , and
  , or
  , all
  , any

  -- ** Sequential folds

  -- $seq_folds

  , foldlS
  , foldrS
  , ifoldlS
  , ifoldrS

  -- *** Monadic
  , foldlM
  , foldrM
  , foldlM_
  , foldrM_
  , ifoldlM
  , ifoldrM
  , ifoldlM_
  , ifoldrM_

  -- *** Special folds
  , foldrFB
  , lazyFoldlS
  , lazyFoldrS

  -- ** Parallel folds

  -- $par_folds

  , foldlP
  , foldrP
  , ifoldlP
  , ifoldrP
  , foldlOnP
  , ifoldlIO
  , foldrOnP
  , ifoldlOnP
  , ifoldrOnP
  , ifoldrIO
  ) where

import           Data.Massiv.Array.Ops.Fold.Internal
import           Data.Massiv.Array.Ops.Map           (map)
import           Data.Massiv.Core
import           Data.Massiv.Core.Common
import           Data.Monoid
import           Prelude                             hiding (all, and, any,
                                                      foldl, foldr, map,
                                                      maximum, minimum, or,
                                                      product, sum)

-- | /O(n)/ - Monoidal fold over an array. Also known as reduce.
--
-- @since 0.1.4
foldMono ::
     (Source r ix e, Monoid m)
  => (e -> m) -- ^ Convert each element of an array to an appropriate `Monoid`.
  -> Array r ix e -- ^ Source array
  -> m
foldMono f = foldl (<>) mempty (<>) mempty . map f
{-# INLINE foldMono #-}


-- | /O(n)/ - Compute maximum of all elements.
maximum :: (Source r ix e, Ord e) =>
           Array r ix e -> e
maximum = \arr ->
  if isEmpty arr
    then error "Data.Massiv.Array.maximum - empty"
    else fold max (evaluateAt arr zeroIndex) arr
{-# INLINE maximum #-}


-- | /O(n)/ - Compute minimum of all elements.
minimum :: (Source r ix e, Ord e) =>
           Array r ix e -> e
minimum = \arr ->
  if isEmpty arr
    then error "Data.Massiv.Array.minimum - empty"
    else fold max (evaluateAt arr zeroIndex) arr
{-# INLINE minimum #-}


-- | /O(n)/ - Compute sum of all elements.
sum :: (Source r ix e, Num e) =>
        Array r ix e -> e
sum = fold (+) 0
{-# INLINE sum #-}


-- | /O(n)/ - Compute product of all elements.
product :: (Source r ix e, Num e) =>
            Array r ix e -> e
product = fold (*) 1
{-# INLINE product #-}


-- | /O(n)/ - Compute conjunction of all elements.
and :: (Source r ix Bool) =>
       Array r ix Bool -> Bool
and = fold (&&) True
{-# INLINE and #-}


-- | /O(n)/ - Compute disjunction of all elements.
or :: Source r ix Bool =>
      Array r ix Bool -> Bool
or = fold (||) False
{-# INLINE or #-}


-- | Determines whether all element of the array satisfy the predicate.
all :: Source r ix e =>
       (e -> Bool) -> Array r ix e -> Bool
all f = foldl (\acc el -> acc && f el) True (&&) True
{-# INLINE all #-}

-- | Determines whether any element of the array satisfies the predicate.
any :: Source r ix e =>
       (e -> Bool) -> Array r ix e -> Bool
any f = foldl (\acc el -> acc || f el) False (||) False
{-# INLINE any #-}


{- $unstruct_folds

Functions in this section will fold any `Source` array with respect to the inner
`Comp`utation strategy setting.

-}


{- $seq_folds

Functions in this section will fold any `Source` array sequentially, regardless of the inner
`Comp`utation strategy setting.

-}


{- $par_folds

__Note__ It is important to compile with @-threaded -with-rtsopts=-N@ flags, otherwise there will be
no parallelization.

Functions in this section will fold any `Source` array in parallel, regardless of the inner
`Comp`utation strategy setting. All of the parallel structured folds are performed inside `IO`
monad, because referential transparency can't generally be preserved and results will depend on the
number of cores/capabilities that computation is being performed on.

In contrast to sequential folds, each parallel folding function accepts two functions and two
initial elements as arguments. This is necessary because an array is first split into chunks, which
folded individually on separate cores with the first function, and the results of those folds are
further folded with the second function.

-}
