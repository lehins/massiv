{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- |
-- Module      : Data.Massiv.Array.Ops.Construct
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Ops.Construct
  ( -- ** From a function
    makeArray
  , makeArrayR
  , makeVectorR
  , singleton
    -- *** Applicative
  , makeArrayA
  , makeArrayAR
    -- ** Enumeration
  , range
  , rangeStep
  , enumFromN
  , enumFromStepN
    -- ** Expansion
  , expandWithin
  , expandWithin'
  , expandOuter
  , expandInner
  ) where

import           Data.Massiv.Array.Delayed.Pull
import           Data.Massiv.Array.Ops.Map      as A
import           Data.Massiv.Core.Common
import           Prelude                        as P


-- | Just like `makeArray` but with ability to specify the result representation as an
-- argument. Note the `Data.Massiv.Array.U`nboxed type constructor in the below example.
--
-- >>> makeArrayR U Par (2 :> 3 :. 4) (\ (i :> j :. k) -> i * i + j * j == k * k)
-- (Array U Par (2 :> 3 :. 4)
--   [ [ [ True,False,False,False ]
--     , [ False,True,False,False ]
--     , [ False,False,True,False ]
--     ]
--   , [ [ False,True,False,False ]
--     , [ False,False,False,False ]
--     , [ False,False,False,False ]
--     ]
--   ])
--
makeArrayR :: Construct r ix e => r -> Comp -> ix -> (ix -> e) -> Array r ix e
makeArrayR _ = makeArray
{-# INLINE makeArrayR #-}


-- | Same as `makeArrayR`, but restricted to 1-dimensional arrays.
makeVectorR :: Construct r Ix1 e => r -> Comp -> Ix1-> (Ix1 -> e) -> Array r Ix1 e
makeVectorR _ = makeArray
{-# INLINE makeVectorR #-}

-- | Similar to `makeArray`, but construct the array sequentially using an `Applicative` interface
-- disregarding the supplied `Comp`.
--
-- @since 0.2.6
--
makeArrayA :: (Mutable r a b, Applicative f) => Comp -> a -> (a -> f b) -> f (Array r a b)
makeArrayA comp sz f = traverseA f $ makeArrayR D comp sz id
{-# INLINE makeArrayA #-}

-- | Same as `makeArrayA`, but with ability to supply result array representation.
--
-- @since 0.2.6
--
makeArrayAR :: (Mutable r a b, Applicative f) => r -> Comp -> a -> (a -> f b) -> f (Array r a b)
makeArrayAR _ = makeArrayA
{-# INLINE makeArrayAR #-}

-- | Create a vector with a range of @Int@s incremented by 1.
-- @range k0 k1 == rangeStep k0 k1 1@
--
-- >>> range Seq 1 6
-- (Array D Seq (5)
--   [ 1,2,3,4,5 ])
-- >>> range Seq (-2) 3
-- (Array D Seq (5)
--   [ -2,-1,0,1,2 ])
--
range :: Comp -> Int -> Int -> Array D Ix1 Int
range comp !from !to = makeArray comp (max 0 (to - from)) (+ from)
{-# INLINE range #-}


-- | Same as `range`, but with a custom step.
--
-- >>> rangeStep Seq 1 2 6
-- (Array D Seq (3)
--   [ 1,3,5 ])
--
rangeStep :: Comp -- ^ Computation strategy
          -> Int -- ^ Start
          -> Int -- ^ Step (Can't be zero)
          -> Int -- ^ End
          -> Array D Ix1 Int
rangeStep comp !from !step !to
  | step == 0 = error "rangeStep: Step can't be zero"
  | otherwise =
    let (sz, r) = (to - from) `divMod` step
    in makeArray comp (sz + signum r) (\i -> from + i * step)
{-# INLINE rangeStep #-}


-- | Same as `enumFromStepN` with step @delta = 1@.
--
-- >>> enumFromN Seq (5 :: Double) 3
-- (Array D Seq (3)
--   [ 5.0,6.0,7.0 ])
--
enumFromN :: Num e =>
             Comp
          -> e -- ^ @x@ - start value
          -> Int -- ^ @n@ - length of resulting vector.
          -> Array D Ix1 e
enumFromN comp !from !sz = makeArray comp sz $ \ i -> fromIntegral i + from
{-# INLINE enumFromN #-}


-- | Create a vector with length @n@ that has it's 0th value set to @x@ and gradually increasing
-- with @step@ delta until the end. Similar to: @`Data.Massiv.Array.fromList'` `Seq` $ `take` n [x,
-- x + delta ..]@. Major difference is that `fromList` constructs an `Array` with manifest
-- representation, while `enumFromStepN` is delayed.
--
-- >>> enumFromStepN Seq 1 (0.1 :: Double) 5
-- (Array D Seq (5)
--   [ 1.0,1.1,1.2,1.3,1.4 ])
--
enumFromStepN :: Num e =>
                 Comp
              -> e -- ^ @x@ - start value
              -> e -- ^ @delta@ - step value
              -> Int -- ^ @n@ - length of resulting vector
              -> Array D Ix1 e
enumFromStepN comp !from !step !sz = makeArray comp sz $ \ i -> from + fromIntegral i * step
{-# INLINE enumFromStepN #-}


-- | Function that expands an array to one with a higher dimension.
--
-- This is useful for constructing arrays where there is shared computation
-- between multiple cells.  The makeArray method of constructing arrays:
--
-- > makeArray :: Construct r ix e => Comp -> ix -> (ix -> e) -> Array r ix e
--
-- ...runs a function @ix -> e@ at every array index. This is inefficient if
-- there is a substantial amount of repeated computation that could be shared
-- while constructing elements on the same dimension. The expand functions make
-- this possible. First you construct an @Array r (Lower ix) a@ of one fewer
-- dimensions where @a@ is something like @`Array` r `Ix1` a@ or @`Array` r `Ix2` a@. Then
-- you use 'expandWithin' and a creation function @a -> Int -> b@ to create an
-- @`Array` `D` `Ix2` b@ or @`Array` `D` `Ix3` b@ respectfully.
--
-- @since 0.2.6
--
-- ====__Examples__
--
-- >>> a = makeArrayR U Seq (Ix1 6) (+10) -- Imagine (+10) is some expensive function
-- >>> a
-- (Array U Seq (6)
--   [ 10,11,12,13,14,15 ])
-- >>> expandWithin Dim1 5 (\ e j -> (j + 1) * 100 + e) a :: Array D Ix2 Int
-- (Array D Seq (6 :. 5)
--   [ [ 110,210,310,410,510 ]
--   , [ 111,211,311,411,511 ]
--   , [ 112,212,312,412,512 ]
--   , [ 113,213,313,413,513 ]
--   , [ 114,214,314,414,514 ]
--   , [ 115,215,315,415,515 ]
--   ])
-- >>> expandWithin Dim2 5 (\ e j -> (j + 1) * 100 + e) a :: Array D Ix2 Int
-- (Array D Seq (5 :. 6)
--   [ [ 110,111,112,113,114,115 ]
--   , [ 210,211,212,213,214,215 ]
--   , [ 310,311,312,313,314,315 ]
--   , [ 410,411,412,413,414,415 ]
--   , [ 510,511,512,513,514,515 ]
--   ])
--
expandWithin
  :: (IsIndexDimension ix n, Manifest r (Lower ix) a)
  => Dimension n
  -> Int
  -> (a -> Int -> b)
  -> Array r (Lower ix) a
  -> Array D ix b
expandWithin dim k f arr = do
  makeArray (getComp arr) sz $ \ix ->
    let (i, ixl) = pullOutDimension ix dim
     in f (unsafeIndex arr ixl) i
  where
    szl = size arr
    sz = insertDimension szl dim k
{-# INLINE expandWithin #-}

-- | Similar to `expandWithin`, except that dimension is specified at a value level, which means it
-- will throw an exception on an invalid dimension.
--
-- @since 0.2.6
expandWithin'
  :: (Index ix, Manifest r (Lower ix) a)
  => Dim
  -> Int
  -> (a -> Int -> b)
  -> Array r (Lower ix) a
  -> Array D ix b
expandWithin' dim k f arr =
  makeArray (getComp arr) sz $ \ix ->
    let (i, ixl) = pullOutDim' ix dim
     in f (unsafeIndex arr ixl) i
  where
    szl = size arr
    sz = insertDim' szl dim k
{-# INLINE expandWithin' #-}

-- | Similar to `expandWithin`, except it uses the outermost dimension.
--
-- @since 0.2.6
expandOuter
  :: (Index ix, Manifest r (Lower ix) a)
  => Int
  -> (a -> Int -> b)
  -> Array r (Lower ix) a
  -> Array D ix b
expandOuter k f arr =
  makeArray (getComp arr) sz $ \ix ->
    let (i, ixl) = unconsDim ix
     in f (unsafeIndex arr ixl) i
  where
    szl = size arr
    sz = consDim k szl
{-# INLINE expandOuter #-}

-- | Similar to `expandWithin`, except it uses the innermost dimension.
--
-- @since 0.2.6
expandInner
  :: (Index ix, Manifest r (Lower ix) a)
  => Int
  -> (a -> Int -> b)
  -> Array r (Lower ix) a
  -> Array D ix b
expandInner k f arr =
  makeArray (getComp arr) sz $ \ix ->
    let (ixl, i) = unsnocDim ix
     in f (unsafeIndex arr ixl) i
  where
    szl = size arr
    sz = snocDim szl k
{-# INLINE expandInner #-}
