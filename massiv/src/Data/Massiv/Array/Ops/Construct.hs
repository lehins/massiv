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
-- Copyright   : (c) Alexey Kuleshevich 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Ops.Construct
  ( makeArray
  , makeArrayR
  , makeVectorR
  , expandWithin
  , expandWithin'
  , expandOuter
  , expandInner
  , singleton
  , range
  , rangeStep
  , enumFromN
  , enumFromStepN
  ) where

import           Data.Massiv.Array.Delayed.Internal
import           Data.Massiv.Core.Common
import           Prelude                            as P


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
-- >> makeArray :: Construct r ix e => Comp -> ix -> (ix -> e) -> Array r ix e
--
-- Runs a function `ix -> e` at every array index. This is inefficient if there
-- is a substantial amount of repeated computation that could be shared while
-- constructing elements on the same dimension, Also since there are no monadic
-- construction functions yet that use `ix -> m e`, this can't be accomplished
-- by using some kind of IORef approach. The expand family of functions make
-- this possible. First you construct an `Array r (Lower ix) a` of one fewer
-- dimensions where `a` is something like `Vector a` or `Array r Ix1 a`. Then
-- you use 'expandWithin`' and a creation function `a -> Int -> b) to create an
-- `Array D ix b`.
--
-- @since 0.2.6
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
  :: (Index ix, Manifest r (Lower ix) t)
  => Int
  -> (t -> Int -> e)
  -> Array r (Lower ix) t
  -> Array D ix e
expandOuter k f arr = expandWithin' (pred $ dimensions $ size arr) k f arr
{-# INLINE expandOuter #-}

-- | Similar to `expandWithin`, except it uses the innermost dimension.
--
-- @since 0.2.6
expandInner
  :: (Index ix, Manifest r (Lower ix) t)
  => Int
  -> (t -> Int -> e)
  -> Array r (Lower ix) t
  -> Array D ix e
expandInner k f arr = expandWithin' 1 k f arr
{-# INLINE expandInner #-}
