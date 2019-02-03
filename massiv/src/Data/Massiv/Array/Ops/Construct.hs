{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
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
  , empty
  , replicate
  , replicateR
    -- *** Applicative
  , makeArrayA
  , makeArrayAR
    -- ** Enumeration
  , range
  , rangeStepM
  , rangeStep'
  , rangeStep
  , rangeInclusive
  , rangeStepInclusiveM
  , rangeSize
  , rangeStepSize
  , enumFromN
  , enumFromStepN
  --, enumFromTo
    -- ** Expansion
  , expandWithin
  , expandWithin'
  , expandOuter
  , expandInner
  ) where

import           Control.Applicative            hiding (empty)
import           Control.Monad.ST
import           Data.Massiv.Array.Delayed.Pull
--import           Data.Massiv.Array.Delayed.Push
import           Data.Massiv.Core.Common
--import           Data.Massiv.Scheduler          (traverse_)
import           Prelude                        as P hiding (enumFromTo,
                                                      replicate)

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
makeArrayR :: Construct r ix e => r -> Comp -> Sz ix -> (ix -> e) -> Array r ix e
makeArrayR _ = makeArray
{-# INLINE makeArrayR #-}


-- | Same as `makeArrayR`, but restricted to 1-dimensional arrays.
makeVectorR :: Construct r Ix1 e => r -> Comp -> Sz1 -> (Ix1 -> e) -> Array r Ix1 e
makeVectorR _ = makeArray
{-# INLINE makeVectorR #-}


newtype STA r ix a = STA {_runSTA :: forall s. MArray s r ix a -> ST s (Array r ix a)}

runSTA :: Mutable r ix e => Sz ix -> STA r ix e -> Array r ix e
runSTA !sz (STA m) = runST (unsafeNew sz >>= m)
{-# INLINE runSTA  #-}

-- | Similar to `makeArray`, but construct the array sequentially using an `Applicative` interface
-- disregarding the supplied `Comp`.
--
-- /Note/ - using `Data.Massiv.Array.Mutable.generateArray` will always be faster, althought not always possible.
--
--
-- @since 0.2.6
--
makeArrayA :: (Mutable r ix e, Applicative f) => Comp -> Sz ix -> (ix -> f e) -> f (Array r ix e)
makeArrayA !comp !sz f =
  let n = totalElem sz
      go !i
        | i < n =
          liftA2
            (\e (STA st) -> STA (\ma -> unsafeLinearWrite ma i e >> st ma))
            (f (fromLinearIndex sz i))
            (go (i + 1))
        | otherwise = pure (STA (unsafeFreeze comp))
   in runSTA sz <$> go 0
{-# INLINE makeArrayA  #-}


-- | Same as `makeArrayA`, but with ability to supply result array representation.
--
-- @since 0.2.6
--
makeArrayAR :: (Mutable r ix e, Applicative f) => r -> Comp -> Sz ix -> (ix -> f e) -> f (Array r ix e)
makeArrayAR _ = makeArrayA
{-# INLINE makeArrayAR #-}

-- |
--
-- @since 0.3.0
replicate :: Construct r ix e => Comp -> Sz ix -> e -> Array r ix e
replicate comp sz e = makeArray comp sz (const e)
{-# INLINE replicate #-}


-- |
--
-- @since 0.3.0
replicateR :: Construct r ix e => r -> Comp -> Sz ix -> e -> Array r ix e
replicateR _ comp sz e = makeArray comp sz (const e)
{-# INLINE replicateR #-}




-- | Create an array of indices with a range from start to finish (not-including), where indices are
-- incrimeted by one.
--
-- prop> range comp from to == rangeStep comp from 1 to
--
-- >>> range Seq 1 6
-- (Array D Seq (5)
--   [ 1,2,3,4,5 ])
-- >>> fromIx2 <$> range Seq (-1) (2 :. 2)
-- (Array D Seq (Sz2 (3 :. 3))
--   [ [ (-1,-1),(-1,0),(-1,1) ]
--   , [ (0,-1),(0,0),(0,1) ]
--   , [ (1,-1),(1,0),(1,1) ]
--   ])
--
range :: Index ix => Comp -> ix -> ix -> Array D ix ix
range comp !from !to = rangeSize comp from (Sz (liftIndex2 (-) to from))
{-# INLINE range #-}


-- | Same as `range`, but with a custom step.
--
-- >>> rangeStep Seq 1 2 6
-- (Array D Seq (3)
--   [ 1,3,5 ])
--
rangeStep :: (Index ix) =>
             Comp -- ^ Computation strategy
          -> ix -- ^ Start
          -> ix -- ^ Step (Can't have zeros)
          -> ix -- ^ End
          -> Maybe (Array D ix ix)
rangeStep = rangeStepM
{-# INLINE rangeStep #-}
{-# DEPRECATED rangeStep "In favor of more general `rangeStepM`" #-}

-- | Same as `range`, but with a custom step.
--
-- >>> rangeStep Seq 1 2 6
-- (Array D Seq (3)
--   [ 1,3,5 ])
--
rangeStepM :: (MonadThrow m, Index ix) =>
             Comp -- ^ Computation strategy
          -> ix -- ^ Start
          -> ix -- ^ Step (Can't have zeros)
          -> ix -- ^ End
          -> m (Array D ix ix)
rangeStepM comp !from !step !to
  | foldlIndex (\acc i -> acc || i == 0) False step = throwM $ IndexZeroException step
  | otherwise =
    let dist = liftIndex2 (-) to from
        sz = liftIndex2 div dist step
        r = liftIndex signum $ liftIndex2 mod dist step
     in pure $ rangeStepSize comp from step (Sz (liftIndex2 (+) sz r))
{-# INLINE rangeStepM #-}

-- | Same as `rangeStep`, but will throw an error whenever @step@ contains zeros.
--
-- @since 0.3.0
rangeStep' :: Index ix => Comp -> ix -> ix -> ix -> Array D ix ix
rangeStep' comp from step = either throw id  . rangeStepM comp from step

-- | Just like `range`, except the finish index is included.
--
-- @since 0.3.0
rangeInclusive :: Index ix => Comp -> ix -> ix -> Array D ix ix
rangeInclusive comp ixFrom ixTo =
  rangeSize comp ixFrom (Sz (liftIndex2 (-) (liftIndex (+ 1) ixTo) ixFrom))
{-# INLINE rangeInclusive #-}


-- | Just like `rangeStep`, except the finish index is included.
--
-- @since 0.3.0
rangeStepInclusiveM :: (MonadThrow m, Index ix) => Comp -> ix -> ix -> ix -> m (Array D ix ix)
rangeStepInclusiveM comp ixFrom step ixTo = rangeStepM comp ixFrom step (liftIndex (1 +) ixTo)
{-# INLINE rangeStepInclusiveM #-}


-- | Create an array of specified size with indices starting with some index at position @0@ and
-- incremented by @1@ until the end of the array is reached
--
-- @since 0.3.0
rangeSize :: Index ix =>
               Comp
            -> ix -- ^ @x@ - start value
            -> Sz ix -- ^ @sz@ - Size of resulting array
            -> Array D ix ix
rangeSize comp !from !sz = makeArray comp sz (liftIndex2 (+) from)
{-# INLINE rangeSize #-}

-- | Same as `rangeSize`, but with ability to specify the step.
--
-- @since 0.3.0
rangeStepSize :: Index ix =>
               Comp
            -> ix -- ^ @x@ - start value
            -> ix -- ^ @delta@ - step value
            -> Sz ix -- ^ @sz@ - Size of resulting array
            -> Array D ix ix
rangeStepSize comp !from !step !sz =
  makeArray comp sz (liftIndex2 (+) from . liftIndex2 (*) step)
{-# INLINE rangeStepSize #-}


-- | Same as `enumFromStepN` with step @delta = 1@.
--
-- >>> enumFromN Seq (5 :: Double) 3
-- (Array D Seq (3)
--   [ 5.0,6.0,7.0 ])
--
enumFromN :: Num e =>
             Comp
          -> e -- ^ @x@ - start value
          -> Sz1 -- ^ @n@ - length of resulting vector.
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
              -> Sz1 -- ^ @n@ - length of resulting vector
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
    szl = unSz (size arr)
    sz = Sz (insertDimension szl dim k)
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
    szl = unSz (size arr)
    sz = Sz (insertDim' szl dim k)
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
    sz = consSz (Sz k) szl
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
    sz = snocSz szl (Sz k)
{-# INLINE expandInner #-}
